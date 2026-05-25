# PCL Command Plan

## Goal

Replace the current `run-pcl.sh` / manual-SBCL workflow with two polished commands:

- **`pcl`** — run a Perl script (like `perl`); caches a compiled FASL for fast repeated runs
- **`pclbuild`** — compile a script to a redistributable FASL or a standalone executable

---

## Shell environment setup

Add the PCL project root to `PATH`.  All three scripts (`pl2cl`, `pcl`,
`pclbuild`) live there and find their siblings via `dirname(abs_path($0))`, so
no `PERL5LIB` or other variables are required.

```bash
# ~/.bashrc or ~/.bash_profile
export PATH="$HOME/pcl:$PATH"   # adjust to wherever you cloned the repo
```

SBCL must be on PATH:

```bash
# Debian/Ubuntu
sudo apt install sbcl

# macOS (Homebrew)
brew install sbcl

# Verify
sbcl --version
```

Optional environment overrides:

| Variable | Default | Purpose |
|---|---|---|
| `PCL_CACHE_DIR` | `~/.pcl-cache` | FASL cache directory |
| `PCL_CORE` | `~/.pcl-cache/pcl.core` | Saved SBCL core (see §Saved core) |
| `PCL_NO_CACHE` | `0` | Set to `1` to act as if `--no-fasl` always |

---

## `pcl` command

### Interface

```
pcl [options] script.pl [script-args...]
pcl [options] -e 'code'  [script-args...]
```

| Flag | Meaning |
|---|---|
| `-e 'code'` | Inline code (like `perl -e`) |
| `-I dir` | Prepend *dir* to `@INC` |
| `-M Module` | `use Module` before the script |
| `-w` | Enable warnings (passed to transpiler) |
| `-c` | Syntax-check only; print "Syntax OK" and exit |
| `--no-fasl` | Run from `.lisp` without compiling or caching |

### Caching behaviour

By default `pcl` caches a compiled FASL of the main script in `$PCL_CACHE_DIR`:

```
script.pl (mtime T) ──pl2cl──► /tmp/pcl-PID.lisp
                                    │
                               compile-file
                                    │
                    ~/.pcl-cache/MD5HASH.fasl   (cache key = md5(abs-path + mtime))
```

On the next run with the same source and mtime the `.fasl` is loaded directly —
no transpilation, no compile-file, no `.lisp` I/O.  With a saved core (see §)
the full startup drops to ≈0.3 s.

`--no-fasl` skips both the compile and the write; the `.lisp` is left in `/tmp`
and deleted after exit.  Useful during active development where the script
changes on every run.

Module `use`/`require` already go through the existing FASL cache
(`p-load-module-cached`); `pcl` does not change that.

### SBCL invocation

```
sbcl --noinform --non-interactive
     [--core   ~/.pcl-cache/pcl.core     ]  if core exists and is current
     [--load   cl/pcl-runtime.lisp       ]  otherwise
     --eval   "(setf pcl::@ARGV ...)"
     --load   HASH.fasl | script.lisp
```

`@ARGV` is injected via an `--eval` form before `--load` so the script sees its
command-line arguments on startup.

---

## `pclbuild` command

### Interface

```
pclbuild [options] script.pl
```

| Flag | Meaning |
|---|---|
| `-o FILE, --output FILE` | Output path (default: `script.fasl` or `script`) |
| `--exe` | Produce a standalone binary (no SBCL at runtime) |
| `--eval-lib Module::Name` | Pre-load library so `eval "use Module"` works in the exe (repeatable) |
| `-v, --verbose` | Print build steps |

### Mode A — FASL only (default)

```
pclbuild script.pl           →  script.fasl
```

Steps:
1. `pl2cl script.pl > script.tmp.lisp`
2. `sbcl --load runtime.lisp --eval '(compile-file "script.tmp.lisp" :output-file "script.fasl")' --eval '(exit)'`
3. Delete `script.tmp.lisp`

To run the FASL:
```
sbcl --load cl/pcl-runtime.lisp --load script.fasl
```

This is the right artifact for scripts shipped within an environment that
already has PCL and SBCL installed.

### Mode B — standalone executable (`--exe`)

```
pclbuild --exe [--eval-lib Mod::Name ...] script.pl  →  ./script
```

Uses `sb-ext:save-lisp-and-die` to produce a self-contained binary (SBCL core
embedded).  The binary works on any machine with the same OS/arch — no Perl, no
SBCL, no PCL installation required.

**Required transpiler change (`pl2cl --build-mode`):**  
In normal mode all top-level statements execute at load time.  For `--exe` they
must execute at *run* time (when the binary is invoked).  `--build-mode` wraps
all executable top-level statements (everything that is not a `defun`, `defvar`,
`defmacro`, or `eval-when`) in a generated `(defun pcl::pl-__pcl_main__ () ...)`.
`BEGIN` blocks still run at build time (correct Perl semantics).

The `save-lisp-and-die` toplevel calls `pl-__pcl_main__` and then exits:

```lisp
(lambda ()
  (handler-case
      (progn (pcl::pl-__pcl_main__)
             (sb-ext:exit :code 0))
    (sb-ext:exit (e) (sb-ext:exit :code (sb-ext:exit-code e)))
    (error (e) (format *error-output* "~A~%" e)
               (sb-ext:exit :code 1))))
```

**`--eval-lib` mechanics:**  
Libraries listed with `--eval-lib` are transpiled and compiled into the image at
build time.  A small shim registers them as pre-loaded so `p-use`/`p-require`
short-circuits immediately:

```lisp
(setf (gethash "Foo::Bar" pcl::*pcl-preloaded-eval-libs*) t)
```

When the running binary calls `eval "use Foo::Bar; ..."`, `p-use` sees the key
and returns immediately without spawning a transpiler subprocess.

---

## Saved core (biggest startup win)

An SBCL saved core bundles the compiled runtime into a single file.  Loading it
replaces the 0.5–1.0 s of runtime compilation with a ≈0.05 s `mmap`.

```bash
# Build the core (one-time, takes ~5 s)
pcl --make-core          # or: pcl-make-core
# → writes ~/.pcl-cache/pcl.core

# Verify startup speed
time pcl -e 'print "hi\n"'
```

`pcl` uses the core automatically whenever it exists and is newer than
`cl/pcl-runtime.lisp`.  If the runtime changes (new session, new bugfix),
running `pcl --make-core` rebuilds it.

The `pcl-make-core` logic:

```bash
sbcl --noinform --non-interactive \
     --load cl/pcl-runtime.lisp \
     --eval '(sb-ext:save-lisp-and-die "~/.pcl-cache/pcl.core" :executable nil)'
```

---

## Runtime change: `*pcl-preloaded-eval-libs*`

One new defvar in `cl/pcl-runtime.lisp`:

```lisp
(defvar *pcl-preloaded-eval-libs* (make-hash-table :test 'equal)
  "Libraries pre-loaded at exe build time. p-use short-circuits for these.")
```

One guard at the top of `p-use`:

```lisp
(defun p-use (module-name &key imports)
  ;; Fast path: already loaded (including by pclbuild --eval-lib)
  (let ((n (to-string module-name)))
    (when (or (gethash n %INC)
              (gethash n *pcl-preloaded-eval-libs*))
      (return-from p-use t)))
  ...)
```

---

## Code sketches

### `pcl`

```perl
#!/usr/bin/env perl
use v5.30;
use strict; use warnings;
use Getopt::Long qw(:config pass_through require_order);
use File::Basename;
use Cwd qw(abs_path);
use Digest::MD5 qw(md5_hex);

my $ROOT    = dirname(abs_path($0));
my $PL2CL   = "$ROOT/pl2cl";
my $RUNTIME = "$ROOT/cl/pcl-runtime.lisp";

my ($no_fasl, $fasl_dir, $check_only, $make_core);
my ($inline, @inc_dirs, @use_mods);
GetOptions(
  'no-fasl'    => \$no_fasl,
  'fasl-dir=s' => \$fasl_dir,
  'c'          => \$check_only,
  'make-core'  => \$make_core,
  'e=s'        => \$inline,
  'I=s@'       => \@inc_dirs,
  'M=s@'       => \@use_mods,
) or die usage();

$no_fasl  = 1 if $ENV{PCL_NO_CACHE};
$fasl_dir //= $ENV{PCL_CACHE_DIR} // "$ENV{HOME}/.pcl-cache";
mkdir $fasl_dir, 0755 unless -d $fasl_dir;

# --- Build a saved core on request
if ($make_core) { make_core(); exit 0 }

my $source = (!defined $inline && @ARGV) ? shift @ARGV : undef;
my @args   = @ARGV;

# --- Get the file to load into SBCL
my $load_file = get_load_file($source, $inline);

exit 0 if $check_only;

# --- Run
my @argv_strs = map { s/"/\\"/gr } @args;
my $argv_form = '(setf pcl::@ARGV (make-array '
  . scalar(@argv_strs) . ' :adjustable t :fill-pointer '
  . scalar(@argv_strs) . ' :initial-contents (list '
  . join(' ', map { qq("$_") } @argv_strs) . ')))';

my @cmd = ('sbcl', '--noinform', '--non-interactive');
my $core = find_core();
if ($core) { push @cmd, '--core', $core }
else        { push @cmd, '--load', $RUNTIME }
push @cmd, '--eval', $argv_form;
push @cmd, '--load', $load_file;
exec @cmd;

# --- Helpers

sub get_load_file {
  my ($source, $inline) = @_;

  if (defined $source) {
    my $abs = abs_path($source) or die "pcl: not found: $source\n";

    if (!$no_fasl) {
      my $mtime = (stat $abs)[9];
      my $key   = md5_hex("$abs:$mtime");
      my $fasl  = "$fasl_dir/$key.fasl";
      my $lisp  = "$fasl_dir/$key.lisp";

      unless (-f $fasl && (stat $fasl)[9] > $mtime) {
        transpile_file($abs, $lisp);
        compile_to_fasl($lisp, $fasl);
      }
      return $fasl if -f $fasl;
      return $lisp;      # fall back to .lisp if compile failed
    } else {
      my $tmp = "/tmp/pcl-$$.lisp";
      transpile_file($abs, $tmp);
      # register cleanup on exit
      $SIG{__DIE__} = sub { unlink $tmp };
      END { unlink $tmp if $tmp && -f $tmp }
      return $tmp;
    }

  } elsif (defined $inline) {
    my $tmp  = "/tmp/pcl-$$.lisp";
    my $code = join('', map { "use $_;\n" } @use_mods) . $inline . "\n";
    open my $fh, '|-', "perl", $PL2CL, ">$tmp"
      or die "pcl: can't run pl2cl: $!\n";
    print $fh $code; close $fh;
    # simpler: use shell redirect
    my $escaped = $code =~ s/'/'"'"'/gr;
    system("echo '$escaped' | perl \Q$PL2CL\E > \Q$tmp\E") == 0
      or die "pcl: transpile failed\n";
    END { unlink $tmp if $tmp && -f $tmp }
    return $tmp;

  } else {
    die usage();
  }
}

sub transpile_file {
  my ($src, $dest) = @_;
  my @extra = $no_fasl ? ('--no-cache') : ();
  system("perl \Q$PL2CL\E @extra \Q$src\E > \Q$dest\E") == 0
    or die "pcl: transpile failed\n";
}

sub compile_to_fasl {
  my ($lisp, $fasl) = @_;
  my @load = find_core() ? ('--core', find_core()) : ('--load', $RUNTIME);
  my $rc = system('sbcl', '--noinform', '--non-interactive',
                  @load,
                  '--eval', qq{(compile-file "$lisp" :output-file "$fasl")},
                  '--eval', '(sb-ext:exit)');
  warn "pcl: FASL compile failed, using .lisp\n" if $rc;
}

sub find_core {
  my $c = $ENV{PCL_CORE} // "$fasl_dir/pcl.core";
  return $c if -f $c && (stat $c)[9] > (stat $RUNTIME)[9];
  return undef;
}

sub make_core {
  my $c = $ENV{PCL_CORE} // "$fasl_dir/pcl.core";
  say "Building PCL core: $c";
  system('sbcl', '--noinform', '--non-interactive',
         '--load', $RUNTIME,
         '--eval', qq{(sb-ext:save-lisp-and-die "$c" :executable nil)}) == 0
    or die "Core build failed\n";
  say "Done. Startup should now be ~5× faster.";
}

sub usage { "Usage: pcl [-e code] [-I dir] [-M mod] [-c] [--no-fasl] script.pl [args]\n" }
```

### `pclbuild`

```perl
#!/usr/bin/env perl
use v5.30;
use strict; use warnings;
use Getopt::Long;
use File::Basename;
use Cwd qw(abs_path);

my $ROOT    = dirname(abs_path($0));
my $PL2CL   = "$ROOT/pl2cl";
my $RUNTIME = "$ROOT/cl/pcl-runtime.lisp";

my ($exe, $output, @eval_libs, $verbose);
GetOptions(
  'exe'          => \$exe,
  'o|output=s'   => \$output,
  'eval-lib=s@'  => \@eval_libs,
  'v|verbose'    => \$verbose,
) or die usage();

my $source = shift @ARGV or die usage();

$exe ? build_exe($source, $output, \@eval_libs)
     : build_fasl($source, $output);

# ----------------------------------------------------------------

sub build_fasl {
  my ($src, $out) = @_;
  my $abs  = abs_path($src) or die "Not found: $src\n";
  $out   //= basename($src, '.pl') . '.fasl';
  my $tmp  = "$out.tmp.lisp";

  say "Transpiling..." if $verbose;
  system("perl \Q$PL2CL\E \Q$abs\E > \Q$tmp\E") == 0
    or die "Transpile failed\n";

  say "Compiling to $out..." if $verbose;
  system('sbcl', '--noinform', '--non-interactive',
         '--load', $RUNTIME,
         '--eval', qq{(compile-file "$tmp" :output-file "$out")},
         '--eval', '(sb-ext:exit)') == 0
    or die "Compile failed\n";

  unlink $tmp;
  say "Built: $out";
  say "Run:   sbcl --load $ROOT/cl/pcl-runtime.lisp --load $out";
}

sub build_exe {
  my ($src, $out, $libs) = @_;
  my $abs = abs_path($src) or die "Not found: $src\n";
  $out  //= basename($src, '.pl');
  my $tmp = "/tmp/pclbuild-$$.lisp";

  open my $fh, '>', $tmp or die "Cannot write $tmp: $!\n";

  # Pre-load eval libraries into the image
  for my $lib (@$libs) {
    say "Pre-loading eval lib: $lib" if $verbose;
    my $lib_cl = `perl \Q$PL2CL\E --module \Q$lib\E`;
    die "Failed to transpile lib: $lib\n" if $?;
    print $fh $lib_cl;
    print $fh qq{(setf (gethash "$lib" pcl::*pcl-preloaded-eval-libs*) t)\n\n};
  }

  # Main script (--build-mode wraps executable statements in pl-__pcl_main__)
  say "Transpiling $src..." if $verbose;
  my $main_cl = `perl \Q$PL2CL\E --build-mode \Q$abs\E`;
  die "Transpile failed\n" if $?;
  print $fh $main_cl;
  close $fh;

  my $toplevel = join ' ',
    '(lambda ()',
    '  (handler-case',
    '    (progn (pcl::pl-__pcl_main__) (sb-ext:exit :code 0))',
    '    (sb-ext:exit (e) (sb-ext:exit :code (sb-ext:exit-code e)))',
    '    (error (e) (format *error-output* "~A~%" e) (sb-ext:exit :code 1))))';

  say "Saving executable $out..." if $verbose;
  system('sbcl', '--noinform', '--non-interactive',
         '--load', $RUNTIME,
         '--load', $tmp,
         '--eval', qq{(sb-ext:save-lisp-and-die "$out" :toplevel $toplevel :executable t :save-runtime-options t)}) == 0
    or die "Executable build failed\n";

  unlink $tmp;
  say "Built: $out";
}

sub usage { "Usage: pclbuild [--exe] [-o output] [--eval-lib Mod ...] script.pl\n" }
```

---

## Files to create / modify

| File | Action | Notes |
|---|---|---|
| `pcl` | **Create** | The main user command |
| `pclbuild` | **Create** | Compiler/packager |
| `cl/pcl-runtime.lisp` | **Modify** | Add `*pcl-preloaded-eval-libs*` defvar + p-use guard |
| `pl2cl` | **Modify** | Add `--build-mode` flag wrapping exec stmts in `pl-__pcl_main__` |
| `Makefile` (optional) | **Create** | `make install`, `make core`, `make test` targets |

---

## Implementation order

1. **`pcl` script** — highest value; mostly plumbing around existing
   infrastructure.  The one new piece is main-script FASL caching
   (`get_load_file` → `compile_to_fasl`).  No runtime changes needed.

2. **Saved-core support** (`pcl --make-core` / `find_core`) — single
   `save-lisp-and-die` call, ~5× startup win.  Can be done in the same commit
   as `pcl`.

3. **`pclbuild --fasl`** — simple: transpile + `compile-file`.  Does not need
   `--build-mode` or runtime changes.

4. **`*pcl-preloaded-eval-libs*` runtime change** — 3 lines in the runtime;
   needed before `--exe` is useful.

5. **`pl2cl --build-mode`** — transpiler change to wrap exec statements in
   `pl-__pcl_main__`; required for `pclbuild --exe` to produce correct
   executables.

6. **`pclbuild --exe`** — depends on steps 4 and 5.

---

## Open questions (to decide before implementing)

1. **Naming**: `--no-fasl` vs `--no-cache` (mirrors `PCL_NO_CACHE` env var).

2. **`-e` and inline code with FASL**: should `pcl -e 'code'` ever cache?
   Suggested answer: no — the code is ephemeral and the md5 key would grow
   the cache unboundedly.

3. **`pclbuild` as separate script or `pcl --build` subcommand?**
   Separate script is simpler and mirrors the `perl` / `perlbrew` precedent.

4. **`--build-mode` exact semantics**: does a top-level `my $x = foo()` (side
   effect at parse time in Perl) count as executable or declarative?  In
   generated CL it becomes `(defvar $x ...)` followed by `(p-setf $x (pl-foo))`
   — the `defvar` is declarative, the `setf` is executable and should go
   in `pl-__pcl_main__`.

5. **Executable and `eval "string"`**: if the binary is installed in a location
   without `pl2cl`, string eval will fail unless all needed libs were listed
   with `--eval-lib` at build time.  Document as a known limitation.
