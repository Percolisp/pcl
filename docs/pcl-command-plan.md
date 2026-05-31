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
| `PCL_CACHE_DIR` | `~/.pcl-cache` | FASL cache directory (used only with `--cache`) |
| `PCL_CORE` | `~/.pcl-cache/pcl.core` | Saved SBCL core (see §Saved core) |
| `PCL_CACHE` | `0` | Set to `1` to make `--cache` the default |

---

## `pcl` command

### Interface

```
pcl [options] script.pl [script-args...]
pcl [options] -e 'code'  [script-args...]
pcl [options] -E 'code'  [script-args...]
```

| Flag | Meaning |
|---|---|
| `-e 'code'` | Inline code (like `perl -e`) |
| `-E 'code'` | Alias of `-e` (identical; provided for `perl -E` muscle memory) |
| `-I dir` | Prepend *dir* to `@INC` |
| `-M Module` | `use Module` before the script |
| `-w` | Enable warnings (passed to transpiler) |
| `-c` | Syntax-check only; print "Syntax OK" and exit |
| `--cache` | Use the managed md5 FASL cache in `$PCL_CACHE_DIR` |
| `--fasl PATH` (`-o PATH`) | Compile to *PATH* and run it; reuse if newer than source |
| `--clear-cache` | Delete all cached `.fasl`/`.lisp` files in `$PCL_CACHE_DIR`, then exit |

### Caching behaviour

**Default: no FASL.**  `pcl script.pl` transpiles to a temp `.lisp`, loads it,
and deletes the temp on exit.  Nothing is written to any cache.  This is the
simplest path and the right one for active development where the script changes
on every run.

There are two opt-in ways to persist a compiled FASL:

**`--cache` — managed cache (auto-keyed, auto-invalidated):**

```
script.pl (mtime T) ──pl2cl──► /tmp/pcl-PID.lisp
                                    │
                               compile-file
                                    │
                    ~/.pcl-cache/MD5HASH.fasl   (cache key = md5(abs-path + mtime))
```

On the next `--cache` run with the same source and mtime the `.fasl` is loaded
directly — no transpilation, no compile-file, no `.lisp` I/O.  With a saved core
(see §) the full startup drops to ≈0.3 s.  Set `PCL_CACHE=1` to make `--cache`
the default.

**`--fasl PATH` (alias `-o PATH`) — explicit FASL you control:**

```
pcl --fasl build/app.fasl app.pl        # compiles to build/app.fasl, then runs it
pcl --fasl build/app.fasl app.pl        # second run reuses it (PATH newer than app.pl)
```

This is the simple way to save a FASL anywhere on disk without the md5 cache.
It compiles only when *PATH* is missing or older than the source, otherwise
loads it directly.  (For a fully detached redistributable FASL or a standalone
binary, use `pclbuild` — see below.)

`-e`/`-E` inline code **never** caches (the code is ephemeral and an md5 key
would grow the cache unboundedly); `--cache`/`--fasl` are ignored with inline code.

Module `use`/`require` already go through the existing FASL cache
(`p-load-module-cached`); `pcl` does not change that.

### Clearing the cache (`--clear-cache`)

```
pcl --clear-cache
```

Deletes every `.fasl` and `.lisp` file in `$PCL_CACHE_DIR` and exits.  The
expensive saved core (`pcl.core`) is **left intact** — rebuild it with
`--make-core` if you need to.

This is the escape hatch for the one cache-soundness gap described next.

### Caveat: compile-time interface drift

The module cache (`p-load-module-cached`) and the optional main-script `--cache`
are both **per-file, keyed on absolute-path + mtime** (cache key = `sxhash`
of the path; validity = `cache-mtime > source-mtime` AND `age < 7 days`).
`use B`/`require "c.pl"` are transpiled to *runtime calls* (`(p-use "B")`,
`(p-require-file "c.pl")`) that re-check each dependency's mtime when the
enclosing FASL loads — so editing `b.pl`/`c.pl` (which bumps their mtime) *does*
trigger their recompilation, recursively, including any newly-added `use`
statements in the changed file.

The gap is **compile-time interface drift**: a FASL is compiled against its
dependencies' compile-time interface *at build time* — `use constant` values get
inlined, prototypes affect parsing, import lists are resolved.  If you change
**B's compile-time interface** (e.g. a constant's value) but **don't** change
`a.pl`, then B is recompiled (its mtime changed) but `a.fasl` stays "valid"
(a.pl unchanged) and keeps the **stale inlined value**.  Real Perl never hits
this because it recompiles everything from source every run; PCL's per-file FASL
cache does not re-derive `a` when only `b`'s interface changes.  This is the same
hazard that sank Perl's own bytecode-compiler experiments, and it intersects
PCL's long-standing `BEGIN`/load-order sensitivity.

**Mitigations:**
- The **default (no main-script FASL)** sidesteps it entirely — `a.pl` is
  re-transpiled every run, always seeing B's current interface.  The module
  cache underneath still saves compile cost for unchanged modules.
- `pcl --clear-cache` after editing widely-`use`d modules forces a clean rebuild.
- A future hardening (open question below) is to fold transitive dependency
  mtimes (or a content hash) into the cache key so changing `b.pl` invalidates
  `a`'s cache too.

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

my ($use_cache, $fasl_out, $fasl_dir, $check_only, $make_core, $clear_cache);
my ($inline, @inc_dirs, @use_mods);
GetOptions(
  'cache'       => \$use_cache,    # opt in to the managed md5 cache
  'fasl|o=s'    => \$fasl_out,     # explicit FASL path to write/reuse
  'fasl-dir=s'  => \$fasl_dir,
  'c'           => \$check_only,
  'make-core'   => \$make_core,
  'clear-cache' => \$clear_cache,  # wipe cached .fasl/.lisp, then exit
  'e|E=s'       => \$inline,       # -E is an alias of -e
  'I=s@'        => \@inc_dirs,
  'M=s@'        => \@use_mods,
) or die usage();

$use_cache = 1 if $ENV{PCL_CACHE};
$fasl_dir //= $ENV{PCL_CACHE_DIR} // "$ENV{HOME}/.pcl-cache";
mkdir $fasl_dir, 0755 if ($use_cache || $clear_cache) && !-d $fasl_dir;

# --- One-shot maintenance actions (no script needed)
if ($clear_cache) { clear_cache(); exit 0 }
if ($make_core)   { make_core();   exit 0 }

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
    my $abs   = abs_path($source) or die "pcl: not found: $source\n";
    my $mtime = (stat $abs)[9];

    # Tier 2: explicit FASL path (--fasl / -o). Compile if missing/stale, else reuse.
    if (defined $fasl_out) {
      unless (-f $fasl_out && (stat $fasl_out)[9] > $mtime) {
        my $lisp = "/tmp/pcl-$$.lisp";
        transpile_file($abs, $lisp);
        compile_to_fasl($lisp, $fasl_out);
        unlink $lisp;
      }
      return $fasl_out if -f $fasl_out;
      die "pcl: FASL compile failed: $fasl_out\n";
    }

    # Tier 1: managed md5 cache (--cache or PCL_CACHE=1).
    if ($use_cache) {
      my $key  = md5_hex("$abs:$mtime");
      my $fasl = "$fasl_dir/$key.fasl";
      my $lisp = "$fasl_dir/$key.lisp";
      unless (-f $fasl && (stat $fasl)[9] > $mtime) {
        transpile_file($abs, $lisp);
        compile_to_fasl($lisp, $fasl);
      }
      return $fasl if -f $fasl;
      return $lisp;      # fall back to .lisp if compile failed
    }

    # Tier 0 (default): transpile-and-run, no FASL.
    my $tmp = "/tmp/pcl-$$.lisp";
    transpile_file($abs, $tmp);
    END { unlink $tmp if $tmp && -f $tmp }
    return $tmp;

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
  system("perl \Q$PL2CL\E \Q$src\E > \Q$dest\E") == 0
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

sub clear_cache {
  # Remove every cached .fasl/.lisp; leave pcl.core (rebuild with --make-core).
  my @files = (glob("\Q$fasl_dir\E/*.fasl"), glob("\Q$fasl_dir\E/*.lisp"));
  my $n = unlink @files;
  say "Cleared $n cached file(s) from $fasl_dir";
}

sub usage { "Usage: pcl [-e|-E code] [-I dir] [-M mod] [-c] [--cache] [--fasl PATH] [--clear-cache] [--make-core] script.pl [args]\n" }
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

1. **Naming**: ✅ resolved — default is **no FASL**; opt in with `--cache` (managed
   md5 cache) or `--fasl PATH` (explicit file). Env override is `PCL_CACHE=1`.
   `-E` is an alias of `-e`.

2. **`-e` and inline code with FASL**: ✅ resolved — no, inline code never caches
   (ephemeral; md5 key would grow the cache unboundedly). `--cache`/`--fasl` are
   ignored with `-e`/`-E`.

6. **Harden the cache against compile-time interface drift?** (see "Caveat:
   compile-time interface drift" above). Options: (a) leave as-is and rely on the
   no-FASL default + `--clear-cache` (current plan); (b) fold each file's
   transitive dependency mtimes into its cache key; (c) switch the key from
   path+mtime to a content hash of the file *and* its resolved dependency set.
   (b)/(c) make `--cache` sound but require the transpiler to emit each module's
   dependency list so the runtime can hash it. Deferred unless `--cache` becomes
   a common workflow.

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
