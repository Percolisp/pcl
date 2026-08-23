# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

package PCLSbcl;
# The ONE place that builds the SBCL command line a PCL runner spawns.
#
# WHY THIS EXISTS (task #344, found by #324 in s399).  The runners that start
# SBCL to run transpiled code — the gate (Pl/t/PCLCore.pm), the sweep
# (sweep-perl-tests.pl), the companion suite (tools/run-perl-suite.pl),
# ./runpcl, tools/pclperl-for-tests and (since s439) ./pcl — each used to
# hand-write its own option string.  They must
# agree about everything that changes what PCL *is* while a test runs (stack
# size, which libraries are loaded, the cache setting), and they agreed only by
# hand.  When one drifts, the difference reads as a PCL bug in whichever
# measurement is the odd one out:
#
#   * s399/#324: run-perl-suite.pl had NO --control-stack-size, so the
#     companion suite ran PCL on SBCL's 2 MB default while every other
#     measurement used 512 MB.  Four files died `control-stack-exhausted`
#     THERE and nowhere else; one had its snapshot row blaming `(?{ CODE })`
#     and a task filed against the wrong cause.  The probes in that task did
#     not reproduce because they went through ./runpcl, which had the flag.
#   * earlier: the sweep's `--load` of cl/pcl-test.lisp was the same shape.
#
# So: the prefix — everything between `sbcl` and the caller's own --load/--eval
# arguments — is built here, once.  Callers still choose WHAT to load; they no
# longer choose the stack size, the banner flags, or the --core placement.
#
# NB: --core is a C RUNTIME option and must precede the toplevel options, or
# SBCL aborts with "C runtime option --core in the middle of Lisp options".
use strict;
use warnings;
use Exporter 'import';
use Cwd        qw(abs_path);
use Digest::SHA qw(sha1_hex);
use Fcntl      qw(:flock);
use File::Path qw(make_path);
our @EXPORT_OK = qw(sbcl_prefix sbcl_prefix_str cached_core core_cache_dir clear_cached_cores);

# Control stack, MB.  PCL recurses deeply in both the compiler and the runtime;
# SBCL's 2 MB default is not enough (#324).  Changing the value is a decision
# about all four runners at once, which is the point of it living here.
our $STACK_MB = 512;

# The arguments between `sbcl` and the caller's own --load/--eval, as a LIST.
#   core mode:    --core <core> --control-stack-size N --noinform --non-interactive
#   source mode:  --control-stack-size N --noinform --non-interactive --load <runtime>
#
# Options:
#   runtime  => path to cl/pcl-runtime.lisp (source mode; omit to load nothing)
#   core     => path to a saved core, or '' / undef for source mode
#   env_core => 1: take the core from $ENV{PCL_TEST_CORE} when it is fresher
#               than the runtime (the gate's contract — a hand-set stale core
#               can never mask a runtime edit).  An explicit `core` wins.
#   stack_mb => override $STACK_MB for this call
#
# RESOLUTION ORDER for the core (first hit wins):
#   1. an explicit `core`                      (the caller knows best)
#   2. $ENV{PCL_TEST_CORE}, when `env_core`    (the gate's contract, freshness-checked)
#   3. the INSTALLED core <root>/pcl.core      (tools/install-pcl's product, below)
#   4. the CACHED core                          (built once per runtime change, below)
#   5. source mode: --load <runtime>            (the fallback; PCL_NO_CORE=1 forces it)
# PCL_NO_CORE=1 skips 4 — no cache is built or used; a checkout then runs the
# runtime from source — and leaves the explicit asks and the INSTALLED core
# alone (that one is an install's product, not a cache).
#
# THE INSTALLED CORE (task #277).  `tools/install-pcl` compiles the runtime
# into <installed-root>/pcl.core at install time — the USER-ruled model, the
# same one the XS bridge uses: the compile happens once, on the target machine,
# never inside a user's program.  So when a runner asks for source mode and a
# core is sitting beside the runtime's directory, that core IS the install and
# is used.  The freshness test is the same one PCL_TEST_CORE gets, so a core
# older than the runtime next to it is ignored rather than trusted — in a
# CHECKOUT there is no pcl.core at all, which is why this cannot change what
# any development runner spawns (verify with PCL_SHOW_SBCL=1).
sub sbcl_prefix {
    my (%o) = @_;
    my @base = ('--control-stack-size', $o{stack_mb} // $STACK_MB,
                '--noinform', '--non-interactive');
    my $core = $o{core};
    if ($o{env_core} && !(defined $core && length $core)) {
        my $c = $ENV{PCL_TEST_CORE};
        $core = $c if $c && $c ne '1' && -f $c && _fresh($c, $o{runtime});
    }
    $core = _installed_core($o{runtime}) unless defined $core && length $core;
    $core = cached_core($o{runtime})
        unless $ENV{PCL_NO_CORE} || (defined $core && length $core);
    return ('--core', $core, @base) if defined $core && length $core;
    return (@base, defined $o{runtime} ? ('--load', $o{runtime}) : ());
}

# <root>/pcl.core for an INSTALLED tree, whose shape is <root>/cl/<runtime>.
# The `/cl/` in the pattern is deliberate: matching on "two directories up"
# would make the answer depend on whatever happens to sit beside an arbitrary
# caller's runtime path, which is exactly the kind of accidental coupling the
# one-command-line-builder exists to prevent.
sub _installed_core {
    my ($runtime) = @_;
    return undef unless defined $runtime && $runtime =~ m{\A(.*)/cl/[^/]+\z};
    my $core = "$1/pcl.core";
    return (-f $core && _fresh($core, $runtime)) ? $core : undef;
}

# The same thing as a shell string, starting with `sbcl`.
#
# `quote`: whether PATH arguments are quotemeta'd (\Q..\E).  The three string
# callers disagreed about this before the move and the difference is preserved
# so that the #344 command lines are byte-identical to the ones they replace:
# run-perl-suite.pl quoted, the sweep and ./runpcl did not.  Nothing in the
# repo has a path with a shell metacharacter, so the unquoted callers work —
# but they would break on one, which is why quote => 1 is the default.
sub sbcl_prefix_str {
    my (%o) = @_;
    my $quote = exists $o{quote} ? delete $o{quote} : 1;
    my @args  = sbcl_prefix(%o);
    my %is_path;   # only the path arguments get quoted, never the flags
    for my $i (0 .. $#args - 1) {
        $is_path{$i + 1} = 1 if $args[$i] eq '--core' || $args[$i] eq '--load';
    }
    return join ' ', 'sbcl',
        map { $quote && $is_path{$_} ? quotemeta($args[$_]) : $args[$_] } 0 .. $#args;
}

# THE CACHED CORE (USER, s439): by default the runtime is kept COMPILED and
# CACHED.  `--load`ing cl/pcl-runtime.lisp compiles ~17k lines on EVERY sbcl
# spawn (~1.2 s); a saved core loads in ~3 ms.  tools/prove-core and
# tools/install-pcl already built cores for the gate and for an install; this
# makes the same thing the DEFAULT for every runner, in a checkout, with no
# step to remember: the first spawn builds the core, every later spawn uses it.
#
# WHY IT CANNOT GO STALE — the core's file NAME is its key:
#   <cache>/core/pcl-<path8>-<content12>.core
# where <path8> hashes the runtime's ABSOLUTE PATH (a core captures
# *pcl-runtime-directory* at load time, so two checkouts — main and a
# worktree — must never share one even when their runtimes are byte-identical),
# and <content12> hashes the runtime SOURCE plus `sbcl --version` (a core only
# starts on the SBCL that built it), ~/.sbclrc's size+mtime (it is what makes
# Quicklisp's cl-ppcre visible inside the image) and a format version.  Edit
# the runtime, upgrade SBCL, touch .sbclrc: the name changes and the next spawn
# builds a new core; the previous ones for that path are pruned.  Nothing is
# compared by mtime, so no "stale core masks a runtime edit" case exists —
# the failure tools/prove-core's per-run rebuild was designed against.
#
# What is IN the core: cl/pcl-runtime.lisp and what its load pulls in
# (cl-ppcre).  The extensions (pcl-pack / pcl-mro / pcl-warnings / pcl-xs) are
# loaded LAZILY at first use from *pcl-runtime-directory*, so they are read
# fresh from the tree and never need to invalidate the core; cl/pcl-test.lisp
# and cl/skip-registry.lisp are --load'ed by the callers after the prefix, as
# before.  That is also why the key hashes ONE file.
#
# Concurrency: eight gate files starting at once on a cold cache take an
# flock on one lock per path; the first builds (~2 s), the rest wait and find
# the core.  The build is atomic (tmp + rename): a half-written core is never
# a core.  A FAILED build leaves a .failed marker so the next spawns do not
# each pay a failing build — it expires after an hour, and `pcl --make-core`
# (`cached_core(..., force => 1)`) ignores it.  Failure = source mode, once
# announced.  PCL_SHOW_SBCL=1 shows which core a runner spawns, as always.
our $CORE_KEY_VERSION = 1;    # bump when the key's ingredients change

sub core_cache_dir {
    return ($ENV{PCL_CACHE_DIR} // "$ENV{HOME}/.pcl-cache") . "/core";
}

my %CORE_FOR;          # abs runtime path -> core path (or '' = none), per process
my $SBCL_IDENTITY;     # `sbcl --version`, per process

sub cached_core {
    my ($runtime, %opt) = @_;
    return undef unless defined $runtime && -f $runtime;
    my $abs = abs_path($runtime) // return undef;
    return $CORE_FOR{$abs} || undef if exists $CORE_FOR{$abs} && !$opt{force};
    my $ident = _sbcl_identity();
    return $CORE_FOR{$abs} = '' unless defined $ident;     # no sbcl: nothing to build with
    my $content = do { local $/; open my $fh, '<:raw', $abs or return undef; <$fh> };
    my $pathkey = substr(sha1_hex($abs), 0, 8);
    my $ckey    = substr(sha1_hex(join "\0", $content, $ident, _sbclrc_stamp(),
                                              $ENV{SBCL_HOME} // '', $CORE_KEY_VERSION), 0, 12);
    my $dir  = core_cache_dir();
    my $core = "$dir/pcl-$pathkey-$ckey.core";
    if (-f $core && !$opt{force}) { return $CORE_FOR{$abs} = $core }
    my $built = _build_cached_core($abs, $dir, $pathkey, $core, $opt{force});
    $CORE_FOR{$abs} = $built // '';
    return $built;
}

sub _sbcl_identity {
    return $SBCL_IDENTITY if defined $SBCL_IDENTITY;
    my $v = qx{sbcl --version 2>/dev/null};
    return undef if $? != 0 || !defined $v || $v !~ /\S/;
    $v =~ s/\s+\z//;
    return $SBCL_IDENTITY = $v;
}

sub _sbclrc_stamp {
    my $rc = "$ENV{HOME}/.sbclrc";
    return 'none' unless -f $rc;
    my @st = stat $rc;
    return "$st[7]:$st[9]";
}

sub _build_cached_core {
    my ($runtime, $dir, $pathkey, $core, $force) = @_;
    eval { make_path($dir) unless -d $dir; 1 } or return undef;
    my $failed = "$core.failed";
    if (!$force && -f $failed && (time - (stat $failed)[9]) < 3600) { return undef }
    open my $lk, '>>', "$dir/pcl-$pathkey.lock" or return undef;
    flock($lk, LOCK_EX) or do { close $lk; return undef };
    if (-f $core && !$force) { close $lk; return $core }   # a sibling built it while we waited
    my $tmp = "$core.tmp.$$";
    # Progress for a HUMAN (the first spawn pauses ~2 s): only when stderr is a
    # terminal, or on request -- a caller capturing the runner's stderr (the
    # installer's smoke test, a probe diffing output) must not see it.  The
    # FAILURE below is unconditional.
    print STDERR "PCL: compiling the runtime into a cached core (once per runtime change): $core\n"
        if -t STDERR || $ENV{PCL_SHOW_SBCL};
    my $cmd = join ' ', 'sbcl', '--noinform', '--non-interactive',
                        '--load', quotemeta($runtime),
                        '--eval', quotemeta(qq{(sb-ext:save-lisp-and-die "$tmp" :executable nil)});
    my $out = qx{$cmd 2>&1};
    if ($? != 0 || !-f $tmp) {
        unlink $tmp;
        if (open my $mf, '>', $failed) { print $mf $out; close $mf }
        close $lk;
        print STDERR "PCL: cached core build FAILED — running the runtime from source "
                   . "(details in $failed; retried after an hour or on `pcl --make-core`)\n";
        return undef;
    }
    unless (rename $tmp, $core) { unlink $tmp; close $lk; return undef }
    unlink $failed;
    for my $old (glob("\Q$dir\E/pcl-$pathkey-*.core")) {   # this runtime's older cores
        unlink $old unless $old eq $core;
    }
    close $lk;
    return $core;
}

# Remove every cached core (and marker); the next spawn rebuilds.  Returns the
# number of files removed.  `pcl --clear-cache` calls this.
sub clear_cached_cores {
    my $dir = core_cache_dir();
    my @files = (glob("\Q$dir\E/pcl-*.core"), glob("\Q$dir\E/pcl-*.failed"),
                 glob("\Q$dir\E/pcl-*.lock"));
    %CORE_FOR = ();
    return @files ? unlink(@files) : 0;
}

# A core is usable only if it is at least as new as the runtime it must reflect.
sub _fresh {
    my ($core, $runtime) = @_;
    return 0 unless defined $runtime && -f $core && -f $runtime;
    return (stat $core)[9] >= (stat $runtime)[9];
}

1;
