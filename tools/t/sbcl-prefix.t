#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# Unit tests for PCLSbcl — the ONE builder of the SBCL command line all four
# PCL runners spawn (task #344).
#
# NOT part of the Pl/t gate: that gate measures the transpiler, this measures
# the harness.  Run it directly:  prove tools/t/sbcl-prefix.t
use strict;
use warnings;
use Test::More tests => 48;
use File::Temp qw(tempdir);
use FindBin qw($RealBin);
use lib "$RealBin/../lib";
use PCLSbcl qw(sbcl_prefix sbcl_prefix_str cached_core core_cache_dir clear_cached_cores);

# The rows up to the CACHED-core block describe the resolution order WITHOUT
# the cached core (explicit > PCL_TEST_CORE > installed > source), so they run
# with the cache switched off; the fake runtimes below are not loadable Lisp
# and must never trigger a build.  The cached core has its own block at the end.
$ENV{PCL_NO_CORE} = 1;

my $dir = tempdir(CLEANUP => 1);
my $rt  = "$dir/pcl-runtime.lisp";
my $core = "$dir/pcl.core";
for my $f ($rt, $core) { open my $fh, '>', $f or die $!; print $fh "x\n"; close $fh }

# --- source mode -----------------------------------------------------------
is_deeply([ sbcl_prefix(runtime => $rt) ],
          ['--control-stack-size', 512, '--noinform', '--non-interactive',
           '--load', $rt],
          'source mode: stack flag first, runtime last');

is_deeply([ sbcl_prefix() ],
          ['--control-stack-size', 512, '--noinform', '--non-interactive'],
          'no runtime: prefix loads nothing (the caller supplies a core-mode load)');

# --- core mode -------------------------------------------------------------
# --core is a C RUNTIME option: SBCL aborts if it appears after a toplevel one.
is_deeply([ sbcl_prefix(core => $core, runtime => $rt) ],
          ['--core', $core, '--control-stack-size', 512,
           '--noinform', '--non-interactive'],
          'core mode: --core FIRST, and the runtime is not source-loaded');

is_deeply([ sbcl_prefix(core => '', runtime => $rt) ],
          ['--control-stack-size', 512, '--noinform', '--non-interactive',
           '--load', $rt],
          'empty core string falls back to source mode');

# --- stack size ------------------------------------------------------------
my @small = sbcl_prefix(runtime => $rt, stack_mb => 64);
is($small[1], 64, 'stack_mb overrides the default');
is($PCLSbcl::STACK_MB, 512, 'the default the four runners share is 512 MB (#324)');

# --- dynamic space (the heap), task #1590 ----------------------------------
# The DEFAULT must emit NO flag: that is what makes every runner's command
# line byte-identical to the ones built before this knob existed, so a per-file
# allowance in baselines/perl-suite-heap.tsv cannot change the other 527 files'
# measurement (or the gate's eight parallel SBCLs).
ok( !grep({ $_ eq '--dynamic-space-size' } sbcl_prefix(runtime => $rt)),
    'no allowance: no --dynamic-space-size, i.e. SBCL\'s own default' );
is($PCLSbcl::DYNAMIC_SPACE_MB, undef, 'and the package default is "unset", not a number');

is_deeply([ sbcl_prefix(runtime => $rt, dynamic_space_mb => 2048) ],
          ['--control-stack-size', 512, '--dynamic-space-size', 2048,
           '--noinform', '--non-interactive', '--load', $rt],
          'dynamic_space_mb sits with the other C RUNTIME options, before --noinform');

is_deeply([ sbcl_prefix(core => $core, runtime => $rt, dynamic_space_mb => 2048) ],
          ['--core', $core, '--control-stack-size', 512,
           '--dynamic-space-size', 2048, '--noinform', '--non-interactive'],
          'core mode: --core still FIRST, the heap flag after the stack flag');

{
  # The environment spelling exists so a CHILD PCL (tools/pclperl-for-tests)
  # inherits its parent file's allowance without a second plumbing path.
  local $ENV{PCL_DYNAMIC_SPACE_MB} = 3072;
  my @p = sbcl_prefix(runtime => $rt);
  is_deeply([@p[0..3]], ['--control-stack-size', 512, '--dynamic-space-size', 3072],
            'PCL_DYNAMIC_SPACE_MB is honoured');
  my @q = sbcl_prefix(runtime => $rt, dynamic_space_mb => 2048);
  is($q[3], 2048, 'an explicit dynamic_space_mb wins over the environment');
}

# A malformed allowance emits NO flag rather than a heap size nobody wrote
# down: `2048MB`, `0` and a word are all "no information", never a silent
# different number (this is the case a looser check would break).
for my $bad ('2048MB', '0', 'lots', '', '-512', '1.5') {
  local $ENV{PCL_DYNAMIC_SPACE_MB} = $bad;
  ok( !grep({ $_ eq '--dynamic-space-size' } sbcl_prefix(runtime => $rt)),
      "a malformed PCL_DYNAMIC_SPACE_MB ('$bad') emits no flag" );
}

# --- env_core: the gate contract -------------------------------------------
{
  local $ENV{PCL_TEST_CORE} = $core;
  utime time - 100, time - 100, $rt;                 # core is NEWER than runtime
  is_deeply([ sbcl_prefix(runtime => $rt, env_core => 1) ],
            ['--core', $core, '--control-stack-size', 512,
             '--noinform', '--non-interactive'],
            'env_core: a core at least as new as the runtime is used');

  utime time + 100, time + 100, $rt;                 # runtime is NEWER than core
  is_deeply([ sbcl_prefix(runtime => $rt, env_core => 1) ],
            ['--control-stack-size', 512, '--noinform', '--non-interactive',
             '--load', $rt],
            'env_core: a core OLDER than the runtime is refused (never masks an edit)');
  utime time - 100, time - 100, $rt;

  local $ENV{PCL_TEST_CORE} = '1';
  is_deeply([ sbcl_prefix(runtime => $rt, env_core => 1) ],
            ['--control-stack-size', 512, '--noinform', '--non-interactive',
             '--load', $rt],
            'env_core: the legacy "1" value is not a path');

  local $ENV{PCL_TEST_CORE} = "$dir/does-not-exist.core";
  is_deeply([ sbcl_prefix(runtime => $rt, env_core => 1) ],
            ['--control-stack-size', 512, '--noinform', '--non-interactive',
             '--load', $rt],
            'env_core: a missing core file falls back to source mode');

  local $ENV{PCL_TEST_CORE} = "$dir/does-not-exist.core";
  is_deeply([ sbcl_prefix(runtime => $rt, core => $core, env_core => 1) ],
            ['--core', $core, '--control-stack-size', 512,
             '--noinform', '--non-interactive'],
            'an explicit core wins over the environment');
}

# --- string form -----------------------------------------------------------
is(sbcl_prefix_str(runtime => '/a b/rt.lisp'),
   'sbcl --control-stack-size 512 --noinform --non-interactive --load '
   . quotemeta('/a b/rt.lisp'),
   'sbcl_prefix_str quotes PATH arguments by default, never the flags');

is(sbcl_prefix_str(runtime => '/a/rt.lisp', quote => 0),
   'sbcl --control-stack-size 512 --noinform --non-interactive --load /a/rt.lisp',
   'quote => 0 reproduces the callers that never quoted (byte-identical move)');

# --- the INSTALLED core (task #277) ----------------------------------------
# tools/install-pcl compiles the runtime into <root>/pcl.core at install time
# and lays the tree out as <root>/cl/pcl-runtime.lisp.  A runner that asks for
# source mode in such a tree gets the core; a CHECKOUT has no pcl.core, which
# is why this cannot change what any development runner spawns.
{
    my $inst = tempdir(CLEANUP => 1);
    mkdir "$inst/cl" or die $!;
    my $irt   = "$inst/cl/pcl-runtime.lisp";
    my $icore = "$inst/pcl.core";
    open my $fh, '>', $irt or die $!; print $fh "x\n"; close $fh;

    is_deeply([ sbcl_prefix(runtime => $irt) ],
              ['--control-stack-size', 512, '--noinform', '--non-interactive',
               '--load', $irt],
              'installed core: absent -> source mode (a checkout is unaffected)');

    open my $cf, '>', $icore or die $!; print $cf "x\n"; close $cf;
    is_deeply([ sbcl_prefix(runtime => $irt) ],
              ['--core', $icore, '--control-stack-size', 512,
               '--noinform', '--non-interactive'],
              'installed core: <root>/pcl.core beside <root>/cl/ is used');

    # Same freshness contract as PCL_TEST_CORE: a core older than the runtime
    # it must reflect is ignored, never trusted.
    utime time - 100, time - 100, $icore;
    is_deeply([ sbcl_prefix(runtime => $irt) ],
              ['--control-stack-size', 512, '--noinform', '--non-interactive',
               '--load', $irt],
              'installed core: a core older than the runtime is ignored');

    # A runtime that is not under a `cl/` directory must not pick up whatever
    # pcl.core happens to sit two levels up.
    my $loose = "$inst/pcl-runtime.lisp";
    open my $lf, '>', $loose or die $!; print $lf "x\n"; close $lf;
    ok( !grep({ $_ eq '--core' } sbcl_prefix(runtime => $loose)),
        'installed core: the lookup requires the <root>/cl/ layout' );
}

# --- the CACHED core (USER s439): compiled once, kept, keyed by content -----
# The default for every runner in a checkout.  A real (tiny) runtime is used so
# sbcl can actually build the core; the cache dir is a tempdir.
SKIP: {
    skip 'sbcl not on PATH', 10 unless `sbcl --version 2>/dev/null` =~ /SBCL/;
    delete local $ENV{PCL_NO_CORE};
    local $ENV{PCL_CACHE_DIR} = tempdir(CLEANUP => 1);
    my $cdir = core_cache_dir();
    is($cdir, "$ENV{PCL_CACHE_DIR}/core", 'core_cache_dir honours PCL_CACHE_DIR');

    my $rdir = tempdir(CLEANUP => 1);
    my $rrt  = "$rdir/pcl-runtime.lisp";
    my $write = sub { open my $fh, '>', $rrt or die $!; print $fh $_[0]; close $fh };
    $write->("(defvar cl-user::*pcl-fake-runtime* 1)\n");

    my @p = sbcl_prefix(runtime => $rrt);
    is($p[0], '--core', 'default: a runtime with no installed core gets a CACHED core');
    like($p[1], qr{^\Q$cdir\E/pcl-[0-9a-f]{8}-[0-9a-f]{12}\.core$},
         'the core lives under <cache>/core/, named by path hash + content hash');
    ok(-s $p[1] > 1_000_000, 'and it is a real saved core, not a marker');
    my $first = $p[1];
    my $mtime = (stat $first)[9];

    # A second resolution (new process state simulated by a forced re-read) is
    # the same file, not a rebuild.
    is(cached_core($rrt), $first, 'second resolution returns the same core');
    is((stat $first)[9], $mtime, '...without rebuilding it');

    # Edit the runtime: the key changes, a new core is built, the old one pruned.
    # Resolved in a CHILD process — a process memoises its answer per runtime
    # (a runtime does not change under a running gate file), so the cross-run
    # behaviour is what must be tested: a new process, new content, new key.
    $write->("(defvar cl-user::*pcl-fake-runtime* 2)\n");
    my $second = `perl -I\Q$RealBin\E/../lib -MPCLSbcl -e 'print PCLSbcl::cached_core(\$ARGV[0])' \Q$rrt\E 2>/dev/null`;
    isnt($second, $first, 'an edited runtime gets a DIFFERENT core (content-keyed)');
    ok(!-e $first, 'the previous core for that runtime path is pruned');

    # PCL_NO_CORE=1 is source mode even when a core exists.
    {
        local $ENV{PCL_NO_CORE} = 1;
        is_deeply([ sbcl_prefix(runtime => $rrt) ],
                  ['--control-stack-size', 512, '--noinform', '--non-interactive',
                   '--load', $rrt],
                  'PCL_NO_CORE=1: the runtime is loaded from source');
    }

    ok(clear_cached_cores() >= 1 && !glob("\Q$cdir\E/pcl-*.core"),
       'clear_cached_cores removes every cached core');
}

# --- ONE CORE PER EXISTING RUNTIME PATH (task #1863) -----------------------
# The prune above removes older cores for the SAME path.  A core whose runtime
# path is GONE it could not even recognise — `pathkey` is a hash — so every
# deleted worktree left a ~49 MB core behind: 154 cores / 7.2 GB on this box,
# 151 of them (7.03 GB) with no runtime left.  A core now names its runtime in
# a `pcl-<pathkey>.path` sidecar, and the build-time prune collects a core
# whose named path is gone, or one with no sidecar that is older than
# $LEGACY_CORE_MAX_AGE (a legacy core of a tree that has not run since).
#
# The foreign cores are PLANTED, not built: the prune reads their NAMES and
# their sidecars, and building four real 49 MB cores to test a naming rule
# would add a minute to this file for nothing.
SKIP: {
    skip 'sbcl not on PATH', 9 if `sbcl --version 2>/dev/null` !~ /SBCL/;
    delete local $ENV{PCL_NO_CORE};
    local $ENV{PCL_CACHE_DIR} = tempdir(CLEANUP => 1);
    my $cdir = core_cache_dir();

    my $rdir = tempdir(CLEANUP => 1);
    my $rrt  = "$rdir/pcl-runtime.lisp";
    open my $fh, '>', $rrt or die $!;
    print $fh "(defvar cl-user::*pcl-fake-runtime* 3)\n";
    close $fh;

    my $mine = cached_core($rrt) or die "no core built";
    my ($mypk) = $mine =~ m{/pcl-([0-9a-f]{8})-};
    is(do { open my $s, '<', "$cdir/pcl-$mypk.path" or die $!; my $l = <$s>; chomp $l; $l },
       $rrt, 'a freshly built core names its runtime in a .path sidecar');

    # Four foreign entries, one per rule.  The 8-hex pathkeys are arbitrary:
    # nothing derives them back, which is the whole point of the sidecar.
    my $plant = sub {
        my ($pk, $path, $age) = @_;
        my $c = "$cdir/pcl-$pk-0123456789ab.core";
        open my $p, '>', $c or die $!; print $p "core\n"; close $p;
        if (defined $path) {
            open my $s, '>', "$cdir/pcl-$pk.path" or die $!;
            print $s "$path\n"; close $s;
        }
        if ($age) { my $t = time - $age; utime $t, $t, $c or die $! }
        return $c;
    };
    my $dead  = $plant->('aaaaaaaa', "$rdir/gone/pcl-runtime.lisp");
    my $live  = $plant->('bbbbbbbb', $rrt);
    my $fresh = $plant->('cccccccc', undef);
    my $aged  = $plant->('dddddddd', undef, 8 * 24 * 3600);
    # An INSTALLED core is `<root>/pcl.core` and lives elsewhere entirely; one
    # planted here must survive the name-shaped glob all the same.
    open my $ic, '>', "$cdir/pcl.core" or die $!; print $ic "installed\n"; close $ic;

    cached_core($rrt, force => 1);            # a build: the prune runs here

    ok(!-e $dead,  'a core whose sidecar names a path that is GONE is removed');
    ok(!-e "$cdir/pcl-aaaaaaaa.path", '... together with its sidecar');
    ok( -e $live,  'a core whose sidecar names a path that EXISTS is kept');
    ok( -e $fresh, 'a sidecar-less core younger than the legacy age is kept');
    ok(!-e $aged,  'a sidecar-less core older than it is removed (a legacy orphan)');
    ok( -e $mine,  'THE CORE THIS RUN WOULD USE is never removed');
    ok( -e "$cdir/pcl.core", '... and an installed-shaped pcl.core is not touched');

    # A core built before this task has no sidecar.  Its first USE writes one,
    # so a live tree is never mistaken for a deleted one.  In a child process:
    # cached_core memoises its answer per runtime within one process.
    unlink "$cdir/pcl-$mypk.path";
    system($^X, "-I$RealBin/../lib", '-MPCLSbcl',
           '-e', 'PCLSbcl::cached_core($ARGV[0])', $rrt) == 0
        or die "child cached_core failed";
    is(do { open my $s, '<', "$cdir/pcl-$mypk.path" or die $!; my $l = <$s>; chomp $l; $l },
       $rrt, 'the first use of a sidecar-less core stamps the sidecar');
}
