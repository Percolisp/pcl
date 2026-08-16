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
use Test::More tests => 17;
use File::Temp qw(tempdir);
use FindBin qw($RealBin);
use lib "$RealBin/../lib";
use PCLSbcl qw(sbcl_prefix sbcl_prefix_str);

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
