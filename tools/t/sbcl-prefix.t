#!/usr/bin/env perl
# Unit tests for PCLSbcl — the ONE builder of the SBCL command line all four
# PCL runners spawn (task #344).
#
# NOT part of the Pl/t gate: that gate measures the transpiler, this measures
# the harness.  Run it directly:  prove tools/t/sbcl-prefix.t
use strict;
use warnings;
use Test::More tests => 13;
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
