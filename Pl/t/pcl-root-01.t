#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# pcl-root-01.t — ONE root resolver (task #1302 (b), F5 of
# docs/plan-cache-and-install-s471.md).
#
# "Which PCL tree am I part of" had FIVE hand-written spellings, and they
# agreed only because every caller happens to sit at the checkout root or one
# level under it.  An INSTALLED tree — <prefix>/lib/pcl reached through a
# wrapper in <prefix>/bin, possibly through a symlinked bin directory — is
# exactly where that accident stops being free.  They are now one function,
# PCLPaths::root (and its no-environment sibling root_of).
#
# WHAT THIS FILE ASSERTS, and why each half is needed:
#
#   1. THE BOOTSTRAP AND THE RESOLVER AGREE.  A script cannot ask root()
#      anything until it has found tools/lib, which lives in the tree it is
#      asking about — so one `use lib` derivation per script stays, forever.
#      The danger is not that line; it is a SECOND RULE growing beside it.
#      Every caller's old spelling is written out below and must give exactly
#      what root() gives for that caller's hint.
#   2. $PCL_ROOT IS HONOURED, AND AN UNUSABLE ONE DIES NAMING BOTH CANDIDATES
#      (rule 12).  An override that is silently ignored is how a run measures
#      a tree the operator did not mean.
#   3. EVERY CALLER ACTUALLY GOES THROUGH IT — asserted by running each of the
#      seven scripts with $PCL_ROOT pointing at a directory that is not a PCL
#      tree and demanding the resolver's own message.  A caller that still
#      derived its own root would carry on and pass.
#
# The rows are cheap: pure-perl calls plus seven perl startups (no SBCL).

use strict;
use warnings;
use Test::More;
use FindBin qw($RealBin);
use File::Basename qw(dirname);
use Cwd qw(abs_path);
use File::Temp qw(tempdir);

my $root = abs_path("$RealBin/../..");
require "$root/tools/lib/PCLPaths.pm";

plan tests => 28;

# ---------------------------------------------------------------- 1. the answer
# The hint each caller passes: its own $0, or its $FindBin::RealBin.
is(PCLPaths::root("$root/pcl"),      $root, 'root() from pcl\'s $0');
is(PCLPaths::root("$root/pl2cl"),    $root, 'root() from pl2cl\'s $0');
is(PCLPaths::root("$root/runpcl"),   $root, 'root() from runpcl\'s $0');
is(PCLPaths::root("$root/tools"),    $root, 'root() from a tools/ script\'s RealBin');
is(PCLPaths::root("$root/tools/runt"), $root, 'root() from a tools/ script\'s $0');

# root_of asks a DIFFERENT question — "which tree owns this file" — and is the
# one PCLSbcl uses to place an installed pcl.core beside a runtime it is handed.
is(PCLPaths::root_of("$root/cl/pcl-runtime.lisp"), $root,
   'root_of() from the runtime path (PCLSbcl::_installed_core\'s question)');
is(PCLPaths::root_of('/'), undef,
   'root_of() answers undef outside any tree — that IS the "no installed core" case');

# ------------------------------------------------- 2. the bootstrap agrees
# Each line is the spelling that caller used before #1302.  If root() ever
# stops agreeing with one of them, that caller's own `use lib` is finding a
# different tree from the one it then works in.
is(dirname(abs_path("$root/pcl")), PCLPaths::root("$root/pcl"),
   'pcl / pl2cl bootstrap  dirname(abs_path($0))  == root()');
is(abs_path(dirname("$root/runpcl")), PCLPaths::root("$root/runpcl"),
   'runpcl bootstrap  abs_path(dirname($0))  == root()');
is(dirname("$root/tools"), PCLPaths::root("$root/tools"),
   'sweep bootstrap  dirname($FindBin::RealBin)  == root()');
is(abs_path("$root/tools/.."), PCLPaths::root("$root/tools"),
   'pclperl-for-tests bootstrap  "$RealBin/.."  == root() (canonicalised)');
is(dirname(abs_path(dirname("$root/tools/runt"))), PCLPaths::root("$root/tools/runt"),
   'runt / clt bootstrap  dirname(abs_path(dirname($0)))  == root()');

# ------------------------------------------------------------ 3. $PCL_ROOT
{
    local $ENV{PCL_ROOT} = $root;
    is(PCLPaths::root("$root/pcl"),        $root, 'PCL_ROOT at the checkout: same answer (root script)');
    is(PCLPaths::root("$root/tools/runt"), $root, 'PCL_ROOT at the checkout: same answer (tools script)');
    is(PCLPaths::root('/nowhere/at/all'),  $root, 'PCL_ROOT wins over an unusable hint');
}
{
    # Trailing slashes are trimmed, so "$root/" concatenates cleanly.
    local $ENV{PCL_ROOT} = "$root/";
    is(PCLPaths::root("$root/pcl"), $root, 'a trailing slash in PCL_ROOT is trimmed');
}
{
    my $empty = tempdir(CLEANUP => 1);
    local $ENV{PCL_ROOT} = $empty;
    my $err = '';
    my $got = eval { PCLPaths::root("$root/pcl") };
    $err = $@ if $@;
    is($got, undef, 'an unusable PCL_ROOT does not answer');
    like($err, qr/cannot find the PCL tree/, 'it dies (rule 12), naming the missing thing');
    like($err, qr/\Q$empty\E\s+\(\$PCL_ROOT\)/, 'the die names the PCL_ROOT candidate');
    like($err, qr/\Q$root\E\s+\(would have been derived/,
         'and the candidate the derivation WOULD have used — both, side by side');
    like($err, qr/Set PCL_ROOT/, 'and how to fix it');
}

# --------------------------------------------- 4. every caller goes through it
# With $PCL_ROOT at a directory that is not a PCL tree, a caller that resolves
# its root through PCLPaths dies with PCLPaths' message before doing anything
# else.  One that still derived its own would run.  runt/clt need an argument
# (they shift it before the root), the sweep needs a file, runpcl a path — none
# of them is reached.
my $empty = tempdir(CLEANUP => 1);
my @callers = (
    ['pcl',                'pcl',                "$root/pcl --version"],
    ['pl2cl',              'pl2cl',              "$root/pl2cl --help"],
    ['runpcl',             'runpcl',             "$root/runpcl /nonexistent.pl"],
    ['sweep',              'tools/sweep-perl-tests.pl',
                           "$^X $root/tools/sweep-perl-tests.pl --no-gate $root/perl-tests/loopctl.t"],
    ['pclperl-for-tests',  'tools/pclperl-for-tests', "$root/tools/pclperl-for-tests -e 1"],
    ['runt',               'tools/runt',         "$root/tools/runt loopctl"],
    ['clt',                'tools/clt',          "$root/tools/clt loopctl"],
);
for my $c (@callers) {
    my ($name, $path, $cmd) = @$c;
    my $out = do {
        local $ENV{PCL_ROOT} = $empty;
        `$cmd 2>&1`;
    };
    like($out, qr/cannot find the PCL tree.*\Q$empty\E\s+\(\$PCL_ROOT\)/s,
         "$path resolves its root through PCLPaths::root")
        or diag($out);
}
