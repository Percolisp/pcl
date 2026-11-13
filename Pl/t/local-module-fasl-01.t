#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# local-module-fasl-01.t — a module found through -I / use lib / PERL5LIB is
# COMPILED ONCE and its compiled file reused, like an installed module (task
# #4986, the USER's ruling of s540, DECIDED ## s540), and an edited module is
# NEVER served stale.  Until #4986 the s471 default left such a module as text
# ("probably being edited") and SBCL compiled it again on every run.
#
# The ruling's condition is TESTED here, not assumed: the five staleness shapes
# it names, under the DEFAULT policy (no PCL_COMPILE_DIRS), each through the
# fasl path:
#   1. the module edited (same size, a newer mtime);
#   2. only a module it `use`s edited (its prototype: the dependent's PARSE
#      changes, so the dependent must re-transpile though its file did not);
#   3. the module replaced by a file of the SAME SIZE whose mtime is the very
#      second the cached entry was written (an edit in the build's second);
#   4. -I switched to another directory holding a module of the same name --
#      the module itself, and (4b) a dependency of an UNCHANGED module that now
#      resolves in the other directory;
#   5. PCL_OPT changed (it selects an emission: the entry is another one).
# Plus the two mechanisms that make "compiled once" true in one run:
#   #4963 a module first met while ANOTHER module's fasl is being built gets
#         its fasl in the SAME run (deferred to the end of the outer build),
#         so the second run is all hits -- before it, one run per nesting
#         level of `use`;
#   #4987 a program that `use lib`s PCL's own vendor/PPI/lib (a program built
#         on PCL's PPI) is valid on its second run -- before it, the child
#         transpile lost that directory, every PPI dependency looked "moved",
#         and every run re-transpiled.
# Those two groups set PCL_COMPILE_DIRS='*' so that they test their own
# mechanism and not the default (on the base they fail on the mechanism).
#
# Every expected answer is perl 5.40.3's, probed.  The cache is this file's
# OWN directory (PCL_CACHE_DIR), so every first run is cold and nothing here
# depends on, or leaves anything in, the developer's cache; the runtime core
# is the shared one (PCL_CORE), as in module-fasl-cache-01.t's #1303 rows.

use v5.30;
use strict;
use warnings;
use Test::More;
use File::Temp qw(tempdir);
use Cwd ();
use FindBin qw($RealBin);

my $root = Cwd::abs_path("$RealBin/../..");
my $pcl  = "$root/pcl";

use lib "$RealBin/../../tools/lib";
use PCLSbcl ();

plan skip_all => "pcl not found"  if !-x $pcl;
plan skip_all => "sbcl not found" if !`which sbcl 2>/dev/null`;

my $cache = tempdir(CLEANUP => 1);
my $core  = PCLSbcl::cached_core("$root/cl/pcl-runtime.lisp");
my $d1    = Cwd::abs_path(tempdir(CLEANUP => 1));
my $d2    = Cwd::abs_path(tempdir(CLEANUP => 1));

# Write DIR/NAME.pm; with AGE, its mtime is AGE seconds in the past (so the
# entry the next run writes is STRICTLY newer -- p-cache-valid-p's rule -- and
# a row is never decided by which second the run happened to start in).
sub put {
    my ($dir, $name, $body, $age) = @_;
    my $path = "$dir/$name.pm";
    open my $fh, '>', $path or die "write $path: $!";
    print $fh $body;
    close $fh;
    if (defined $age) { my $t = time - $age; utime $t, $t, $path or die "utime $path: $!" }
    return $path;
}

# Run CODE under `pcl` with -I DIRS (default $d1), the default compile policy,
# this file's cache, and the PCL_FASL_DEBUG trace.  Returns (OUTPUT, TRACE):
# the program's output and the `PCL: module ...` lines.
sub run_pcl {
    my ($code, %o) = @_;
    local %ENV = %ENV;
    delete @ENV{qw(PCL_NO_FASL_CACHE PCL_COMPILE_DIRS PCL_NO_COMPILE_DIRS
                   PERL5LIB PCL_OPT PCL_NO_CACHE PCL_NO_RAW_VERDICT PCL_FACTS)};
    $ENV{PCL_CACHE_DIR}  = $cache;
    $ENV{PCL_CORE}       = $core if $core;
    $ENV{PCL_FASL_DEBUG} = 1;
    $ENV{$_} = $o{env}{$_} for keys %{ $o{env} || {} };
    my $inc = join ' ', map { "-I '$_'" } @{ $o{inc} || [$d1] };
    my $all = `$pcl $inc -E '$code' 2>&1`;
    my (@out, @trace);
    for my $l (split /^/, $all) {
        next if $l =~ /^;/ || $l =~ /^PCL Runtime loaded$/;
        if ($l =~ /^PCL: /) { push @trace, $l } else { push @out, $l }
    }
    return (join('', @out), join('', @trace));
}

# The cached FASL of the module whose source is PATH: its manifest names the
# source, and the fasl is <key>-<runtime-id>.fasl beside it.
sub fasl_of {
    my ($path) = @_;
    for my $deps (glob("$cache/modules/*.deps")) {
        open my $fh, '<', $deps or next;
        my $t = do { local $/; <$fh> };
        next if $t !~ m{^source\t\S+\t\Q$path\E$}m;
        (my $key = $deps) =~ s/\.deps\z//;
        my @f = glob("$key-*.fasl");
        return $f[0] if @f == 1;
    }
    return undef;
}

# ─────────────────────────────────────────────────────────────────────────
# THE DEFAULT (#4986): a module under -I is compiled on its first run and its
# fasl is HIT on the next, which compiles nothing.
put($d1, 'LmA', "package LmA; sub v { 'one' } 1;\n", 100);
{
    my ($o, $t) = run_pcl('use LmA; print LmA::v()');
    is($o, 'one', 'default: a -I module answers');
    like($t, qr/^PCL: module LmA\.pm -> fasl-build/m,
         '#4986 DEFAULT: a module reached through -I is COMPILED on its first run');
    ($o, $t) = run_pcl('use LmA; print LmA::v()');
    like($t, qr/^PCL: module LmA\.pm -> FASL HIT/m, '#4986 ... and its fasl is HIT on the next');
    unlike($t, qr/-> (?:fasl-build|TEXT)/, '... which compiles nothing');
}

# SHAPE 1: the module EDITED (the same size, a newer mtime).
put($d1, 'LmA', "package LmA; sub v { 'two' } 1;\n");
sleep 1;   # the rebuilt entry must be strictly newer than this edit
{
    my ($o, $t) = run_pcl('use LmA; print LmA::v()');
    is($o, 'two', 'shape 1: an EDITED module answers its new text');
    like($t, qr/^PCL: module LmA\.pm -> fasl-build/m, '... rebuilt, not served stale');
    ($o, $t) = run_pcl('use LmA; print LmA::v()');
    is($o, 'two', '... and stays right');
    like($t, qr/^PCL: module LmA\.pm -> FASL HIT/m, '... from its new fasl');
}

# SHAPE 2: only a DEPENDENCY edited.  LmC's `zap + 1` is zap()+1 while LmB
# says `sub zap ()` (8) and zap(+1) once the prototype is gone (107); LmC's
# own file never changes.
my $lmb = "package LmB;\nuse Exporter 'import';\nour \@EXPORT = qw(zap);\n"
        . "sub zap () { return 7 + (\@_ ? 100 : 0) }\n1;\n";
put($d1, 'LmB', $lmb, 100);
put($d1, 'LmC', "package LmC;\nuse LmB;\nsub go { return zap + 1 }\n1;\n", 100);
{
    my ($o, $t) = run_pcl('use LmC; print LmC::go()');
    is($o, '8', 'shape 2: LmC reads LmB\'s empty prototype');
    ($o, $t) = run_pcl('use LmC; print LmC::go()');
    like($t, qr/^PCL: module LmC\.pm -> FASL HIT/m, '... and is HIT on the next run');
    (my $noproto = $lmb) =~ s/sub zap \(\)/sub zap/;
    put($d1, 'LmB', $noproto);
    sleep 1;
    ($o, $t) = run_pcl('use LmC; print LmC::go()');
    is($o, '107', 'shape 2: editing ONLY the dependency changes the answer as perl\'s does');
    like($t, qr/^PCL: module LmC\.pm -> fasl-build/m,
         '... because the UNCHANGED dependent is rebuilt (its manifest)');
}

# SHAPE 3: a SAME-SIZE rewrite whose mtime is the second the entry was
# written: an edit in the build's own second.  The entry is valid only while
# it is STRICTLY newer than its source, so this rebuilds.  (A same-size
# rewrite whose mtime is put BACK to before the build is the one shape no
# stat can see: docs/not-supported.md ns-cache-same-size-rewrite.)
my $lmd = put($d1, 'LmD', "package LmD; sub v { 'aaa' } 1;\n", 100);
{
    my ($o, $t) = run_pcl('use LmD; print LmD::v()');
    is($o, 'aaa', 'shape 3: LmD answers');
    ($o, $t) = run_pcl('use LmD; print LmD::v()');
    like($t, qr/^PCL: module LmD\.pm -> FASL HIT/m, '... and is HIT');
    my $fasl = fasl_of($lmd);
    ok(defined $fasl, '... from a fasl whose manifest names its source') or diag "cache: $cache";
    my $m = defined $fasl ? (stat $fasl)[9] : time;
    put($d1, 'LmD', "package LmD; sub v { 'bbb' } 1;\n");
    utime $m, $m, $lmd or die "utime: $!";
    is(-s $lmd, length("package LmD; sub v { 'aaa' } 1;\n"), '... (the rewrite has the same size)');
    ($o, $t) = run_pcl('use LmD; print LmD::v()');
    is($o, 'bbb', 'shape 3: a same-size rewrite in the entry\'s own second is seen');
    like($t, qr/^PCL: module LmD\.pm -> fasl-build/m, '... rebuilt');
}

# SHAPE 4: -I switched to another directory holding a module of the SAME
# NAME; the first directory's entry survives the switch.
put($d1, 'LmG', "package LmG; sub v { 'd1' } 1;\n", 100);
put($d2, 'LmG', "package LmG; sub v { 'd2' } 1;\n", 100);
{
    my ($o, $t) = run_pcl('use LmG; print LmG::v()');
    is($o, 'd1', 'shape 4: -I d1 answers d1');
    run_pcl('use LmG; print LmG::v()');
    ($o, $t) = run_pcl('use LmG; print LmG::v()', inc => [$d2]);
    is($o, 'd2', 'shape 4: -I d2 answers d2, never d1\'s compiled file');
    like($t, qr/^PCL: module LmG\.pm -> fasl-build/m, '... built for d2');
    ($o, $t) = run_pcl('use LmG; print LmG::v()');
    is($o, 'd1', '... and back on -I d1, d1 again');
    like($t, qr/^PCL: module LmG\.pm -> FASL HIT/m, '... from d1\'s fasl');
}

# SHAPE 4b: an UNCHANGED module (d1/LmE.pm, the same file, the same key)
# whose DEPENDENCY now resolves in another directory: -I d2 -I d1 finds
# d2/LmF.pm (no prototype), and perl's parse of LmE changes with it.
put($d1, 'LmE', "package LmE;\nuse LmF;\nsub go { return zap + 1 }\n1;\n", 100);
put($d1, 'LmF', "package LmF;\nuse Exporter 'import';\nour \@EXPORT = qw(zap);\n"
              . "sub zap () { return 7 + (\@_ ? 100 : 0) }\n1;\n", 100);
put($d2, 'LmF', "package LmF;\nuse Exporter 'import';\nour \@EXPORT = qw(zap);\n"
              . "sub zap { return 7 + (\@_ ? 100 : 0) }\n1;\n", 100);
{
    my ($o, $t) = run_pcl('use LmE; print LmE::go()');
    is($o, '8', 'shape 4b: LmE with d1\'s LmF');
    ($o, $t) = run_pcl('use LmE; print LmE::go()');
    like($t, qr/^PCL: module LmE\.pm -> FASL HIT/m, '... HIT');
    ($o, $t) = run_pcl('use LmE; print LmE::go()', inc => [$d2, $d1]);
    is($o, '107', 'shape 4b: the dependency now found in d2 changes the answer as perl\'s does');
    like($t, qr/^PCL: module LmE\.pm -> fasl-build/m, '... the unchanged module is rebuilt');
}

# SHAPE 5: PCL_OPT changed.  It selects an emission (here the constant fold),
# so the entry is another one; the default's entry is untouched.
put($d1, 'LmK', "package LmK;\nuse constant K => 3;\nsub v { K + 1 }\n1;\n", 100);
{
    my ($o, $t) = run_pcl('use LmK; print LmK::v()');
    is($o, '4', 'shape 5: LmK answers');
    ($o, $t) = run_pcl('use LmK; print LmK::v()', env => { PCL_OPT => '-const-fold' });
    is($o, '4', 'shape 5: under PCL_OPT=-const-fold the same answer');
    like($t, qr/^PCL: module LmK\.pm -> fasl-build/m, '... from its own build, not the default\'s fasl');
    ($o, $t) = run_pcl('use LmK; print LmK::v()');
    like($t, qr/^PCL: module LmK\.pm -> FASL HIT/m, '... and the default\'s entry is still a HIT');
}

# ─────────────────────────────────────────────────────────────────────────
# #4963: A uses B uses C.  B and C are first met while A's fasl is being
# built; their builds are deferred to the end of A's, in the same run, so the
# SECOND run is all hits.  (Before #4963: B got its fasl on run 2, C on 3.)
put($d1, 'LmN3', "package LmN3; sub v { '3' } 1;\n", 100);
put($d1, 'LmN2', "package LmN2; use LmN3; sub v { LmN3::v() . '2' } 1;\n", 100);
put($d1, 'LmN1', "package LmN1; use LmN2; sub v { LmN2::v() . '1' } 1;\n", 100);
{
    my %all = (env => { PCL_COMPILE_DIRS => '*' });
    my ($o, $t) = run_pcl('use LmN1; print LmN1::v()', %all);
    is($o, '321', '#4963 a three-level use chain answers');
    like($t, qr/^PCL: module LmN3\.pm -> fasl-build \(deferred\)/m,
         '#4963 the innermost module\'s build is deferred and done in the same run');
    ($o, $t) = run_pcl('use LmN1; print LmN1::v()', %all);
    is($o, '321', '... and answers the same from the cache');
    is(scalar(() = $t =~ /^PCL: module LmN[123]\.pm -> FASL HIT/mg), 3,
       '#4963 the SECOND run loads all three from their fasls')
        or diag $t;
    unlike($t, qr/-> (?:fasl-build|TEXT)/, '... and compiles nothing');
}

# #4987: a program built on PCL's own vendored PPI (`use lib` of that very
# directory).  PPI::Exception::ParserRejection uses PPI::Exception: the
# smallest pair of PPI modules where one depends on another.  PPI::Exception
# uses Params::Util, whose installed build is XS-only unless
# PERL_PARAMS_UTIL_PP is set (PCL has no build of its XS half: the s540b
# measurement used a pure-Perl copy for the same reason, and #4480 names the
# shim it would need), so the rows run with that switch.
SKIP: {
    my $vend = "$root/vendor/PPI/lib";
    skip "no vendored PPI", 3 if !-f "$vend/PPI/Exception/ParserRejection.pm";
    my %all = (env => { PCL_COMPILE_DIRS => '*', PERL_PARAMS_UTIL_PP => 1 });
    my $code = qq{use lib "$vend"; use PPI::Exception::ParserRejection; print "ok"};
    my ($o, $t) = run_pcl($code, %all);
    is($o, 'ok', '#4987 a program that uses lib vendor/PPI/lib loads a PPI module');
    ($o, $t) = run_pcl($code, %all);
    like($t, qr/^PCL: module ParserRejection\.pm -> FASL HIT/m,
         '#4987 ... and its second run is a HIT')
        or diag $t;
    unlike($t, qr/^PCL: module (?:ParserRejection|Exception)\.pm -> (?:fasl-build|TEXT)/m,
           '#4987 ... no PPI module is re-transpiled or rebuilt');
}

done_testing();
