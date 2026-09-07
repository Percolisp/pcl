#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# eval-cache-01.t — the STRING-EVAL DISK CACHE (task #1200) must be invisible.
#
# WHY THESE ROWS.  `eval "..."` transpiles its text through a `pl2cl --server`
# round trip (~16 ms) and then READ+EVALs the CL (~5 ms).  Since #1200 the
# emission is kept under <cache>/evals/ keyed by exactly what p-eval's own
# in-process cache is keyed by — the perl TEXT, the caller's perl package, the
# capture NAMES (#296-B1) and the features in force (#364) — plus
# *pcl-cache-generation*, and validated by the #1261 dependency manifest.  The
# accessor-generating evals of a module load (JSON::PP runs 80 of them, the
# same 80 every run) are then transpiled once ever instead of once per run.
#
# So every row here asks the same question in a different place: does the
# answer depend on whether the entry was already on disk?  Each runs the same
# program TWICE against a FRESH cache directory — pass 1 writes the entries,
# pass 2 reads them — and once more with the cache OFF, which is the reference
# path.  A row that differed between the three would be the cache leaking into
# the program's meaning.
#
# The last three rows are the INVALIDATORS, because a cache whose entries never
# expire is a silent-wrong generator: a failing eval must leave nothing behind,
# an eval that `use`s a module must record that module's content hash, and
# editing the module must make the entry a MISS.

use v5.30;
use strict;
use warnings;
use Test::More;
use File::Temp qw(tempdir);
use File::Spec;
use FindBin qw($RealBin);

my $root = File::Spec->rel2abs("$RealBin/../..");
my $pcl  = "$root/pcl";

plan skip_all => "pcl not found"  unless -x $pcl;
plan skip_all => "sbcl not found" unless `which sbcl 2>/dev/null`;

plan tests => 16;

# A cache directory PCL creates itself: it refuses one made under the ambient
# umask (#1303), so the test names a path inside a tempdir and does not mkdir it.
my $tmp = tempdir(CLEANUP => 1);
my $cache = File::Spec->catdir($tmp, 'cache');
my $lib   = File::Spec->catdir($tmp, 'lib');
mkdir $lib or die "mkdir $lib: $!";

sub run_pcl {
    my ($code, %opt) = @_;
    local $ENV{PCL_CACHE_DIR} = $opt{cache} // $cache;
    local $ENV{PCL_NO_CACHE}  = $opt{nocache} ? 1 : undef;
    delete $ENV{PCL_NO_CACHE} unless $opt{nocache};
    # PERL5LIB, not just -I: `pcl -I` reaches the PROGRAM's transpile, but the
    # runtime spawns `pl2cl --server` with no -I, so an eval whose TEXT `use`s
    # a module would not resolve it at transpile time and the dependency rows
    # below would have nothing to depend on.  (Same gap as #1284.)
    local $ENV{PERL5LIB} = $lib;
    my $out = `$pcl -I '$lib' -E '$code' 2>&1`;
    $out =~ s/^PCL Runtime loaded\n//gm;
    return $out;
}

sub eval_entries {
    my $dir = File::Spec->catdir($cache, 'evals');
    return () unless -d $dir;
    opendir my $dh, $dir or die "opendir $dir: $!";
    my @e = sort grep { /\.lisp$/ } readdir $dh;
    closedir $dh;
    return @e;
}

# ── 1-4  the value of an eval does not depend on where its CL came from ──
# perl 5.40.3: `2`, then `ABA` (#296-B1 — the same eval text in two subs whose
# captured `$x` differs), then `P1 P2` (the same text in two packages).
my $prog = q{my $v = eval "1+1"; print "v=$v ";}
         . q{sub s1 { my $x = "A"; eval q($x) } sub s2 { my $x = "B"; eval q($x) }}
         . q{print s1(), s2(), s1(), " ";}
         . q[{ package P1; sub w { eval q(__PACKAGE__) } }]
         . q[{ package P2; sub w { eval q(__PACKAGE__) } }]
         . q{print P1::w(), " ", P2::w(), "\n";};

my $cold = run_pcl($prog);
is($cold, "v=2 ABA P1 P2\n", 'cold cache: the eval answers are perl\'s');
my $warm = run_pcl($prog);
is($warm, $cold, 'warm cache: byte-identical to the cold run');
my $off = run_pcl($prog, nocache => 1);
is($off, $cold, 'PCL_NO_CACHE=1: byte-identical to the cached run');
cmp_ok(scalar(eval_entries()), '>=', 4, 'the four distinct evals each left an entry');

# ── 5-7  a capture-dependent emission is keyed by its captures, ON DISK ──
# Two entries, not one: the alist NAMES are compiler input, so `$x` compiles as
# the caller's lexical here and would compile as the dynamic special without
# the name.  The row that matters is the WARM one — a shared entry would make
# the second sub answer the first sub's value.
my $capt = q{sub a1 { my $y = 1; my $z = 2; eval q($y+$z) } print a1(), "\n";};
my $c1 = run_pcl($capt);
is($c1, "3\n", 'cold: an eval reading two captured lexicals');
is(run_pcl($capt), $c1, 'warm: same');
is(run_pcl($capt, nocache => 1), $c1, 'cache off: same');

# ── 8-10  a FAILING eval leaves nothing behind, every run ──
# The transpiler's refusal is per run, as perl's compile error is: an entry
# would be a cached failure, and there is nowhere in this design to put one.
my $before = scalar eval_entries();
my $bad = q{for my $i (1..2) { my $r = eval "my \$q = ;"; print defined $r ? "def" : "undef", ($@ ? "/err " : "/ok ") } print "\n";};
my $b1 = run_pcl($bad);
is($b1, "undef/err undef/err \n", 'a syntax error in the eval text: undef and $@, both times');
is(run_pcl($bad), $b1, 'warm: the failure is identical, not cached');
is(scalar eval_entries(), $before, 'a failing eval wrote no cache entry');

# ── 11-16  the DEPENDENCY MANIFEST: an eval that `use`s a module ──
# The eval's transpile reads Dep.pm's prototype, so the entry is only valid
# while Dep.pm hashes to what it read (#1261's rule, one reader).
my $dep = File::Spec->catfile($lib, 'EvDep.pm');
open my $fh, '>', $dep or die "write $dep: $!";
print $fh "package EvDep;\nuse Exporter 'import';\nour \@EXPORT = ('evdep');\nsub evdep (\$) { \$_[0] * 2 }\n1;\n";
close $fh;

my $usep = q{my $r = eval q(use EvDep; evdep(21)); print "r=$r\n";};
is(run_pcl($usep), "r=42\n", 'cold: an eval that `use`s a module');
is(run_pcl($usep), "r=42\n", 'warm: the same, from the cached emission');

sub deps_files {
    return map { my $p = $_; $p =~ s/\.lisp$/.deps/;
                 File::Spec->catfile($cache, 'evals', $p) } eval_entries();
}
sub slurp { my $p = shift; open my $g, '<', $p or return ''; local $/; return <$g> }

is(scalar(grep { -f } deps_files()), scalar(eval_entries()),
   'every cached eval has a .deps sidecar beside it');

my @with_dep = grep { slurp($_) =~ /^dep\tmod\tEvDep\t/m } deps_files();
is(scalar @with_dep, 1, 'exactly one entry records EvDep as a dependency');

my ($hash_before) = slurp($with_dep[0]) =~ /^dep\tmod\tEvDep\t\w+\t(\w+)\t/m;

open $fh, '>>', $dep or die; print $fh "# changed\n"; close $fh;
is(run_pcl($usep), "r=42\n", 'the module changed: the answer is still perl\'s');
my ($hash_after) = slurp($with_dep[0]) =~ /^dep\tmod\tEvDep\t\w+\t(\w+)\t/m;
isnt($hash_after, $hash_before,
     'the entry was re-transpiled: its manifest records the NEW content hash');
