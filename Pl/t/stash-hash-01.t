#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# stash-hash-01.t - a package stash read and written AS A HASH, inside the
# bounded design s522 ruled (docs/not-supported.md "Live symbol-table hashes"):
# every delete spelling that removes a sub goes through the one write-through
# path (#3511), the snapshot lists variables and filehandles too (#1510), and
# an element VALUE that is not a sub dies naming the deferral (rule 12).
# Expected lines are perl 5.40.3's output (probed s522b) except the rule-12
# row, which asserts PCL's refusal.  Inverse: on the aff112d7 base all 8 rows
# fail (a,b,keep / the reader death on `@Pkg::{...}` / empty / the subs-only
# view / a-def / the value read answers).

use v5.30;
use strict;
use warnings;
use Test::More;
use File::Temp qw(tempfile);
use FindBin qw($RealBin);
use lib $RealBin;
use PCLCore;

my $project_root = "$RealBin/../..";
my $pl2cl        = "$project_root/pl2cl";
my $runtime      = "$project_root/cl/pcl-runtime.lisp";
my @sbcl_rt = PCLCore::sbcl_prefix($runtime);

plan skip_all => "pl2cl not found" unless -x $pl2cl;
plan skip_all => "sbcl not found"  unless `which sbcl 2>/dev/null`;

plan tests => 9;

sub run_cl {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;
    my $cl_code = `$pl2cl $pl_file 2>/dev/null`;
    my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cl_fh $cl_code;
    close $cl_fh;
    my $output = `sbcl @sbcl_rt --load $cl_file 2>&1`;
    $output =~ s/^;.*\n//gm;
    $output =~ s/^PCL Runtime loaded\n//gm;
    $output =~ s/^\s*\n//gm;
    return $output;
}

sub test_cl {
    my ($name, $code, $expected) = @_;
    my $got = run_cl($code);
    ref $expected ? like($got, $expected, $name) : is($got, $expected, $name);
}

my $subs = 'package Local::Class; sub a {1} sub b {2} sub keep {3} package main; my @imports = qw(a b); ';
my $left = 'print join(",", sort grep { defined &{"Local::Class::$_"} } qw(a b keep)), "\n";';

test_cl('#3511: delete @{"Pkg::"}{LIST} removes the subs (was a silent no-op)',
    $subs . 'delete @{"Local::Class::"}{@imports}; ' . $left,
    "keep\n");

test_cl('#3511: delete @Pkg::{LIST} transpiles (was a reader death) and removes the subs',
    $subs . 'delete @Local::Class::{@imports}; ' . $left,
    "keep\n");

test_cl('#1510: a package holding only @ISA has a non-empty stash',
    'package Foo; our @ISA = ("Bar"); package main; print scalar(keys %{"Foo::"}) ? "has" : "empty", "\n";',
    "has\n");

test_cl('#1510: keys of a stash list subs, package variables and filehandles',
    'package Foo; our $x = 1; our @y = (1); our %h = (a=>1); sub f {1} open FH, "<", "/dev/null";'
    . ' package Bar; sub g {1} package Baz; our @ISA = ("Bar"); package main;'
    . ' for my $p (qw(Foo Bar Baz)) { no strict "refs"; print "$p: ", join(",", sort keys %{"${p}::"}), "\n" }',
    "Foo: FH,f,h,x,y\nBar: g\nBaz: ISA\n");

test_cl('#1510: exists on a stash element answers for @ISA and a missing name',
    'package Foo; sub f {1} package Baz; our @ISA = ("Foo"); package main;'
    . ' print exists $Foo::{ISA} ? "e":"ne", exists $Baz::{ISA} ? "e":"ne", exists $Foo::{nope} ? "e":"ne", "\n";',
    "neene\n");

test_cl('#3511: delete $Pkg::{name} undefines the sub for defined, *{}{CODE} and method calls',
    'package Local::Class; sub a {1} package main; delete $Local::Class::{a};'
    . ' print defined &{"Local::Class::a"} ? "a-def" : "a-gone", " ", (defined *{"Local::Class::a"}{CODE} ? "code" : "nocode"), "\n";'
    . ' print eval { Local::Class->a; 1 } ? "ran\n" : "died\n";',
    "a-gone nocode\ndied\n");

test_cl('a stash slice READ of subs still answers',
    'package Q; sub b {2} sub keep {3} package main; my @x = @Q::{qw(b keep)}; print scalar(@x), "\n";',
    "2\n");

test_cl('rule 12: the VALUE of a non-CODE stash element dies naming the deferral',
    'package Foo; our $x = 1; package main; print eval { my $v = $Foo::{x}; 1 } ? "read\n" : "died: $@";',
    qr/\Adied: PCL: stash element value for a non-CODE symbol is not supported \(docs\/not-supported\.md "Live symbol-table hashes"\)/);

# #3321: a stash ALIAS `*A:: = *B::` redirects RUN-TIME name resolution (bless,
# method calls, isa, can) to B's stash; names bound at COMPILE time (r4's `sub
# later`, r6's `$A0::v`) stay with A's own stash, as in perl.  Program = Fable's
# s522 merge probe b3-stash-alias.pl; expected = perl 5.40.3.  Inverse: the
# aff112d7 base prints r1 `0 none`, r2 A0, r3 cannot, r5 none, r7 none.
test_cl('#3321: a stash alias redirects bless / methods / isa / can; compile-time names stay', <<'EOF',
package B0; sub hi { "hi" } package A0; *A0:: = *B0::; package main; my $o = bless {}, "A0";
print "r1 ", ($o->isa("B0") ? 1 : 0), " ", (eval { $o->hi } // "none"), "\n";
print "r2 ", ref($o), "\n";
print "r3 ", (A0->can("hi") ? "can" : "cannot"), " ", (A0->isa("A0") ? 1 : 0), "\n";
package A0; sub later { "later" } package main; print "r4 ", (eval { B0->later } // "none"), "\n";
package B0; our @ISA = ("Base0"); package Base0; sub bz { "bz" } package main; print "r5 ", (eval { A0->bz } // "none"), "\n";
$A0::v = 1; print "r6 ", ($B0::v // "undef"), "\n";
package JSON::PP::Boolean; sub new { bless {}, shift } sub val { "v" } package main; *Dummy::True:: = *JSON::PP::Boolean::; print "r7 ", (eval { Dummy::True->new->val } // "none"), "\n";
EOF
    "r1 1 hi\nr2 B0\nr3 can 1\nr4 none\nr5 bz\nr6 undef\nr7 v\n");
