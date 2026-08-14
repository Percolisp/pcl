#!/usr/bin/env perl
# my-decl-tail-01.t — task #314 families F-A1 and F-B: a DECLARATION whose
# statement continues as an ordinary expression — `my VAR <non-'=' trailing>;`
# and its `our` twin `our NAMES <non-assignment trailing>;`.
#
#   my @raw, @upgraded, @utf8;
#
# Perl declares ONLY the first variable here.  The rest of the statement is an
# ordinary expression in void context whose first operand happens to be the
# fresh lvalue, so `@upgraded` and `@utf8` are PACKAGE variables (perl warns
# "Parenthesize" but compiles it, and opbasic/*.t files run without strict, so
# real code does this).  PCL handled the SCALAR spelling since s3xx and refused
# the CONTAINER one with `Parser2 TODO: unsupported declaration` — one
# predicate too narrow.  That refusal was the whole of opbasic/cmp.t: 12078
# assertions, 96% of the E4.1 flip's cost on the companion suite.
#
# The second half of the family is what the statement DECLARES, as seen by the
# capture analysis: while `_collect_lexical_names` answered "every symbol in
# the statement" (its unknown-shape fallback), a later named sub reading one of
# the TAIL names refused with "file lexical 'x' captured by sub f" — for a name
# that is a package global.  Both consumers now ask ONE predicate,
# `_lead_decl_with_expr_tail` (CLAUDE.md 11).
#
# Every expectation below is the live `perl` answer (probed s393).

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

plan tests => 13;

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
    is(run_cl($code), $expected, $name);
}

# ── the shape opbasic/cmp.t opens with ──────────────────────────────────────
test_cl('array comma-list my: first is lexical, the rest are package vars',
    q{my @raw, @upgraded, @utf8;
push @raw, "r"; push @upgraded, "u"; push @utf8, "8";
print scalar(@raw), scalar(@upgraded), scalar(@utf8), "\n";
print "$raw[0]$upgraded[0]$utf8[0]\n";},
    "111\nru8\n");

test_cl('hash comma-list my',
    q{my %h, %g; $h{a}=1; $g{b}=2;
print scalar(keys %h), scalar(keys %g), "\n";},
    "11\n");

test_cl('mixed sigils in one comma-list my',
    q{my @a, $b, %c; push @a,7; $b=8; $c{k}=9; print "$a[0]$b$c{k}\n";},
    "789\n");

# The trailing operator does not have to be a comma — any non-'=' operator
# leaves the first name declared and the rest an expression.
test_cl('container with a non-comma trailing operator',
    q{my @a . "x"; push @a, 5; print scalar(@a), "\n";},
    "1\n");

# ── the tail names really are GLOBALS, not lexicals ─────────────────────────
# A named sub is hoisted OUTSIDE the file's lets, so if the collector had
# called @up a file lexical this would refuse to compile at all.
test_cl('a tail name read by a named sub is a package global, not a capture',
    q{my @raw, @up; push @up, "g"; sub g2 { return $up[0] } print g2(), "\n";},
    "g\n");

test_cl('same, scalar spelling (the pre-existing half of the bug)',
    q{my $a1, $b1; $b1 = "g"; sub g3 { return $b1 } print g3(), "\n";},
    "g\n");

# ── scoping of the one name that IS declared ────────────────────────────────
test_cl('the declared container is block-scoped like any other my',
    q{my @a; push @a,1;
{ my @a, @b; push @a,2; push @b,3; print scalar(@a), scalar(@b), "\n"; }
print scalar(@a), "\n";},
    "11\n1\n");

test_cl('inside a sub body',
    q{sub f { my @x, @y; push @x,1; push @y,2; return scalar(@x).scalar(@y) }
print f(), "\n";},
    "11\n");

test_cl('a fresh container per loop iteration',
    q{for my $i (1..2) { my @a, @b; push @a,$i; print scalar(@a); } print "\n";},
    "11\n");

# ── the `our` twin of the same shape (#314 family F-B, s395) ────────────────
# `our NAMES <tail>` is the same statement: declare the package cell(s), then
# evaluate `NAMES <tail>` as an ordinary expression.  It was refused with
# `Parser2 TODO: unsupported our declaration` because the gate demanded an
# ASSIGNMENT operator after the name — which made op/inccode.t (89 rows) and
# op/repeat.t (50) whole TRANSPILE-FAIL files over one tied `FETCH`.
test_cl('`our $count++` declares the cell and increments it',
    q{package Foo;
sub bump { our $count++; return $count }
package main;
print Foo::bump(), Foo::bump(), $Foo::count, "\n";},
    "122\n");

test_cl('a non-assignment tail still runs its side effects',
    q{sub side { print "S"; return 1 }
our $x, side();
$x = 4;
print "$x\n";},
    "S4\n");

test_cl('container `our @a, @b;` declares both as package arrays',
    q{our @a, @b;
push @a, 1; push @b, 2, 3;
print scalar(@a), scalar(@b), "$a[0]$b[1]\n";},
    "1213\n");

# ── INVERSE guard: the ordinary declarations must not have moved ────────────
test_cl('plain `my @a = (…)` and bare `my @a;` are untouched',
    q{my @a = (1,2,3); my @b; push @b, 9;
print scalar(@a), scalar(@b), "\n";},
    "31\n");
