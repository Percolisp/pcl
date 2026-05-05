#!/usr/bin/env perl
# wantarray-01.t: wantarray() context propagation tests

use v5.30;
use strict;
use warnings;
use Test::More;
use File::Temp qw(tempfile);
use FindBin qw($RealBin);

my $project_root = "$RealBin/../..";
my $pl2cl        = "$project_root/pl2cl";
my $runtime      = "$project_root/cl/pcl-runtime.lisp";

plan skip_all => "pl2cl not found" unless -x $pl2cl;
plan skip_all => "sbcl not found"  unless `which sbcl 2>/dev/null`;

sub run_cl {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;

    my $cl_code = `$pl2cl $pl_file 2>&1`;

    my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cl_fh $cl_code;
    close $cl_fh;

    my $out = `sbcl --noinform --non-interactive --load $runtime --load $cl_file 2>&1`;
    $out =~ s/^;.*\n//gm;
    $out =~ s/^caught .*\n//gm;
    $out =~ s/^compilation unit.*\n//gm;
    $out =~ s/^\s*Undefined.*\n//gm;
    $out =~ s/^-->.*\n//gm;
    $out =~ s/^==>.*\n//gm;
    $out =~ s/^PCL Runtime loaded\n//gm;
    $out =~ s/^\s*\n//gm;
    return $out;
}

sub test_cl {
    my ($name, $code, $expected) = @_;
    $expected .= "\n" unless $expected =~ /\n$/;
    my $out = run_cl($code);
    is($out, $expected, $name) or diag("Got: [$out]");
}

plan tests => 21;

# ── Bug 2: p-wantarray return values ──────────────────────────────────────

test_cl('wantarray returns 1 in list context',
    'sub f { wantarray() }
     my @a = (f());
     print $a[0] == 1 ? "yes" : "no", "\n";',
    'yes');

test_cl('wantarray returns "" in scalar context',
    'sub f { wantarray() }
     my $s = f();
     print defined($s) && !$s ? "yes" : "no", "\n";',
    'yes');

test_cl('wantarray returns undef in void context',
    'sub f { print defined(wantarray()) ? "def" : "undef", "\n" }
     f();',
    'undef');

# ── Bug 1: scalar-context isolation — the leakage bug ─────────────────────

test_cl('scalar call inside list-context sub does not leak',
    'sub inner { wantarray() ? "list" : "scalar" }
     sub outer { my $x = inner(); $x }
     push my @a, outer();
     print $a[0], "\n";',
    'scalar');

test_cl('list context correctly seen inside list-context sub',
    'sub inner { wantarray() ? "list" : "scalar" }
     sub outer { my @r = inner(); @r }
     my @a = outer();
     print $a[0], "\n";',
    'list');

# ── Bug 3: return propagation ──────────────────────────────────────────────

test_cl('return propagates list context',
    'sub inner { wantarray() ? "list" : "scalar" }
     sub outer { return inner() }
     my @a = (outer());
     print $a[0], "\n";',
    'list');

test_cl('return propagates scalar context',
    'sub inner { wantarray() ? "list" : "scalar" }
     sub outer { return inner() }
     my $s = outer();
     print $s, "\n";',
    'scalar');

# ── Tail-position propagation ─────────────────────────────────────────────

test_cl('tail expr propagates list context',
    'sub inner { wantarray() ? "list" : "scalar" }
     sub outer { inner() }
     my @a = (outer());
     print $a[0], "\n";',
    'list');

test_cl('tail expr propagates scalar context',
    'sub inner { wantarray() ? "list" : "scalar" }
     sub outer { inner() }
     my $s = outer();
     print $s, "\n";',
    'scalar');

# ── Method calls ──────────────────────────────────────────────────────────

test_cl('wantarray in method call list context',
    'package Tctx;
     sub new { bless {}, shift }
     sub ctx { wantarray() ? "list" : "scalar" }
     package main;
     my $t = Tctx->new;
     my @a = $t->ctx;
     print $a[0], "\n";',
    'list');

test_cl('wantarray in method call scalar context',
    'package Tctx;
     sub new { bless {}, shift }
     sub ctx { wantarray() ? "list" : "scalar" }
     package main;
     my $t = Tctx->new;
     my $s = $t->ctx;
     print $s, "\n";',
    'scalar');

# ── Ternary branches get parent context, not wantarray condition context ────
# This tested the gen_ternary bug: when the ternary condition is wantarray,
# the branches must still get the OUTER context (scalar here), not list.

test_cl('ternary branches use outer scalar context, not wantarray cond',
    'sub simple { wantarray() ? 1 : 2 }
     sub inline {
       my $a = wantarray() ? simple() : simple();
       $a;
     }
     my @b = inline();
     print "@b\n";',
    '2');  # simple() called in scalar ctx => returns 2; @b=(2)

test_cl('ternary branches use outer list context',
    'sub simple { wantarray() ? "L" : "S" }
     sub inline {
       my @r = wantarray() ? (simple()) : (simple());
       @r;
     }
     my @b = inline();
     print "@b\n";',
    'L');  # simple() in list ctx => "L"; @b=("L")

# ── Void context isolation: /g in sub body non-tail statement ───────────────
# When sub is called in list context, bare $a=~/(.)/g inside should still be
# a one-shot match (not collect all matches).

test_cl('/g regex in sub body non-tail stmt is void (not list)',
    'my $a;
     sub foo {
       $a = "abcd";
       $a =~ /(.)/g;
       return $1;
     }
     my @x = foo();
     print $x[0], "\n";',
    'a');  # if leaks list ctx, $1 would be "d"

# ── do BLOCK context propagation ────────────────────────────────────────────

test_cl('do BLOCK in list context returns last expression in list',
    'sub ctx { wantarray() ? "list" : "scalar" }
     my @r = do { ctx() };
     print $r[0], "\n";',
    'list');

test_cl('do BLOCK in scalar context returns last expression in scalar',
    'sub ctx { wantarray() ? "list" : "scalar" }
     my $s = do { ctx() };
     print $s, "\n";',
    'scalar');

# ── Context through || and && short-circuit RHS ─────────────────────────────

test_cl('|| RHS inherits list context',
    'sub ctx { wantarray() ? "list" : "scalar" }
     sub outer { my $f = 0; $f || ctx() }
     my @r = outer();
     print $r[0], "\n";',
    'list');

test_cl('|| RHS inherits scalar context',
    'sub ctx { wantarray() ? "list" : "scalar" }
     sub outer { my $f = 0; $f || ctx() }
     my $s = outer();
     print $s, "\n";',
    'scalar');

# ── Context through code-ref calls ──────────────────────────────────────────

test_cl('code ref call propagates list context',
    'my $f = sub { wantarray() ? "list" : "scalar" };
     my @r = $f->();
     print $r[0], "\n";',
    'list');

test_cl('code ref call propagates scalar context',
    'my $f = sub { wantarray() ? "list" : "scalar" };
     my $s = $f->();
     print $s, "\n";',
    'scalar');

# ── Nested sub context ───────────────────────────────────────────────────────

test_cl('innermost sub sees its own caller context, not grandparent',
    'sub inner { wantarray() ? "L" : "S" }
     sub middle { my $v = inner(); $v }   # scalar context to inner
     sub outer  { middle() }              # tail: inherits caller ctx
     my @r = outer();
     print $r[0], "\n";',
    'S');  # middle() forces inner() to scalar; outer list ctx doesn't leak
