#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# elided-call-01.t — task #411 (Option B phase 2, Track B3.1): a `(args)` list
# DIRECTLY after a completed postfix element is an ELIDED-ARROW CALL of that
# element's result.  Design: docs/b3-operand-collapse-s428.md.
#
# perl lets you drop the `->` between chain links, so all of these CALL the
# coderef the left side yields — and every one was DROPPED WHOLE by PCL ("Bug.
# Fell through. Missing case: [", 8 statements over the companion suite:
# op/closure.t, op/current_sub.t, op/ref.t, and their perl-tests twins):
#
#     $s2->()()        $a[0]()        $h{k}()        $r->{m}()      (sub{})[0]()
#
# The fix is a single normalizing pre-pass, Pl::PExpr::_insert_elided_call_arrows:
# it makes the elided arrow EXPLICIT (`$a[0]()` -> `$a[0]->()`), exactly as the
# _retag_* passes normalize PPI's predecessor-classified braces, so the ONE
# existing `-> ( args )` path (walker W2 + reduction Case 2) handles every shape
# with no new reduction logic.  Building a fresh token list makes the insertion
# CASCADE: `$a[0]()(0)` becomes `$a[0]->()->(0)`.
#
# The discriminator is "a completed POSTFIX element, never the bare primary":
# the guards below (`$foo(1)` is not a call, `func(1)` is the word's own args,
# `$cr->(9)` is already explicit, a slice/method-with-args) must be UNCHANGED.
# `f()()` is NOT tested: it is a perl SYNTAX ERROR (a paren-less call's result
# cannot be called without `->`), not a #411 shape.
#
# Every expectation is the live `perl` answer (probed s428).

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

plan tests => 4;

sub write_pl {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;
    return $pl_file;
}

# Transpile (a DROPPED statement fails the row, via PCLCore) and run.
sub run_cl {
    my ($code) = @_;
    my $cl_code = PCLCore::transpile("$pl2cl " . write_pl($code));
    my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cl_fh $cl_code;
    close $cl_fh;
    my $output = `sbcl @sbcl_rt --load $cl_file 2>&1`;
    $output =~ s/^;.*\n//gm;
    $output =~ s/^PCL Runtime loaded\n//gm;
    $output =~ s/^\s*\n//gm;
    return $output;
}

sub emitted { return PCLCore::transpile("$pl2cl " . write_pl($_[0])) }

# ── 1. The census + core #411 shapes all CALL the result ─────────────────────
is(run_cl(<<'PL'), "B:30\nC:11\nE:baz\nH:hk(7)\nI:rm\nJ:a0\n",
my $s2 = sub { my $n = shift // 3; sub { $n * 10 } };
print "B:", $s2->()(), "\n";                 # $x->()()      op/closure.t
my @ss = (sub { sub { 11 } }, sub { sub { 22 } });
print "C:", $ss[0]()(0), "\n";               # $a[0]()(0)    op/current_sub.t (cascade)
print "E:", (sub {"baz"})[0](), "\n";        # (sub{})[0]()  op/ref.t
my %h = (k => sub { "hk(@_)" });
print "H:", $h{k}(7), "\n";                  # $h{k}(args)
my $r = { m => sub { "rm" } };
print "I:", $r->{m}(), "\n";                 # $r->{m}()
my @a = (sub { "a0" });
print "J:", $a[0](), "\n";                   # $a[0]()
PL
   'elided-arrow call of a postfix result — all shapes call, cascade works');

# ── 2. The guards: shapes one character away must be UNCHANGED ────────────────
is(run_cl(<<'PL'), "K:f(1 2)\nL:cr(9)\nM:10 20\nN:2\nO:m(1 2)\n",
sub func { "f(@_)" }
print "K:", func(1,2), "\n";                 # word call — the word's OWN args
my $cr = sub { "cr(@_)" };
print "L:", $cr->(9), "\n";                  # already-explicit arrow call
my @arr = (10,20,30);
print "M:", "@arr[0,1]", "\n";               # array slice, no call
print "N:", ((1,2,3)[1]), "\n";              # list slice, no call
my $o = bless {}, 'X'; sub X::m { my $s=shift; "m(@_)" }
print "O:", $o->m(1,2), "\n";                # method call — args are the method's
PL
   'non-call guards (word args, explicit arrow, slice, method args) unchanged');

# ── 3. The shape: the elided arrow is made explicit, so a funcall-ref emits ──
like(emitted(q{my @a = (sub{1}); my $x = $a[0]();}),
     qr/funcall-ref|ref-funcall/i,
     '$a[0]() emits a coderef call, not a dropped statement');

# ── 4. A bare primary followed by `(...)` is NOT rewritten into a call ────────
# `$foo(1,2)` is not a call in perl (needs `->` or `&`); the pre-pass must not
# touch it, so it keeps whatever the bare-primary path did — never a funcall-ref
# on `$foo`.
unlike(emitted(q{my $foo = 5; my @l = ($foo, (1,2));}),
       qr/PARSE ERROR/,
       'a Symbol/paren-list that is not a postfix call is left alone');
