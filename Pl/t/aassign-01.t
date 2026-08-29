#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# aassign-01.t — AASSIGN_COMMON self-assignment regression tests
# Covers the p-array-= fix that snapshots the RHS before clearing the LHS.
# Without the fix, @a = @a clears @a and then reads from the (now empty) array.

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

plan tests => 24;

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
    is($got, $expected, $name);
}

# ── Self-assignment: @a = @a ──────────────────────────────────────────────────
# Without the AASSIGN_COMMON fix, p-array-= clears fill-pointer before reading,
# so @a = @a emptied the array.

test_cl('@a = @a preserves elements',
    'our @a = (1, 2, 3); @a = @a; print "@a\n";',
    "1 2 3\n");

test_cl('@a = @a with strings',
    'our @a = ("foo", "bar", "burbl", "blah"); @a = @a; print "@a\n";',
    "foo bar burbl blah\n");

# ── Embedding self: @a = (x, @a, y) ─────────────────────────────────────────
# The nested @a inside (vector x @a y) was not snapshotted before clearing.

test_cl('@a = (100, @a, 200) embeds self correctly',
    'our @a = ("bar", "burbl", "blah"); @a = (100, @a, 200); print "@a\n";',
    "100 bar burbl blah 200\n");

test_cl('@a = ("X", @a, "Y") with string context',
    'our @a = ("bar", "burbl", "blah"); @a = ("XXX", @a, "YYY"); print "@a\n";',
    "XXX bar burbl blah YYY\n");

# ── List assignment with self-reference: (undef, @a) = @a ───────────────────
# p-list-= uses %p-flatten-list which already snapshots; ensure it still works.

test_cl('(undef, @a) = @a shifts first element',
    'our @a = ("foo", "bar", "burbl", "blah"); (undef, @a) = @a; print "@a\n";',
    "bar burbl blah\n");

# ── Chained: @a = @a; then embed ─────────────────────────────────────────────

test_cl('sequential self-assigns stay consistent',
    'our @a = (1,2,3); @a = @a; @a = (0, @a); print "@a\n";',
    "0 1 2 3\n");

# ── nil (deleted element) preservation through p-array-= ─────────────────────
# The snapshot must not drop nil slots (which mark deleted elements).

test_cl('delete then @a = @a preserves deleted slot',
    'my @a = (1,2,3,4); delete $a[1]; @a = @a;
     print exists($a[1]) ? "exists" : "deleted", "\n";',
    "deleted\n");

test_cl('delete then @a = reverse @a preserves nil slot',
    'my @a = (1,2,3,4); @a = reverse @a;
     delete $a[1];
     @a = reverse @a;
     print exists($a[2]) ? "exists" : "deleted", "\n";',
    "deleted\n");

# ── our (...) = (1..N) — RHS is LIST context, so .. is a range not a flip-flop ──
# Was generating (p-flipflop-num ...) for the RHS, yielding all-empty values.

test_cl('our (...) = (1..3) is a range',
    'our ($x, $y, $z) = (1..3); print "$x $y $z\n";',
    "1 2 3\n");

# ── AASSIGN_COMMON via our: RHS snapshotted before the LHS is overwritten ──────

test_cl('our (...) self-assign snapshots RHS',
    'our ($x,$y,$z) = (1..3); our ($y,$z) = ($x,$y); print "$x $y $z\n";',
    "1 1 2\n");

# ── Logical && in list context returns the list, not a scalar ──────────────────
# `$cond && ($x,$y)` in list context must yield ($x,$y); previously forced scalar.

test_cl('&& propagates list context to its value operand',
    'our ($x,$y,$z) = (1..3); my $t = 1; (our $y, our $z) = $t && ($x,$y); print "$x $y $z\n";',
    "1 1 2\n");

# ── #570: an `undef` PLACEHOLDER in a sub's leading `my (LIST) = @_` ───────────
# The signature fast path (Parser2::_extract_params) is POSITIONAL — the Nth
# name binds @_[N-1] — and it used to build the list by GREPPING the Symbol
# tokens out of it.  An `undef` placeholder is a Word, so it vanished and every
# later name moved one slot LEFT: `my (undef, $x) = @_` bound $_[0].  Silent
# wrong, and only in that one position: the same list with any other RHS, or
# after any other statement, was already right (rows 4-6 below).  The fix makes
# _extract_params DECLINE a list that is not exactly names and commas, so the
# statement lowers through the ordinary `my (LIST) = @_` path.
# Every expected string is real perl 5.40.3's output for the same program;
# rows 1-3 print A / A/B / A on the base tree (measured before the fix).

test_cl('#570 leading undef placeholder binds $_[1]',
    'sub f { my (undef, $x) = @_; return $x }
     print f(qw(A B)), "\n";',
    "B\n");

test_cl('#570 interior + repeated placeholders',
    'sub f2 { my ($a, undef, $c) = @_; return "$a/$c" }
     sub f3 { my (undef, undef, $z) = @_; return $z }
     sub f4 { my (undef, $x, undef, $y) = @_; return "$x$y" }
     print f2(qw(A B C)), " ", f3(qw(A B C)), " ", f4(qw(A B C D)), "\n";',
    "A/C C BD\n");

test_cl('#570 the placeholder does not disturb @_ or a later write',
    'sub g { my (undef, $x) = @_; $x .= "!"; return "$x:" . scalar(@_) . ":$_[0]" }
     print g(qw(A B)), "\n";',
    "B!:2:A\n");

# The shapes that were ALREADY right and must stay right (the discriminator:
# these prove the general list-assign path handles placeholders, which is why
# declining is a correct fix rather than a workaround).

test_cl('#570 regression: no placeholder, trailing placeholder, other RHS',
    'sub h1 { my ($a, $b) = @_; return "$a-$b" }
     sub h2 { my ($p, undef) = @_; return $p }
     sub h3 { my @c = @_; my (undef, $x) = @c; return $x }
     sub h4 { my (undef, $x) = @_[0..1]; return $x }
     print h1(qw(A B)), " ", h2(qw(A B)), " ", h3(qw(A B)), " ", h4(qw(A B)), "\n";',
    "A-B A B B\n");

test_cl('#570 insignificant tokens keep the fast path (nested parens, comment)',
    'sub k1 { my (($a), $b) = @_; return "$a$b" }
     sub k2 { my ($a, # a comment
                   $b) = @_; return "$a$b" }
     sub k3 { my ($a => $b) = @_; return "$a$b" }
     sub k4 { my ($a, $b,) = @_; return "$a$b" }
     print k1(qw(A B)), " ", k2(qw(A B)), " ", k3(qw(A B)), " ", k4(qw(A B)), "\n";',
    "AB AB AB AB\n");

# The EMISSION shape: the ordinary two-name list still takes the fast path (the
# decline must not fire on it), and the placeholder list must not.
{
    my ($fh, $pl) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh "sub ok1 { my (\$p, \$q) = \@_; return \$p }\n"
            . "sub no1 { my (undef, \$x) = \@_; return \$x }\n"
            . "print ok1(1,2), no1(1,2), \"\\n\";\n";
    close $fh;
    my $cl = `$pl2cl $pl 2>/dev/null`;
    like($cl, qr/p-raw-params \(\$p \$q\)/,
         '#570 shape: a plain name list still takes the params fast path');
    unlike($cl, qr/p-raw-params \(\$x\)/,
           '#570 shape: a placeholder list does NOT reach p-raw-params');
}

# ── #610: `my (undef) = LIST` — a list that declares NO NAME ──────────────────
# #570's residue, and a LOUD one: `Pl::Parser2::_multi_decl` reports "no names"
# for an all-placeholder list and `_lower_block`'s
# `die "Parser2 TODO: unsupported declaration"` took the WHOLE FILE, anywhere in
# the program.  The MIXED list is fine (that is #570 above), so the refusal was
# exactly "this list declares nothing".
#
# `my (undef) = LIST` declares nothing and evaluates LIST — once, in LIST
# context — discarding it; it is perl's idiomatic "consume and ignore".  It is
# NOT the `my ()` no-op (task #227), which has no RHS to run.  Every expected
# string below is real perl 5.40.3's output for the same program.

test_cl('#610 the three repro shapes: all-placeholder my-lists',
    'sub f7 { my (undef) = @_; return defined($_[0]) ? "def" : "undef" }
     sub g7 { my (undef, undef) = @_; "g" }
     my (undef) = (1,2);
     print f7(qw(A B)), " ", g7(1,2), " ok\n";',
    "def g ok\n");

# The RHS is a real list assignment: it RUNS, exactly once, in LIST context.
test_cl('#610 the RHS is evaluated ONCE, in LIST context',
    'my $n = 0; my @seen;
     sub h7 { $n++; push @seen, (wantarray ? "LIST" : defined(wantarray) ? "SCALAR" : "VOID"); return (7,8,9) }
     my (undef) = h7();
     print "$n @seen\n";',
    "1 LIST\n");

test_cl('#610 the statement value in SCALAR context is the RHS count',
    'sub v7 { my (undef) = (10,20,30) }
     sub w7 { my (undef, undef) = @_; return scalar(@_) }
     print v7(), " ", w7(1,2,3), "\n";',
    "3 3\n");

# `my (undef)` reaches STRING EVAL too — the refusal used to escape as $@.
test_cl('#610 ... and inside a string eval',
    'my $r = eval "my (undef) = (1,2,3); q(ok)";
     print defined($r) ? $r : "ERR:$@", "\n";',
    "ok\n");

test_cl('#610 an ARRAY on the RHS is read, not consumed',
    'my @a = (1,2,3);
     my (undef) = @a;
     my %h = (a=>1);
     my (undef) = %h;
     print "@a ", join(",", map {"$_=$h{$_}"} sort keys %h), "\n";',
    "1 2 3 a=1\n");

# The inverses: the mixed list (#570), the `my ()` no-op (#227) and a
# placeholder list with NO initialiser must all be untouched.  (No
# all-placeholder decl WITH an init here — that is the positive shape above;
# this row must pass on the base tree.)
test_cl('#610 inverse: mixed list, my (), and a no-init placeholder list',
    'sub m7 { my (undef, $x) = @_; return $x }
     sub n7 { my ($y, undef) = @_; return $y }
     my (); my (undef);
     print m7(qw(A B)), " ", n7(qw(A B)), "\n";',
    "B A\n");
