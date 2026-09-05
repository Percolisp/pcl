#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# perf-levers-02.t — the round-28 AGGREGATE levers
# (docs/plan-speed-and-ir-s470.md §A.2 rows 4-6), guarded the way
# perf-levers-01.t guards round 27's.
#
#   #1203 const-subscript slice ASSIGNMENT: `@h{qw(a b)} = …`, `@a[1..3] = …`
#     become N direct element stores.
#   #1204 array fill from a RANGE: `@a = (1..$n)`, `@a = (1..20, $_)` fill the
#     destination in one counting loop, with no materialised range.
#   #1205 raw-element rvalue SLICES: `@a[1..5]` / `@h{@k}` read the slot VALUES
#     into a fresh vector when the consumer COPIES them.
#
# WHY THE MECHANISM ROWS ARE MACROEXPANSIONS.  All three levers are runtime
# MACROS (`p-setf`, `p-array-=`) with no emission to switch, the way task
# #1181's bulk fill and %p-vpush have none — so `pl2cl`'s output is
# byte-identical with and without them and a transpile grep can say nothing.
# What can be asserted is the macro's own answer, which is what actually
# decides the shape: each row macroexpands ONE form and looks for the fast
# path, and every fast row has a NEGATIVE beside it (a dynamic subscript list,
# a non-range RHS, a slice in a consumer that does not copy) which must still
# take the general path.  Without the negatives a lever that fired everywhere
# would pass every row here and be wrong everywhere else.
#
# The RUN rows are perl 5.40.3's own answers, probed
# (scratch/s470bp/probe-slice.pl in the s470bp worktree).

use v5.30;
use strict;
use warnings;
use Test::More;
use File::Temp qw(tempfile);
use FindBin qw($RealBin);
use lib $RealBin;
use lib "$RealBin/../..";
use PCLCore;

my $project_root = "$RealBin/../..";
my $pl2cl   = "$project_root/pl2cl";
my $runtime = "$project_root/cl/pcl-runtime.lisp";
my @sbcl_rt = PCLCore::sbcl_prefix($runtime);
plan skip_all => "pl2cl not found" unless -x $pl2cl;
plan skip_all => "sbcl not found"  unless `which sbcl 2>/dev/null`;
plan tests => 11;

# ── the two instruments ──────────────────────────────────────────────────────

# MACROEXPAND one form in the :pcl package and return the expansion as one
# line.  This is the lever's own decision, read directly.
sub expand {
    my (@forms) = @_;
    my $prog = "(in-package :pcl)\n";
    $prog .= "(format t \"~A~%\" (substitute #\\Space #\\Newline "
           . "(prin1-to-string (macroexpand-1 '$_))))\n" for @forms;
    my ($fh, $file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $fh $prog;
    close $fh;
    my $out = `sbcl @sbcl_rt --load $file 2>&1`;
    $out =~ s/^;.*\n//gm;
    $out =~ s/^(?:PCL Runtime loaded).*\n//gm;
    return $out;
}

# RUN a Perl program through PCL and return its output.
sub run_pl {
    my ($src) = @_;
    my ($fh, $file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $src;
    close $fh;
    my $cl = PCLCore::transpile(qq{$pl2cl $file});
    my ($cfh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cfh $cl;
    close $cfh;
    my $out = `sbcl @sbcl_rt --load $cl_file 2>&1`;
    $out =~ s/^;.*\n//gm;
    $out =~ s/^(?:caught |compilation unit|-->|==>|PCL Runtime loaded).*\n//gm;
    $out =~ s/^\s*Undefined.*\n//gm;
    $out =~ s/^\s*\n//gm;
    return $out;
}

# ─────────────────────────────────────────────────────────────────────────────
# #1203 — a CONSTANT-subscript slice assignment is N direct stores
# ─────────────────────────────────────────────────────────────────────────────
{
    my $mx = expand(
        '(p-setf (p-aslice @a (p-.. 1 3)) (vector 7 8 9))',
        '(p-setf (p-hslice %h "a" "b") (vector 1 2))',
        '(p-setf (p-aslice @a @i) (vector 7 8 9))',
        '(p-setf (p-hslice %h @k) (vector 1 2))',
    );
    my @l = split /\n/, $mx;
    like($l[0], qr/\(setf \(p-aref \@a 1\).*\(setf \(p-aref \@a 2\).*\(setf \(p-aref \@a 3\)/s,
         '#1203: a literal index RANGE becomes one store per index');
    unlike($l[0], qr/dolist/,
           '#1203: ... and the run-time index walk is gone');
    like($l[1], qr/\(setf \(p-gethash %h "a"\).*\(setf \(p-gethash %h "b"\)/s,
         '#1203: literal hash KEYS become one store per key');
    # THE NEGATIVES.  A subscript list that is not a compile-time constant must
    # keep the general path — the walk is how a run-time list is flattened.
    like($l[2], qr/dolist/,
         '#1203 NEGATIVE: a dynamic index list keeps the run-time walk');
    like($l[3], qr/dolist/,
         '#1203 NEGATIVE: dynamic hash keys keep the run-time walk');
}

{
    # perl 5.40.3, probed row by row.  Each is a value the program consumes:
    # the exact fill, a short RHS (missing values are undef, not holes), a long
    # one (extras dropped), the aliasing snapshot a swap needs, growth over a
    # gap, negative indices, a DESCENDING literal range (perl assigns nothing),
    # the qw() key spelling, a reference value, and a container that is a
    # dereference rather than a name.
    my $want = <<'OUT';
1:0 7 8 9 0
2:0,7,U,U,0
3:0 7 8 0 0
4:2 1 3
5:1,U,U,9,10 n=5
6:1 9 8
7:a=1,b=2
8:a=1,b=U,c=U
9:21
10:ARRAY23
11:4 0 5
12:12
13:1 2 3
OUT
    is(run_pl(<<'PERL'), $want, '#1203: thirteen slice-assignment shapes are perl 5.40.3\'s answers');
use strict; use warnings;
my @a=(0)x5; @a[1..3]=(7,8,9);            print "1:@a\n";
my @b=(0)x5; @b[1..3]=(7);                print "2:", join(",", map { defined $_ ? $_ : "U" } @b), "\n";
my @c=(0)x5; @c[1,2]=(7,8,9);             print "3:@c\n";
my @d=(1,2,3); @d[0,1]=@d[1,0];           print "4:@d\n";
my @e=(1); @e[3,4]=(9,10);                print "5:", join(",", map { defined $_ ? $_ : "U" } @e), " n=", scalar(@e), "\n";
my @f=(1,2,3); @f[-1,-2]=(8,9);           print "6:@f\n";
my %h; @h{qw(a b)}=(1,2);                 print "7:", join(",", map {"$_=$h{$_}"} sort keys %h), "\n";
my %i; @i{qw(a b c)}=(1);                 print "8:", join(",", map {"$_=".(defined $i{$_}?$i{$_}:"U")} sort keys %i), "\n";
my %j=(p=>1,q=>2); @j{qw(p q)}=@j{qw(q p)}; print "9:$j{p}$j{q}\n";
my %k; my $r=[1,2]; @k{qw(a b)}=($r,3);   print "10:", ref($k{a}), $k{a}[1], $k{b}, "\n";
my $s=[0,0,0]; @{$s}[0,2]=(4,5);          print "11:@$s\n";
my %m; my @kk=qw(a b); @m{@kk}=(1,2);     print "12:$m{a}$m{b}\n";
my @n=(1,2,3); @n[3..1]=(7);              print "13:@n\n";
PERL
}

# ─────────────────────────────────────────────────────────────────────────────
# #1204 — an array assignment from a RANGE is not materialised
# ─────────────────────────────────────────────────────────────────────────────
{
    my $mx = expand(
        '(p-array-= @a (p-.. 1 5))',
        '(p-array-= @a (vector (p-.. 1 20) $x))',
        '(p-array-= @a @b)',
        '(p-array-= @a (vector $x $y))',
    );
    my @l = split /\n/, $mx;
    like($l[0], qr/%p-array-fill-range/i,
         '#1204: a bare range RHS becomes a direct segment fill');
    like($l[1], qr/%p-array-fill-range.*%p-array-add-items/is,
         '#1204: a mixed list fills in segments — the range direct, the rest through the one walk');
    # THE NEGATIVES.  Without a range there is nothing to avoid materialising,
    # and the general path already has the block copy (task #1181) — this must
    # not steal its work.
    unlike($l[2], qr/%p-array-fill-range/i,
           '#1204 NEGATIVE: a whole-array RHS keeps p-array-fill');
    unlike($l[3], qr/%p-array-fill-range/i,
           '#1204 NEGATIVE: a list with no range keeps p-array-fill');
}

{
    # perl 5.40.3, probed.  The range alone and mixed with other pieces, a
    # DYNAMIC bound, an EMPTY range, a magical STRING range (which has no
    # counting form), a range in the MIDDLE, self-assignment through a range
    # (the RHS must be read before the destination is cleared), the each()
    # iterator a whole-array assignment resets, and a range inside a call.
    my $want = <<'OUT';
1:1 2 3 4 5 n=5
2:1 2 3 9
3:1 2 3 4
4:n=0
5:a b c d e
6:0 1 2 3 4
7:1 2 1 2
8:0
9:5 4 3
OUT
    is(run_pl(<<'PERL'), $want, '#1204: nine array-fill shapes are perl 5.40.3\'s answers');
use strict; use warnings;
my @a; @a=(1..5);                 print "1:@a n=", scalar(@a), "\n";
my @b; my $x=9; @b=(1..3,$x);     print "2:@b\n";
my @c; my $n=4; @c=(1..$n);       print "3:@c\n";
my @d=(9); my $z=0; @d=(1..$z);   print "4:n=", scalar(@d), "\n";
my @e; @e=('a'..'e');             print "5:@e\n";
my @f; @f=(0,1..3,4);             print "6:@f\n";
my @g=(1,2); @g=(1..2,@g);        print "7:@g\n";
my @h=(1,2,3); my ($i0,$v0)=each @h; @h=(1..3); my ($i1,$v1)=each @h; print "8:$i1\n";
my @j; @j=(reverse(3..5));        print "9:@j\n";
PERL
}
