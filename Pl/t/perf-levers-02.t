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
plan tests => 17;

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

# ─────────────────────────────────────────────────────────────────────────────
# #1205 — an rvalue SLICE in a COPYING consumer reads VALUES
# ─────────────────────────────────────────────────────────────────────────────
{
    my $mx = expand(
        '(p-array-= @v (p-aslice @a 1 2))',
        '(p-array-= @w (p-hslice %h @k))',
        '(p-foreach ($_ (p-aslice @a 1 2)) (p-print $_))',
        '(p-print (p-aslice @a 1))',
    );
    my @l = split /\n/, $mx;
    like($l[0], qr/\(%p-aslice-copy \@a 1 2\)/,
         '#1205: an array slice assigned to an array reads the slot VALUES');
    like($l[1], qr/\(%p-hslice-copy %h \@k\)/,
         '#1205: a hash slice assigned to an array reads the slot VALUES');
    # THE NEGATIVES.  Only a consumer that COPIES may drop the aliasing, and a
    # slice is a list of ALIASES everywhere else (`for (@a[0,1]) { $_ *= 10 }`
    # writes through).  Both of these must keep the alias-building call.
    unlike($l[2], qr/%p-aslice-copy/,
           '#1205 NEGATIVE: a foreach over a slice keeps the aliases');
    # ... and since #1010 (s473b) the alias-building call it walks is the
    # VIVIFYING one: a foreach ALIASES each element to the loop variable, so
    # perl creates the missing slots when the list is built (`my @a=(1); for
    # (@a[0,3]) {}` leaves four elements — probed 5.40.3, guard
    # Pl/t/autoviv-02.t row 14).  #1205's own claim is the row above and is
    # unchanged: a foreach must never get the COPYING read.
    like($l[2], qr/\(%p-aslice-viv \@a 1 2\)/,
         '#1010: ... the alias-building call it walks VIVIFIES (was p-aslice)');
    unlike($l[3], qr/%p-aslice-copy/,
           '#1205 NEGATIVE: a slice that is merely printed keeps the generic path');
}

{
    # perl 5.40.3, probed row by row (scratch/s470bp/probe-slice.pl's twelve
    # slice-READ shapes).  The copy is a COPY (writing $v[0] does not reach
    # @a), out-of-range positions are real undefs and not holes (row 3 —
    # `exists` on them is TRUE, which is what the alias path got wrong),
    # negative indices, a reference element kept as a reference, a missing hash
    # key that is NOT vivified, dynamic keys, a sparse source — and the two
    # ALIASING rows (9 and 10), which are the whole licence: a foreach over a
    # slice writes THROUGH, so those must not take the copying path.
    my $want = <<'OUT';
1:99 3 4|1 2 3 4 5
2:1,2,U,U n=4
3:E
4:5 4
5:ARRAY23
6:9 2|1
7:1,U n=2 ex=N
8:2 1
9:10 20 3
10:K1K2
11:2 3
12:1,U,U,4
OUT
    is(run_pl(<<'PERL'), $want, '#1205: twelve slice-READ shapes are perl 5.40.3\'s answers, aliasing included');
use strict; use warnings;
my @a1=(1..5); my @v1=@a1[1..3]; $v1[0]=99;      print "1:@v1|@a1\n";
my @a2=(1,2);  my @v2=@a2[0..3];                 print "2:", join(",", map { defined $_ ? $_ : "U" } @v2), " n=", scalar(@v2), "\n";
my @a3=(1,2);  my @v3=@a3[0..3];                 print "3:", (exists $v3[3] ? "E" : "N"), "\n";
my @a4=(1..5); my @v4=@a4[-1,-2];                print "4:@v4\n";
my @a5=([1,2],3); my @v5=@a5[0,1];               print "5:", ref($v5[0]), $v5[0][1], $v5[1], "\n";
my %h6=(a=>1,b=>2); my @v6=@h6{'a','b'}; $v6[0]=9; print "6:@v6|$h6{a}\n";
my %h7=(a=>1); my @v7=@h7{'a','zz'};             print "7:", join(",", map { defined $_ ? $_ : "U" } @v7), " n=", scalar(@v7), " ex=", (exists $h7{zz} ? "E":"N"), "\n";
my %h8=(a=>1,b=>2); my @k8=('b','a'); my @v8=@h8{@k8}; print "8:@v8\n";
my @a9=(1..3); for (@a9[0..1]) { $_ *= 10 }      print "9:@a9\n";
my %h10=(a=>1,b=>2); for (@h10{qw(a b)}) { $_ = "K".$_ } print "10:$h10{a}$h10{b}\n";
my @a11=(1..5); my @o11; push @o11, @a11[1,2];   print "11:@o11\n";
my @a12=(1); $a12[3]=4; my @v12=@a12[0..3];      print "12:", join(",", map { defined $_ ? $_ : "U" } @v12), "\n";
PERL
}
