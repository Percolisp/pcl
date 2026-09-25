#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# array-window-01.t — THE ARRAY WINDOW (s494p, task #2098): `shift`,
# `unshift` and `splice(@a, 0, K)` are amortized O(1) per element.  They were
# QUADRATIC: `shift` copied every remaining element down one slot, so draining
# a 200k queue took 166-189 s where perl takes 0.01 s, and `unshift` /
# front-`splice` followed the same curve.  The array header's WINDOW now moves
# instead (cl/pcl-runtime.lisp, the block above %p-storable-raw; normative
# statement docs/ir-spec.md §2.3).
#
# Three kinds of rows:
#   * MECHANISM — read out of the loaded runtime: the window moves (the
#     displacement advances, the data vector is the SAME object, %p-vec-data
#     answers NIL while displaced), a drained array resets, a short array keeps
#     the copy-down, and the self-test answered T.  These FAIL on a
#     pre-s494p tree.
#   * COMPLEXITY — one ratio row per operation: time at 4N <= 8 x time at N
#     (with a 20 ms floor on the N side, so a sub-millisecond run cannot make
#     the bound flaky).  Linear gives ~4x; the old quadratic code gave ~16x.
#   * ANSWERS — perl 5.40.3's own output for every shape the window could
#     break (aliases through a reference and through @_, foreach over a
#     shifting array, local, $#a, undef, delete, negative indices, sort /
#     reverse / map / grep / each over a shifted array, read-only arrays, a
#     deque, splice's list and scalar values).  Probed perl -> the 3e8bff3e
#     base -> this tree, identical on all three (scratch/s494p/brk*.pl in the
#     s494p worktree); they are the correctness NET and pass on the base too
#     (the s473v rule for a pure speed lever).
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
plan skip_all => "pl2cl not found" if !-x $pl2cl;
plan skip_all => "sbcl not found"  if !`which sbcl 2>/dev/null`;

sub run_lisp {
    my ($src) = @_;
    my ($fh, $file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $fh $src;
    close $fh;
    return scalar `sbcl @sbcl_rt --load $file 2>&1`;
}

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
    return $out;
}

# ─────────────────────────────────────────────────────────────────────────────
# THE MECHANISM
# ─────────────────────────────────────────────────────────────────────────────
my $mech = run_lisp(<<'LISP');
(in-package :pcl)
(defun mk (n)
  (let ((a (make-array 0 :adjustable t :fill-pointer 0)))
    (dotimes (i n a) (vector-push-extend i a))))
(defun disp (a)
  (if (sb-kernel::%array-displaced-p a) (sb-kernel:%array-displacement a) 0))
(format t "selftest ~a~%" (and *p-array-window-ok* t))
(let* ((a (mk 40)) (d (sb-kernel:%array-data a)))
  (p-shift a) (p-shift a) (p-shift a)
  (format t "advance ~a ~a ~a ~a~%" (disp a) (eq d (sb-kernel:%array-data a))
          (null (%p-vec-data a)) (aref a 0))
  (format t "cleared ~a~%" (eql 0 (svref d 0)))
  (p-unshift a 77)
  (format t "retreat ~a ~a ~a~%" (disp a) (unbox (aref a 0)) (length a))
  (loop while (> (length a) 0) do (p-shift a))
  (format t "reset ~a ~a~%" (disp a) (eq (%p-vec-data a) d)))
(let ((a (mk 4)))
  (p-shift a)
  (format t "short ~a ~a ~a~%" (disp a) (and (%p-vec-data a) t) (aref a 0)))
(let ((a (mk 40)))
  (let ((*wantarray* t)) (p-splice-impl a 0 5))
  (format t "splice ~a ~a ~a~%" (disp a) (aref a 0) (length a)))
(let ((a (mk 40)))
  (p-unshift a 88)
  (format t "unshift-slack ~a ~a ~a~%" (> (disp a) 0) (unbox (aref a 0)) (aref a 40)))
;; THE FALLBACK: with the window OFF the answers are the same and the
;; announcement says so once.
(setf *p-array-window-ok* nil)
(let ((a (mk 40)))
  (p-shift a) (p-unshift a 66) (p-shift a)
  (format t "fallback ~a ~a ~a ~a~%" (disp a) (aref a 0) (aref a 38) (length a)))
LISP

like($mech, qr/^selftest T$/mi,
     'the load-time self-test passed: SET-ARRAY-HEADER behaves as the window needs');
like($mech, qr/^advance 3 T T 3$/mi,
     'shift MOVES THE WINDOW: displacement 3, same data vector, %p-vec-data NIL while displaced');
like($mech, qr/^cleared T$/mi,
     'a vacated slot is cleared, so the shifted-out element is not retained');
like($mech, qr/^retreat 2 77 38$/mi,
     'unshift into front room moves the window BACK (O(items), no copy)');
like($mech, qr/^reset 0 T$/mi,
     'a drained array resets its window to slot 0: capacity and fast paths return');
like($mech, qr/^short 0 T 1$/mi,
     'a SHORT array (<= 16) keeps the copy-down: @_ after `my $self = shift` stays undisplaced');
like($mech, qr/^splice 5 5 35$/mi,
     'splice(@a, 0, K) with no replacement list moves the window by K');
like($mech, qr/^unshift-slack T 88 39$/mi,
     'unshift with no front room reallocates ONCE with front slack');
like($mech, qr/^fallback 0 1 39 39$/mi,
     'window OFF: the portable copy-down gives the same answers');
like($mech, qr/^PCL: array window unavailable on this SBCL \(.*\); shift and unshift are O\(n\)$/mi,
     'window OFF: the fallback says so once on stderr');

# ─────────────────────────────────────────────────────────────────────────────
# THE COMPLEXITY CLASS — 4N costs at most 8x N (linear ~4x, the old code ~16x).
# In-process, so compile and startup are not in the timing.
# ─────────────────────────────────────────────────────────────────────────────
my $cx = run_lisp(<<'LISP');
(in-package :pcl)
(defun mk (n)
  (let ((a (make-array 0 :adjustable t :fill-pointer 0)))
    (dotimes (i n a) (vector-push-extend i a))))
(defun secs (fn n)
  (let ((t0 (get-internal-real-time)))
    (funcall fn n)
    (/ (- (get-internal-real-time) t0) (float internal-time-units-per-second))))
(defun ratio-ok (name fn n)
  (funcall fn n)                                     ; warm
  (let ((a (secs fn n)) (b (secs fn (* 4 n))))
    (format t "~a ~a ~,4f ~,4f~%" name (<= b (* 8 (max a 0.02))) a b)))
(ratio-ok "shift" (lambda (n) (let ((a (mk n))) (loop while (> (length a) 0) do (p-shift a)))) 5000)
(ratio-ok "unshift" (lambda (n) (let ((a (mk 0))) (dotimes (i n) (p-unshift a i)))) 5000)
(ratio-ok "splice" (lambda (n) (let ((a (mk n)))
                                 (loop while (> (length a) 0) do (p-splice-impl a 0 1)))) 5000)
LISP

like($cx, qr/^shift T /mi,   "shift drain scales linearly (4N <= 8 x N)")   or diag $cx;
like($cx, qr/^unshift T /mi, "unshift loop scales linearly (4N <= 8 x N)")  or diag $cx;
like($cx, qr/^splice T /mi,  "splice(\@a, 0, 1) drain scales linearly (4N <= 8 x N)") or diag $cx;

# ─────────────────────────────────────────────────────────────────────────────
# THE ANSWERS — perl 5.40.3's own output.
# ─────────────────────────────────────────────────────────────────────────────
my @cases = (
  ['ref alias sees the shift',
   'my @big = (1 .. 40); my $r = \@big; shift @big; shift @big; print "@$r[0..2] n=", scalar(@$r), "\n";',
   "3 4 5 n=38\n"],
  ['@_ element alias survives a shift (short)',
   'my @a = (1 .. 30); sub f { shift; $_[0] = 9 } f(@a); print "$a[0] $a[1]\n";',
   "1 9\n"],
  ['@_ element alias survives a window shift (long)',
   'my @b = (1 .. 30); sub g { shift for 1 .. 20; $_[0] = "X" } g(@b); print "$b[19] $b[20] $b[21]\n";',
   "20 X 22\n"],
  ['foreach over an array while shifting it',
   'my @c = (1 .. 30); my @seen; for my $x (@c) { push @seen, $x; shift @c if @c > 25 } print "@seen | @c\n";',
   "1 3 5 7 9 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 | 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30\n"],
  ['local @a over a shifted array',
   'our @l = (1 .. 30); sub lf { local @l = (7, 8); shift @l; "@l" } shift @l for 1 .. 20; print lf(), " | @l\n";',
   "8 | 21 22 23 24 25 26 27 28 29 30\n"],
  ['$#a = N, $#a = -1 and push after shifts',
   'my @d = (1 .. 30); shift @d for 1 .. 5; $#d = 3; print "@d\n"; $#d = -1; print scalar(@d), "\n"; push @d, 5, 6; print "@d\n";',
   "6 7 8 9\n0\n5 6\n"],
  ['@a = () and undef @a after shifts',
   'my @e = (1 .. 30); shift @e for 1 .. 5; @e = (); @e = (1 .. 3); print "@e\n"; shift @e; undef @e; push @e, "z"; print "@e\n";',
   "1 2 3\nz\n"],
  ['delete / exists / negative indices after shifts',
   'my @g = (1 .. 30); shift @g for 1 .. 5; delete $g[0]; print defined $g[0] ? "d" : "u", exists $g[1] ? "e" : "n", " $g[1] $g[-1] $g[-24] ", defined $g[-26] ? "d" : "u", " ", scalar(@g), "\n";',
   "ue 7 30 7 u 25\n"],
  ['a shifted array returned from a sub',
   'sub wa { my @x = (1 .. 30); shift @x for 1 .. 3; return @x } my @w = wa(); print "$w[0] $w[-1] ", scalar(@w), "\n";',
   "4 30 27\n"],
  ['sort / reverse / map / grep / join over a shifted array',
   'my @s = map { ($_ * 7) % 31 } 1 .. 30; shift @s for 1 .. 4; my @t = sort { $a <=> $b } @s; my @r = reverse @s; my @m = map { $_ * 2 } @s; my @g = grep { $_ % 2 } @s; print "@t[0..2] $r[0] $m[0] $g[0] ", join(",", @s[0..2]), "\n";',
   "1 2 3 24 8 11 4,11,18\n"],
  ['each over a shifted array',
   'my @e2 = (1 .. 30); shift @e2 for 1 .. 3; my @k; while (my ($i, $v) = each @e2) { push @k, "$i=$v"; last if @k > 2 } print "@k\n";',
   "0=4 1=5 2=6\n"],
  ['an unshift loop builds the array in reverse',
   'my @u; unshift @u, $_ for 1 .. 40; print "@u[0..3] $u[-1] ", scalar(@u), "\n";',
   "40 39 38 37 1 40\n"],
  ['a deque mixing push / unshift / shift / pop',
   'my @dq = (1 .. 20); for my $i (1 .. 100) { push @dq, $i; unshift @dq, -$i; shift @dq; pop @dq if $i % 3 == 0 } print "@dq[0..4] ", scalar(@dq), "\n";',
   "1 2 3 4 5 87\n"],
  ['splice(@a, 0, K) list and scalar values',
   'my @sp = (1 .. 40); my @rm = splice(@sp, 0, 3); my $last = splice(@sp, 0, 2); print "@rm | $last $sp[0] ", scalar(@sp), "\n"; splice(@sp, 0, 35); push @sp, 1; print "@sp\n";',
   "1 2 3 | 5 6 35\n1\n"],
  ['a steady-size queue',
   'my @q = (1 .. 100); my $c = 0; for (1 .. 1000) { push @q, $_; $c += shift @q } print "$c ", scalar(@q), " $q[0] $q[-1]\n";',
   "410500 100 901 1000\n"],
  ['read-only array: shift / unshift / splice still die',
   'my @ro = (1 .. 30); Internals::SvREADONLY(@ro, 1); for my $c (sub { shift @ro }, sub { unshift @ro, 1 }, sub { splice(@ro, 0, 2) }) { eval { $c->() }; print $@ =~ /^Modification of a read-only value/ ? "died " : "no " } print scalar(@ro), "\n";',
   "died died died 30\n"],
  ['references and blessed values keep their identity through the window',
   'my @h = ({a => 1}, [2], \"s", 4, "five", 6 .. 30); shift @h; my $x = shift @h; print ref($x), " ", ref($h[0]), " $h[1] $h[2]\n";',
   "ARRAY SCALAR 4 five\n"],
  ['foreach aliasing, slices and @_ flattening over a shifted array',
   'sub cnt { scalar @_ } my @o = (1 .. 50); shift @o for 1 .. 10; my @c = @o[5 .. 9]; $_ *= 2 for @o; @o = (@o, "x"); print "@c ", cnt(@o), " $o[0] $o[-1] $#o\n";',
   "16 17 18 19 20 41 22 x 40\n"],
  ['a mid-array splice after a front splice',
   'my @y = (1 .. 50); splice(@y, 0, 10); splice(@y, 2, 1, "A", "B"); print "@y[0..4] ", scalar(@y), "\n";',
   "11 12 A B 14 41\n"],
  ['unshift of a list, and of nothing',
   'my @t2 = (1 .. 20); unshift @t2, 1 .. 30; my @e; unshift @e, (); print "$t2[0] $t2[29] $t2[30] ", scalar(@t2), " ", scalar(@e), "\n";',
   "1 30 1 50 0\n"],
);
# ONE program for every case (one SBCL launch — the file's wall time is the
# metric): case K prints a "#K" marker line, then its own output.
my $prog = "no warnings;\n";
$prog .= "{ print \"#$_\\n\"; $cases[$_][1] }\n" for 0 .. $#cases;
my %got;
my $k;
for my $line (split /^/, run_pl($prog)) {
    if ($line =~ /^#(\d+)$/) { $k = $1; $got{$k} = ""; next }
    $got{$k} .= $line if defined $k;
}
for my $i (0 .. $#cases) {
    my ($desc, undef, $want) = @{ $cases[$i] };
    is($got{$i}, $want, $desc);
}

done_testing();
