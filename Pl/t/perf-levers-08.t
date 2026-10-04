#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# perf-levers-08.t — perf round 39 (s507p, docs/faster-codegen-suggestions.md
# §0.2w).  RUNTIME levers: the generated code is unchanged, so each lever's
# MECHANISM row reads the loaded runtime (it fails on the round's base), and
# its ANSWER rows run a program and compare with perl 5.40.3's own output
# (probed; the expected text below IS perl's).
#
#   #2539 one predicate for box-set's two pos() resets (%p-clear-match-pos):
#         a store asks the table only when some pos() exists;
#   #2637 %p-tied refuses a NON-EMPTY container before the weak tie table
#         (a tied container is an empty shell), so one live tie no longer
#         costs every untied container a weak-table lookup.
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

sub src_file {
    my ($src) = @_;
    my ($fh, $file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $src;
    close $fh;
    return $file;
}

sub run_pl {
    my ($src) = @_;
    my $file = src_file($src);
    my $cl = PCLCore::transpile(qq{$pl2cl $file});
    my ($cfh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cfh $cl;
    close $cfh;
    my $out = `sbcl @sbcl_rt --load $cl_file 2>&1`;
    $out =~ s/^;.*\n//gm;
    $out =~ s/^(?:caught |compilation unit|-->|==>|PCL Runtime loaded).*\n//gm;
    $out =~ s/^\s*\n//gm;
    return $out;
}

# Every line of EXPECTED must appear, whole, in OUT, in order and alone.
sub answers {
    my ($src, $expected, $what) = @_;
    is(run_pl($src), $expected, $what);
}

# The runtime's own answers to a few forms (the MECHANISM rows).
sub lisp_out {
    my ($forms) = @_;
    my ($lfh, $lfile) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $lfh "(in-package :pcl)\n$forms";
    close $lfh;
    return scalar `sbcl @sbcl_rt --load $lfile 2>&1`;
}

# ─────────────────────────────────────────────────────────────────────────────
# MECHANISM (runtime reads)
# ─────────────────────────────────────────────────────────────────────────────
my $mech = lisp_out(<<'LISP');
(format t "clearpos ~a~%" (and (fboundp '%p-clear-match-pos) t))
(format t "tiedshape ~a~%"
  (and (search "%p-tie-shell-shape-p"
               (string-downcase (prin1-to-string (macroexpand-1 '(%p-tied x)))))
       t))
(format t "shapes ~a~%"
  (list (%p-tie-shell-shape-p (make-hash-table))
        (%p-tie-shell-shape-p (let ((h (make-hash-table))) (setf (gethash :__class__ h) "C") h))
        (%p-tie-shell-shape-p (let ((h (make-hash-table :test 'equal)))
                                (setf (gethash "a" h) 1 (gethash "b" h) 2) h))
        (%p-tie-shell-shape-p (make-array 0 :adjustable t :fill-pointer 0))
        (%p-tie-shell-shape-p (make-array 2 :adjustable t :fill-pointer 2))
        (%p-tie-shell-shape-p "")
        (%p-tie-shell-shape-p (make-p-box 1))))
LISP
like($mech, qr/^clearpos T$/mi,
     '#2539: box-set\'s two pos() resets share %p-clear-match-pos');
like($mech, qr/^tiedshape T$/mi,
     '#2637: %p-tied asks the shell-shape test before the weak tie table');
like($mech, qr/^shapes \(T T NIL T NIL NIL NIL\)$/mi,
     '#2637: only an empty vector or a hash of at most one entry can be a tied shell');

# ─────────────────────────────────────────────────────────────────────────────
# ANSWERS (perl's)
# ─────────────────────────────────────────────────────────────────────────────
answers(<<'PERL', <<'EXPECTED', '#2539: a store resets pos() on both store arms');
my $s = "aaa"; $s =~ /a/g; print "p1 ", pos($s), "\n";
$s = "bbbb"; print "fast ", (defined pos($s) ? "def" : "undef"), "\n";
$s =~ /b/g; $s =~ /b/g; print "p2 ", pos($s), "\n";
my $t = "cc"; $s = $t; print "general ", (defined pos($s) ? "def" : "undef"), "\n";
$s =~ /c/g; my $r = [1]; $s = $r; print "ref ", (defined pos($s) ? "def" : "undef"), "\n";
my $u = "zz"; my $n = 0; $n++ while $u =~ /z/g; print "loop $n\n";
my $w = "abab"; my @p; while ($w =~ /b/g) { push @p, pos($w) } $w = "x"; print "w @p ", (defined pos($w) ? "def" : "undef"), "\n";
PERL
p1 1
fast undef
p2 2
general undef
ref undef
loop 2
w 2 4 undef
EXPECTED

answers(<<'PERL', <<'EXPECTED', '#2637: untied containers beside a live tie, and the tied ones');
require Tie::Hash; require Tie::Array;
my %one = (a => 1); my %empty; my @emp; my @full = (1, 2);
my $obj = bless {}, 'Foo'; my $obj1 = bless { x => 1 }, 'Foo';
my %pre = (h => 9); tie my %t, 'Tie::StdHash'; $t{x} = 1; $t{y} = 2;
tie my @ta, 'Tie::StdArray'; push @ta, 3, 4;
tie %pre, 'Tie::StdHash';
print "t ", join(",", map { "$_=$t{$_}" } sort keys %t), "\n";
print "ta ", join(",", @ta), " n=", scalar(@ta), "\n";
print "pre tied ", (exists $pre{h} ? "has-h" : "no-h"), " n=", scalar(keys %pre), "\n";
$pre{z} = 3; print "pre z ", $pre{z}, "\n";
$one{b} = 2; print "one ", join(",", sort keys %one), "\n";
$empty{q} = 1; print "empty ", join(",", keys %empty), "\n";
push @emp, 5; print "emp @emp\n"; push @full, 3; print "full @full\n";
my @c = @full; my %c = %one; print "copies ", scalar(@c), " ", scalar(keys %c), "\n";
$obj->{k} = 1; print "obj ", ref($obj), " ", join(",", keys %$obj), "\n";
print "obj1 ", $obj1->{x}, "\n";
print "tied? ", (tied(%t) ? 1 : 0), (tied(%one) ? 1 : 0), (tied(@ta) ? 1 : 0), (tied(@emp) ? 1 : 0), (tied(%empty) ? 1 : 0), "\n";
my $bt = bless {}, 'Bar'; tie %$bt, 'Tie::StdHash'; $bt->{m} = 4; print "blessed tied ", ref($bt), " ", $bt->{m}, " ", (tied(%$bt) ? 1 : 0), "\n";
untie %pre; print "pre after untie ", join(",", map { "$_=$pre{$_}" } sort keys %pre), "\n";
untie @ta; print "ta after untie n=", scalar(@ta), "\n";
tie my @ta2, 'Tie::StdArray'; @ta2 = (1, 2, 3); print "ta2 ", shift(@ta2), " ", scalar(@ta2), "\n";
PERL
t x=1,y=2
ta 3,4 n=2
pre tied no-h n=0
pre z 3
one a,b
empty q
emp 5
full 1 2 3
copies 3 2
obj Foo k
obj1 1
tied? 10100
blessed tied Bar 4 1
pre after untie h=9
ta after untie n=0
ta2 1 2
EXPECTED

done_testing();
