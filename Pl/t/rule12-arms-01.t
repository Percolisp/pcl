#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# rule12-arms-01.t -- guard for s525u M3 (docs/rule12-arm-audit-s525.md): the
# four default arms the audit converted from a silent `(t "")' / `(t 0)' to a
# DIE naming the unhandled value (CLAUDE.md rule 12).
#
# None of the four is reachable from a Perl program -- each closed a dispatch
# whose legal values all have arms -- so the PERL rows below pin the legal
# NEIGHBOURS the conversion must not disturb (sprintf's `#' prefix per base,
# vec's every width at every alignment and past the end, wantarray in its
# three contexts; expectations are perl 5.40.3's output of the same program),
# and the two CL rows call the converted arms directly with the one value no
# Perl program can hand them and assert the die text.  sprintf-one's prefix
# arm and p-vec's read arm cannot be reached even from CL (their dispatch value
# is computed inside the function), so they have no row: converted for the
# invariant only.

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

plan tests => 15;

sub run_pcl {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;
    my $cl_code = `$pl2cl $pl_file 2>/dev/null`;
    my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cl_fh $cl_code;
    close $cl_fh;
    my $out = `sbcl @sbcl_rt --load $cl_file 2>&1`;
    $out =~ s/^;.*\n//gm;
    $out =~ s/^PCL Runtime loaded\n//gm;
    return $out;
}

my $PROG = <<'PERL';
no warnings;
printf "f01=%#d|%#i|%#u|%#x|%#X|%#o|%#b|%#B\n", 7, -7, 7, 255, 255, 8, 5, 5;
printf "f02=%#d|%#x|%#o|%#b|%#.0d|%#5d|%-#6x|\n", 0, 0, 0, 0, 0, 42, 42;
my $s = "abcdefghij";
print "v01=", join(",", map { vec($s, $_, 1) } 0 .. 9), "\n";
print "v02=", join(",", map { vec($s, $_, 2) } 0 .. 9), "\n";
print "v03=", join(",", map { vec($s, $_, 4) } 0 .. 9), "\n";
print "v04=", join(",", map { vec($s, $_, 8) } 0 .. 11), "\n";
print "v05=", join(",", map { vec($s, $_, 16) } 0 .. 5), "\n";
print "v06=", join(",", map { vec($s, $_, 32) } 0 .. 3), "\n";
print "v07=", join(",", map { vec($s, $_, 64) } 0 .. 2), "\n";
print "v08=", vec($s, -1, 8), ",", vec("", 0, 64), ",", vec($s, 100, 1), "\n";
sub ctx { wantarray ? "list" : defined(wantarray) ? "scalar" : "void" }
my @l = ctx(); my $c = ctx(); our $v; sub vctx { $v = ctx() } ;
sub inner { my $r = wantarray; print "w01=", (defined $r ? "[$r]" : "undef"), "\n" } inner();
my @x = (sub { wantarray ? "L" : "S" })->(); my $y = (sub { wantarray ? "L" : "S" })->();
print "w02=$l[0]|$c|$x[0]|$y\n";
sub sc { my $w = wantarray; return defined $w ? ($w eq "" ? "empty" : $w) : "undef" } my $z = sc(); my @zz = sc();
print "w03=$z|$zz[0]\n";
PERL

my %EXPECT = (
    'f01' => '7|-7|7|0xff|0XFF|010|0b101|0B101',
    'f02' => '0|0|0|0||   42|0x2a  |',
    'v01' => '1,0,0,0,0,1,1,0,0,1',
    'v02' => '1,0,2,1,2,0,2,1,3,0',
    'v03' => '1,6,2,6,3,6,4,6,5,6',
    'v04' => '97,98,99,100,101,102,103,104,105,106,0,0',
    'v05' => '24930,25444,25958,26472,26986,0',
    'v06' => '1633837924,1701209960,1768554496,0',
    'v07' => '7017280452245743464,7595883721513762816,0',
    'v08' => '0,0,0',
    'w01' => 'undef',
    'w02' => 'list|scalar|L|S',
    'w03' => 'empty|1',
);

my $out = run_pcl($PROG);
my %got;
$got{$1} = $2 while $out =~ /^([fvw]\d\d)=(.*)$/mg;
for my $k (sort keys %EXPECT) {
    is($got{$k} // "<MISSING; output was:\n$out>", $EXPECT{$k}, "perl row $k");
}

# The converted arms, reached from CL with the value no Perl program produces.
my ($lfh, $lfile) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
print $lfh <<'CL';
(defun r12-try (thunk)
  (handler-case (progn (funcall thunk) "NO ERROR")
    (error (e) (princ-to-string e))))
(format t "c01=~A~%" (r12-try (lambda () (pcl::sprintf-format-int 5 7 nil t))))
(format t "c02=~A~%" (r12-try (lambda () (let ((pcl:*pcl-caller-wantarray* :list))
                                           (pcl:p-wantarray)))))
CL
close $lfh;
my $cl = `sbcl @sbcl_rt --load $lfile 2>&1`;
like($cl, qr/^c01=PCL internal: sprintf-format-int: unhandled base 7$/m,
     'sprintf-format-int dies naming an unlisted base');
like($cl, qr/^c02=PCL internal: wantarray: the caller's context is :(?i:list), not T \/ NIL \/ :VOID$/m,
     'wantarray dies naming a context outside the three-valued protocol');
