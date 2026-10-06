#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# perf-levers-09.t — perf round 40 (s510p, docs/faster-codegen-suggestions.md
# §0.2x).  Each lever has a MECHANISM row (it fails on the round's base,
# main 0b401b91) and an ANSWER row: a program whose expected output below IS
# perl's own (probed on 5.40.3; nothing in them needs perl >= 5.38).
#
#   #2770 ONE typed string copy (%pcl-str-blit) at every short-string copy
#         site: the str-buffer and :strbuf appends, join, `.` (%p-concat-2)
#         and interpolation;
#   #2772 grep over ONE untied @array walks a simple-vector snapshot of its
#         cells (%p-grep-cells), and a boxed fixnum's truth is one compare;
#   #2773 `unshift` joins `push` on the raw-topic allowlist, so
#         `unshift @a, $_ for A..B` binds $_ raw (no box per iteration).
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
like(lisp_out(q{(progn (print (not (null (fboundp '%p-concat-2)))) (let ((d (make-string 6 :initial-element #\-))) (%pcl-str-blit d 0 (coerce "ab" 'simple-base-string) 2) (%pcl-str-blit d 2 (make-array 2 :element-type 'character :initial-contents "cd" :adjustable t) 2) (%pcl-str-blit d 4 (copy-seq "ef") 2) (print d)))}), qr/\Q\E\n\Qt \E\n\Q"abcdef" \E/, '#2770: %p-concat-2 exists; %pcl-str-blit copies a base, a non-simple and a character string');
answers(<<'END_SRC', <<'END_EXP', '#2770 answers: .=, ., join, interpolation, lc.uc.ucfirst, :strbuf appends, overloaded .');
use utf8; binmode STDOUT, ':utf8';
my $s = ''; $s .= 'xy' for 1..5; $s .= "é" x 20; $s .= substr("abcdefghijklmnopqrstuvwxyz0123", 3, 20);
print "$s\n", length($s), "\n";
my $t = ''; for my $i (1..4) { $t = $t . "ab$i" } print "$t\n";
my @a = (1..8, 'ü', "x" x 30); print join(',', @a), "\n"; print join('', @a), "\n"; print join("~", "a"), "|", join("--", ()), "|\n";
print "@a[0..3]\n";
my $u = "Ligne \xC3\xA9t\xC3\xA9 NO 5"; my $v = lc($u) . uc($u) . ucfirst($u); print length($v), " ", $v, "\n";
my %h = (k => 'a'); $h{k} .= 'bc' for 1..3; $h{k} .= "Z" x 40; print "$h{k}\n";
my @b = ('q'); $b[0] .= "r$_" for 1..20; print "$b[0]\n";
my $r = \my $w; $$r .= "é$_" for 1..10; print "$w\n";
my $x = "A" . "" . "B"; print "$x", "" . "", "\n"; my $z = 3 . 4; print $z + 1, "\n";
my $big = "0123456789" x 5; my $c = $big . $big; print length($c), substr($c, 45, 10), "\n";
my $self = "ab"; $self .= $self for 1..5; print "$self\n";
{ package O; use overload '.' => sub { my ($a,$b,$sw)=@_; "[" . ($sw ? "$b|$a->[0]" : "$a->[0]|$b") . "]" }, '""' => sub { "O" }; }
my $o = bless ['o'], 'O'; print "a $o b", "\n"; print $o . "x", "\n";
END_SRC
xyxyxyxyxyéééééééééééééééééééédefghijklmnopqrstuvw
50
ab1ab2ab3ab4
1,2,3,4,5,6,7,8,ü,xxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
12345678üxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
a||
1 2 3 4
48 ligne Ã©tÃ© no 5LIGNE Ã©TÃ© NO 5Ligne Ã©tÃ© NO 5
abcbcbcZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ
qr1r2r3r4r5r6r7r8r9r10r11r12r13r14r15r16r17r18r19r20
é1é2é3é4é5é6é7é8é9é10
AB
35
1005678901234
abababababababababababababababababababababababababababababababab
[a |o] b
[o|x]
END_EXP

done_testing();
