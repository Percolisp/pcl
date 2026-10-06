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
like(lisp_out(q{(let ((a (make-array 3 :adjustable t :fill-pointer 3 :initial-contents (list 1 nil 3)))) (let ((v (%p-grep-cells a))) (print (list (simple-vector-p v) (p-box-p (aref a 0)) (eq (aref v 0) (aref a 0)) (aref a 1)))))}), qr/\Q\E\n\Q(t t t nil) \E/, '#2772: %p-grep-cells snapshots cells, promotes a raw slot in place, leaves a hole a hole');
{
    my $file = src_file('my $n = 3; my @a; unshift @a, $_ for 1 .. $n; print "@a\n";');
    my $cl = PCLCore::transpile(qq{$pl2cl $file});
    like($cl, qr/\(p-foreach-range-raw \(\$_ 1 \$n\) \(p-unshift \@a \$_\)\)/,
         '#2773: `unshift @a, $_ for 1..$n` binds $_ raw (p-foreach-range-raw), as push does');
}
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
answers(<<'END_SRC', <<'END_EXP', '#2772 answers: grep in scalar/boolean/list context, writes through $_, snapshots, holes, tied');
my @p = map { $_ % 3 ? 1 : 0 } 1 .. 20;
my $n = grep $_, @p; print "n=$n\n";
if (grep { $_ > 0 } @p) { print "some\n" }
print "s=", scalar(grep { !$_ } @p), "\n";
my @a = (1, 2, 3); my $k = grep { $_ *= 10; 1 } @a; print "k=$k @a\n";
my @m = (5, 6, 7); my @r = grep { $_ > 5 } @m; $_++ for @r; print "@m | @r\n";
my @s = (1 .. 5); my @g = grep { push @s, 99 if $_ == 1; 1 } @s; print scalar(@g), " ", scalar(@s), "\n";
my @x = (1, 2); my %hh = (a => 1); my @y = (3, 0);
print scalar(grep { $_ } @x, @y, %hh), "\n";
my @e = (); print "e=", scalar(grep { 1 } @e), "\n";
print "nest=", scalar(grep { my $v = $_; grep { $_ == $v } (2, 3) } (1 .. 4)), "\n";
sub wa { my @l = grep { defined(wantarray) ? 1 : 0 } (1, 2); scalar @l } print "wa=", wa(), "\n";
my @hole; $hole[3] = 1; my $c = grep { !defined } @hole; print "holes=$c exists1=", (exists $hole[1] ? 1 : 0), "\n";
my @u = ('0', '', '0.0', 'a', 0, 1, -1, 2**40, 0.0); print "u=", scalar(grep { $_ } @u), "\n";
for my $i (1 .. 3) { my @q = grep { $_ == $i } @p, 3; print "i$i=", scalar(@q) } print "\n";
my @z = (1 .. 6); my $cnt = 0; for my $e (1..2) { $cnt += grep { $_ & 1 } @z } print "cnt=$cnt\n";
my $f = eval { grep { die "boom\n" if $_ == 2; 1 } (1, 2, 3) }; print "err=$@";
sub g { for (1) { my $w = grep { return 7 if $_ == 2; 1 } (1, 2, 3) } 0 } print "ret=", g(), "\n";
package T; sub TIEARRAY { bless { a => [1, 0, 2] } } sub FETCH { $_[0]{a}[$_[1]] } sub FETCHSIZE { scalar @{$_[0]{a}} } sub STORE { $_[0]{a}[$_[1]] = $_[2] }
package main; tie my @ta, 'T'; print "tied=", scalar(grep { $_ } @ta), "\n";
my @refs = ([1], [], undef); print "refs=", scalar(grep { $_ } @refs), "\n";
END_SRC
n=14
some
s=6
k=3 10 20 30
5 6 7 | 7 8
5 6
5
e=0
nest=2
wa=2
holes=3 exists1=0
u=5
i1=14i2=0i3=1
cnt=6
err=boom
ret=7
tied=2
refs=2
END_EXP
answers(<<'END_SRC', <<'END_EXP', '#2773 answers: unshift in raw loops, aliasing, refs, self, tied, local, return value');
my @a; unshift @a, $_ for 1 .. 5; print "@a\n";
$_ *= 2 for @a; print "@a\n";
my @b; for (1 .. 3) { unshift @b, $_ } $b[0] = 'x'; print "@b\n";
my @c; for (1 .. 3) { unshift @c, $_; $_ = 0 if 0 } my $r = \$c[1]; $$r = 'R'; print "@c\n";
my @src = (1, 2, 3); my @d; for (@src) { unshift @d, $_ } $src[0] = 9; print "@d | @src\n";
my @e = (1, 2); unshift @e, @e; print "@e\n";
my @f; unshift @f; print scalar(@f), "\n";
my @g; for (1 .. 3) { unshift @g, $_, "s$_", [$_] } print join(',', map { ref $_ ? "[$$_[0]]" : $_ } @g), "\n";
my @h = (0); for (1 .. 4) { unshift @h, $_ * 1.5 } print "@h\n";
our @l = (1); sub show { print "@l\n" } { local @l = (); unshift @l, $_ for 1 .. 3; show() } show();
package T; sub TIEARRAY { bless { a => [] } } sub FETCH { $_[0]{a}[$_[1]] } sub FETCHSIZE { scalar @{$_[0]{a}} }
sub UNSHIFT { my $s = shift; unshift @{$s->{a}}, map { "t$_" } @_ } sub STORE { $_[0]{a}[$_[1]] = $_[2] } sub STORESIZE {} sub EXTEND {}
package main; tie my @t, 'T'; unshift @t, $_ for 1 .. 3; print join(',', map { $t[$_] } 0 .. 2), "\n";
my $cnt = 0; for (1 .. 1000) { unshift @a, $_ } print scalar(@a), " $a[0] $a[-1]\n";
my @m; for (1 .. 3) { my $x = unshift @m, $_; print "$x " } print "\n";
my @w = (1); for (1 .. 2) { unshift @w, "$_" . "x" } print "@w\n";
END_SRC
5 4 3 2 1
10 8 6 4 2
x 2 1
3 R 1
3 2 1 | 9 2 3
1 2 1 2
0
3,s3,[3],2,s2,[2],1,s1,[1]
6 4.5 3 1.5 0
3 2 1
1
t3,t2,t1
1005 1000 2
1 2 3 
2x 1x 1
END_EXP

done_testing();
