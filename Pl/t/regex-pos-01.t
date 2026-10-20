#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# regex-pos-01.t: a match that starts at pos() reads the WHOLE subject (s523a).
#
# #2900: cl-ppcre reads a scan's START as the start of the string for ^, \A,
# \b, \B and lookbehind.  Every PCL scan from a pos() past 0 (a /g step, a \G
# anchor) handed cl-ppcre that pos as START, so `pos($t)=1; $t =~ /\G\b(y)/gc`
# matched inside "my" (Text::Balanced's quotelike parser), and
# `$n++ while "aaa" =~ /^a/g` counted three.  The expected text of every row
# was PROBED on perl 5.40.3 (and holds on 5.38: nothing here is new syntax).
#
# #1568: the lvalue builtins pos, vec, substr and $#array under ++, --, +=,
# -=, .=, x= stored NOTHING (the compound macros BOX-SET the new value onto a
# number): Text::Balanced's `pos($$textref)--` before the second block of
# `s{a}{b}`, so extract_quotelike and extract_tagged failed.
#
# #1511 (Algorithm::Diff): a `..` in a sub's return / tail position is a RANGE
# when the caller wants a list (it was always the flip-flop for non-literal
# operands); `X || not $v = E` reduced `not $v` alone; and PPI lexes the `&`
# of a statement-initial `&$cb(...)` after `{ 1 while $x; }` as bitwise AND.
#
# #966's shapes (pos of an element after a //g match) are kept here as rows;
# they were fixed by #3063 (Pl/t/elem-pos-01.t is that fix's own guard).

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

plan skip_all => "pl2cl not found" if ! -x $pl2cl;
plan skip_all => "sbcl not found"  if ! `which sbcl 2>/dev/null`;

sub run_cl {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;
    my $cl_code = PCLCore::transpile(qq{$pl2cl $pl_file});
    my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cl_fh $cl_code;
    close $cl_fh;
    my $out = `sbcl @sbcl_rt --load $cl_file 2>&1`;
    $out =~ s/^;.*\n//gm;
    $out =~ s/^PCL Runtime loaded\n//gm;
    $out =~ s/^\s*\n//gm;
    return $out;
}

# Each program prints one "name: answer" line per row; EXPECTED is perl's.
my @cases = (
  [ 'the assertions before pos (#2900)', <<'PL', <<'EXP' ],
my $t = "my \$p = 1;";
pos($t) = 1; print "G-b: ", ($t =~ /\G\b(y)\b/gc ? "match" : "no"), "\n";
pos($t) = 1; print "G-b-alt: ", ($t =~ m/\G(?:\s*)(\b(?:m|s|y)\b)/gc ? "match" : "no"), "\n";
my $u = "ab cd";
pos($u) = 1; print "G-B: ", ($u =~ /\G\B(b)/gc ? "match" : "no"), " pos=", pos($u)//'u', "\n";
pos($u) = 1; print "G-lb: ", ($u =~ /\G(?<=a)(b)/gc ? "match" : "no"), "\n";
pos($u) = 1; print "G-nlb: ", ($u =~ /\G(?<!a)(b)/gc ? "match" : "no"), "\n";
pos($u) = 1; print "G-caret: ", ($u =~ /\G^(b)/gc ? "match" : "no"), "\n";
pos($u) = 1; print "G-A: ", ($u =~ /\G\A(b)/gc ? "match" : "no"), "\n";
my $s = "aaa"; my $n = 0; $n++ while $s =~ /^a/g; print "caret-g: $n\n";
$s = "aaa"; $n = 0; $n++ while $s =~ /\Aa/g; print "A-g: $n\n";
$s = "ab"; pos($s)=1; my @l = $s =~ /\G\bb/g; print "list-Gb: [@l]\n";
$s = "ab"; pos($s)=1; @l = $s =~ /^b/g; print "list-caret: [@l]\n";
$s = "xy zy"; pos($s)=3; if ($s =~ /\G\b(z)/gc) { print "after-G: pos=", pos($s), " -=@- +=@+ pre=$` m=$& post=$'\n" }
$s = "xy zy"; pos($s)=1; print "single-G-b: ", ($s =~ /\G\by/ ? "match" : "no"), "\n";
PL
G-b: no
G-b-alt: no
G-B: match pos=2
G-lb: match
G-nlb: no
G-caret: no
G-A: no
caret-g: 1
A-g: 1
list-Gb: []
list-caret: []
after-G: pos=4 -=3 3 +=4 4 pre=xy  m=z post=y
single-G-b: no
EXP
  [ 'pos(ELEM) after ELEM =~ //g (#966, fixed by #3063)', <<'PL', <<'EXP' ],
my $r = { m => "abcabc" }; $r->{m} =~ /c/g; print "href: ", pos($r->{m}) // "undef", "\n";
my @a = ("zzz");           $a[0] =~ /z/g;    print "aelem: ", pos($a[0]) // "undef", "\n";
my %h = (p => "abc");      $h{p} =~ /b/g;    print "helem: ", pos($h{p}) // "undef", "\n";
my $ar = ["qqq"];          $ar->[0] =~ /q/g; print "aref: ", pos($ar->[0]) // "undef", "\n";
my $s = "abc"; $s =~ /b/g; print "scalar: ", pos($s) // "undef", "\n";
my $sr = \$s; $$sr =~ /c/g; print "deref: ", pos($$sr) // "undef", " same=", pos($s) // "undef", "\n";
$_ = "xyz"; /y/g; print "topic: ", pos() // "undef", "\n";
$h{p} =~ /z/g;  print "helem-fail-reset: ", pos($h{p}) // "undef", "\n";
$h{p} = "abc"; $h{p} =~ /b/g; $h{p} =~ /z/gc; print "helem-fail-keep: ", pos($h{p}) // "undef", "\n";
my %k = (k => "axbxcx"); my $n = 0; while ($k{k} =~ /x/g) { $n++; print "loop-pos: ", pos($k{k}), "\n" } print "loop: $n\n";
pos($h{p}) = 1; print "write-then-read: ", pos($h{p}), "\n";
my @m = ($h{p} =~ /./g); print "list: ", scalar(@m), " pos=", pos($h{p}) // "undef", "\n";
PL
href: 3
aelem: 1
helem: 2
aref: 1
scalar: 2
deref: 3 same=3
topic: 2
helem-fail-reset: undef
helem-fail-keep: 2
loop-pos: 2
loop-pos: 4
loop-pos: 6
loop: 3
write-then-read: 1
list: 2 pos=undef
EXP
  [ 'compound and ++/-- on an lvalue builtin (#1568)', <<'PL', <<'EXP' ],
my $s = "abcdef"; pos($s) = 3; pos($s)--; print "pos-postdec: ", pos($s), "\n";
my $r = \$s; pos($s) = 3; pos($$r)--; print "pos-deref-postdec: ", pos($s), "\n";
pos($s) = 3; my $v = pos($$r)--; print "pos-postdec-value: $v ", pos($s), "\n";
pos($s) = 3; pos($$r) -= 1; print "pos-minus: ", pos($s), "\n";
pos($s) = 3; pos($$r)++; print "pos-postinc: ", pos($s), "\n";
pos($s) = 3; --pos($$r); print "pos-predec: ", pos($s), "\n";
pos($s) = 3; pos($s) += 9; print "pos-plus-clamps: ", pos($s), "\n";
sub f { my $t = \$_[0]; pos($$t) = 4; pos($$t)--; return pos($$t) } my $u = "xyzxyz"; print "pos-through-ref-arg: ", f($u), " ", pos($u), "\n";
my %h = (k => "abcd"); pos($h{k}) = 1; pos($h{k})++; print "pos-elem-inc: ", pos($h{k}), "\n";
my $w = "\x05"; vec($w, 0, 8) += 2; print "vec-plus: ", ord($w), "\n";
$w = "\x05"; vec($w, 0, 8)++; print "vec-postinc: ", ord($w), "\n";
my $t = "abc"; substr($t, 0, 1) .= "X"; print "substr-cat: $t\n";
$t = "5bc"; substr($t, 0, 1)++; print "substr-postinc: $t\n";
$t = "5bc"; substr($t, 0, 1) += 3; print "substr-plus: $t\n";
$t = "abc"; substr($t, 1) x= 2; print "substr-2arg-x: $t\n";
my @a = (1,2,3); $#a -= 2; print "arylen-minus: ", scalar(@a), "\n";
@a = (1,2,3); $#a += 2; print "arylen-plus: ", scalar(@a), "\n";
PL
pos-postdec: 2
pos-deref-postdec: 2
pos-postdec-value: 3 2
pos-minus: 2
pos-postinc: 4
pos-predec: 2
pos-plus-clamps: 6
pos-through-ref-arg: 3 3
pos-elem-inc: 2
vec-plus: 7
vec-postinc: 6
substr-cat: aXbc
substr-postinc: 6bc
substr-plus: 8bc
substr-2arg-x: abcbc
arylen-minus: 1
arylen-plus: 5
EXP
  [ 'range or flip-flop decided by the caller\'s context (#1511)', <<'PL', <<'EXP' ],
sub r1 { my ($x, $y) = @_; return ($x) .. ($y - 1); }
my @a = r1(2, 5); print "ret-paren: [@a]\n";
sub r2 { my ($x, $y) = @_; return $x .. $y; }
@a = r2(3, 4); print "ret-plain: [@a]\n";
sub r3 { my ($x, $y) = @_; $x .. $y }
@a = r3(1, 3); print "implicit: [@a]\n";
my @b = (10, 20, 30, 40); print "slice: [@b[r2(1, 2)]]\n";
sub r4 { my ($x, $y) = @_; return wantarray ? ($x .. $y) : -1 }
@a = r4(0, 2); print "ternary: [@a]\n";
my $n = 0 + (() = r2(1, 4)); print "count: $n\n";
sub r5 { my ($x, $y) = @_; my @r = ($x .. $y); return @r }
@a = r5(5, 6); print "via-array: [@a]\n";
PL
ret-paren: [2 3 4]
ret-plain: [3 4]
implicit: [1 2 3]
slice: [20 30]
ternary: [0 1 2]
count: 4
via-array: [5 6]
EXP
  [ 'flip-flop positions stay flip-flops (#1511 inverse)', <<'PL', <<'EXP' ],
my @x = grep { /b/ .. /d/ } qw(a b c d e); print "grep: @x\n";
my @y = grep { $_ eq "b" .. $_ eq "c" } qw(a b c d); print "grep2: @y\n";
my @m = map { scalar(/b/ .. /c/) } qw(a b c d); print "map: [@m]\n";
my @r; for (qw(a b c d e)) { push @r, $_ if /b/ .. /d/ } print "if: @r\n";
sub inner { my $s = shift; return ($s =~ /b/) .. ($s =~ /c/) } my @l = inner("b"); print "sub-list: [@l]\n";
my $sc = inner("b"); print "sub-scalar: [$sc]\n";
sub tailff { $_[0] =~ /b/ .. $_[0] =~ /d/ } my @t = map { scalar tailff($_) } qw(a b c d e); print "tail-scalar: [@t]\n";
my @u = (sub { 2 .. 4 })->(); print "anon: [@u]\n";
my @v = do { 1 .. 3 }; print "do: [@v]\n";
sub ctx { my $n = shift; return 1 .. $n } my $cnt = () = ctx(3); print "cnt: $cnt\n";
print "list-print: ", join(",", ctx(4)), "\n";
PL
grep: b c d
grep2: b c
map: [ 1 2E0 ]
if: b c d
sub-list: []
sub-scalar: [1]
tail-scalar: [ 1 2 3E0 ]
anon: [2 3 4]
do: [1 2 3]
cnt: 3
list-print: 1,2,3,4
EXP
  [ '`X || not $v = E` is `X || not($v = E)` (#1511)', <<'PL', <<'EXP' ],
my %h = (min => 5);
my $k; my $r = (0 || not $k = $h{min}); print "or-not-assign: r=[$r] k=", $k // "u", "\n";
my $x = not 5; print "assign-not: [$x]\n";
my @a = not 0; print "list-not: [@a]\n";
my ($p, $q) = (1, 2); print "not-eq: [", (not $p == $q), "]\n";
my $y; my $s = (1 && not $y = 0); print "and-not-assign: s=[$s] y=$y\n";
my $t = (0 || not 1 ? "A" : "B"); print "or-not-ternary: [$t]\n";
my $u = (0 || not $p and 0); print "or-not-and: [", $u // "u", "]\n";
my $w; $w = 0 || not $p + $q; print "or-not-plus: [$w]\n";
my $v = 0 || not 0; print "or-not-const: [$v]\n";
my @l = (1, 0 || not 0, 2); print "in-list: [@l]\n";
PL
or-not-assign: r=[] k=5
assign-not: []
list-not: [1]
not-eq: [1]
and-not-assign: s=[1] y=0
or-not-ternary: []
or-not-and: []
or-not-plus: []
or-not-const: [1]
in-list: [1 ]
EXP
  [ 'a statement-initial &$cb after a block is a call (#1511, PPI §38)', <<'PL', <<'EXP' ],
sub tr2 {
    my @ev;
    my $discardACallback = sub { push @ev, "A$_[0]" };
    my $discardBCallback = sub { push @ev, "B$_[1]" };
    my $finishedACallback; my $finishedBCallback;
    my ($ai, $bi, $lastA, $lastB) = (8, 9, 9, 11);
    while ( $ai <= $lastA or $bi <= $lastB )
    {
        if ( $ai == $lastA + 1 and $bi <= $lastB )
        {
            if ( defined($finishedACallback) ) { $finishedACallback = undef; }
            else { &$discardBCallback( $ai, $bi++, @_ ) while $bi <= $lastB; }
        }
        if ( $bi == $lastB + 1 and $ai <= $lastA )
        {
            if ( defined($finishedBCallback) ) { $finishedBCallback = undef; }
            else { &$discardACallback( $ai++, $bi, @_ ) while $ai <= $lastA; }
        }
        &$discardACallback( $ai++, $bi, @_ ) if $ai <= $lastA;
        &$discardBCallback( $ai, $bi++, @_ ) if $bi <= $lastB;
    }
    return "@ev";
}
print "full: ", tr2(), "\n";
sub tr3 {
    my @ev; my $cb = sub { push @ev, "A$_[0]" };
    my ($ai, $lastA) = (8, 9);
    &$cb( $ai++, 0, @_ ) if $ai <= $lastA;
    return "@ev";
}
print "with-args: ", tr3(), "\n";
sub tr4 {
    my @ev; my $cb = sub { push @ev, "A$_[0]" };
    my ($ai, $lastA) = (8, 9);
    &$cb( $ai++, 0 ) if $ai <= $lastA;
    return "@ev";
}
print "no-args: ", tr4(), "\n";
PL
full: A8 B9 A9 B10 B11
with-args: A8
no-args: A8
EXP
);

my $rows = 0;
$rows += scalar(split /\n/, $_->[2]) for @cases;
plan tests => $rows;

for my $c (@cases) {
    my ($what, $prog, $exp) = @$c;
    my @got = split /\n/, run_cl($prog);
    my @exp = split /\n/, $exp;
    for my $i (0 .. $#exp) {
        my ($name) = $exp[$i] =~ /^([^:]+):/;
        is($got[$i] // '(no line)', $exp[$i], "$what: $name");
    }
}
