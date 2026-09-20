#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# global-match-01.t — perl's advance rule for a GLOBAL match (task #1719).
#
# perl's regexec takes a MINEND: the match's end must be at least that many
# characters past the start position.  A global match passes 1 exactly when the
# match that set the current position was ZERO-LENGTH (pp_hot.c's was_zero_len
# for m//g and s///g; pp_split passes 1 unconditionally).  That one number is
# the whole rule: the attempt both FORBIDS repeating the empty match at this
# position and ALLOWS a longer match starting here — perl's retry — and,
# failing both, an ordinary match further along.
#
# PCL had neither half consistently.  The scalar m//g iterator never advanced
# past a zero-length match at all, so `while ($s =~ /(\w*)/g)` NEVER TERMINATED
# (heap exhaustion, measured); s///g and list-context m//g used cl-ppcre's
# do-scans, which advances one character instead of retrying, so every
# non-empty alternative available at that position was lost.
#
# Every expectation below is perl 5.40.3's own output for the same program.
# The `while` loops are BOUNDED (`last if ++$i > N`) and every child runs under
# a timeout, so a regression fails a row instead of hanging the gate.
#
# Known divergence NOT asserted here: after s///g, perl's $1 reflects the LAST
# match and PCL's the FIRST (`$z='a1b22c'; $z =~ s/(\d+)/-/g` leaves $1 = 22 in
# perl, 1 in PCL) — task #1751, a separate mechanism (the up-front scan that
# sets the match variables), untouched by this rule.

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

plan tests => 5;

sub run_cl {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;
    my $cl_code = `$pl2cl $pl_file 2>/dev/null`;
    my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cl_fh $cl_code;
    close $cl_fh;
    # A regression in the advance rule is an INFINITE LOOP, so the child is
    # killed rather than left to hang the gate (or eat the heap): the row then
    # fails on the timeout marker.
    my $output = `timeout -s KILL 90 sbcl @sbcl_rt --load $cl_file 2>&1`;
    $output .= "TIMEOUT/KILLED\n" if $? == 9 || ($? & 127) == 9;
    $output =~ s/^;.*\n//gm;
    $output =~ s/^PCL Runtime loaded\n//gm;
    $output =~ s/^\s*\n//gm;
    return $output;
}

# ── 1. the divergence table: a pattern that can match empty AND non-empty ──
# at one position is where the retry shows.  Rows 4-7 are the pure-empty cases
# that need only the advance and already agreed — they are the regression risk
# (a retry without the advance turns `s/x*/-/g` into its own infinite loop).
my $table = <<'PROG';
$_ = 'xxxx'; s/\d*|x/<$&>/g; print "1 subst-alt: $_\n";
my @m = ('xax' =~ /\d*|x/g); print "2 list-alt: ", scalar(@m), " [", join("|", map { defined $_ ? $_ : "U" } @m), "]\n";
my $p = 'abc'; my @ps;
for (1..3) { $p =~ /b*/g; push @ps, defined(pos($p)) ? pos($p) : "U"; }
print "3 pos-b*: @ps\n";
$_ = 'abc'; s/x*/-/g; print "4 subst-x*: $_\n";
$_ = 'aXbXc'; s/X*/./g; print "5 subst-X*: $_\n";
print "6 split-x*: ", join(",", split(/x*/, 'abc')), "\n";
my $n = () = ('aaa' =~ /a*/g); print "7 count-a*: $n\n";
PROG
is(run_cl($table), <<'EXPECT', 'zero-length match: the retry table matches perl 5.40.3');
1 subst-alt: <><x><><x><><x><><x><>
2 list-alt: 6 [|x|||x|]
3 pos-b*: 0 2 2
4 subst-x*: -a-b-c-
5 subst-X*: .a..b..c.
6 split-x*: a,b,c
7 count-a*: 2
EXPECT

# ── 2. TERMINATION, \G, and the multiline anchors ───────────────────────────
# Row 8 is the bug's worst face: `while ($s =~ /(\w*)/g)` on the base never
# ends (this loop is bounded at 41, so the base ANSWERS 41 instead of hanging).
# Row 9: a zero-length match does not move the \G anchor, so ('aXbXc' =~
# /\G[a-z]*/g) is ('a','') — two matches, not one per character.
my $loops = <<'PROG';
my $s = 'ab cd';
my $i = 0; my @got;
while ($s =~ /(\w*)/g) { push @got, "[$1]"; last if ++$i > 40; }
print "8 while-w*: $i <", join("", @got), ">\n";
my $t = 'aXbXc';
my @g = ($t =~ /\G[a-z]*/g);
print "9 G-list: ", scalar(@g), " [", join("|", @g), "]\n";
my $u = 'ab cd'; my $j = 0;
while ($u =~ /\b/g) { $j++; last if $j > 40; }
print "10 while-b: $j\n";
$_ = "a\nb"; s/^/> /mg; my $x = $_; $x =~ s/\n/\\n/g; print "11 caret-mg: <$x>\n";
$_ = "a\nb"; s/$/!/mg; my $y = $_; $y =~ s/\n/\\n/g; print "12 dollar-mg: <$y>\n";
$_ = 'aabb'; my $c = tr/a/A/; print "13 tr: $_ $c\n";
my $w = 'abc'; my $k = ($w =~ s/x*/-/g); print "16 subst-count: $k\n";
PROG
is(run_cl($loops), <<'EXPECT', 'zero-length match: loops terminate with perl\'s counts');
8 while-w*: 4 <[ab][][cd][]>
9 G-list: 2 [a|]
10 while-b: 4
11 caret-mg: <> a\n> b>
12 dollar-mg: <a!\nb!>
13 tr: AAbb 2
16 subst-count: 4
EXPECT

# ── 3. split: perl passes minend 1 on EVERY attempt (pp_split) ──────────────
# cl-ppcre's split already DROPPED an empty match at the field's own start,
# which is that rule's second half; the retry was missing, so `split /|x/,
# 'xax'` was ("x","a","x") where perl says ("","a").  The other 21 rows are
# the ones that already agreed and must keep agreeing.
my $splits = <<'PROG';
sub show { my ($n, @f) = @_; print "$n: ", scalar(@f), " [", join("|", map { defined $_ ? $_ : "U" } @f), "]\n"; }
show("s1  /x*/ abc",      split(/x*/, 'abc'));
show("s2  /d*|x/ xax",    split(/\d*|x/, 'xax'));
show("s3  /,/ a,b,c",     split(/,/, 'a,b,c'));
show("s4  /x*/ axxb",     split(/x*/, 'axxb'));
show("s5  /(x*)/ axb",    split(/(x*)/, 'axb'));
show("s7  /\\s+/ ' a b '", split(/\s+/, ' a b '));
show("s8  / / ' a b '",   split(' ', ' a b '));
show("s9  /b*/ abc",      split(/b*/, 'abc'));
show("s10 /,/ a,b, lim2",  split(/,/, 'a,b,c', 2));
show("s11 /x*/ abc lim2",  split(/x*/, 'abc', 2));
show("s12 /(?=b)/ abcb",  split(/(?=b)/, 'abcb'));
show("s13 /|x/ xax",      split(/|x/, 'xax'));
show("s14 /\\b/ 'ab cd'",  split(/\b/, 'ab cd'));
show("s15 /a|/ 'aXa'",    split(/a|/, 'aXa'));
show("s16 /,|/ 'a,b'",    split(/,|/, 'a,b'));
show("s17 /x*/ '' ",      split(/x*/, ''));
show("s18 /x*/ 'x'",      split(/x*/, 'x'));
show("s19 /(a)|b/ 'zbz'", split(/(a)|b/, 'zbz'));
show("s20 /\\d*/ 'a1b'",   split(/\d*/, 'a1b'));
show("s21 /x*/ abc lim-1", split(/x*/, 'abc', -1));
show("s22 /o/ 'foo' ",    split(/o/, 'foo'));
show("s23 /(,)|(;)/",     split(/(,)|(;)/, 'a,b;c'));
PROG
is(run_cl($splits), <<'EXPECT', 'split: minend on every attempt matches perl 5.40.3');
s1  /x*/ abc: 3 [a|b|c]
s2  /d*|x/ xax: 2 [|a]
s3  /,/ a,b,c: 3 [a|b|c]
s4  /x*/ axxb: 2 [a|b]
s5  /(x*)/ axb: 3 [a|x|b]
s7  /\s+/ ' a b ': 3 [|a|b]
s8  / / ' a b ': 2 [a|b]
s9  /b*/ abc: 2 [a|c]
s10 /,/ a,b, lim2: 2 [a|b,c]
s11 /x*/ abc lim2: 2 [a|bc]
s12 /(?=b)/ abcb: 3 [a|bc|b]
s13 /|x/ xax: 2 [|a]
s14 /\b/ 'ab cd': 3 [ab| |cd]
s15 /a|/ 'aXa': 2 [|X]
s16 /,|/ 'a,b': 2 [a|b]
s17 /x*/ '' : 0 []
s18 /x*/ 'x': 0 []
s19 /(a)|b/ 'zbz': 3 [z|U|z]
s20 /\d*/ 'a1b': 2 [a|b]
s21 /x*/ abc lim-1: 4 [a|b|c|]
s22 /o/ 'foo' : 1 [f]
s23 /(,)|(;)/: 7 [a|,|U|b|U|;|c]
EXPECT

# ── 4. the ordinary global matches the rule must leave alone ────────────────
# pos() after each iteration, /gc, the replacement count, interleaved /g loops
# on two different scalars (the state is per scalar, as perl's pos magic is),
# list-context /g, /r, /i, a 500-match loop.
my $battery = <<'PROG';
my $s = "a1b22c333";
my @n; while ($s =~ /(\d+)/g) { push @n, "$1\@".pos($s); }
print "c1 digits: @n\n";
my $t = "aaa"; my $c = ($t =~ s/a/b/g); print "c2 count: $c $t\n";
my $u = "hello world"; my @w = ($u =~ /(\w+)\s+(\w+)/); print "c3 caps: @w\n";
my $v = "abcabc"; my $k = 0; while ($v =~ /b/gc) { $k++; } print "c4 gc: $k pos=", defined(pos($v))?pos($v):"U", "\n";
my $p = "xyz"; $p =~ /y/g; print "c5 pos: ", pos($p), "\n";
pos($p) = 0; $p =~ /z/g; print "c6 setpos: ", pos($p), "\n";
my @all = ("a,b,c" =~ /(\w)/g); print "c7 listg: @all\n";
my $str = "foofoo"; my $m = () = $str =~ /foo/g; print "c8 countg: $m\n";
my $e = "abc"; my $r = ($e =~ s/(\w)/[$1]/g); print "c9 e: $r $e\n";
my $q = "aXbXc"; my @g = ($q =~ /\G(\w)/g); print "c10 Gcap: ", scalar(@g), " [@g]\n";
$_ = "aaa"; my $n2 = s/a*/X/g; print "c11 star: $n2 $_\n";
$_ = "abc"; s/(?=b)/-/g; print "c12 look: $_\n";
$_ = "aaa"; my @z = /a??/g; print "c13 lazy: ", scalar(@z), "\n";
my $big = "ab" x 500; my $cc = () = $big =~ /b/g; print "c14 many: $cc\n";
my $nest = "a1b2"; my $o = ""; while ($nest =~ /(\w)/g) { my $x = $1; my $in = "xy"; while ($in =~ /(\w)/g) { $o .= "$x$1"; } } print "c15 nested: $o\n";
my $trail = "aaa"; $trail =~ s/$/!/g; print "c16 dollar: $trail\n";
my $mg = "a\nb"; my $cn = () = $mg =~ /^/mg; print "c17 caretmg: $cn\n";
my $rr = "aaa" =~ s/a/b/gr; print "c19 r: $rr\n";
my $ci = "AbAb"; my $n3 = ($ci =~ s/a/z/gi); print "c20 i: $n3 $ci\n";
PROG
is(run_cl($battery), <<'EXPECT', 'ordinary global matches unchanged (pos, /gc, nesting, /r, /i)');
c1 digits: 1@2 22@5 333@9
c2 count: 3 bbb
c3 caps: hello world
c4 gc: 2 pos=5
c5 pos: 2
c6 setpos: 3
c7 listg: a b c
c8 countg: 2
c9 e: 3 [a][b][c]
c10 Gcap: 5 [a X b X c]
c11 star: 2 XX
c12 look: a-bc
c13 lazy: 7
c14 many: 500
c15 nested: axay1x1ybxby2x2y
c16 dollar: aaa!
c17 caretmg: 2
c19 r: bbb
c20 i: 2 zbzb
EXPECT

# ── 5. #2001: a LIST-context m//g starts AT pos and RESETS it ──────────────
# perlop: in list context //g returns every match FROM pos (so a preceding
# scalar //g or a `pos() =` assignment positions the scan), and on completion
# pos goes back to undef unless /c keeps it at the end of the last match.
# PCL started the list scan at 0 and never touched pos, so after `$t =~ /;/g`
# the list match `my %h = $t =~ /(\w+)=(\w+)/g` collected the FIRST pair too.
# The anchored \G arm has always read pos and cleared it; this is the same rule
# for the ordinary spelling, and rows 5-9 are the negatives that must not move.
is(run_cl(<<'PROG'), <<'EXPECT', '#2001: list-context m//g honours and resets pos');
my $s = "aXbXcXd"; pos($s) = 3; my @m = $s =~ /X/g;
print "1 ", scalar(@m), " pos=", (defined pos($s) ? pos($s) : "undef"), "\n";
my $t = "k1=v1;k2=v2;k3=v3"; $t =~ /;/g; my %h = $t =~ /(\w+)=(\w+)/g;
print "2 ", join(",", map { "$_=$h{$_}" } sort keys %h), "\n";
my $u = "abcabc"; $u =~ /b/g; my $c = () = $u =~ /b/g; print "3 $c\n";
my $x = "x1x2"; my @g = $x =~ /(\d)/gc;
print "4 ", scalar(@g), " pos=", (defined pos($x) ? pos($x) : "undef"), "\n";
my $y = "abc"; pos($y) = 1; my @n = $y =~ /(z)/g;
print "5 ", scalar(@n), " pos=", (defined pos($y) ? pos($y) : "undef"), "\n";
my $z = "abc"; pos($z) = 1; my @n2 = $z =~ /(z)/gc;
print "6 ", scalar(@n2), " pos=", (defined pos($z) ? pos($z) : "undef"), "\n";
my $w = "aXbXc"; my $k = 0; $k++ while $w =~ /X/g;
print "7 $k pos=", (defined pos($w) ? pos($w) : "undef"), "\n";
my $a = "aaab"; pos($a) = 1; my @ga = $a =~ /\Ga/g; print "8 ", scalar(@ga), "\n";
my $q = "k1=v1;k2=v2"; my @one = $q =~ /(\w+)=(\w+)/g;
my @two = $q =~ /(\w+)=(\w+)/g; print "9 ", scalar(@one), " ", scalar(@two), "\n";
PROG
1 2 pos=undef
2 k2=v2,k3=v3
3 1
4 2 pos=4
5 0 pos=undef
6 0 pos=1
7 2 pos=undef
8 2
9 4 4
EXPECT
