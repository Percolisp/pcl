#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# perf-levers-12.t — perf round 43 (s513e, docs/faster-codegen-suggestions.md
# "Round 43 movers").  MECHANISM rows (the runtime's own answer), TIMED rows
# (seconds on the round's base, main 462169f6; well under the bound here) and
# ANSWER rows: programs whose expected output below IS perl's own (probed on
# 5.40.3; nothing in them needs perl >= 5.38).
#
#   #2980 split picks its engine by the pattern's SHAPE: awk mode, a FIXED
#         STRING (any pattern that is only literal characters -- `','`,
#         `/,/`, `'\\.'`, `/\t/` -- scanned by a typed loop, perl's fbm
#         path), or the regex engine.
#   #2981 s///g drives its own match loop (perl's global rule, no generic
#         SCAN dispatch per match) and assembles the result in one string.
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

sub transpile { my ($src) = @_; return PCLCore::transpile(qq{$pl2cl } . src_file($src)) }

sub run_pl {
    my ($src) = @_;
    my $cl = transpile($src);
    my ($cfh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cfh $cl;
    close $cfh;
    my $out = `sbcl @sbcl_rt --load $cl_file 2>&1`;
    $out =~ s/^;.*\n//gm;
    $out =~ s/^(?:caught |compilation unit|-->|==>|PCL Runtime loaded).*\n//gm;
    $out =~ s/^\s*\n//gm;
    return $out;
}

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

use Time::HiRes qw(time);

# ─────────────────────────────────────────────────────────────────────────────
# #2980 MECHANISM: the shape each pattern spelling gets.
like(lisp_out(<<'END_LISP'),
(let ((*print-pretty* nil)) (dolist (p (list (p-regex :pat "\\." :flags "") "\\." "," "::" " " "" (p-regex :pat "\\t" :flags "") (p-regex :pat "x" :flags "i") (p-regex :pat " , " :flags "x") "a+" (p-regex :pat "\\d" :flags "") (p-regex :pat "(,)" :flags ""))) (multiple-value-bind (s d) (%p-split-shape p) (format t "~(~A~) ~S|" s (if (stringp d) d "-")))))
END_LISP
     qr/\Qliteral "."|literal "."|literal ","|literal "::"|awk "-"|literal ""|literal "	"|regex "-"|regex "-"|regex "-"|regex "-"|regex "-"|\E/,
     '#2980: a pattern of literal characters (regex or string, escaped punctuation, \\t) is :literal; " " is :awk; /i /x, a metacharacter, a class, a group go to the engine');

{
    # 300 000 iterations of a regex-STRING split and a literal-regex split:
    # 2.45 s on the base (cl-ppcre's split loop + generic scan per field),
    # 0.67 s here.  The bound is RELATIVE to perl's own time on the same
    # program, measured in this run, because an absolute 1.5 s tripped on the
    # CI runner (1.68 s there against 0.53 s here, s513 2026-10-09): 8x perl
    # + 0.5 s for the transpile and the SBCL start-up.  Here that is ~3.1 s
    # against 0.51 s on the tree (perl 0.32 s); on the runner ~3-6 s against
    # 1.68 s.  This row is the GROSS-regression bound and passes on the base
    # too; the INVERSE guard of the lever is row 1, the mechanism (a literal
    # pattern never reaches the engine), which fails on the base.
    my $src = q{my $l = join(".", 1 .. 20); my $m = join(",", 1 .. 20); my $s = 0; for (1 .. 300000) { my @f = split(q(\.), $l); my @g = split(/,/, $m); $s += @f + @g } print "$s\n";};
    my ($pfh, $pfile) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $pfh $src;
    close $pfh;
    my $p0 = time;
    my $perl_out = `$^X $pfile 2>&1`;
    my $pdt = time - $p0;
    die "perl's own answer is wrong: $perl_out" if $perl_out ne "12000000\n";
    my $t0 = time;
    my $out = run_pl($src);
    my $dt = time - $t0;
    my $bound = 8 * $pdt + 0.5;
    is($out, "12000000\n", '#2980 timed: the answer');
    cmp_ok($dt, '<', $bound, sprintf('#2980 timed: 600 000 literal splits within 8x perl + 0.5 s = %.2f s (took %.2f s, perl %.2f s)', $bound, $dt, $pdt));
}

answers(<<'END_SRC', <<'END_EXP', '#2980 answers: limits, leading/trailing empty fields, captures, awk vs / / vs \\s+, metacharacter strings, escaped literals, $_ default, scalar context, a wide subject, $;, /i and /x stay regexes, qr, overlapping literal');
use strict; use warnings;
sub show { my ($n, @f) = @_; print "$n: ", scalar(@f), " [", join("|", map { defined $_ ? $_ : 'U' } @f), "]\n" }
my $l = "a,b,,c,,";
show('01 comma str', split(',', $l));
show('02 comma regex', split(/,/, $l));
show('03 limit 3', split(',', $l, 3));
show('04 limit -1', split(',', $l, -1));
show('05 limit 1', split(',', $l, 1));
show('06 leading empty', split(',', ",x,y"));
show('07 empty pattern', split(//, "abc"));
show('08 empty str pattern', split('', "abc", 2));
show('09 capture', split(/(,)/, "a,b,c"));
show('10 awk', split(' ', "  a b\t c  "));
show('11 single space regex', split(/ /, "  a b  c"));
show('12 \\s+', split(/\s+/, "  a b  c"));
show('13 dot str is regex', split('.', "a.b.c"));
show('14 escaped dot str', split('\\.', "a.b.c"));
show('15 pipe str', split('|', "a|b"));
show('16 escaped pipe', split('\\|', "a|b||"));
show('17 multi literal', split('::', "A::B::::C"));
show('18 multi literal regex', split(/::/, "A::B::C::", -1));
$_ = "p:q:r"; show('19 $_ default', split(/:/));
my $sc = split(/,/, "a,b,c,,"); print "21 scalar: $sc\n";
show('22 utf8 subject', split(/\x{e9}/, "caf\x{e9}x\x{e9}y"));
show('23 $; sep', split($;, join($;, 'k1', 'k2', 'k3')));
show('24 tab regex', split(/\t/, "a\tb\t\tc"));
show('25 /i literal', split(/x/i, "aXbxc"));
show('26 escaped backslash', split(/\\/, 'a\\b\\c'));
show('27 limit 2 regex lit', split(/\./, "1.2.3.4", 2));
show('28 /x space', split(/ , /x, "a , b,c"));
my $re = qr/;/; show('29 qr literal', split($re, "a;b;c"));
show('30 newline', split(/\n/, "l1\nl2\n\nl3\n\n"));
show('31 number pattern', split(1, "a1b1c"));
show('32 trailing only', split(/,/, ",,,"));
show('33 hyphen escape', split(/\-/, "a-b-c"));
show('34 awk limit', split(' ', " a b c d ", 2));
my $p = '\\.'; show('35 var pattern', split($p, "x.y.z"));
show('37 escape e/t', split(/\e/, "a\eb"));
show('38 brace', split('}', "a}b}c"));
show('39 caret alone str', split('^', "a\nb\nc"));
show('40 overlapping', split('aa', "aaaaab"));
END_SRC
01 comma str: 4 [a|b||c]
02 comma regex: 4 [a|b||c]
03 limit 3: 3 [a|b|,c,,]
04 limit -1: 6 [a|b||c||]
05 limit 1: 1 [a,b,,c,,]
06 leading empty: 3 [|x|y]
07 empty pattern: 3 [a|b|c]
08 empty str pattern: 2 [a|bc]
09 capture: 5 [a|,|b|,|c]
10 awk: 3 [a|b|c]
11 single space regex: 6 [||a|b||c]
12 \s+: 4 [|a|b|c]
13 dot str is regex: 0 []
14 escaped dot str: 3 [a|b|c]
15 pipe str: 3 [a|||b]
16 escaped pipe: 2 [a|b]
17 multi literal: 4 [A|B||C]
18 multi literal regex: 4 [A|B|C|]
19 $_ default: 3 [p|q|r]
21 scalar: 3
22 utf8 subject: 3 [caf|x|y]
23 $; sep: 3 [k1|k2|k3]
24 tab regex: 4 [a|b||c]
25 /i literal: 3 [a|b|c]
26 escaped backslash: 3 [a|b|c]
27 limit 2 regex lit: 2 [1|2.3.4]
28 /x space: 3 [a | b|c]
29 qr literal: 3 [a|b|c]
30 newline: 4 [l1|l2||l3]
31 number pattern: 3 [a|b|c]
32 trailing only: 0 []
33 hyphen escape: 3 [a|b|c]
34 awk limit: 2 [a|b c d ]
35 var pattern: 3 [x|y|z]
37 escape e/t: 2 [a|b]
38 brace: 3 [a|b|c]
39 caret alone str: 3 [a
|b
|c]
40 overlapping: 3 [||ab]
END_EXP

# ─────────────────────────────────────────────────────────────────────────────
# #2981 MECHANISM: s///g's own loop -- perl's global rule (an empty match is
# retried under minend, so `x*` on "aaa" is -a-a-a-), the count, and NIL on
# no match.  The lever is ~1.5x on a whole s///ge program (1.03 s here, 1.55 s
# on the base), too small to separate from CI noise, so it has no timed row:
# the mechanism rows and the answer tables below are its guard.
like(lisp_out(<<'END_LISP'),
(let ((*print-pretty* nil)) (dolist (c (list (list "\\s+" "a b  c" "_") (list "x*" "aaa" "-") (list "q" "abc" "z") (list "(?=b)" "abab" "!"))) (format t "~S|" (multiple-value-list (%p-subst-replace-all (%pcl-create-scanner (first c) nil) (cons (first c) nil) (second c) (third c))))))
END_LISP
     qr/\Q("a_b_c" 2)|("-a-a-a-" 4)|(nil 0)|("a!ba!b" 2)|\E/i,
     '#2981: %p-subst-replace-all answers s///g with perl\'s empty-match rule and the count');

answers(<<'END_SRC', <<'END_EXP', '#2981 answers: an overloaded subject, a growing (.=) subject, a wide subject, a template, /e numbers, foreach alias, hash element, no-match count, $_, long subject');
use strict; use warnings;
package O; use overload '""' => sub { "a-b-c" }; sub new { bless {}, shift }
package main;
my $o = O->new; my $n = ($o =~ s/-/+/g); print "01 $n <$o>\n";
my $acc = ""; $acc .= "x y " for 1..3; $n = ($acc =~ s/ /_/g); print "02 $n <$acc>\n";
my $w = "\x{263a} \x{263a}"; $n = ($w =~ s/\x{263a}/S/g); print "03 $n <$w>\n";
my $d = "b"; $d =~ s/(b)/$1$1/g; print "04 <$d>\n";
my $e = "aaa"; $n = ($e =~ s/a/1+1/ge); print "05 $n <$e>\n";
my @a = ("p q", "r s"); s/ /-/g for @a; print "06 @a\n";
my %h = (k => "1 2 3"); $h{k} =~ s/ //g; print "07 $h{k}\n";
my $z = "abc"; my $c = ($z =~ s/x//g); print "08 <$c>\n";
$_ = "t t"; s/t/T/g; print "09 $_\n";
my $m = "aXbXc"; ($m =~ s/X/\n/g); print "10 ", length($m), "\n";
my $big = "ab" x 1000; $n = ($big =~ s/b/c/g); print "11 $n ", substr($big, 0, 6), "\n";
my $q = "a.b"; $q =~ s/\./\$/g; print "12 <$q>\n";
END_SRC
01 2 <a+b+c>
02 6 <x_y_x_y_x_y_>
03 2 <S S>
04 <bb>
05 3 <222>
06 p-q r-s
07 123
08 <>
09 T T
10 5
11 1000 acacac
12 <a$b>
END_EXP

answers(<<'END_SRC', <<'END_EXP', '#2981 answers: s///ge with $1, empty matches (x*, //, lookahead), /r, templates (\\1 $& \\u), nested s///e, $` and @- in /e, /i, named captures, $1 after the loop');
use strict; use warnings;
my $u;
$u = "field-1 value"; my $n = ($u =~ s/([aeiou])/uc($1)/ge); print "01 $n <$u>\n";
$u = "a  b\tc "; $n = ($u =~ s/\s+/_/g); print "02 $n <$u>\n";
$u = "xyz"; $n = ($u =~ s/q/r/g); print "03 <$n> <$u>\n";
$u = "aaa"; $n = ($u =~ s/x*/-/g); print "04 $n <$u>\n";
$u = "abc"; $n = ($u =~ s//-/g); print "05 $n <$u>\n";
$u = "hello"; my $r = ($u =~ s/l/L/gr); print "06 <$r> <$u>\n";
$u = "a1b22c"; $u =~ s/(\d+)/<$1>/g; print "07 <$u>\n";
$u = "a1b2"; $u =~ s/(\d)/$1*2/ge; print "08 <$u>\n";
$u = "ab"; $u =~ s/(.)/my $c = $1; $c =~ s{(.)}{uc $1}e; "[$c]"/ge; print "09 <$u>\n";
$u = "abc"; $u =~ s/b/\\/g; print "10 <$u>\n";
$u = "a.b.c"; $u =~ s/\./\$&/g; print "11 <$u>\n";
$u = "a-b"; $u =~ s/-/$&$&/g; print "12 <$u>\n";
$u = "xaxbx"; $u =~ s/x/"pre($`)"/ge; print "13 <$u>\n";
$u = "a1b2c3"; $u =~ s/\d/$-[0]/ge; print "15 <$u>\n";
$u = "\x{e9}t\x{e9}"; $u =~ s/\x{e9}/E/g; print "16 <", length($u), ">\n";
$u = "aaa"; $u =~ s/a/b/; print "17 <$u>\n";
$u = "ab"; $u =~ s/(?=b)/!/g; print "19 <$u>\n";
$u = "abab"; $u =~ s/(a)|b/defined $1 ? "A" : "B"/ge; print "20 <$u>\n";
$u = "foo bar"; $u =~ s/(\w+)/\u$1/g; print "21 <$u>\n";
my @w = map { "field-$_ value" } 1..3; for my $s (@w) { my $t = $s; $t =~ s/([aeiou])/uc($1)/ge; $t =~ s/\s+/_/g; print "22 $t\n" }
$u = "aXbXc"; $u =~ s/x/-/gi; print "23 <$u>\n";
$u = "a b"; $u =~ s/ /\t/g; print "24 <$u>\n";
$u = "12"; $u =~ s/(\d)/$1+1/eg; print "25 <$u> $1\n";
$u = "ab"; $u =~ s/(?<n>a)/<$+{n}>/g; print "26 <$u>\n";
END_SRC
01 5 <fIEld-1 vAlUE>
02 3 <a_b_c_>
03 <> <xyz>
04 4 <-a-a-a->
05 4 <-a-b-c->
06 <heLLo> <hello>
07 <a<1>b<22>c>
08 <a2b4>
09 <[A][B]>
10 <a\c>
11 <a$&b$&c>
12 <a--b>
13 <pre()apre(xa)bpre(xaxb)>
15 <a1b3c5>
16 <3>
17 <baa>
19 <a!b>
20 <ABAB>
21 <Foo Bar>
22 fIEld-1_vAlUE
22 fIEld-2_vAlUE
22 fIEld-3_vAlUE
23 <a-b-c>
24 <a	b>
25 <23> 2
26 <<a>b>
END_EXP

# #2981 ANSWERS: every attempt of the loop starts at the previous match's end,
# but lookbehind, \b / \B and ^ read the WHOLE subject (cl-ppcre's
# *real-start-pos*; without it re/subst.t lost 6 rows and
# re/regex_sets_compat.t 63 in the round's companion run).
answers(<<'END_SRC', <<'END_EXP', '#2981 s///g: lookbehind, \\b, \\B and ^ see the text before the attempt');
my $n;
$_="ccccc"; $n = s/(?<!x)c/x/g; print "1 $_ $n\n";
$_="foobbarfoobbar"; $n = s/(?<!r)foobbar/foobar/g; print "2 $_ $n\n";
$_="foobbarfoobbar"; $n = s/(?<!ar)(foobbar)/foobar/g; print "3 $_ $n\n";
$_='aaaa'; $n = s/\ba/./g; print "4 $_ $n\n";
$_="Charles Bronson"; $n = s/\B\w//g; print "5 $_ $n\n";
$_='aaa'; $n = s/^a/x/g; print "6 $_ $n\n";
$_='ab ab'; $n = s/(?<=a)b/B/g; print "7 $_ $n\n";
$_='aaa'; $n = s/(?<=a)a/uc($&)/ge; print "8 $_ $n\n";
$_='x y z'; $n = s/\b(\w)/<$1>/g; print "9 $_ $n\n";
$_="a\nb\nc"; $n = s/^/> /mg; print "10 $_ $n\n";
$_='abc'; $n = s/\Ab/X/g; print "11 $_ $n\n";
$_='aXbXc'; $n = s/(?<!^)X/-/g; print "12 $_ $n\n";
$_='hello world'; $n = s/(?<=o)\b/!/g; print "13 $_ $n\n";
$_='ab'; my $r = s/(?<=a)b/[$&]/gr; print "14 $r\n";
END_SRC
1 xxxxx 5
2 foobarfoobbar 1
3 foobarfoobbar 1
4 .aaa 1
5 C B 12
6 xaa 1
7 aB aB 2
8 aAA 2
9 <x> <y> <z> 3
10 > a
> b
> c 3
11 abc 
12 a-b-c 2
13 hello! world 1
14 a[b]
END_EXP

done_testing();
