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
    # 0.67 s here.  The bound is generous.
    my $t0 = time;
    my $out = run_pl(q{my $l = join(".", 1 .. 20); my $m = join(",", 1 .. 20); my $s = 0; for (1 .. 300000) { my @f = split(q(\.), $l); my @g = split(/,/, $m); $s += @f + @g } print "$s\n";});
    my $dt = time - $t0;
    is($out, "12000000\n", '#2980 timed: the answer');
    cmp_ok($dt, '<', 1.5, sprintf('#2980 timed: 600 000 literal splits in under 1.5 s (took %.2f s)', $dt));
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

done_testing();
