#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# perf-levers-10.t — perf round 41 (s512p, docs/faster-codegen-suggestions.md
# "Round 41 movers").  A MECHANISM row (it fails on the round's base,
# main 5adcc156), a TIMED row (minutes on the base, about a second here) and
# an ANSWER row: a program whose expected output below IS perl's own
# (probed on 5.40.3; nothing in it needs perl >= 5.38).
#
#   #2723 an END-ANCHORED regex of bounded match length starts its scan at
#         the tail (%pcl-tail-reach / %pcl-tail-start-scanner, wrapped in
#         %pcl-build-scanner); unbounded or not end-anchored = unchanged.
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


use Time::HiRes qw(time);

# ─────────────────────────────────────────────────────────────────────────────
like(lisp_out(q{(progn (print (list (%pcl-tail-reach "9\\\\n\\\\z" nil) (%pcl-tail-reach "\\\\s+\\\\z" nil) (%pcl-tail-reach "b\$" (quote (:multi-line-mode t))) (%pcl-tail-reach "x{2,5}\$" nil) (%pcl-tail-reach "(a|bcd)\\\\Z" nil) (%pcl-tail-reach "ab" nil) (%pcl-tail-reach "(?m)b\$" nil))) (print (handler-case (%pcl-regex-max-length (quote (:bogus-node "a"))) (error (e) (princ-to-string e)))))}),
     qr/\Q(2 nil nil 6 4 nil nil) \E\n\Q"PCL: %pcl-regex-max-length has no case for the cl-ppcre parse-tree node :bogus-node"\E/,
     '#2723: %pcl-tail-reach = max length + the anchor newline allowance; unbounded, /m, (?m) and unanchored decline; an unknown node DIES');

{
    # A FIXED 200 KB string matched 30 000 times against an end-anchored
    # pattern: the base scans the whole string per match (~20 s on the dev
    # box), the tree starts at the tail.  The bound is generous (CI is slower).
    my $t0 = time;
    my $out = run_pl(q{my $s = ("line 12345\n" x 18000) . "tail9\n"; my $m = 0; for my $i (1 .. 30000) { $m++ if $s =~ /9\n\z/; $m++ if $s =~ /x\z/ } print "$m\n";});
    my $dt = time - $t0;
    is($out, "30000\n", '#2723 timed: the answer');
    cmp_ok($dt, '<', 8, sprintf('#2723 timed: 60 000 end-anchored matches on a 200 KB string in under 8 s (took %.2f s)', $dt));
}

answers(<<'END_SRC', <<'END_EXP', '#2723 answers: fixed / bounded / unbounded / multi-line / lookbehind / pos / s/// / split / captures / tie / overload / minend');
my $n = 0;
sub row { my ($name, @v) = @_; $n++; print "$n $name: ", join("|", map { defined $_ ? $_ : "U" } @v), "\n" }
my $s = "abc\nxy9\n";
row("fixed \\z hit", $s =~ /9\n\z/ ? 1 : 0, "@-", "@+");
row("fixed \\z miss", "ab9\nc" =~ /9\n\z/ ? 1 : 0);
row("dot \$ no nl", "a.b." =~ /\.$/ ? "$-[0]" : "no");
row("dot \$ with nl", "a.b.\n" =~ /\.$/ ? "$-[0]" : "no");
row("\\Z alone", "ab\n" =~ /\Z/ ? "$-[0]/$+[0]" : "no", "ab" =~ /\Z/ ? "$-[0]" : "no");
row("\\Z after nl nl", "ab\n\n" =~ /b\Z/ ? 1 : 0, "ab\n\n" =~ /\n\Z/ ? "$-[0]" : "no");
row("bounded \\r?\\n", "x\r\n" =~ /\r?\n\z/ ? "$-[0]" : "no", "x\n" =~ /\r?\n\z/ ? "$-[0]" : "no");
row("alt (a|bcd)", "zzbcd" =~ /(a|bcd)\z/ ? "$1 $-[0] $-[1]" : "no", "zza" =~ /(a|bcd)\z/ ? "$1 $-[0]" : "no");
row("x{2,5}\$", "axxxxxxx" =~ /x{2,5}$/ ? "$-[0] $&" : "no", "ax\n" =~ /x{2,5}$/ ? 1 : 0);
row("unbounded \\s+\\z", "a  \t " =~ /\s+\z/ ? "$-[0]" : "no");
row("unbounded ;\\s*\$", "a; \n" =~ /;\s*$/ ? "$-[0]" : "no");
row("/m with \$", "ab\ncd" =~ /b$/m ? "$-[0]" : "no", "ab\ncd" =~ /b$/ ? 1 : 0);
row("inline (?m)", "ab\ncd" =~ /(?m)b$/ ? "$-[0]" : "no");
row("\\b at tail", "foo bar" =~ /\bbar\z/ ? "$-[0]" : "no", "foobar" =~ /\bbar\z/ ? 1 : 0);
row("lookbehind at tail", "xx9" =~ /(?<=x)9\z/ ? "$-[0]" : "no", "y9" =~ /(?<=x)9\z/ ? 1 : 0);
row("^...\\z", "a" =~ /^a\z/ ? 1 : 0, "" =~ /^\z/ ? 1 : 0, "ba" =~ /^a\z/ ? 1 : 0, "" =~ /^a?\z/ ? 1 : 0);
my $g = "axbxcx"; my @p; while ($g =~ /x\z/g) { push @p, pos($g) } row("/g loop", "@p");
$g = "axbx"; pos($g) = 3; row("/g pos past tail start", scalar($g =~ /x\z/g) ? pos($g) : "no");
$g = "axbx"; pos($g) = 2; row("/g pos before tail", scalar($g =~ /\Gbx\z/g) ? pos($g) : "no");
(my $t = "abx") =~ s/x\z/y/; (my $t2 = "ab\n") =~ s/\n\z//; row("s///", $t, $t2);
row("split", join(",", split /\n\z/, "a\nb\n"));
row("captures \@- \@+", "hello world" =~ /(w)(or)ld\z/ ? "@- / @+" : "no");
row("shorter than L", "ab" =~ /abc\z/ ? 1 : 0, "b" =~ /(a|bcd|b)\z/ ? "$-[0]" : "no");
{ package Ov; use overload '""' => sub { "ovl9\n" }; }
my $o = bless {}, 'Ov'; row("overloaded", $o =~ /9\n\z/ ? "$-[0]" : "no");
{ package TS; sub TIESCALAR { bless [] } sub FETCH { "tied9" } }
tie my $ts, 'TS'; row("tied", $ts =~ /9\z/ ? "$-[0]" : "no");
my @z; my $zz = "axx"; while ($zz =~ /x?\z/g) { push @z, "$-[0]-$+[0]" } row("minend x?\\z", "@z");
my @w; my $ww = "ax"; while ($ww =~ /x*\z/g) { push @w, "$-[0]-$+[0]" } row("minend x*\\z", "@w");
row("/i", "ABC" =~ /c\z/i ? "$-[0]" : "no");
row("/x", "ab9\n" =~ / 9 \n \z /x ? "$-[0]" : "no");
my $q = qr/b\z/; row("qr interp", "aab" =~ /a$q/ ? "$-[0]" : "no", "aab" =~ $q ? "$-[0]" : "no");
row("backref", "abab" =~ /(ab)\1\z/ ? "$-[0]" : "no");
row("unicode", "\x{100}\x{101}" =~ /\x{101}\z/ ? "$-[0]" : "no");
row("cond (?(1))", "xab" =~ /(a)?(?(1)b|c)\z/ ? "$-[0]" : "no");
row("lookahead tail", "ab\n" =~ /b(?=\n)\Z/ ? "$-[0]" : "no");
row("alt anchored mix", "ab" =~ /a\z|b\z/ ? "$-[0]" : "no", "ab" =~ /a|b\z/ ? "$-[0]" : "no");
row("empty \$", "abc" =~ /$/ ? "$-[0]" : "no", "abc\n" =~ /$/ ? "$-[0]" : "no", "abc\n" =~ /\z/ ? "$-[0]" : "no");
(my $sg = "a\nb\n") =~ s/\n\z/!/g; (my $sg2 = "xx") =~ s/x\z/y/g; row("s///g tail", $sg, $sg2);
row("named \\z", "zzab" =~ /(?<n>ab)\z/ ? "$+{n} $-[0]" : "no");
my $pv = "9\\n"; row("runtime pattern", "ab9\n" =~ /$pv\z/ ? "$-[0]" : "no");
row("/s dot", "a\n" =~ /.\z/s ? "$-[0]" : "no", "a\n" =~ /.\z/ ? "$-[0]" : "no", "a\n" =~ /.$/ ? "$-[0]" : "no");
row("(?i) inline", "xAB" =~ /(?i)ab\z/ ? "$-[0]" : "no");
row("opt group \$", "ab" =~ /a(?:b$)?/ ? "$&" : "no", "abc" =~ /(?:c\z)+/ ? "$-[0]" : "no");
my @all = ("a1\nb2\nc3" =~ /(\d)$/mg); row("/mg list", "@all");
row("list ctx caps", join("|", "key=val\n" =~ /(\w+)=(\w+)$/));
row("\\Z bounded alt", "ab\n" =~ /(?:b|ab)\Z/ ? "$-[0]" : "no", "ab" =~ /(?:b|ab)\Z/ ? "$-[0]" : "no");
END_SRC
1 fixed \z hit: 1|6|8
2 fixed \z miss: 0
3 dot $ no nl: 3
4 dot $ with nl: 3
5 \Z alone: 2/2|2
6 \Z after nl nl: 0|2
7 bounded \r?\n: 1|1
8 alt (a|bcd): bcd 2 2|a 2
9 x{2,5}$: 3 xxxxx|0
10 unbounded \s+\z: 1
11 unbounded ;\s*$: 1
12 /m with $: 1|0
13 inline (?m): 1
14 \b at tail: 4|0
15 lookbehind at tail: 2|0
16 ^...\z: 1|1|0|1
17 /g loop: 6
18 /g pos past tail start: 4
19 /g pos before tail: 4
20 s///: aby|ab
21 split: a
b
22 captures @- @+: 6 6 7 / 11 7 9
23 shorter than L: 0|0
24 overloaded: 3
25 tied: 4
26 minend x?\z: 2-3 3-3
27 minend x*\z: 1-2 2-2
28 /i: 2
29 /x: 2
30 qr interp: 1|2
31 backref: 0
32 unicode: 1
33 cond (?(1)): 1
34 lookahead tail: 1
35 alt anchored mix: 1|0
36 empty $: 3|3|4
37 s///g tail: a
b!|xy
38 named \z: ab 2
39 runtime pattern: 2
40 /s dot: 1|no|0
41 (?i) inline: 1
42 opt group $: ab|2
43 /mg list: 1 2 3
44 list ctx caps: key|val
45 \Z bounded alt: 0|0
END_EXP

done_testing();
