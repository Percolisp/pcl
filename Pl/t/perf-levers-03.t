#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# perf-levers-03.t — the round-29 REGEX-OP levers
# (docs/plan-speed-and-ir-s470.md §A.4), guarded the way perf-levers-02.t
# guards round 28's.
#
#   #1250 a regex/subst/tr literal whose every ingredient is a compile-time
#     STRING builds its op ONCE per call site (`%p-op-once`), instead of
#     consing a `(list flags pattern)` key and doing an EQUAL gethash on it —
#     or, for s/// and tr///, allocating a fresh op struct — on EVERY
#     evaluation.
#   #1251 an s/// op carries its COMPILED form (`%p-subst-compiled`), the
#     sibling of m//'s `%p-regex-compiled` (task #680): the perl->ppcre
#     pattern translation, the replacement translation, the modifier booleans
#     and the scanner are computed once per op instead of per evaluation.
#
# WHY THE MECHANISM ROWS ARE MACROEXPANSIONS.  Both levers are runtime MACROS
# and runtime functions with no emission to switch — `pl2cl`'s output is
# byte-identical with and without them (corpus-diff IDENTICAL), so a transpile
# grep can say nothing.  What can be asserted is the macro's own answer, and
# every fast row has a NEGATIVE beside it (a pattern that is a run-time value,
# a replacement that is a closure, `qr//` whose object identity is
# per-evaluation).  Without the negatives a lever that fired everywhere would
# pass every row here and be wrong everywhere else.
#
# The RUN rows are perl 5.40.3's own answers, probed
# (scratch/s470bu/probe1.pl + probe2.pl in the s470bu worktree).

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
plan skip_all => "pl2cl not found" unless -x $pl2cl;
plan skip_all => "sbcl not found"  unless `which sbcl 2>/dev/null`;
plan tests => 11;

# ── the two instruments (same as perf-levers-02.t) ──────────────────────────

sub expand {
    my (@forms) = @_;
    my $prog = "(in-package :pcl)\n";
    $prog .= "(format t \"~A~%\" (substitute #\\Space #\\Newline "
           . "(prin1-to-string (macroexpand-1 '$_))))\n" for @forms;
    my ($fh, $file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $fh $prog;
    close $fh;
    my $out = `sbcl @sbcl_rt --load $file 2>&1`;
    $out =~ s/^;.*\n//gm;
    $out =~ s/^(?:PCL Runtime loaded).*\n//gm;
    return $out;
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
    $out =~ s/^\s*Undefined.*\n//gm;
    $out =~ s/^\s*\n//gm;
    return $out;
}

# ─────────────────────────────────────────────────────────────────────────────
# #1250 — a literal op is built ONCE per SITE
# ─────────────────────────────────────────────────────────────────────────────
{
    my $mx = expand(
        '(p-regex :pat "a" :flags "g" :tier :native)',
        '(p-regex :pat (p-esc "\\\\n") :flags "" :tier :native)',
        '(p-regex :pat (p-string-concat "a" $x) :flags "" :tier :native)',
        '(p-regex-from-parts :pat $x :flags "g" :tier :dynamic)',
        '(p-subst :pat "a" :rep "b" :flags "g" :tier :native)',
        '(p-subst :pat "a" :rep (lambda nil $x) :flags "eg" :tier :native)',
        '(p-subst :pat (p-string-concat "a" $x) :rep "b" :flags "" :tier :native)',
        '(p-tr :from "a" :to "b" :flags "")',
        '(p-qr :pat "a" :flags "" :tier :native)',
    );
    my @l = split /\n/, $mx;
    like($l[0], qr/%p-op-once.*%p-regex-op "a" "g"/i,
         '#1250: a literal m// pattern builds its op once per site');
    like($l[1], qr/%p-op-once/i,
         '#1250: ... and so does a p-esc payload, which IS a compile-time string');
    unlike($l[2], qr/%p-op-once/i,
           '#1250 NEGATIVE: an interpolated m// pattern keeps the per-evaluation memo');
    unlike($l[3], qr/%p-op-once/i,
           '#1250 NEGATIVE: p-regex-from-parts is a run-time pattern, never a site');
    like($l[4], qr/%p-op-once.*%p-subst-parts/i,
         '#1250: a literal s/// pattern AND replacement build the op once per site');
    unlike($l[5], qr/%p-op-once/i,
           '#1250 NEGATIVE: a CLOSURE replacement (s///e, interpolation) stays fresh');
    unlike($l[6], qr/%p-op-once/i,
           '#1250 NEGATIVE: an interpolated s/// pattern stays fresh');
    like($l[7], qr/%p-op-once.*%p-tr-parts/i,
         '#1250: a literal tr/// builds its op once per site');
    unlike($l[8], qr/%p-op-once/i,
           '#1250 NEGATIVE: qr// is a fresh object per evaluation (perl identity)');
}
# ─────────────────────────────────────────────────────────────────────────────
# THE RUN ROWS — perl 5.40.3's own answers
# ─────────────────────────────────────────────────────────────────────────────
{
    # Each row is a value a program consumes: /g pos state carried across
    # iterations of ONE site, the same site reached with different subjects,
    # capture + $& + $` + $' from a cached op, a literal s///g run repeatedly,
    # s///e with a closure that reads a changing outer lexical, s///r, tr///
    # count and /r, a run-time pattern at a site that must NOT cache, qr//
    # identity, named captures twice through one site, and \Q..\E.
    my $want = <<'OUT';
1: 2 pos=undef
2: aa -> 0
2: bbbb -> 4
2:  -> 0
3: k/1 $&=k=1 $`=[] $'=[]
3: z/99 $&=z=99 $`=[] $'=[]
4: 1-1-1 3
4: 2-2-2 3
4: 3-3-3 3
5: 2 4 6
5: 3 6 9
6: heLLo orig=hello
7: bAnAnA 3
7: bAnAnA 3
7r: bANANA
8: a 1
8: b 1
9: diff
10: 2020 01
10: 1999 12
11: 01
12: ..bb
12: aa..
13: 2 3
13: 0 1
14: dd 6
15: y
15: y
OUT
    my $got = run_pl(<<'PL');
use strict; use warnings;
my $x = "aXbXc"; my $n = 0;
while ($x =~ /X/g) { $n++ }
print "1: $n pos=", (defined pos($x) ? pos($x) : "undef"), "\n";
for my $s ("aa", "bbbb", "") {
  my $c = 0;
  $c++ while $s =~ /b/g;
  print "2: $s -> $c\n";
}
for my $t ("k=1", "z=99") { $t =~ /(\w+)=(\d+)/; print "3: $1/$2 \$&=$& \$`=[$`] \$'=[$']\n" }
for my $i (1..3) { my $u = "o-o-o"; my $k = ($u =~ s/o/$i/g); print "4: $u $k\n" }
my $mul = 2;
for my $i (1..2) { my $u = "1 2 3"; $u =~ s/(\d+)/$1*$mul/ge; print "5: $u\n"; $mul++ }
my $base = "hello"; print "6: ", ($base =~ s/l/L/gr), " orig=$base\n";
for my $i (1..2) { my $u = "banana"; my $c = ($u =~ tr/a/A/); print "7: $u $c\n" }
print "7r: ", ("banana" =~ tr/an/AN/r), "\n";
for my $p ("a", "b") { my $s = "ab"; my $c = () = $s =~ /$p/g; print "8: $p $c\n" }
my $q1 = qr/x/; my $q2 = qr/x/;
print "9: ", ($q1 == $q2 ? "same" : "diff"), "\n";
for my $s ("2020-01", "1999-12") { $s =~ /(?<y>\d+)-(?<m>\d+)/; print "10: $+{y} $+{m}\n" }
print "11: ", ("AB" =~ /ab/ ? 1 : 0), ("AB" =~ /ab/i ? 1 : 0), "\n";
for my $p ("a", "b") { my $s = "aabb"; $s =~ s/$p/./g; print "12: $s\n" }
for my $s ("xxAyy", "AAAA") { $s =~ /A/; print "13: $-[0] $+[0]\n" }
my $t14 = "aabbccdd"; my $c14 = ($t14 =~ tr/a-c//d); print "14: $t14 $c14\n";
for my $i (1..2) { print "15: ", ("a.b" =~ /a\Q.\Eb/ ? "y" : "n"), "\n" }
PL
    is($got, $want, '#1250/#1251 RUN: 15 regex/subst/tr shapes match perl 5.40.3');
}

# One more RUN row that only a CACHED op can get wrong: two sites whose
# pattern TEXT is identical but whose flags differ, interleaved, plus a site
# reached from two different packages.
{
    my $want = <<'OUT';
A:1 B:0
A:1 B:0
sub:ab-ab
OUT
    my $got = run_pl(<<'PL');
use strict; use warnings;
for my $i (1..2) {
  my $a = ("XY" =~ /xy/i) ? 1 : 0;
  my $b = ("XY" =~ /xy/)  ? 1 : 0;
  print "A:$a B:$b\n";
}
sub f { my $s = shift; $s =~ s/\d//g; return $s }
print "sub:", f("a1b2"), "-", f("a3b4"), "\n";
PL
    is($got, $want, '#1250 RUN: same pattern text under different flags, and a site inside a sub');
}
