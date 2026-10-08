#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# perf-levers-11.t — perf round 42 (s513c, docs/faster-codegen-suggestions.md
# "Round 42 movers").  EMISSION rows (the slot class the verdict gives), a
# TIMED row (seconds on the round's base, main 33721af7; well under one here)
# and ANSWER rows: programs whose expected output below IS perl's own (probed
# on 5.40.3; nothing in them needs perl >= 5.38).
#
#   #2880 an append-only accumulator declared WITHOUT an initializer
#         (`my $s;`) gets the str-buffer slot when every read is in
#         VarAnnotator's %STRBUF_USE; undef is the zero-capacity buffer, so
#         `length` keeps answering undef until the first append.
#   #2881 a read that keeps nothing (substr's extraction, a single m//,
#         length) reads a :strbuf cell live; an element reaches it as its
#         slot box (%p-peek-helem / -aelem).
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
# #2880 EMISSION: every definedness-insensitive read (and `length`) widens; a
# read outside %STRBUF_USE keeps the ordinary :scalar slot.
{
    my %slot;
    my $cl = transpile(<<'END_SRC');
{ my $ua; $ua .= "a" for 1..3; print "[$ua]\n"; }
{ my $ub; print(($ub ? "T" : "F"), "\n"); $ub .= "x"; }
{ my $uc; print(($uc eq "" ? 1 : 0), "\n"); $uc .= "y"; }
{ my $ue; print substr($ue, 0, 1); $ue .= "zz"; }
{ my $uf; print $uf; $uf .= "p"; }
{ my $ul; print length($ul) // "undef"; $ul .= "p"; }
sub f { my $ug; for my $i (1..3) { $ug .= $i } return "$ug" }
{ my $acc; for my $w (qw(a b)) { { my $t; $t .= $w; $acc .= $t } } print $acc; }
{ my $wa; print defined $wa ? 1 : 0; $wa .= "x"; }
{ my $wb; my $n = $wb // "d"; $wb .= "x"; }
{ my $wd; my $v = $wd; $wd .= "x"; }
{ my $wf; push my @l, $wf; $wf .= "x"; }
END_SRC
    $slot{$1} = $2 while $cl =~ /\(\$(\w+) (:[\w-]+) /g;
    is(join(' ', map { "$_=$slot{$_}" } qw(ua ub uc ue uf ul ug acc t)),
       join(' ', map { "$_=:str-buffer" } qw(ua ub uc ue uf ul ug acc t)),
       '#2880 emission: `my $x;` + .= with only str/bool/length reads (also in a sub, a nested block, an inner loop accumulator) is a :str-buffer slot');
    is(join(' ', map { "$_=" . ($slot{$_} // 'none') } qw(wa wb wd wf)),
       'wa=:scalar wb=:scalar wd=:scalar wf=:scalar',
       '#2880 emission: defined / `//` / a copy / a push of the value withhold the verdict (the slot stays :scalar)');
    like($cl, qr/\(\$ua :str-buffer \(%pcl-str-buffer \(p-undef\)\)\)/,
         '#2880 emission: the undef start is (%pcl-str-buffer (p-undef)) -- the zero-capacity buffer');
}

{
    # 30 000 appends to `my $u;`: ~2 s on the base (a fresh concatenation per
    # append), ~0.1 s here.  The bound is generous (CI is slower).
    my $t0 = time;
    my $out = run_pl(q{my $u; for my $i (1 .. 30000) { $u .= "line $i\n" } print length($u), " ", substr($u, -6);});
    my $dt = time - $t0;
    is($out, "318894 30000\n", '#2880 timed: the answer');
    cmp_ok($dt, '<', 1.5, sprintf('#2880 timed: 30 000 appends to an undef-declared accumulator in under 1.5 s (took %.2f s)', $dt));
}

answers(<<'END_SRC', <<'END_EXP', '#2880 answers: widened reads and the withheld definedness reads are perl\'s');
{ my $ua; $ua .= "a" for 1..3; print "[$ua]\n"; }
{ my $ub; print(($ub ? "T" : "F"), "\n"); $ub .= "x"; print "$ub\n"; }
{ my $uc; print(($uc eq "" ? 1 : 0), "\n"); $uc .= "y"; print "$uc\n"; }
{ my $ud; print(($ud =~ /a/ ? 1 : 0), "\n"); $ud .= "a"; print "$ud\n"; }
{ my $ue; print "[", substr($ue, 0, 1), "]\n"; $ue .= "zz"; print substr($ue, 0, 1), "\n"; }
{ my $uf; print $uf; $uf .= "p"; print $uf, "\n"; }
sub f { my $ug; for my $i (1..3) { $ug .= $i } return "$ug" } print f(), "\n";
{ my $acc; for my $w (qw(a b c)) { { my $t; $t .= $w; $t .= "!"; $acc .= $t } } print "$acc\n"; }
{ my $wa; print defined $wa ? 1 : 0; $wa .= "x"; print "\n"; }
{ my $wb; my $n = $wb // "d"; $wb .= "x"; print "$n\n"; }
{ my $wc; print length($wc) // "undef"; $wc .= "x"; print "\n"; }
{ my $wd; my $v = $wd; $wd .= "x"; print defined $v ? 1 : 0, "\n"; }
{ my $xa; print length($xa) // "undef", "\n"; $xa .= ""; print length($xa) // "undef", "\n"; $xa .= "ab"; print length($xa), "\n"; }
sub nothing { return undef } { my $xb = nothing(); for my $i (1..2) { $xb .= "" if $i > 5 } print length($xb) // "undef", "\n"; }
{ my $xc; for my $i (1..5) { $xc .= $i } print length($xc), " $xc\n"; }
{ my $xd; for my $i (1..2) { $xd .= "" } print length($xd) // "undef", "\n"; }
{ my $xe; print $xe ? "T\n" : "F\n"; $xe .= "0"; print $xe ? "T\n" : "F\n"; }
{ my $xf; print lc($xf), ord($xf), index($xf, "x"), index("abc", $xf), "|", join(",", $xf, $xf), "|", $xf x 2, "|", ($xf cmp ""), "\n"; $xf .= "Q"; print lc($xf), "\n"; }
END_SRC
[aaa]
F
x
1
y
0
a
[]
z
p
123
a!b!c!
0
d
undef
0
undef
0
2
undef
5 12345
0
F
F
0-10|,||0
q
END_EXP

# ─────────────────────────────────────────────────────────────────────────────
# #2881 a READ of a growing accumulator does not copy it: substr's extraction,
# a single m// and length read a :strbuf cell LIVE (%p-live-text), and an
# ELEMENT reaches them as its slot box (%p-peek-helem / -aelem, compiler
# macros).  MECHANISM: the peek spellings; /g, s/// and 4-argument substr
# keep the ordinary read.
like(lisp_out(q{(let ((*print-pretty* nil)) (print (list (funcall (compiler-macro-function (quote p-substr)) (quote (p-substr (p-gethash h "u") -2 1)) nil) (funcall (compiler-macro-function (quote p-=~)) (quote (p-=~ (p-aref a 2) (p-regex :pat "x" :flags ""))) nil) (funcall (compiler-macro-function (quote p-length)) (quote (p-length (p-gethash h "u"))) nil))) (print (list (funcall (compiler-macro-function (quote p-substr)) (quote (p-substr (p-gethash h "u") 0 1 "r")) nil) (funcall (compiler-macro-function (quote p-=~)) (quote (p-=~ (p-gethash h "u") (p-regex :pat "x" :flags "g"))) nil) (funcall (compiler-macro-function (quote p-=~)) (quote (p-=~ (p-gethash h "u") (p-subst :pat "x"))) nil))))}),
     qr/\Q((p-substr (%p-peek-helem h "u") -2 1) (p-=~ (%p-peek-aelem a 2) (p-regex :pat "x" :flags "")) (p-length (%p-peek-helem h "u"))) \E\n\Q((p-substr (p-gethash h "u") 0 1 "r") (p-=~ (p-gethash h "u") (p-regex :pat "x" :flags "g")) (p-=~ (p-gethash h "u") (p-subst :pat "x"))) \E/,
     '#2881: substr / m// / length of an element read it through %p-peek-*; 4-arg substr, /g and s/// do not');

{
    # 30 000 appends to a hash element, each followed by a substr AND a match
    # of the growing string: ~4 s on the base (two whole-string copies per
    # iteration), well under a second here.  The bound is generous.
    my $t0 = time;
    my $out = run_pl(q{my %h; my $m = 0; for my $i (1 .. 30000) { $h{u} .= "line $i\n"; $m++ if substr($h{u}, -2, 1) eq "9"; $m++ if $h{u} =~ /9\n\z/ } print "$m\n";});
    my $dt = time - $t0;
    is($out, "6000\n", '#2881 timed: the answer');
    cmp_ok($dt, '<', 1.5, sprintf('#2881 timed: 30 000 append + substr + match on a growing hash element in under 1.5 s (took %.2f s)', $dt));
}

answers(<<'END_SRC', <<'END_EXP', '#2881 answers: a copy keeps its value, $& / captures / substr results survive later appends, pos, s///, 4-arg substr, memfh, boxed lexical, global');
my $n = 0;
sub row { my ($name, @v) = @_; $n++; print "$n $name: ", join("|", map { defined $_ ? $_ : "U" } @v), "\n" }
my %h; my @a; our $g; my $u = "";
for my $i (1 .. 60) { $h{u} .= "line $i\n"; $a[2] .= "item $i\n"; $g .= "g$i,"; $u .= "u$i;" }
my $copy = $h{u}; $h{u} .= "MORE\n";
row("copy keeps", length($copy), substr($copy, -3), length($h{u}));
row("substr helem", substr($h{u}, -5, 4), substr($h{u}, 0, 4), length(substr($h{u}, 5)));
row("substr aelem", substr($a[2], -3, 2), length($a[2]));
row("match helem", $h{u} =~ /line (\d+)\nMORE\n\z/ ? "$1 $-[0] $+[0]" : "no");
$h{u} .= "after\n";
row("\$& after append", $&, $1, length($`), $', length($h{u}));
row("match aelem list", join(",", $a[2] =~ /item (5)(\d)/));
row("match miss", ($h{u} =~ /nope/) ? 1 : 0, $1);
row("global var", substr($g, -4), $g =~ /g60,\z/ ? 1 : 0, length($g));
row("boxed lexical", $u =~ /u(\d+);\z/ ? $1 : "no", substr($u, 0, 3));
my $m = 0; while ($h{u} =~ /line (\d)\n/g) { $m++ } row("helem //g", $m);
my $p = $u; pos($p) = 0; $p =~ /u1/g; row("pos after //g", pos($p));
$h{u} =~ s/after/AFTER/; row("s/// helem", substr($h{u}, -6, 5));
substr($h{u}, 0, 4, "LINE"); row("4-arg substr", substr($h{u}, 0, 6));
my $w = substr($h{u}, 0, 4); $h{u} .= "x"; row("substr result kept", $w);
my @caps = ($h{u} =~ /^(LINE) (1)/); $h{u} .= "y"; row("list caps kept", @caps);
row("index", index($h{u}, "MORE"), rindex($h{u}, "line"));
row("eq", ($h{u} eq $h{u}) ? 1 : 0, ($copy eq $h{u}) ? 1 : 0);
my $buf = ""; open my $fh, ">", \$buf; for my $i (1 .. 50) { print $fh "rec $i\n" } row("memfh", length($buf), $buf =~ /rec (50)\n\z/ ? $1 : "no", substr($buf, 0, 5));
row("pos(\$h{u})", defined pos($h{u}) ? pos($h{u}) : "U");
END_SRC
1 copy keeps: 471|60
|476
2 substr helem: MORE|line|471
3 substr aelem: 60|471
4 match helem: 60 463 476
5 $& after append: line 60
MORE
|60|463||482
6 match aelem list: 5,0
7 match miss: 0|5
8 global var: g60,|1|231
9 boxed lexical: 60|u1;
10 helem //g: 9
11 pos after //g: 2
12 s/// helem: AFTER
13 4-arg substr: LINE 1
14 substr result kept: LINE
15 list caps kept: LINE|1
16 index: 471|463
17 eq: 1|0
18 memfh: 341|50|rec 1
19 pos($h{u}): U
END_EXP

done_testing();
