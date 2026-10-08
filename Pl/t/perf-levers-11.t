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

done_testing();
