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
