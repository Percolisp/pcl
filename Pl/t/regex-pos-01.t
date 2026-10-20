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
