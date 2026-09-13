#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# posix-class-01.t — perl's POSIX bracket classes, both spellings.
#
# cl-ppcre has no `[:name:]` syntax, so the runtime rewrites the form into an
# equivalent range list (`+p-posix-class-ranges+`).  The NEGATED spelling
# `[:^name:]` used to fall through untranslated: `[[:^alnum:]]` then reached
# cl-ppcre as the class `[[:^alnum:]` followed by a literal `]` — two
# characters where perl needs one — so it matched NOTHING, and `[^[:^alnum:]]`
# matched nothing either.  A class and its complement agreeing is exactly what
# t/re/reg_posixcc.t's 6,630 extra rows assert cannot happen.  `[:ascii:]` was
# missing from the table for the same reason (a silent fall-through).
#
# The expectations here are perl 5.40.3's own answers, probed.  The MEMBERSHIP
# of a class is still ASCII-only above 0x7F (perl's /u classes are
# Unicode-aware — #1036, out of scope by USER decision s465), so every
# expectation below stays inside 0x00-0x7F; row 2 is the complement property,
# which must hold at every code point.

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

plan tests => 3;

sub run_cl {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;
    my $cl_code = `$pl2cl $pl_file 2>/dev/null`;
    my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cl_fh $cl_code;
    close $cl_fh;
    my $output = `sbcl @sbcl_rt --load $cl_file 2>&1`;
    $output =~ s/^;.*\n//gm;
    $output =~ s/^PCL Runtime loaded\n//gm;
    $output =~ s/^\s*\n//gm;
    return $output;
}

sub test_cl {
    my ($name, $code, $expected) = @_;
    is(run_cl($code), $expected, $name);
}

# ── 1. the matrix: 32 pattern spellings × 20 subjects, perl's own answers ──
# The subjects include every character the rewrite could mis-emit as a class
# element: `]` `[` `^` `\` `-`, plus NUL and DEL at the range ends.
my $matrix_prog = <<'PROG';
use strict; use warnings; use feature 'unicode_strings';
my @subj = ('A','z','5','_',' ',"\t",'-',']','[','^','\\','~',
            '!','/',':','@','`','{',"\x00","\x7f");
my @pats = ('[[:alpha:]]', '[^[:alpha:]]', '[[:^alpha:]]',
            '[[:alpha:][:digit:]]', '[a[:digit:]z]', '[[:digit:]-]',
            '[[:word:]]', '[[:^word:]]', '[[:punct:]]', '[[:^punct:]]',
            '[[:cntrl:]]', '[[:^cntrl:]]', '[[:ascii:]]', '[[:^ascii:]]',
            '[[:space:]]', '[[:^space:]]', '[[:blank:]]', '[[:^blank:]]',
            '[[:print:]]', '[[:^print:]]', '[[:graph:]]', '[[:^graph:]]',
            '[[:xdigit:]]', '[[:^xdigit:]]', '[[:upper:]]', '[[:^upper:]]',
            '[[:lower:]]', '[[:^lower:]]', '[[:alnum:]]', '[[:^alnum:]]',
            '[[:^digit:]]', '[^[:^digit:]]');
for my $p (@pats) {
    my $row = '';
    for my $s (@subj) { $row .= (($s =~ /$p/) ? 1 : 0) }
    print "$row\n";
}
PROG
test_cl('POSIX classes: 32 spellings x 20 subjects match perl 5.40.3',
    $matrix_prog, <<'EXPECT');
11000000000000000000
00111111111111111111
00111111111111111111
11100000000000000000
01100000000000000000
00100010000000000000
11110000000000000000
00001111111111111111
00010011111111111100
11101100000000000011
00000100000000000011
11111011111111111100
11111111111111111111
00000000000000000000
00001100000000000000
11110011111111111111
00001100000000000000
11110011111111111111
11111011111111111100
00000100000000000011
11110011111111111100
00001100000000000011
10100000000000000000
01011111111111111111
10000000000000000000
01111111111111111111
01000000000000000000
10111111111111111111
11100000000000000000
00011111111111111111
11011111111111111111
00100000000000000000
EXPECT

# ── 2. the COMPLEMENT PROPERTY, which is reg_posixcc.t's own assertion ──
# For all fourteen names and every code point 0..255, `[[:N:]]` and
# `[[:^N:]]` must disagree, and `[^[:^N:]]` must agree with `[[:N:]]`.
# Before the fix this failed for all 14 × 256 pairs (both answers were 0).
my $complement_prog = <<'PROG';
use strict; use warnings; use feature 'unicode_strings';
my @n = qw(alpha digit alnum upper lower word space blank print graph
           punct cntrl xdigit ascii);
my ($same, $dneg) = (0, 0);
for my $n (@n) {
    for my $b (0 .. 255) {
        my $c   = chr($b);
        my $in  = ($c =~ /[[:$n:]]/)   ? 1 : 0;
        my $out = ($c =~ /[[:^$n:]]/)  ? 1 : 0;
        my $dd  = ($c =~ /[^[:^$n:]]/) ? 1 : 0;
        $same++ if $in == $out;
        $dneg++ if $in != $dd;
    }
}
print "same=$same doubleneg=$dneg\n";
PROG
test_cl('POSIX class and [:^class:] are complements at every code point',
    $complement_prog, "same=0 doubleneg=0\n");

# ── 3. the two spellings that had no table entry at all ──
test_cl('[:ascii:] exists and [:^ascii:] is its complement',
    q{print "[", ("A" =~ /[[:ascii:]]/ ? 1:0), "]",}
  . q{      "[", ("A" =~ /[[:^ascii:]]/ ? 1:0), "]",}
  . q{      "[", (chr(0x100) =~ /[[:^ascii:]]/ ? 1:0), "]",}
  . q{      "[", (chr(0x100) =~ /[[:ascii:]]/ ? 1:0), "]\n";},
    "[1][0][1][0]\n");
