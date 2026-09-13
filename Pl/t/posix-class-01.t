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
#
# Rows 4-5 are the same shape for \h \H \v \V \R (task #1713), which were not
# in cl-ppcre's six escapes at all and so matched the LETTER: `/\h+/` matched a
# run of h.  Those two sets are NOT ASCII-only — they ARE their Unicode selves
# in perl (probed code point by code point over 0..0x11000) — and the rewrite
# has to know whether the escape sits inside a bracket class, because a class's
# own `^` applies to the whole class.

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

# ── 4. \h \H \v \V: membership, and the complement property at every code ──
# point up to 0x3100, which is reg_posixcc.t's own assertion for these pairs.
# The four rows of the code-point table are the task's: TAB, SPACE, NL, VT, the
# LETTERS h and H (which must NOT match \h/\v), NBSP and LINE SEPARATOR.
my $hv_prog = <<'PROG';
use strict; use warnings;
for my $cp (0x09, 0x20, 0x0A, 0x0B, 0x68, 0x48, 0xA0, 0x2028) {
    my $c = chr($cp);
    printf "%04X %d %d %d %d\n", $cp,
        ($c =~ /\h/) ? 1 : 0, ($c =~ /\H/) ? 1 : 0,
        ($c =~ /\v/) ? 1 : 0, ($c =~ /\V/) ? 1 : 0;
}
my ($bad, $n) = (0, 0);
for my $cp (0 .. 0x3100) {
    next if $cp >= 0xD800 && $cp <= 0xDFFF;
    my $c = chr($cp); $n++;
    $bad++ if (($c =~ /\h/) ? 1 : 0) == (($c =~ /\H/) ? 1 : 0);
    $bad++ if (($c =~ /\v/) ? 1 : 0) == (($c =~ /\V/) ? 1 : 0);
    $bad++ if (($c =~ /[\h]/) ? 1 : 0) != (($c =~ /[^\H]/) ? 1 : 0);
    $bad++ if (($c =~ /[\v]/) ? 1 : 0) != (($c =~ /[^\V]/) ? 1 : 0);
}
print "complement: $bad bad of $n\n";
PROG
test_cl('\h \H \v \V: perl 5.40.3 membership, and complements at every code point',
    $hv_prog, <<'EXPECT');
0009 1 0 0 1
0020 1 0 0 1
000A 0 1 1 0
000B 0 1 1 0
0068 0 1 0 1
0048 0 1 0 1
00A0 1 0 0 1
2028 0 1 1 0
complement: 0 bad of 12545
EXPECT

# ── 5. \R, and the cases the rewrite must NOT touch ────────────────────────
# `\\h` is an escaped backslash then the letter h; a `\h` inside \Q…\E is
# literal text; `\R` INSIDE a class is the letter R in perl 5.40.3 (probed) and
# is left for the engine to read that way; a class whose first character is `]`
# keeps it literal; and the /x and /xx normalisers must not eat the whitespace
# the rewrite itself emits into a class.
# (perl also WARNS on `[\R]` — "Unrecognized escape \R in character class" — on
# stderr; PCL emits no warnings-gated diagnostic at all, #221, so the
# expectation is perl's STDOUT, which is what this row compares.)
my $r_prog = <<'PROG';
use strict; use warnings;
my @m = ("a\r\nb" =~ /(\R)/g); printf "R1 %d %d\n", scalar(@m), length($m[0]);
print "R2 ", ("x\x{2028}y" =~ /\R/) ? 1 : 0, ("xy" =~ /\R/) ? 1 : 0, "\n";
my $s = "a\r\nb\nc"; my $k = ($s =~ s/\R/|/g); print "R3 $k $s\n";
print "R4 ", ("R" =~ /[\R]/) ? 1 : 0, ("\n" =~ /[\R]/) ? 1 : 0, "\n";
print "B1 ", ("a\\hb" =~ /\\h/) ? 1 : 0, ("h" =~ /\h/) ? 1 : 0, "\n";
print "B2 ", ("a\\hb" =~ /\Q\h\E/) ? 1 : 0, (" " =~ /\Q\h\E/) ? 1 : 0, "\n";
print "B3 ", ("a\tb" =~ /(?x: a \h b )/) ? 1 : 0, ("a\tb" =~ /a [ \h ] b/xx) ? 1 : 0, "\n";
print "B4 ", ("]" =~ /[]\h]/) ? 1 : 0, ("\t" =~ /[]\h]/) ? 1 : 0, ("[" =~ /[[\h]/) ? 1 : 0, "\n";
print "B5 ", ("\t" =~ /[^\h]/) ? 1 : 0, ("z" =~ /[^\h]/) ? 1 : 0, "\n";
print "B6 ", join("|", split /\h+/, "a  b\tc"), "\n";
PROG
test_cl('\R at top level, and \\\\h / \Q\h\E / [\R] / (?x:) left alone',
    $r_prog, <<'EXPECT');
R1 1 2
R2 10
R3 2 a|b|c
R4 10
B1 10
B2 10
B3 11
B4 111
B5 01
B6 a|b|c
EXPECT
