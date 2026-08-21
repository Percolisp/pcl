#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# wide-codepoint-01.t — task #419: ONE code point above U+10FFFF in a string
# literal used to cost the WHOLE emitted file.
#
# perl can hold code points beyond Unicode's maximum and encodes them with its
# own extended UTF-8 (`chr(0x4000000)` is the pre-2003 six-byte
# `fc 84 80 80 80 80`).  SBCL cannot hold them at all — `char-code-limit` is
# #x110000 — and PCL used to write the character RAW into the emitted CL, so
# SBCL's UTF-8 reader rejected the source at the first buffer holding one and
# NOTHING in the file loaded: `t/re/pat.t` has eight such characters and its
# 1263 rows under perl measured as 0.
#
# The rule is the #138 rule — the failure must be the size of the EXPRESSION,
# not of the file.  `Pl/ExprToCL.pm`'s _cl_string_literal_form (the one writer
# every quote form now goes through) emits `(p-unrepresentable-char N)` in the
# character's place: it READS, and dies when evaluated, naming the code point
# and citing docs/not-supported.md.
#
# Deliberate asymmetry guarded by the last row: `chr(N)` at RUN time keeps
# answering U+FFFD (the older blessed ruling, docs/fable-answers-s318.md §11 /
# not-supported.md "Code points above U+10FFFF"), because there the compiler
# has no literal to refuse.

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

plan tests => 8;

sub write_pl {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    binmode($fh, ':raw');
    print $fh $code;
    close $fh;
    return $pl_file;
}

sub emitted { return PCLCore::transpile("$pl2cl " . write_pl($_[0])) }

sub run_cl {
    my ($code) = @_;
    my $cl_code = emitted($code);
    my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    binmode($cl_fh, ':raw');
    print $cl_fh $cl_code;
    close $cl_fh;
    my $output = `sbcl @sbcl_rt --load $cl_file 2>&1`;
    $output =~ s/^;.*\n//gm;
    $output =~ s/^PCL Runtime loaded\n//gm;
    $output =~ s/^\s*\n//gm;
    return $output;
}

sub run_perl { my $f = write_pl($_[0]); return scalar `perl $f 2>&1` }

# Every spelling that DECODES a \x{}/\N{U+} escape into a character.  The
# regex forms are deliberately absent: a pattern's text reaches cl-ppcre
# unprocessed, so `qr/\x{4000000}/` never builds the character here.
my $ALL_SPELLINGS = <<'PL';
no warnings;
my $a = "\x{4000000}";
my $b = qq{x\x{4000000}y};
my $c = "\N{U+4000000}";
my $d = <<"EOT";
\x{4000000}
EOT
my $v = 1;
my $e = "p\x{4000000}$v";
(my $f = "z") =~ s/z/\x{4000000}/;
(my $g = "z") =~ tr/\x{4000000}/Q/;
PL

# ── 1. The point of the task: the FILE still runs ────────────────────────────
# A literal in code that never executes costs nothing at all, and the
# statements around it are untouched.
is(run_cl(<<'PL'), "A\nB\nC\n",
no warnings;
print "A\n";
if (0) { my $s = "\x{4000000}"; print length($s), "\n"; }
print "B\n";
my @x = (1, 2, 3);
print "C\n" if @x == 3;
PL
   'a >U+10FFFF literal in dead code costs nothing — the file loads and runs');

# ── 2. Evaluating it dies, perl-visibly, naming the code point ───────────────
is(run_cl(<<'PL'), "A\ndef:0\ncp:1\ndoc:1\nB\n",
no warnings;
print "A\n";
my $r = eval { my $s = "\x{4000000}"; length($s) };
print "def:", (defined $r ? 1 : 0), "\n";
print "cp:",  ($@ =~ /code point 0x4000000/     ? 1 : 0), "\n";
print "doc:", ($@ =~ /not-supported\.md/        ? 1 : 0), "\n";
print "B\n";
PL
   'the expression dies into $@, naming the code point and citing the doc');

# ── 3. Every decoding spelling reaches the one writer ────────────────────────
{
    my $cl = emitted($ALL_SPELLINGS);
    my @forms = $cl =~ /\(p-unrepresentable-char 67108864\)/g;
    is(scalar(@forms), 7,
       'dq, qq{}, \N{U+}, heredoc, interpolated, s/// replacement and tr/// '
       . 'all emit (p-unrepresentable-char 67108864)');

    # The property #419 is actually about: SBCL's UTF-8 reader must accept
    # every byte of the file.  It rejects 5/6-byte extended forms (leads
    # 0xF8-0xFD), the 4-byte forms above U+10FFFF (0xF5-0xF7, and 0xF4
    # followed by 0x90-0xBF), and encoded surrogates (0xED 0xA0-0xBF).
    ok($cl !~ /[\xF5-\xFD]/ && $cl !~ /\xF4[\x90-\xBF]/ && $cl !~ /\xED[\xA0-\xBF]/,
       'the emitted file holds no byte sequence SBCL\'s UTF-8 reader rejects');
}

# ── 4. Only the character is replaced; its neighbours stay in the literal ────
like(emitted(q{no warnings; my $s = "a\x{4000000}b";}),
     qr/\(concatenate 'string "a" \(p-unrepresentable-char 67108864\) "b"\)/,
     'a mixed literal keeps its representable runs and splits out the character');

# The sibling case must not move: a surrogate is representable as an SBCL
# character (it just cannot be written into a UTF-8 file), so it keeps the
# (string (code-char N)) spelling it has always had.
like(emitted(q{no warnings; my $s = "x\x{D800}y";}),
     qr/\(concatenate 'string "x" \(string \(code-char 55296\)\) "y"\)/,
     'a surrogate still emits (string (code-char N)) — unchanged');

# ── 5. The representable boundary, against the perl oracle ──────────────────
# U+10FFFF is the LAST code point SBCL can hold and must keep working; so must
# the noncharacters and the surrogate, which the same writer splits out.
{
    my $prog = <<'PL';
no warnings;
print length("\x{10FFFF}"), ":", ord("\x{10FFFF}"), "\n";
my $s = "a\x{10FFFE}b\x{FFFE}c\x{D800}d";
print length($s), ":", join(",", map { ord } split //, $s), "\n";
print "eq:", ("\x{10FFFF}" eq chr(0x10FFFF) ? 1 : 0), "\n";
PL
    is(run_cl($prog), run_perl($prog),
       'U+10FFFF, the noncharacters and a surrogate are unaffected (perl oracle)');
}

# ── 6. The deliberate asymmetry: chr() at RUN time is still U+FFFD ──────────
# perl answers ord 67108864 here; PCL answers 65533.  That divergence is the
# older blessed ruling (not-supported.md), NOT an oversight — the compiler has
# no literal to refuse when the argument is computed, and making the runtime
# die instead would be a second answer for one gap.  If this row ever needs to
# change, change not-supported.md in the same commit.
is(run_cl(<<'PL'), "A\n1:65533\nB\n",
no warnings;
print "A\n";
my $s = chr(0x4000000);
print length($s), ":", ord($s), "\n";
print "B\n";
PL
   'chr(N) above U+10FFFF still yields U+FFFD at run time and does not die');
