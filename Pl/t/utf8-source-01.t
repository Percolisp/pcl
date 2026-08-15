#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# Regression: `use utf8` must decode the *source* as UTF-8, so multi-byte
# sequences in string literals and identifiers are single CHARACTERS (length /
# substr / index / regex see chars, not bytes).  Without the pragma, high bytes
# stay Latin-1 (byte semantics), matching Perl.
#
# This file is written with raw UTF-8 bytes on purpose (é = 0xC3 0xA9, etc.).

use v5.30;
use strict;
use warnings;
use utf8;                       # so the literals below are UTF-8 in THIS file too

use Test::More;
use File::Temp qw(tempfile);
use Encode qw(encode_utf8 decode_utf8);

use lib ".";
use Pl::Parser2;

sub run_bytes {
    # CODE is a *byte* string (as PCL reads a source file); transpile + run.
    my $code = shift;
        my $cl_code = Pl::Parser2->parse_code($code);
    my ($fh, $filename) = tempfile(SUFFIX => '.lisp');
    binmode($fh, ':encoding(utf-8)');   # pl2cl binmodes its output to utf8 too
    print $fh $cl_code;
    close $fh;
    my $output = `sbcl --noinform --non-interactive --load cl/pcl-runtime.lisp --load "$filename" 2>&1`;
    unlink $filename;
    $output =~ s/^;.*\n//gm;
    $output =~ s/PCL Runtime loaded\n?//g;
    $output =~ s/^\s*\n//gm;
    $output =~ s/^\s+//;
    # sbcl output is UTF-8 bytes; decode so it compares against this file's
    # `use utf8` character literals.
    return decode_utf8($output);
}

plan tests => 6;

# café = 4 chars under use utf8 (é is one char), 5 bytes without it.
is(run_bytes(encode_utf8('use utf8; my $s = "café"; print length($s), "\n";')),
   "4\n", 'use utf8: length("café") == 4 (chars)');

is(run_bytes(encode_utf8('my $s = "café"; print length($s), "\n";')),
   "5\n", 'no use utf8: length("café") == 5 (bytes)');

# substr on a decoded string indexes by character.
is(run_bytes(encode_utf8('use utf8; my $s = "héllo"; print substr($s,1,1), "\n";')),
   "é\n", 'use utf8: substr indexes by character');

# UTF-8 identifiers parse and round-trip under use utf8.
is(run_bytes(encode_utf8("use utf8;\nmy \$café = 42;\nprint \$café, \"\\n\";")),
   "42\n", 'use utf8: UTF-8 identifier');

# index() is character-based under use utf8.
is(run_bytes(encode_utf8('use utf8; my $s = "axé"; print index($s,"é"), "\n";')),
   "2\n", 'use utf8: index() is character-based');

# Task #313 (LOAD-TIME CRASH, found by the s392 companion-suite audit): a
# package whose name STARTS with a non-ASCII letter must land in the ORDINARY
# global partition like any other user package.  Pl::GlobalPartition matched a
# segment's first character ASCII-only, so `બʑ::@ISA` was misread as "not
# word-shaped" = EXCEPTION and declared `defvar` — while p-defpackage had
# already made that package's @ISA a symbol-macro cell.  SBCL then refused the
# file outright ("Cannot proclaim a macro variable special").  The nested-block
# shape is required: it is what makes the declaration come out QUALIFIED.
is(run_bytes(encode_utf8(
     "use utf8;\nuse strict; use warnings;\n"
   . "{\n"
   . "    { package Ascii; our \@ISA = ('X'); }\n"
   . "    { package બʑ;    our \@ISA = ('X'); }\n"
   . "}\n"
   . "print \"ok\\n\";\n")),
   "ok\n", 'utf8 package name in a nested block: @ISA is an ordinary cell (#313)');
