#!/usr/bin/env perl

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
use Pl::Parser;

sub run_bytes {
    # CODE is a *byte* string (as PCL reads a source file); transpile + run.
    my $code = shift;
    my $parser = Pl::Parser->new(code => $code);
    my $cl_code = $parser->parse();
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

plan tests => 5;

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
