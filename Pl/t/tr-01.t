#!/usr/bin/env perl
# tr-01.t — transliteration (tr///) complement and modifier semantics.
#
# Covers (session 216) the /c complement family and /r:
#  - /c   complement search-list: each non-matched char is ranked by its position
#         among all codepoints NOT in the search list, then mapped positionally
#         into the replacement list (last repl char repeats past its end).
#  - /cd  complemented chars past the end of the replacement list are deleted.
#  - /cs  consecutive chars transliterated to the same char are squeezed.
#  - /r   returns the transliterated copy, leaving the original unchanged.
#  - squeeze only applies to *translated* chars (pass-through chars break the run).

use v5.30;
use strict;
use warnings;
use Test::More;
use File::Temp qw(tempfile);
use FindBin qw($RealBin);

my $project_root = "$RealBin/../..";
my $pl2cl        = "$project_root/pl2cl";
my $runtime      = "$project_root/cl/pcl-runtime.lisp";

plan skip_all => "pl2cl not found" unless -x $pl2cl;
plan skip_all => "sbcl not found"  unless `which sbcl 2>/dev/null`;

plan tests => 8;

sub run_cl {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;
    my $cl_code = `$pl2cl $pl_file 2>/dev/null`;
    my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cl_fh $cl_code;
    close $cl_fh;
    my $output = `sbcl --noinform --non-interactive --load $runtime --load $cl_file 2>&1`;
    $output =~ s/^;.*\n//gm;
    $output =~ s/^PCL Runtime loaded\n//gm;
    $output =~ s/^\s*\n//gm;
    return $output;
}

sub test_cl {
    my ($name, $code, $expected) = @_;
    is(run_cl($code), $expected, $name);
}

# Plain tr still works
test_cl('basic tr/a-z/A-Z/',
    'my $s="hello"; (my $c = $s) =~ tr/a-z/A-Z/; print "$c\n";',
    "HELLO\n");

# /c complement: replace everything NOT in [aeiou] with "_"
test_cl('/c complement maps non-matched to repl',
    'my $s="hello world"; $s =~ tr/aeiou/_/c; print "$s\n";',
    "_e__o__o___\n");

# /c count = number of complemented (matched) chars
test_cl('/c count',
    'my $s="hello world"; my $c = ($s =~ tr/aeiou/_/c); print "$c\n";',
    "8\n");

# /cd: complemented chars with no replacement char are deleted (empty repl)
test_cl('/cd deletes non-matched',
    'my $s="hello world"; $s =~ tr/aeiou//cd; print "$s\n";',
    "eoo\n");

# /cs: squeeze runs translated to the same char
test_cl('/cs squeezes complemented run',
    'my $s="hello world"; $s =~ tr/aeiou/_/cs; print "$s\n";',
    "_e_o_o_\n");

# /r returns a copy and leaves the original alone
test_cl('/r returns copy, original unchanged',
    'my $s="hello"; my $t = ($s =~ tr/a-z/A-Z/r); print "$t:$s\n";',
    "HELLO:hello\n");

# ranked /c mapping into a multi-char replacement list: a,b,c are codepoints
# 97/98/99, and their rank among non-(d-z) codepoints includes all of 0..96,
# so all three rank past the 3-char replacement and map to its LAST char ('3').
test_cl('/c ranked positional mapping past repl end',
    'my $s="abcabc"; $s =~ tr/d-z/123/c; print "$s\n";',
    "333333\n");

# squeeze does not collapse pass-through chars
test_cl('/s leaves pass-through duplicates intact',
    'my $s="aabbccaa"; $s =~ tr/a/x/s; print "$s\n";',
    "xbbccx\n");
