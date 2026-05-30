#!/usr/bin/env perl
# hexfloat-01.t — hex/binary/octal float literal preprocessing (Pl/Parser.pm
# _preprocess_source) must convert NUMERIC literals while leaving identical-looking
# text inside quoted strings untouched.
#
# Regression (session 216): '0x1p+0' as a single-quoted string was being rewritten
# to '1' because the float-literal substitution ran over the whole source, including
# string contents. This corrupted the entire sprintf2.t hex-float test data table.

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

plan tests => 6;

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

# single-quoted hex-float-looking text stays literal
test_cl('single-quoted 0x1p+0 stays literal',
    q{my $s = '0x1p+0'; print "$s\n";},
    "0x1p+0\n");

# double-quoted too
test_cl('double-quoted 0x1p+0 stays literal',
    q{my $s = "0x1p+0"; print "$s\n";},
    "0x1p+0\n");

# inside a list (the sprintf2.t data-table shape)
test_cl('hex-float string in a list element',
    q{my @a = ('%a', '1', '0x1p+0'); print "$a[2]\n";},
    "0x1p+0\n");

# binary- and octal-float-looking strings stay literal too
test_cl('binary-float string stays literal',
    q{my $s = '0b10p-2'; print "$s\n";},
    "0b10p-2\n");

# a REAL hex-float numeric literal is still converted to its decimal value
test_cl('real hex-float literal 0x1.8p+1 == 3',
    q{my $x = 0x1.8p+1; print "$x\n";},
    "3\n");

# sprintf %a of an actual number still produces the hex float
test_cl('sprintf %a still works',
    q{printf "%a\n", 1;},
    "0x1p+0\n");
