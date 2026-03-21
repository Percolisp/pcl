#!/usr/bin/env perl
# list-slice-01.t - List slice (list)[indices] and (list)[range]

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

plan tests => 10;

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
    my $got = run_cl($code);
    is($got, $expected, $name);
}

# array.t test 27: (list)[0..5] — full range in list context
test_cl("list slice full range",
    q{print join("", ("a","b","c","d","e","f")[0..5]), "\n";},
    "abcdef\n");

# array.t test 28: (list)[0..1]
test_cl("list slice partial range",
    q{print join("", ("a","b","c","d","e","f")[0..1]), "\n";},
    "ab\n");

# single index in list context
test_cl("list slice single index",
    q{print join("", ("a","b","c","d","e","f")[1]), "\n";},
    "b\n");

# explicit multi-index in list context
test_cl("list slice explicit indices",
    q{print join(",", ("a","b","c","d","e","f")[0,2,4]), "\n";},
    "a,c,e\n");

# assigned to array
test_cl("list slice assigned to array",
    q{my @x = ("a","b","c","d","e","f")[0..2]; print "@x\n";},
    "a b c\n");

# out of bounds = undef
test_cl("list slice out of bounds is undef",
    q{my $x = ("a","b","c","d","e","f")[6]; print defined($x) ? "defined" : "undef", "\n";},
    "undef\n");

# reverse order
test_cl("list slice reversed indices",
    q{print join("", ("a","b","c","d","e","f")[5,4,3,2,1,0]), "\n";},
    "fedcba\n");

# range 1..3
test_cl("list slice range 1..3",
    q{my $x = join("|", ("a","b","c","d","e","f")[1..3]); print "$x\n";},
    "b|c|d\n");

# list slice assigned then joined
test_cl("list slice assigned then joined",
    q{my @x = ("x","y","z","w")[1..3]; print join("-", @x), "\n";},
    "y-z-w\n");

# negative index
test_cl("list slice negative index",
    q{print join("", ("a","b","c","d","e","f")[-1]), "\n";},
    "f\n");
