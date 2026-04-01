#!/usr/bin/env perl
# hashassign-01.t - Tests for hash assignment with mixed literal and hash args

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

plan tests => 4;

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

# hashassign.t tests 44-46: hash used in list context alongside literal pairs
# p-hash must flatten hash-table arguments into key-value pairs

test_cl('hash flattened in list: "%" key overrides',
    'my %names = ("@" => "Array", "%" => "Hash", "&" => "Code");
     my %copy;
     %copy = ("%", "Associative Array", %names);
     print $copy{"%"}, "\n";',
    "Hash\n");   # %names overrides the literal "%" key

test_cl('hash flattened in list: "*" key',
    'my %names = ("@" => "Array", "%" => "Hash", "&" => "Code");
     my %copy;
     %copy = ("*", "Typeglob", %names);
     print $copy{"@"}, "\n";',
    "Array\n");

test_cl('hash flattened in list: all keys present',
    'my %names = ("@" => "Array", "%" => "Hash", "&" => "Code");
     my %copy;
     %copy = ("%", "Associative Array", %names);
     my @k = sort keys %copy;
     print "@k\n";',
    "% & \@\n");

test_cl('hash flattened in p-hash: hash-table arg expands to pairs',
    'my %a = (x => 1, y => 2);
     my %b = (z => 3, %a);
     print join(",", map { "$_=$b{$_}" } sort keys %b), "\n";',
    "x=1,y=2,z=3\n");
