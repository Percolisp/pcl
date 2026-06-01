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

plan tests => 9;

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

# Regression: p-gethash must not unbox hash-ref values — session 176 fix.
# box-set treats a raw hash-table as %hash-in-scalar-context and converts it to
# key count, so p-gethash must return the p-box as-is for hash-table-valued entries.

test_cl('hash-ref value survives hash round-trip',
    'my %h = (a => {b => 1});
     my $x = $h{a};
     print ref($x), "\n";
     print $x->{b}, "\n";',
    "HASH\n1\n");

test_cl('hash-ref retrieved from hash not converted to key count',
    'my %h = (k => {x => 10, y => 20});
     my $r = $h{k};
     print scalar(%$r) ? "ok" : "fail", "\n";
     print $r->{x}, "\n";',
    "ok\n10\n");

test_cl('delete key inside hash-ref retrieved from hash',
    'my %h = (a => {b => 42, c => 99});
     my $r = $h{a};
     my $v = delete $r->{b};
     print $v, "\n";
     print exists $r->{b} ? "exists" : "gone", "\n";
     print $r->{c}, "\n";',
    "42\ngone\n99\n");

test_cl('hash flattened in p-hash: hash-table arg expands to pairs',
    'my %a = (x => 1, y => 2);
     my %b = (z => 3, %a);
     print join(",", map { "$_=$b{$_}" } sort keys %b), "\n";',
    "x=1,y=2,z=3\n");

# Regression: a bare-scalar RHS is a one-element list — `%h = "x"` means
# `%h = ("x")`, giving key "x" with an undef value (Perl pads the odd element).
# Previously p-hash-= dropped a non-vector RHS and produced an empty hash.
test_cl('scalar RHS becomes a single key with undef value',
    'my %h; %h = "x";
     print join(":", %h), "|", scalar(keys %h), "\n";',
    "x:|1\n");

# The hash.t "self-assign" case: %h = $h{a} reads the scalar element (here "x")
# and assigns it as the sole key, value undef.
test_cl('hash self-assign from own scalar element',
    'my %h = qw(a x b y c z);
     no warnings;
     %h = $h{a};
     print join(":", %h), "\n";',
    "x:\n");
