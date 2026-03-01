#!/usr/bin/env perl
# Tests for each/keys/values on arrays (extracted from perl-tests/each_array.t).

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

sub run_cl {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;

    my $cl_code = `$pl2cl $pl_file 2>&1`;

    my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cl_fh $cl_code;
    close $cl_fh;

    my $output = `sbcl --noinform --non-interactive --load $runtime --load $cl_file 2>&1`;
    $output =~ s/^;.*\n//gm;
    $output =~ s/^caught .*\n//gm;
    $output =~ s/^compilation unit.*\n//gm;
    $output =~ s/^\s*Undefined.*\n//gm;
    $output =~ s/^PCL Runtime loaded\n//gm;
    $output =~ s/^# PCL Test library loaded\n//gm;
    $output =~ s/^WARNING:.*\n//gm;
    $output =~ s/^\s*\n//gm;
    return $output;
}

sub test_io {
    my ($name, $code) = @_;
    my ($fh, $file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;
    my $perl_out = `perl $file 2>&1`;
    my $cl_out   = run_cl($code);
    is($cl_out, $perl_out, $name) or diag("Perl: $perl_out\nCL:   $cl_out");
}

plan tests => 12;

# --- Basic each @array: returns (index, value) pairs ---
test_io('each @array: first element', <<'PERL');
my @array = qw(crunch zam bloop);
my ($k, $v) = each @array;
print "$k $v\n";
PERL

test_io('each @array: successive calls advance iterator', <<'PERL');
my @array = qw(crunch zam bloop);
my ($k, $v) = each @array;
($k, $v) = each @array;
print "$k $v\n";
PERL

test_io('each @array: exhaustion returns empty', <<'PERL');
my @array = qw(crunch zam bloop);
each @array for 1..3;
my @r = each @array;
print scalar(@r), "\n";
PERL

test_io('each @array: resets after exhaustion', <<'PERL');
my @array = qw(a b c);
each @array for 1..3;
each @array;            # consume the empty-return (exhaustion signal)
my ($k, $v) = each @array;  # fresh restart
print "$k $v\n";
PERL

# --- each on array ref ---
test_io('each @$ar: works on dereferenced array ref', <<'PERL');
my $ar = ['bacon', 'eggs'];
my ($k, $v) = each @$ar;
print "$k $v\n";
($k, $v) = each @$ar;
print "$k $v\n";
PERL

# --- keys @array returns indices ---
test_io('keys @array: returns 0..n-1', <<'PERL');
my @array = qw(crunch zam bloop);
my @keys = keys @array;
print "@keys\n";
PERL

test_io('keys @array: resets each iterator', <<'PERL');
my @array = qw(a b c);
my ($k, $v) = each @array;  # advance to 1
keys @array;                # reset
($k, $v) = each @array;     # should be back at 0
print "$k $v\n";
PERL

# --- values @array returns elements ---
test_io('values @array: returns array contents', <<'PERL');
my @array = qw(crunch zam bloop);
my @vals = values @array;
print "@vals\n";
PERL

test_io('values @array: resets each iterator', <<'PERL');
my @array = qw(a b c);
my ($k, $v) = each @array;
values @array;
($k, $v) = each @array;
print "$k $v\n";
PERL

# --- scalar-context each returns just index ---
test_io('each @array in scalar context returns index', <<'PERL');
my @array = qw(x y z);
my $k = each @array;
print "$k\n";
$k = each @array;
print "$k\n";
PERL

# --- while with defined-each to safely iterate all indices ---
test_io('each @array: defined-while iterates all elements', <<'PERL');
my @array = qw(alpha beta gamma);
my $k;
my @out;
while (defined($k = each @array)) {
    push @out, $k;
}
print join(',', @out), "\n";
PERL

# --- C-style for with empty init ---
test_io('for (;cond;) with each: empty init does not crash', <<'PERL');
my @array = qw(p q r);
my ($k, $v);
my @out;
for (; ($k, $v) = each @array ;) {
    push @out, "$k=$v";
}
print join(',', @out), "\n";
PERL
