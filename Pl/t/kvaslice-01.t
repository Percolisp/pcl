#!/usr/bin/env perl
# kvaslice-01.t - Tests for %arr[indices] KV array slice (kvaslice.t failures)

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

plan tests => 13;

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

# Basic %arr[indices] — correct key-value order
test_cl('%arr[0,1,2] join',
    'my @a = "a".."z";
     print join(":", %a[0,1,2]), "\n";',
    "0:a:1:b:2:c\n");

test_cl('%arr[2,1,0] reversed join',
    'my @a = "a".."z";
     print join(":", %a[2,1,0]), "\n";',
    "2:c:1:b:0:a\n");

# Convert to hash
test_cl('%arr[5,6] as hash',
    'my @a = "a".."z";
     my %h = %a[5,6];
     print $h{5}, ":", $h{6}, "\n";',
    "f:g\n");

# Empty slice
test_cl('empty slice',
    'my @a = "a".."z";
     my @r = %a[()];
     print scalar(@r), "\n";',
    "0\n");

# Non-existing element returns undef
test_cl('out-of-bounds element is undef',
    'my @a = ("a".."d");
     my %h = %a[3, 4];
     print defined($h{3}) ? "defined" : "undef", ":",
           defined($h{4}) ? "defined" : "undef", "\n";',
    "defined:undef\n");

# No autovivification from reading
test_cl('no autovivification',
    'my @a = ("a".."d");
     my @t = %a[1,2];
     print exists($a[5]) ? "exists" : "absent", "\n";',
    "absent\n");

# Repeated keys
test_cl('repeated keys',
    'my @a = "a".."d";
     my @r = %a[(1) x 3];
     print join(":", @r), "\n";',
    "1:b:1:b:1:b\n");

# %$ref[indices] — scalar ref to array
test_cl('%$ref[indices] ref form',
    'my $a = ["a".."z"];
     print join(":", %$a[2,3,4]), "\n";',
    "2:c:3:d:4:e\n");

# %{$ref}[indices] — block-deref form
test_cl('%{$ref}[indices] block-deref form',
    'my $a = ["a".."z"];
     print join(":", %{$a}[2,3,4]), "\n";',
    "2:c:3:d:4:e\n");

# No interpolation inside strings
test_cl('no interpolation in strings',
    'my @a = "a".."z";
     print "%a[1,2]\n";',
    "%a[1,2]\n");

# Ref of slice: \%arr[...] produces list of refs
test_cl('ref of slice: all elements are refs',
    'my @a = "a".."z";
     my @tmp = \%a[2,3,4];
     my $all_refs = 1;
     $all_refs = 0 if grep { !ref $_ } @tmp;
     print $all_refs ? "ok" : "fail", "\n";',
    "ok\n");

# Regression: exists $hashref->{key} still works after named-unary fix
test_cl('exists $r->{key} still correct',
    'my %h = (a => 1, b => 2);
     my $r = \%h;
     print exists($r->{a}) ? "yes" : "no", "\n";
     print exists($r->{z}) ? "yes" : "no", "\n";',
    "yes\nno\n");

# Regression: exists $r->{key} chained
test_cl('exists chained hash-ref access',
    'my %h = (x => {y => 1});
     my $r = \%h;
     print exists($r->{x}{y}) ? "yes" : "no", "\n";
     print exists($r->{x}{z}) ? "yes" : "no", "\n";',
    "yes\nno\n");
