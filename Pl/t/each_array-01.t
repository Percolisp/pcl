#!/usr/bin/env perl
# each_array-01.t - each() in scalar context while/for loops and iterator reset

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
    my $got = run_cl($code);
    is($got, $expected, $name);
}

# ── Scalar context each in while — Perl treats as defined() automatically ───

test_cl('while ($k = each @a) iterates all elements including index 0',
    'our (@a, $k, $v);
     @a = qw(crunch zam bloop);
     $v = 0;
     while ($k = each @a) {
         print "k=$k\n";
         $v++;
     }
     print "count=$v\n";',
    "k=0\nk=1\nk=2\ncount=3\n");

test_cl('while ($k = each @a) then each @a restarts at 0',
    'our (@a, $k, $v);
     @a = qw(x y z);
     while ($k = each @a) { }
     ($k, $v) = each @a;
     print "$k-$v\n";',
    "0-x\n");

# ── Scalar context each in C-style for — same defined() wrapping needed ─────

test_cl('for(; $k = each @a ;) iterates all elements including index 0',
    'our (@a, $k, $v);
     @a = qw(crunch zam bloop);
     $v = 0;
     for (; $k = each @a ;) {
         print "k=$k\n";
         $v++;
     }
     print "count=$v\n";',
    "k=0\nk=1\nk=2\ncount=3\n");

test_cl('for(; $k = each @a ;) then for(; ($k,$v) = each @a ;) starts at 0',
    'our (@a, $k, $v, $c);
     @a = qw(crunch zam bloop);
     for (; $k = each @a ;) { }
     $c = 0;
     for (; ($k, $v) = each @a ;) {
         print "$k-$c\n";
         $c++;
     }',
    "0-0\n1-1\n2-2\n");

# ── Array assignment resets each iterator ────────────────────────────────────

test_cl('assigning to @a resets each iterator',
    'my @a = qw(a b c);
     my ($i, $v) = each @a;
     print "$i-$v\n";
     @a = qw(A B C);
     ($i, $v) = each @a;
     print "$i-$v\n";',
    "0-a\n0-A\n");

test_cl('assigning to @a after partial iteration resets to 0',
    'my @a = qw(a b c);
     my ($i, $v) = each @a;
     ($i, $v) = each @a;
     @a = qw(X Y Z);
     ($i, $v) = each @a;
     print "$i-$v\n";',
    "0-X\n");

# ── local @array restores iterator on scope exit ─────────────────────────────

test_cl('local @array restores iterator state on scope exit',
    'our @array;
     @array = qw(a b c);
     my ($i, $v) = each @array;
     {
         local @array = qw(A B C);
         my ($ii, $vv) = each @array;
         ($ii, $vv) = each @array;
     }
     ($i, $v) = each @array;
     print "$i-$v\n";',
    "1-b\n");

# ── each in while loop followed by each in list context starts at 0 ──────────

test_cl('while scalar each then list for-each starts at index 0',
    'our (@array, $k, $v, $c);
     @array = qw(crunch zam bloop);
     while (each @array) { }
     while ($k = each @array) { }
     $c = 0;
     for (; ($k, $v) = each @array ;) {
         print "$k-$c\n";
         $c++;
     }',
    "0-0\n1-1\n2-2\n");
