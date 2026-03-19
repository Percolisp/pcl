#!/usr/bin/env perl
# split-01.t - Tests for split() failures from perl-tests/split.t

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

plan tests => 15;

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

# ── split.t test 32: split empty string ────────────────────────────────────
# $_ = join('|', split(/x/, '', -1), 'Z'); is($_, "Z")

test_cl("split empty string with -1: join gives Z",
    'my $_ = join("|", split(/x/, "", -1), "Z"); print $_, "\n";',
    "Z\n");

test_cl("split empty string: scalar context count is 0",
    'my $cnt = split(/x/, ""); print $cnt, "\n";',
    "0\n");

# ── split.t tests 58-59: expression result as pattern ──────────────────────
# is('p:q:r:s', join ':', split('abc' =~ /b/, 'p1q1r1s'));
# 'abc' =~ /b/ returns 1 (truthy), split(1, 'p1q1r1s') splits on '1'

test_cl("split: regex match result as pattern (returns 1, splits on '1')",
    'my @r = split("abc" =~ /b/, "p1q1r1s");
     print join(":", @r), "\n";',
    "p:q:r:s\n");

test_cl("split: regex match result count",
    'my $cnt = split("abc" =~ /b/, "p1q1r1s");
     print $cnt, "\n";',
    "4\n");

# ── hash ref slice: @$_{qw(...)} ─────────────────────────────────────────-
# This underlies split.t tests 185-188

test_cl("hashref slice: @\$_{qw(a b c)}",
    'my $h = {a => "x", b => "y", c => "z"};
     my @v = @{$h}{qw(a b c)};
     print join(",", @v), "\n";',
    "x,y,z\n");

test_cl("hashref slice with empty string value",
    'my $h = {sep => "0", eff => "", txt => "ab"};
     my ($s, $e, $t) = @{$h}{qw(sep eff txt)};
     print "s=$s e=" . (defined $e ? qq("$e") : "undef") . " t=$t\n";',
    "s=0 e=\"\" t=ab\n");

test_cl("hashref slice via \$_",
    'my @recs = ({a=>"1",b=>"2"}, {a=>"3",b=>"4"});
     my @vals;
     for (@recs) { my ($x,$y) = @$_{qw(a b)}; push @vals, "$x/$y"; }
     print join(",", @vals), "\n";',
    "1/2,3/4\n");

# ── split.t tests 149-151: $n = @a = split(...) ───────────────────────────
# In scalar context, $n should get the count (not the array)

test_cl("split scalar context: \$n = \@a = split gives count",
    'our @a; my $n = @a = split(/,/, "a,b,c");
     print "$n\n";',
    "3\n");

test_cl("split scalar context: empty string gives 0",
    'our @a; my $p = ""; my $n = @a = split(/,/, $p);
     print "$n\n";',
    "0\n");

# ── split.t tests 161/165/169/173: array in scalar context ────────────────
# After split assigns to @a, using @a in scalar context should give count

test_cl("split to lexical array: scalar context outer",
    q{my @arr = split(/,/, "1,2,3,4,5");
     my $s = @arr;
     print "$s\n";},
    "5\n");

test_cl("split to package array: scalar context",
    q{our @arr = split(/,/, "a,b,c");
     my $s = @arr;
     print "$s\n";},
    "3\n");

# ── split.t tests 185-188: /o modifier + empty separator ──────────────────

test_cl("split: empty separator after s/0//o",
    'my $sep = "0"; $sep =~ s/0//;
     my @r = split($sep, "ab");
     print join(",", @r), "\n";',
    "a,b\n");

test_cl("split: non-empty separator unchanged",
    'my $sep = ";"; $sep =~ s/0//;
     my @r = split($sep, "a;b");
     print join(",", @r), "\n";',
    "a,b\n");

# ── split.t tests 156-157: local $_ and split ─────────────────────────────

test_cl("split: local \$_ with split default",
    'our $_ = "original";
     {
         local $_ = "a b c";
         my @r = split;
         print join(",", @r), "\n";
     }
     print "$_\n";',
    "a,b,c\noriginal\n");

# ── split.t: splitting on numeric string '1' ──────────────────────────────

test_cl("split on string '1': p1q1r1s -> (p,q,r,s)",
    'my @r = split("1", "p1q1r1s");
     print join(":", @r), "\n";',
    "p:q:r:s\n");
