#!/usr/bin/env perl
# arylen-ref-01.t — \$#array is a live write-through reference to the array-length
# (arylen) magic, implemented via a p-magic-cell intercepted at the box chokepoints
# (unbox / box-set / box-sv / box-nv), the same mechanism `tie` uses.
#
# Session 218: previously \$#a compiled to (p-backslash (p-array-last-index @a)),
# which backslashes a COPY of the integer — so $$ref = N did not resize @a. Now it
# compiles to (p-arylen-ref @a): reading the ref yields the last index, writing it
# resizes the array. Fixes array.t 92,95,98,101,103,105.

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

sub transpile {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;
    return scalar `$pl2cl $pl_file 2>/dev/null`;
}

# codegen: \$#a uses the live p-arylen-ref, not a backslashed copy
like(transpile('my @a=(1..4); my $r=\$#a;'),
    qr/\(p-arylen-ref \@a\)/, '\\$#array compiles to (p-arylen-ref @a)');

# reading the ref yields the current last index
test_cl('read \$#a through ref', q{my @a=(1..4); my $r=\$#a; print "$$r\n";}, "3\n");

# writing the ref GROWS the array (write-through)
test_cl('write through ref grows array',
    q{my @a=(1..4); my $r=\$#a; $$r=6; print scalar(@a),"\n";}, "7\n");

# writing the ref SHRINKS the array
test_cl('write through ref shrinks array',
    q{my @a=(1..4); my $r=\$#a; $$r=1; print "@a\n";}, "1 2\n");

# numeric context fires the getter (not the raw struct)
test_cl('numeric context through ref',
    q{my @a=(1..4); my $r=\$#a; my $n=$$r+10; print "$n\n";}, "13\n");

# copying the deref ($c = $$r) copies the VALUE, not an alias to the magic cell
test_cl('copy of deref is a value snapshot',
    q{my @a=(1..4); my $r=\$#a; my $c=$$r; $$r=6; print "$c $$r\n";}, "3 6\n");
