#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# slice-args-01.t — the ONE flattener every slice function shares
# (%p-flatten-slice-args, task #985, s464ax).
#
# It answers a SIMPLE-VECTOR and a COUNT, and in the single-vector case — which
# is every slice the corpus emits — that vector is the ARGUMENT'S OWN backing
# store, so nothing is allocated.  Two things follow, and both are guarded here
# because a wrong answer is silent:
#
#   * the store is the array's CAPACITY, not its length.  `my @k = (0..9);
#     pop @k; pop @k;` leaves an 8-element array in a 10-slot store, so a
#     reader that used (LENGTH store) would slice TEN entries.  Rows
#     `capacity`, `cap-kv`, `cap-delete`, `lvalue-array` are that case in the
#     four shapes that reach it; measured inverse-guard (the count replaced by
#     the capacity): ten of eleven probe rows change.
#   * the store can be a LIVE container (`delete @a[@a]`).  Reading it is safe,
#     but the two array DELETES write the array they are indexing by, and perl
#     evaluates the index list onto the stack first — so they snapshot.  Rows
#     `delete-self` / `read-self`.
#
# Every expectation below is what perl 5.40.3 prints for the same program.

use v5.30;
use strict;
use warnings;
use Test::More;
use File::Temp qw(tempfile);
use FindBin qw($RealBin);
use lib $RealBin;
use PCLCore;

my $project_root = "$RealBin/../..";
my $pl2cl        = "$project_root/pl2cl";
my $runtime      = "$project_root/cl/pcl-runtime.lisp";
my @sbcl_rt = PCLCore::sbcl_prefix($runtime);

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
    my $output = `sbcl @sbcl_rt --load $cl_file 2>&1`;
    $output =~ s/^;.*\n//gm;
    $output =~ s/^PCL Runtime loaded\n//gm;
    $output =~ s/^\s*\n//gm;
    return $output;
}

sub test_cl {
    my ($name, $code, $expected) = @_;
    is(run_cl($code), $expected, $name);
}

# ----------------------------------------------------------------------
# The single-vector fast path: ONE non-string vector argument.

test_cl('array slice through a range',
    'my @a = (10,20,30,40,50,60); print join(":", @a[1..4]), "\n";',
    "20:30:40:50\n");

test_cl('hash slice through an array of keys',
    'my %h = map { $_ => $_*2 } 1..10; my @k = (2,4,6);
     print join(":", @h{@k}), "\n";',
    "4:8:12\n");

# The general path: several arguments, or one that is not a vector.

test_cl('array slice, explicit index list',
    'my @a = (10,20,30,40,50,60); print join(":", @a[0,2,4]), "\n";',
    "10:30:50\n");

test_cl('hash slice, literal keys around an array',
    'my %h = (a=>1,b=>2,c=>3,z=>9); my @k = ("b","c");
     print join(":", @h{"a", @k, "z"}), "\n";',
    "1:2:3:9\n");

# A STRING is a vector in CL and must stay ONE key, never its characters
# (p-aslice used to explode "12" into #\1 #\2).
test_cl('a string key is one key, not its characters',
    'my %h = (12 => "twelve", 1 => "one");
     print join(":", @h{"12","1"}), "\n";',
    "twelve:one\n");

test_cl('empty slice yields no elements',
    'my @a = (1,2,3); my @e; my @v = @a[@e]; print scalar(@v), "\n";',
    "0\n");

# ----------------------------------------------------------------------
# CAPACITY is not COUNT.  @k below holds 8 entries in a 10-slot store.

test_cl('array slice reads the index array LENGTH, not its capacity',
    'my @a = (10,20,30,40,50,60,70,80,90,100); my @k = (0..9); pop @k; pop @k;
     print scalar(@k), ":", join(",", @a[@k]), "\n";',
    "8:10,20,30,40,50,60,70,80\n");

test_cl('KV array slice reads the index array LENGTH',
    'my @a = (10,20,30,40); my @k = (0..3); pop @k;
     print join(",", %a[@k]), "\n";',
    "0,10,1,20,2,30\n");

test_cl('delete of a hash slice reads the key array LENGTH',
    'my %h = map { $_ => $_ } 0..5; my @k = (0..5); pop @k; pop @k;
     my @r = delete @h{@k};
     print join(",", @r), "|", join(",", sort { $a <=> $b } keys %h), "\n";',
    "0,1,2,3|4,5\n");

test_cl('list-assign to a hash slice reads the key array LENGTH',
    'my %g; my @k = ("m","n","o"); pop @k; @g{@k} = (5,6,7);
     print join(",", map { defined $g{$_} ? $g{$_} : "U" } qw(m n o)),
           "|", scalar(keys %g), "\n";',
    "5,6,U|2\n");

# ----------------------------------------------------------------------
# A range as the ONE argument of a delete is many indices, not one
# (task #394: this used to delete element 0).

test_cl('delete of an array slice through a range',
    'my @s = (1,2,3,4,5); my @r = delete @s[1..2];
     print join(",", @r), "|", join(",", map { defined $_ ? $_ : "U" } @s), "\n";',
    "2,3|1,U,U,4,5\n");

# ----------------------------------------------------------------------
# The index vector IS the container.  perl evaluates the index list onto the
# stack before the first store, so the delete sees a snapshot.

# (2,1,0) would answer the same either way — the index order happens to make
# the live reads agree.  (1,2,0) does not: without the snapshot the second
# index is read out of the slot the first store emptied, so PCL answered
# `2,1,U|3` where perl says `2,0,1|0` (measured, s464ax).
test_cl('delete of an array slice indexed by the array itself',
    'no warnings "uninitialized";
     my @s = (1,2,0); my @r = delete @s[@s];
     print join(",", map { defined $_ ? $_ : "U" } @r), "|", scalar(@s), "\n";',
    "2,0,1|0\n");

test_cl('delete of a KV array slice indexed by the array itself',
    'no warnings "uninitialized";
     my @t = (1,2,0); my @u = delete %t[@t];
     print join(",", map { defined $_ ? $_ : "U" } @u), "|", scalar(@t), "\n";',
    "1,2,2,0,0,1|0\n");

test_cl('array slice indexed by the array itself',
    'my @s = (2,1,0); print join(",", @s[@s]), "|", join(",", @s), "\n";',
    "0,1,2|2,1,0\n");

# A slice is a list of ALIASES (#818) — still true when the index list is an
# array, which is the path this change rewrote.
test_cl('a slice through an index array still writes through',
    'my @w = (1,2,3); my @k = (0,2); for (@w[@k]) { $_ *= 10 }
     print join(",", @w), "\n";',
    "10,2,30\n");
