#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# listop-refs-01.t - a REFERENCE in a list operator's argument list is ONE
# element (task #1991).
#
# `map { ref } \@a, \@b` answered two empty strings: the runtime collectors
# unboxed each item before asking "is this a vector to spread?", so the array
# a `\@a` points at was flattened and the block saw its ELEMENTS.  A silent
# wrong -- plausible values, never a crash -- and it reached `map`, `grep`,
# `sort`, `reverse` and `join`, plus every ARRAY-REF VALUE handed to one of
# them (`map { ref } $h->{list}`).
#
# The rule (%p-spread-vector-p): a RAW non-string vector is an array's live
# storage and spreads; a vector inside a p-box is a REFERENCE and is one
# element.  `p-flatten-args` (the @_ builder) always read it that way, which
# is why `f(\@a,\@b)` and `push @l,\@a,\@b` were right all along.
#
# Every expectation below is perl 5.40.3's own output.  The second half is the
# other direction -- the spreading and aliasing the fix must NOT break.

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

plan skip_all => "pl2cl not found" if !-x $pl2cl;
plan skip_all => "sbcl not found"  if !`which sbcl 2>/dev/null`;

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
    my $output = `sbcl @sbcl_rt --load $cl_file 2>&1`;
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

my $DECL = 'my @a = (); my @b = (1,2); my @c = ("x"); my %h = (k => 1);
            my $s = "sc"; my $r = \@b; my $hr = { k => [7,8,9] };' . "\n";

# --- the block-form family: map / grep / sort -------------------------------
test_cl('map/grep/sort: \@array in the argument list is ONE element',
    $DECL . 'print join(",", map { ref } \@b), "\n";
             print join(",", map { ref } \@a, \@b), "\n";
             print join(",", map(ref, \@a, \@b)), "\n";
             print scalar(grep { ref } \@a, \@b), "\n";
             print scalar(my @z = sort { @$a <=> @$b } \@b, \@a, \@c), "\n";
             print join(",", map { scalar @$_ } \@b, \@c), "\n";',
    "ARRAY\nARRAY,ARRAY\nARRAY,ARRAY\n2\n3\n2,1\n");

# --- the same rule for a ref that was never spelled `\` here ----------------
test_cl('an array-ref VALUE is one element too',
    $DECL . 'print join(",", map { ref } $hr->{k}), "\n";
             print join(",", map { ref } $r), "\n";
             print join(",", map { ref } \@$r), "\n";
             print join(",", map { ref($_) || "p:$_" } \@b, @b), "\n";',
    "ARRAY\nARRAY\nARRAY\nARRAY,p:1,p:2\n");

# --- reverse and join are the same family ----------------------------------
test_cl('reverse and join keep a reference whole',
    $DECL . 'print join(",", map { ref($_) } reverse \@a, \@b), "\n";
             print((join(",", \@a) =~ /^ARRAY\(0x/ ? "ARRAY" : "OTHER"), "\n");
             print((join(",", $hr) =~ /^HASH\(0x/ ? "HASH" : "OTHER"), "\n");
             print scalar(reverse("ab", "cd")), "\n";',
    "ARRAY,ARRAY\nARRAY\nHASH\ndcba\n");

# --- the other direction: an @array still SPREADS ---------------------------
test_cl('an array, hash, slice or deref still spreads',
    $DECL . 'print join(",", map { $_ } @b, @c), "\n";
             print join(",", map { $_ } @$r), "\n";
             print join(",", map { $_ } @{$hr->{k}}), "\n";
             print join(",", sort map { $_ } %h), "\n";
             print join(",", map { $_ } @b[0,1]), "\n";
             print join(",", map { $_ } values %h), "\n";
             print join("|", "a", @b, "z"), "\n";
             print scalar(reverse(@b)), "\n";',
    "1,2,x\n1,2\n7,8,9\n1,k\n1,2\n1\na|1|2|z\n21\n");

# --- aliasing: map/grep/sort still write through -----------------------------
test_cl('map/grep/sort keep aliasing the source elements',
    'my @x = (1,2); map { $_ *= 10 } @x; print "@x\n";
     my @y = (1,2); grep { $_ *= 10 } @y; print "@y\n";
     my @z = (3,1,2); $_++ for sort { $a <=> $b } @z; print "@z\n";
     my @w = (1,2); my @o = map { $_ } @w; $o[0] = 9; print "$w[0]\n";',
    "10 20\n10 20\n4 2 3\n1\n");

# `\(@b)` is refs to each ELEMENT, not one ref to the array -- unchanged.
test_cl('\(@array) is still a reference per element',
    $DECL . 'print join(",", map { ref } \(@b)), "\n";
             my @l; push @l, \@a, \@b; print join(",", map { ref } @l), "\n";
             sub cnt { scalar(@_) } print cnt(\@a, \@b), "\n";',
    "SCALAR,SCALAR\nARRAY,ARRAY\n2\n");
