#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# list-slice-01.t - List slice (list)[indices] and (list)[range]

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

# ── #892: `\` DISTRIBUTES over a slice, because a slice IS a list ────────────
# `\@A[0,1]` is `(\$A[0], \$A[1])` in perl — a list of one SCALAR ref per
# element, each aliasing the container's slot.  PCL answered ONE ref, to the
# slice's LAST element: wrong in the count AND in the target, silently, so
# `my ($p,$q) = \@A[0,1]` left $q undef and $p pointing at $A[1].  The gate
# asked "was the operand written with parens" (the \(LIST) metadata) where
# perl asks "does the operand produce a LIST".  Every expectation below is
# real perl's output (probed, perl 5.40.3).

test_cl("#892 backslash distributes over every slice spelling",
    q{my %H=(a=>1,b=>2); my @A=(10,20,30); my $h=\%H; my $a=\@A;
      sub sh { my ($t,@r)=@_; print "$t:", scalar(@r), ":",
                 join(",", map { ref($_) } @r), ":",
                 join(",", map { $$_ } @r), "\n" }
      sh("named-a",  \@A[0,1]);
      sh("named-h",  \@H{qw(a b)});
      sh("deref-a",  \@$a[0,1]);
      sh("deref-h",  \@$h{qw(a b)});
      sh("block-a",  \@{$a}[0,1]);
      sh("block-h",  \@{$h}{qw(a b)});
      sh("postfix",  \$a->@[0,1]);
      sh("range",    \@A[0..2]);
      sh("one-elem", \@A[0]);
      sh("kv-h",     \%H{qw(a b)});
      sh("kv-a",     \%A[0,1]);
      sh("paren",    \(@A[0,1]));
      sh("m-slice",  \($A[2], @A[0,1]));},
    "named-a:2:SCALAR,SCALAR:10,20\n"
  . "named-h:2:SCALAR,SCALAR:1,2\n"
  . "deref-a:2:SCALAR,SCALAR:10,20\n"
  . "deref-h:2:SCALAR,SCALAR:1,2\n"
  . "block-a:2:SCALAR,SCALAR:10,20\n"
  . "block-h:2:SCALAR,SCALAR:1,2\n"
  . "postfix:2:SCALAR,SCALAR:10,20\n"
  . "range:3:SCALAR,SCALAR,SCALAR:10,20,30\n"
  . "one-elem:1:SCALAR:10\n"
  . "kv-h:4:SCALAR,SCALAR,SCALAR,SCALAR:a,1,b,2\n"
  . "kv-a:4:SCALAR,SCALAR,SCALAR,SCALAR:0,10,1,20\n"
  . "paren:2:SCALAR,SCALAR:10,20\n"
  . "m-slice:3:SCALAR,SCALAR,SCALAR:30,10,20\n");

test_cl("#892 a distributed ref ALIASES the container slot",
    q{my @A=(10,20); my %H=(a=>1,b=>2);
      my @s = \@A[0,1];      ${$s[0]} = 99;
      my @t = \@H{qw(a b)};  ${$t[1]} = 88;
      print "$A[0],$A[1],$H{a},$H{b}\n";},
    "99,20,1,88\n");

# INVERSE — the shapes that must NOT distribute.  perlref's spread is the
# SPECIAL case for one parenthesized aggregate; an array or hash VARIABLE is
# one container ref, and in a MULTI-term \( ) list it stays one (probed:
# `\(@A,$x)` is (ARRAY,SCALAR), `\(@A,@B)` is (ARRAY,ARRAY)).  A slice in
# explicit SCALAR context is the comma operator's last element, as before.
# Every row here PASSES ON THE BASE COMPILER TOO — that is the point: it is
# what proves the widened predicate did not swallow the aggregate spellings.
test_cl("#892 inverse: aggregates, multi-term lists and scalar context",
    q{my %H=(a=>1); my @A=(10,20); my @B=(30,40); my $a=\@A;
      sub sh { my ($t,@r)=@_; print "$t:", scalar(@r), ":",
                 join(",", map { ref($_) } @r), "\n" }
      sh("whole-a", \@A); sh("whole-h", \%H); sh("deref-whole", \@$a);
      sh("elem",    \$A[0]); sh("anon", \@{[1,2]});
      sh("m-arr",   \(@A, $B[0])); sh("m-arr2", \(@A, @B));
      sh("m-hash",  \(%H, $A[0]));
      my $s1 = \@A[0,1]; my $s2 = \@A[0];
      print "scalar:", $$s1, ",", $$s2, "\n";},
    "whole-a:1:ARRAY\nwhole-h:1:HASH\nderef-whole:1:ARRAY\n"
  . "elem:1:SCALAR\nanon:1:ARRAY\n"
  . "m-arr:2:ARRAY,SCALAR\nm-arr2:2:ARRAY,ARRAY\nm-hash:2:HASH,SCALAR\n"
  . "scalar:20,10\n");
