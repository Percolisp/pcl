#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later
# anon-array-flatten-01.t — AN ANONYMOUS ARRAY CONSTRUCTOR FLATTENS A LIST
# AT EVERY DEPTH (task #4205, s531d).
#
# perl's [ LIST ] flattens LIST completely: a ternary arm that is itself a
# list -- `[ $p ? ((7) x $n) : () ]`, `[ $p ? (@a, 6) : () ]` -- spreads
# into the new array.  PCL emits the ternary's arm as a vector HOLDING the
# inner list's vector, and p-array-init flattened only ONE level, so the
# inner vector became one element: a reference (`[ARRAY(..)]`).
# Template::Plugin::Table::col's `$self->{_PAD} ? ((undef) x $blanks) : ()`
# printed `ARRAY(0x4)` into a rendered table.  The list assignment
# `my @x = (...)` was always right: %p-flatten-list's walk recurses.
#
# Every expected line is perl 5.40.3's output (nothing here is
# version-dependent: no feature newer than 5.8).

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

plan tests => 2;

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

test_cl('a list nested in a ternary arm spreads into [ ... ] (#4205)',
        <<'PL', <<'OUT');
my $p = 1; my $n = 2; my @a = (4, 5); my %h = (k => 1);
sub show { my $r = shift; print scalar(@$r), " [", join(",", map { defined $_ ? (ref $_ ? "REF" : $_) : "u" } @$r), "]\n" }
show([ $p ? ((7) x $n) : () ]);
show([ $p ? ((undef) x $n) : () ]);
show([ $p ? (@a, 6) : () ]);
show([ 0 ? () : ((1) x 3) ]);
show([ ($p ? ((3) x 2) : ()), 9 ]);
show([ $p ? (%h) : () ]);
show([ $p ? ((@a) x 2) : () ]);
show([ $p ? ((7) x $n) : (), $p ? (8) : () ]);
my @e; show([ $p ? (@e, @e) : 1 ]);
my $r = [ $p ? ((7) x $n) : () ]; $r->[0] = 9; print "$r->[0] $r->[1]\n";
PL
2 [7,7]
2 [u,u]
3 [4,5,6]
3 [1,1,1]
3 [3,3,9]
2 [k,1]
4 [4,5,4,5]
3 [7,7,8]
0 []
9 7
OUT

test_cl('references inside a nested list stay ONE element each; controls',
        <<'PL', <<'OUT');
my $p = 1; my @a = (4, 5);
sub show { my $r = shift; print scalar(@$r), " [", join(",", map { defined $_ ? (ref $_ ? "REF" : $_) : "u" } @$r), "]\n" }
show([ (1, (2, (3))) ]);
show([ ((1) x 2), ((2) x 0) ]);
show([ $p ? (1, (2)) : () ]);
show([ $p ? ([1], {}) : () ]);
show([ $p ? (\@a, 3) : () ]);
show([ ((@a) x 2) ]);
show([ map { ($_) x 2 } 1, 2 ]);
show([ $p ? (map { ($_, [$_]) } 1, 2) : () ]);
show([ $p ? (undef, (undef)) : () ]);
my @x = ($p ? ((7) x 2) : ()); print scalar(@x), "\n";
PL
3 [1,2,3]
2 [1,1]
2 [1,2]
2 [REF,REF]
2 [REF,3]
4 [4,5,4,5]
4 [1,1,2,2]
4 [1,REF,2,REF]
2 [u,u]
2
OUT
