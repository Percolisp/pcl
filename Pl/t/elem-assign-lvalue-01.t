#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# elem-assign-lvalue-01.t -- an assignment TO AN ELEMENT is an lvalue: its
# value is the element itself (task #4303, s532c; the reduction of #3835).
#
# perl's value of an assignment is its LEFT side as an lvalue, so
# `($block->{$1} = $2) =~ s/^ *(.*?) *$/$1/` (YAML::Tiny's TestML::Tiny)
# trims the ELEMENT and `($h{a} = "x") .= "y"` appends to it.  PCL lowered an
# element assignment to a setf whose value is the stored VALUE: the s/// then
# wrote into the right-hand side (the read-only $2 -- "Modification of a
# read-only value attempted", which stopped YAML-Tiny 30_yaml_spec_tml.t and
# 31_local_tml.t) and `.=` / `+=` were lost.  The scalar spelling and
# `chomp($h{k} = ...)` were already right (rows 11-12 are those controls).
# Perl ($^X) is the oracle at test time; the row FAILS on ce665621.

use v5.30;
use strict;
use warnings;
use Test::More;
use File::Temp qw(tempdir);
use FindBin qw($RealBin);
use lib $RealBin;
use PCLCore;

my $project_root = "$RealBin/../..";
my $pl2cl        = "$project_root/pl2cl";
my $runtime      = "$project_root/cl/pcl-runtime.lisp";
my @sbcl_rt      = PCLCore::sbcl_prefix($runtime);

plan skip_all => "pl2cl not found" unless -x $pl2cl;
plan skip_all => "sbcl not found"  unless `which sbcl 2>/dev/null`;

my $workdir = tempdir(CLEANUP => 1);

sub both {
    my ($tag, $body) = @_;
    my $src = "$workdir/$tag.pl";
    open(my $s, '>', $src) or die; print {$s} $body; close $s;
    my $perl = `cd $workdir && $^X $src 2>&1`;
    my $cl_code = `cd $workdir && $pl2cl $src 2>/dev/null`;
    my $cl_file = "$workdir/$tag.lisp";
    open(my $c, '>', $cl_file) or die; print {$c} $cl_code; close $c;
    my $pcl = `cd $workdir && sbcl @sbcl_rt --load $cl_file 2>&1`;
    $pcl =~ s/^;.*\n//gm;
    $pcl =~ s/^PCL Runtime loaded\n//gm;
    $pcl =~ s/^\s*\n//gm;
    return ($perl, $pcl);
}

my ($perl, $pcl) = both('elem', <<'PL');
my %h; my @a; my $r = {}; my $ar = [];
($h{a} = "x") .= "y";             print "1 $h{a}\n";
($a[0] = 1) += 5;                 print "2 $a[0]\n";
($h{b} = "foo") =~ s/o/0/g;       print "3 $h{b}\n";
($a[1] = "abc") =~ tr/a-c/A-C/;   print "4 $a[1]\n";
($r->{c} = "p q") =~ s/ /_/;      print "5 $r->{c}\n";
($ar->[0] = "zz") =~ s/z/Z/;      print "6 $ar->[0]\n";
my $v = "src";
($h{e} = $v) =~ s/s/S/;           print "7 $h{e} $v\n";
my $i = 0;
($a[$i++] = "m") =~ s/m/M/;       print "8 $a[0] i=$i\n";
my $line = "--- warning: E_DUPKEY ";
my $block = {};
if ($line =~ /^--- +(\w+): +(.*?) *$/) {
    ($block->{$1} = $2) =~ s/^ *(.*?) *$/$1/;
}
print "9 ", join(",", map { "$_=$block->{$_}" } sort keys %$block), "\n";
($h{f} .= "q") =~ s/q/Q/;         print "10 $h{f}\n";
"ab" =~ /(b)/;
(my $m = $1) =~ s/b/B/;           print "11 $m\n";
chomp($h{d} = "line\n");          print "12 [$h{d}]\n";
PL
is($pcl, $perl, 'an element assignment used as an lvalue (=~ s///, tr///, .=, +=) edits the element');

done_testing();
