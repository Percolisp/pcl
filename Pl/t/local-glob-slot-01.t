#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# local-glob-slot-01.t — task #2080: `local *name = REF` localizes ONLY the
# slot REF names.
#
# perl replaces one slot of the glob and leaves the others alone.  PCL cleared
# all four, which is right for bare `local *name` and for `local *name =
# *other` and wrong for every reference form — and the slot it wrongly cleared
# most often was `@_`:
#
#     sub f { my $w = shift; local *_ = \my $a; ... foreach (@_) ... }
#
# is the opening of core File::Find's `_find_opt`, so `find(sub {...}, $dir)`
# called `wanted` ZERO times and said nothing.  `File::Find` is in most
# distributions' test suites, so this was a wide silent wrong.
#
# THE ONE READING is `%p-glob-rhs-slot-kind` — the classification
# `%p-glob-assign-slots` already made to decide WHERE to store, lifted out so
# `local` can ask it BEFORE the assignment.  All three localizers
# (p-local-glob, p-local-glob-if, p-local-glob-dynamic) consult it.
# p-local-glob gained the RHS in the same argument slot its two siblings
# already had it in, so the codegen's wrapping `let` + separate
# `p-glob-assign` first body form is gone: the macro is what knows that the
# RHS must be evaluated before the slots are touched.
#
# A second bug fell out of the same read (rule 12): `%p-glob-clear` unbound the
# CODE slot's function but left its `*p-declared-subs*` entry, so inside a bare
# `local *X` the guard `defined &X` was TRUE with nothing to call.  Save and
# restore had always carried that entry; only the clear half was missing.
#
# Every expectation is the live `perl` answer.  Inverse-verified on a 5ce155cb
# extraction: rows 1 and 3-5 fail there (row 2's file dies outright).

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

plan tests => 5;

sub write_pl {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;
    return $pl_file;
}

sub run_cl {
    my ($code) = @_;
    my $cl_code = PCLCore::transpile("$pl2cl " . write_pl($code));
    my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cl_fh $cl_code;
    close $cl_fh;
    my $output = `sbcl @sbcl_rt --load $cl_file 2>&1`;
    $output =~ s/^;.*\n//gm;
    $output =~ s/^PCL Runtime loaded\n//gm;
    $output =~ s/^\s*\n//gm;
    return $output;
}

sub both_agree {
    my ($code, $desc) = @_;
    my $perl = `perl @{[ write_pl($code) ]} 2>&1`;
    my $pcl  = run_cl($code);
    is($pcl, $perl, $desc);
}

# ONE glob name, so the rows really ask "what happened to the OTHER slots".
my $GLOB = <<'P';
our $X = "s-outer"; our @X = (1, 2, 3); our %X = (k => "v"); sub X { "f-outer" }
sub show { "\$X=$X \@X=@X \%X=" . join(",", map {"$_=$X{$_}"} sort keys %X)
         . " X()=" . X() }
our $Y = "s-other"; our @Y = (9); sub Y { "f-other" }
P

both_agree($GLOB . <<'P', 'each of the five reference forms replaces ONE slot');
print "0 ", show(), "\n";
{ local *X = \my $s; $s = "s-in"; print "1 ", show(), "\n"; }
{ local *X = \my @a; @a = (7, 8); print "2 ", show(), "\n"; }
{ local *X = \my %h; $h{n} = 1;   print "3 ", show(), "\n"; }
{ local *X = sub { "f-mock" };    print "4 ", show(), "\n"; }
{ local *X = \&Y;                 print "5 ", show(), "\n"; }
print "6 ", show(), "\n";
P

both_agree($GLOB . <<'P', 'glob-to-glob and bare local still take the WHOLE glob');
{ local *X = *Y; print "1 \$X=", (defined $X ? $X : "U"), " \@X=@X X()=", X(), "\n"; }
{ local *X;      print "2 \$X=", (defined $X ? $X : "U"), " \@X=", scalar(@X),
                       " X()=", (defined &X ? X() : "gone"), "\n"; }
print "3 ", show(), "\n";
P

both_agree(<<'P', '@_ survives `local *_ = \my $a` — the File::Find opening');
sub argtest { local *_ = \my $t; return scalar(@_) . ":" . ($_[0] // "U") }
print "1 ", argtest("p", "q"), "\n";
sub inner2 { return scalar(@_) }
sub outer2 { local *_ = \my $u; return &inner2 }
print "2 ", outer2(1, 2, 3), "\n";
sub h { local *_; return scalar(@_) }
print "3 ", h(1, 2, 3), "\n";
sub g { local $_ = "topic"; return scalar(@_) }
print "4 ", g(1, 2), "\n";
P

both_agree(<<'P', 'the test-mock idiom leaves the package scalar and array alone');
our $Cfg = "cfg"; our @Cfg = ("a"); sub Cfg { "real" }
{ local *Cfg = sub { "stub" }; print "1 ", Cfg(), " $Cfg @Cfg\n"; }
print "2 ", Cfg(), " $Cfg @Cfg\n";
my $cr = sub { "byref" };
{ local *Cfg = $cr; print "3 ", Cfg(), " $Cfg @Cfg\n"; }
print "4 ", Cfg(), " $Cfg @Cfg\n";
P

# ACCEPTANCE: core File::Find, which opens `_find_opt` with the broken shape.
both_agree(<<'P', 'core File::Find: find() visits every entry');
use File::Find;
my $root = "/tmp/pcl-s492b-find-$$"; mkdir $root; mkdir "$root/d";
for my $f ("$root/1.txt", "$root/d/2.txt") { open(my $o, ">", $f) or die; close $o }
my (@a, @b, @d);
find(sub { push @a, $_ }, $root);
find(sub { push @b, $File::Find::name if -f }, $root);
find({ wanted => sub { push @d, $File::Find::name if -f $File::Find::name },
       no_chdir => 1 }, $root);
print "1 all=", scalar(@a), " [@{[sort @a]}]\n2 files=", scalar(@b),
      "\n3 no_chdir=", scalar(@d), "\n";
unlink "$root/1.txt", "$root/d/2.txt"; rmdir "$root/d"; rmdir $root;
P
