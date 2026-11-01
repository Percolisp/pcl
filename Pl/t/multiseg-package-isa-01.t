#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# multiseg-package-isa-01.t -- an inherited AUTOLOAD and an inherited
# overload for a class whose name has two or more segments that are ALL CAPS
# (URI::URL, URI::IRI) or all lowercase (task #3821).
#
# The AUTOLOAD walk, the overload walks' @ISA reader and the $AUTOLOAD setter
# resolved the package with (find-package (%pcl-invert-case NAME)), which
# flips an all-caps or all-lowercase multi-segment name to a package that
# does not exist; ordinary method dispatch used the one resolver
# %pcl-find-package and worked.  Perl ($^X) is the oracle at test time; the
# row FAILS on ade297e7 (Can't locate object method "zap" via package
# "UU::RL").

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
my $src = "$workdir/isa.pl";
open(my $s, '>', $src) or die;
print {$s} <<'PL';
package Wb; use overload '""' => sub { "W:" . ref $_[0] }, '==' => sub { 1 }, fallback => 1;
sub new { bless [], shift }
our $AUTOLOAD; sub AUTOLOAD { my $m = $AUTOLOAD; return if $m =~ /DESTROY/; "auto($m)" }
package UU::RL; our @ISA = ('Wb');
package aa::bb; our @ISA = ('Wb');
package CC::DD; our @ISA = ('UU::RL');
package EE::FF; our $AUTOLOAD; sub new { bless {}, shift } sub AUTOLOAD { my $m = $AUTOLOAD; return if $m =~ /DESTROY/; "own($m)" }
package main;
for my $c (qw(UU::RL aa::bb CC::DD)) { my $o = $c->new; print "$c [$o] ", $o->zap, " ", ($o == 3 ? "eq" : "ne"), "\n" }
my $e = EE::FF->new; print "EE::FF ", $e->zip, "\n";
PL
close $s;
my $perl = `$^X $src 2>&1`;
my $cl = `$pl2cl $src 2>/dev/null`;
open(my $c, '>', "$workdir/isa.lisp") or die; print {$c} $cl; close $c;
my $pcl = `sbcl @sbcl_rt --load $workdir/isa.lisp 2>&1`;
$pcl =~ s/^;.*\n//gm; $pcl =~ s/^PCL Runtime loaded\n//gm; $pcl =~ s/^\s*\n//gm;
is($pcl, $perl, 'inherited AUTOLOAD, overloads and $AUTOLOAD for ALL-CAPS and all-lowercase multi-segment classes');

done_testing;
