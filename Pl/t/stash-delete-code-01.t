#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# stash-delete-code-01.t -- `delete $Pkg::{name}` of a SUB vs the code that
# names it (tasks #3831, #3696).
#
# perl's compiled call holds the GLOB, so a call compiled before the delete
# still reaches the sub (YAML::Tiny deletes its imported refaddr at the end of
# the file and calls it later), while every RUN-TIME lookup by name misses it:
# method resolution, can, a symbolic call, symbolic defined / exists, the
# stash's keys.  A literal `defined &Pkg::name` binds its glob when it is
# COMPILED: before the delete it still answers true, after it (namespace::clean
# deletes in a BEGIN, the main line checks later) false.  A sub installed again
# is found again.  Perl ($^X) is the oracle at test time; every row FAILS on
# ade297e7 (the delete unbound the function: the compiled call died
# "Undefined subroutine", the after-delete defined answered true).

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

my ($perl, $pcl) = both('compiled', <<'PL');
no strict 'refs';
package Base; sub new { bless {}, shift } sub hello { "base-hello" } sub only_base { "ob" }
package Kid; our @ISA = ('Base'); sub hello { "kid-hello" } sub tool { "tool" } sub uses_tool { tool() }
sub chk { defined &Kid::tool ? "pinned-def" : "pinned-undef" }
package P; sub use_it { return helper(5) }
BEGIN { *helper = sub { "h$_[0]" } }
package main;
my $k = Kid->new;
print "1 ", $k->hello, "\n";
delete $Kid::{hello};
print "2 ", $k->hello, " can=", (Kid->can('hello') == \&Base::hello ? "base" : "other"), "\n";
delete $Kid::{tool};
delete $P::{helper};
print "3 compiled-call=", Kid::uses_tool(), " ", P::use_it(), "\n";
print "4 symbolic-defined=", (defined &{"Kid::tool"} ? 1 : 0), " exists=", (exists &{"Kid::tool"} ? 1 : 0), "\n";
print "5 can-tool=", (Kid->can('tool') ? "yes" : "no"), "\n";
print "6 method-call: ", (eval { $k->tool; 1 } ? "called" : "died"), "\n";
print "7 symbolic-call: ", (eval { &{"Kid::tool"}(); 1 } ? "called" : "died"), "\n";
print "8 compiled-before defined: ", Kid::chk(), "\n";
print "9 eval-after defined: ", (eval "defined &Kid::tool" ? "def" : "undef"), "\n";
eval "sub Kid::tool { 'tool2' }";
print "10 reinstalled: method=", $k->tool, " can=", (Kid->can('tool') ? "yes" : "no"), " sym=", &{"Kid::tool"}(), "\n";
delete $Kid::{only_base};
print "11 inherited still: ", $k->only_base, "\n";
PL
is($pcl, $perl, 'a stash delete of a sub: compiled calls keep it, run-time lookups miss it, a reinstall is found');

open(my $pm, '>', "$workdir/NsC.pm") or die;
print {$pm} <<'PM';
package NsC;
use Scalar::Util qw(blessed);
sub isobj { blessed($_[0]) ? 1 : 0 }
sub hasb { defined &NsC::blessed ? 1 : 0 }
BEGIN { delete $NsC::{blessed} }
sub hasb2 { defined &NsC::blessed ? 1 : 0 }
sub exb2 { exists &NsC::blessed ? 1 : 0 }
1;
PM
close $pm;

($perl, $pcl) = both('nsclean', <<'PL');
use lib '.';
use NsC;
print "module: isobj=", NsC::isobj(bless {}), " before=", NsC::hasb(), " after=", NsC::hasb2(), " exists-after=", NsC::exb2(),
  " can=", (NsC->can('blessed') ? 1 : 0), " main-line=", (defined &NsC::blessed ? 1 : 0), "\n";
package Loc;
use Scalar::Util qw(reftype);
sub rt { reftype($_[0]) }
BEGIN { delete $Loc::{reftype} }
package main;
print "script: compiled-call=", Loc::rt([]), " defined-after=", (defined &Loc::reftype ? 1 : 0), " can=", (Loc->can('reftype') ? 1 : 0), "\n";
PL
is($pcl, $perl, 'namespace::clean shape: a BEGIN-time delete; a literal defined &NAME binds its glob when compiled');

done_testing;
