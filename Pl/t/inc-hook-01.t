#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# inc-hook-01.t -- @INC HOOKS (task #1815, perlfunc require).
#
# An @INC element that is a reference is a hook: a CODE ref called with
# (itself, NAME), an ARRAY ref called through its [0] with the array as the
# first argument, a blessed object through its INC method (an AUTOLOAD
# answers for it) or INCDIR (directories to search instead), a blessed CODE
# ref with neither called as code, a ""-overloaded object used as a
# directory.  The answer is an optional ref to the initial source, an
# optional filehandle, an optional CODE ref (a generator, or a line filter
# over the handle) and its state -- or nothing: go on.  %INC holds the hook
# unless the hook stored its own entry.  `do FILE` and `require "x.pl"`
# walk the same hooks.  Perl ($^X) is the oracle at test time; every row
# FAILS on ade297e7, where a non-string @INC element reached SBCL's
# `namestring` and died with a raw host type error.

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
    my $perl = `$^X $src 2>&1`;
    my $cl_code = `$pl2cl $src 2>/dev/null`;
    my $cl_file = "$workdir/$tag.lisp";
    open(my $c, '>', $cl_file) or die; print {$c} $cl_code; close $c;
    my $pcl = `sbcl @sbcl_rt --load $cl_file 2>&1`;
    $pcl =~ s/^;.*\n//gm;
    $pcl =~ s/^PCL Runtime loaded\n//gm;
    $pcl =~ s/^\s*\n//gm;
    return ($perl, $pcl);
}

my ($perl, $pcl) = both('hookforms', <<'PL');
use strict; use warnings;
our ($sr, $sf);
sub fh_for { my ($src) = @_; open my $fh, "<", \$src or die; $fh }
my @calls;
unshift @INC, sub { push @calls, $_[1]; return };
unshift @INC, sub { return unless $_[1] eq 'HkA.pm'; fh_for("package HkA; sub v { 'A' } 1;\n") };
require HkA;
print "1 v=", HkA::v(), " inc=", ref($INC{'HkA.pm'}), " fell-through=", scalar(grep { $_ eq 'HkA.pm' } @calls), "\n";
unshift @INC, sub { return unless $_[1] eq 'Hidden.pm'; my $c = qq{die "Can't locate Hidden.pm (hidden)\n"}; open my $fh, "<", \$c; return $fh };
eval { require Hidden }; print "2 ", (split /\n/, $@)[0], "\n";
unshift @INC, sub { return unless $_[1] eq 'SetInc.pm'; $INC{$_[1]} = '/custom/SetInc.pm'; fh_for("package SetInc; 1;") };
require SetInc; print "3 inc=$INC{'SetInc.pm'}\n";
my $ar = [ sub { my ($self, $f) = @_; return unless $f eq 'ArH.pm'; print "4 self=", ref($self), " extra=$self->[1]\n"; fh_for("package ArH; 1;") }, 'state1' ];
unshift @INC, $ar;
require ArH; print "4 inc=", ref($INC{'ArH.pm'}), " same=", ($INC{'ArH.pm'} == $ar ? 1 : 0), "\n";
{ package ObjH; sub new { bless {}, shift } }
sub ObjH::INC { my ($self, $f) = @_; return unless $f eq 'ObH.pm'; print "5 invocant=", ref($self), "\n"; main::fh_for("package ObH; 1;") }
unshift @INC, ObjH->new;
require ObH; print "5 inc=", ref($INC{'ObH.pm'}), "\n";
my $wrap = [ ObjH->new ];
{ local @INC = ($wrap, @INC); eval { require ObH2 }; }
unshift @INC, sub { return unless $_[1] eq 'SrH.pm'; \"package SrH; sub v { 'S' } 1;" };
require SrH; print "6 v=", SrH::v(), "\n";
unshift @INC, sub { return unless $_[1] eq 'GenH.pm'; my @l = ("package GenH;\n", "sub v { 'G' }\n", "1;\n");
  sub { $_ = shift @l; defined($_) ? 1 : 0 } };
require GenH; print "7 v=", GenH::v(), "\n";
unshift @INC, sub { return unless $_[1] eq 'FiH.pm'; (fh_for("package FiH;\nsub v { 'XX' }\n1;\n"), sub { s/XX/$_[1]/; return }, 'FILT') };
require FiH; print "8 v=", FiH::v(), "\n";
unshift @INC, sub { return unless $_[1] eq 'FiStop.pm'; (fh_for("package FiStop;\nour \$a1 = 1;\nour \$a2 = 2;\nour \$a3 = 3;\n1;\n"), sub { /a2/ ? 0 : 1 }) };
{ no warnings 'once'; eval { require FiStop }; print "9 a1=", ($FiStop::a1 // 'u'), " a2=", ($FiStop::a2 // 'u'), " a3=", ($FiStop::a3 // 'u'), "\n"; }
eval { require NoSuchModQ }; print "10 ", ($@ =~ /^Can't locate NoSuchModQ.pm in \@INC/ ? "cant-locate" : "other"), "\n";
unshift @INC, sub { return unless $_[1] eq 'dofile.pl'; fh_for("return 'dv';") };
my $dv = do 'dofile.pl'; print "11 dv=$dv inc=", ref($INC{'dofile.pl'}), "\n";
unshift @INC, sub { return unless $_[1] eq 'strreq.pl'; fh_for("\$main::sr = 12; 1;") };
require "strreq.pl"; print "12 sr=$sr\n";
{ package NoMeth; sub new { bless {}, shift } }
{ local @INC = (NoMeth->new, @INC); eval { require NoMethMod }; print "13 ", ($@ =~ /^Can't locate object method "INC"/ ? "no-INC-die" : "other: $@"), "\n"; }
{ local @INC = ([NoMeth->new], @INC); eval { require NoMethArr }; print "14 ", ($@ =~ /^Can't locate object method "INC".* in object in ARRAY hook/ ? "no-INC-array-die" : "other: $@"), "\n"; }
my $bc = bless sub { my ($s, $f) = @_; return unless $f eq 'BcMod.pm'; main::fh_for("package BcMod; 1;") }, 'BCode';
{ local @INC = ($bc, @INC); require BcMod; print "15 inc=", ref($INC{'BcMod.pm'}), "\n"; }
{ package AutoI; our $AUTOLOAD; sub new { bless {}, shift } sub DESTROY {}
  sub AUTOLOAD { my ($s, $f) = @_; print "16 AUTOLOAD=$AUTOLOAD\n"; return unless $f eq 'AutoMod.pm'; main::fh_for("package AutoMod; 1;") } }
{ local @INC = (AutoI->new, @INC); require AutoMod; print "16 loaded\n"; }
my $dir = "/tmp/pcl-inc-hook-01-$$";
mkdir $dir; open my $pm, ">", "$dir/IncDirMod.pm" or die; print $pm "package IncDirMod; sub v { 'D' } 1;\n"; close $pm;
{ package DirH; sub new { bless {}, shift } }
sub DirH::INCDIR { return ("/nonexistent-a", $dir) }
{ local @INC = (DirH->new, @INC); require IncDirMod; print "17 v=", IncDirMod::v(), "\n"; }
{ package SOv; use overload '""' => sub { $dir }, fallback => 1; sub new { bless {}, shift } }
{ local @INC = (SOv->new, @INC); delete $INC{'IncDirMod.pm'}; require IncDirMod; print "18 loaded\n"; }
unlink "$dir/IncDirMod.pm"; rmdir $dir;
{ local @INC = (undef, @INC); eval { require UndefElemMod }; print "19 ", ($@ =~ /^Can't locate UndefElemMod.pm/ ? "cant-locate" : "other"), "\n"; }
unshift @INC, sub { die "hookdied\n" if $_[1] eq 'HdMod.pm'; return };
eval { require HdMod }; print "20 err=$@";
PL
is($pcl, $perl, '@INC hooks: code / array / object INC / INCDIR / AUTOLOAD / overload, the four answer items, do FILE, require "x.pl"');

($perl, $pcl) = both('hookexplicit', <<'PL');
my $got;
{ local @INC = (sub { $got = $_[1]; return }); eval { require "/nonexistent-dir-q/test_require.pm" }; }
print "explicit path offered to the hook: ", ($got // 'undef'), "\n";
PL
is($pcl, $perl, 'an explicit path that does not open is offered to the @INC hooks only');

done_testing;
