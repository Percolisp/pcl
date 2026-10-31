#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# use-base-phase-01.t -- task #3860 (RULED s528): `use base` / `use parent`
# push @ISA at BEGIN time, at the statement's own source position.
#
# base.pm and parent.pm run inside the `use`, i.e. at compile time.  PCL
# emitted the @ISA push in the RUN-time stream, so a call that ran BEFORE the
# package section saw an empty @ISA ("Can't locate object method new via
# package Blurb"), and so did code after a `use base` inside
# `BEGIN { package X; ... }` -- where, in addition, the push named the READER
# package's @ISA (main's), because the inner (in-package :X) does not take
# effect while the BEGIN form is read.  The push now goes inside `p-BEGIN` in
# the definitions bucket (what a user-written `BEGIN { push @ISA, ... }` gets),
# and a scheduled block sets _seam_outer_pkg like a do{}/eval{} body does.
#
# The first three rows are the filing's three probes (Pod-Simple fake-closers /
# xhtml20 / xhtml25 shapes).  The last row runs a program TWICE through `pcl`
# with its cache on: the second run is the cached fasl, where a p-BEGIN fires
# at the fasl's load (measured s528, the ruling's premise).

use v5.30;
use strict;
use warnings;
use Test::More;
use File::Temp qw(tempfile tempdir);
use FindBin qw($RealBin);

my $project_root = "$RealBin/../..";
my $pcl_bin      = "$project_root/pcl";

plan skip_all => "pcl not found"  unless -x $pcl_bin;
plan skip_all => "sbcl not found" unless `which sbcl 2>/dev/null`;

plan tests => 13;

my $dir = tempdir(CLEANUP => 1);
my $n = 0;

sub write_pl {
    my ($code) = @_;
    my $f = "$dir/p" . ++$n . ".pl";
    open(my $fh, '>', $f) or die "$f: $!";
    print $fh $code;
    close $fh;
    return $f;
}

# PCL_XSERVER= : the gate's transpile server does not carry -I (task #4041).
sub run_pcl { my ($f) = @_; return `PCL_XSERVER= $pcl_bin $f 2>&1 </dev/null` }

sub both_agree {
    my ($code, $desc) = @_;
    my $f    = write_pl($code);
    my $perl = `perl $f 2>&1 </dev/null`;
    my $pcl  = run_pcl($f);
    is($pcl, $perl, "$desc (perl: " . ($perl =~ s/\n/\\n/gr) . ")");
}

# ---- the filing's three probes --------------------------------------------
both_agree(<<'PL', '`use base` is in force ABOVE its package section, like a BEGIN push');
print "[@Foo::ISA] [@Bar::ISA]\n";
package Foo; use base "File::Spec";
package Bar; BEGIN { push our @ISA, "File::Spec" }
PL
both_agree(<<'PL', '`use base` inside `BEGIN { package X; }` pushes X\'s @ISA, and X->new resolves');
BEGIN { package MyX; use base "Pod::Simple::XHTML"; sub start_code { 1 } }
print "isa=@MyX::ISA\n"; my $p = MyX->new; print ref($p), "\n";
PL
both_agree(<<'PL', 'SUPER::new through a parent\'s OWN @ISA, called above the section');
my $p = Blurb->new; print ref($p), "\n";
package Blurb; use base qw/Pod::Simple::Methody/;
sub new { my $n = shift->SUPER::new(@_); $n }
PL

# ---- the same rule for the other spellings and positions ------------------
both_agree(<<'PL', '`use parent -norequire` is in force above its section too');
print "[@P1::ISA]\n";
package Q1; sub hi { "q1" }
package P1; use parent -norequire, "Q1";
package main; print P1->hi, "\n";
PL
both_agree(<<'PL', 'a BEGIN after the `use parent` sees the parent, one before does not');
BEGIN { print "begin:[@Z::ISA]\n" }
package Z; use parent "File::Spec";
BEGIN { print "after:[@Z::ISA]\n" }
PL
both_agree(<<'PL', '`use base` inside a sub body runs at compile time, not when the sub runs');
sub f { package W; use base "File::Spec"; 1 }
print "[@W::ISA]\n";
PL
both_agree(<<'PL', 'a block-scoped `package` + `use base` pushes that package\'s @ISA only');
{ package Y; use base "File::Spec"; }
print "[@Y::ISA] [@main::ISA]\n";
PL
both_agree(<<'PL', 'an inherited method through a grandparent, called above the section');
package Base1; sub new { my $c = shift; return bless {}, $c } sub hi { "base-hi" }
package Mid; our @ISA = ('Base1');
package main;
my $p = Kid->new; print ref($p), " ", $p->hi, "\n";
package Kid; use base qw/Mid/;
sub new { my $n = shift->SUPER::new(@_); $n }
PL
both_agree(<<'PL', 'two `use parent` lines keep their order');
package M2; use parent -norequire, "A1"; use parent -norequire, "B1";
package main; print "[@M2::ISA]\n";
PL

# ---- a cached program run (the fasl) sees the BEGIN-time push --------------
{
    my $f = write_pl(<<'PL');
my $p = Blurb2->new; print ref($p), " [@Blurb2::ISA]\n";
package Blurb2; use base qw/Pod::Simple::Methody/;
sub new { my $n = shift->SUPER::new(@_); $n }
PL
    my $perl = `perl $f 2>&1 </dev/null`;
    my $t = time - 60;
    utime($t, $t, $f);                       # old enough to be cached
    my $run1 = run_pcl($f);
    my $run2 = run_pcl($f);
    is($run1, $perl, 'cached program: the first run (perl: ' . ($perl =~ s/\n/\\n/gr) . ')');
    is($run2, $perl, '... and the second run, from the compiled file');
}

# ---- #4040 (s530a): `use base` skips a parent the class already isa; `use parent` does not ----
both_agree(<<'PL', '`use base` skips an already-isa parent (a repeat, one already inherited, a BEGIN @ISA); `use parent` pushes twice; repeated parents never reach CLOS twice');
package Super; sub new { bless {}, shift } sub hi { "super" }
package Sub; our @ISA = ('Super');
package A; sub a { "a" }
package X; use base "File::Spec"; use base "File::Spec";
package Y; use base qw(Sub Super);
package Z; use parent -norequire, "Super", "Super";
package W; use base qw(Super); use base qw(Sub);
package V; BEGIN { our @ISA = ('Super') } use base 'Super', 'Sub';
package U; use base qw(A A);
package main;
print "X [@X::ISA]\nY [@Y::ISA]\nZ [@Z::ISA]\nW [@W::ISA]\nV [@V::ISA]\nU [@U::ISA]\n";
print "hi ", Y->new->hi, " ", W->new->hi, " ", U->a, "\n";
PL
both_agree(<<'PL', 'the skip asks the ->isa METHOD (an overriding isa is honoured), qualified packages, a BEGIN-nested use base');
package Foo; sub new { bless {}, shift } sub who { "foo" }
package Foo::Bar; use base 'Foo'; use base 'Foo';
package Odd; sub isa { 1 } sub who { "odd" }
package Q; use base 'Odd'; use base 'Foo';
package R; BEGIN { package R; use base 'Foo' } use base 'Foo';
package main;
print "1 [@Foo::Bar::ISA] ", Foo::Bar->new->who, "\n";
print "2 [@Q::ISA]\n";
print "3 [@R::ISA]\n";
PL
