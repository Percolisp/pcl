#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# method-dispatch-01.t: what method dispatch must still answer after #73's
# speed work, and SUPER::'s resolution order (#533).
#
# #73 (s446m) put a FAST PATH in p-method-call (a plain method name found in
# the invocant's own class package returns straight away) and memoized the two
# lookups a dispatch repeats (class name -> CL package, method name -> pl-NAME
# symbol name).  Nothing about the RESOLUTION is cached, deliberately: perl
# programs redefine methods, glob-assign them and rewrite @ISA at run time, and
# every one of those must be visible on the very next call.  The first family
# below is that requirement, row by row.
#
# The second family is #533: a SUPER:: call whose parent chain has nothing
# finishes its lookup the way any other method call does -- UNIVERSAL and
# UNIVERSAL's own @ISA, then the isa/can/DOES built-ins, then import/unimport
# as no-ops, then AUTOLOAD in the PARENTS (with $AUTOLOAD spelled
# Current::SUPER::method), and only then a perl-shaped, trappable
# "Can't locate object method".  It used to die with a raw CL error --
# `No SUPER::isa found from Solo` -- that no eval could recognise.
#
# EVERY expected string below is real perl's output for the same program
# (perl 5.40.3), taken before the change.

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

plan skip_all => "pl2cl not found" if ! -x $pl2cl;
plan skip_all => "sbcl not found"  if ! `which sbcl 2>/dev/null`;

sub run_cl {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;
    my $cl_code = PCLCore::transpile(qq{$pl2cl $pl_file});
    my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cl_fh $cl_code;
    close $cl_fh;
    my $out = `sbcl @sbcl_rt --load $cl_file 2>&1`;
    $out =~ s/^;.*\n//gm;
    $out =~ s/^PCL Runtime loaded\n//gm;
    $out =~ s/^\s*\n//gm;
    return $out;
}

plan tests => 6;

# ── 1. dispatch stays live under runtime redefinition (#73) ───────────────
# One SBCL launch: each line is one requirement the fast path must not break.
my $redef = <<'EOF';
package P1; sub hello { "P1" } sub new { bless {}, shift }
package P2; sub hello { "P2" }
package Kid; sub new { bless {}, shift }
package main;
@Kid::ISA = ('P1');
my $k = Kid->new;
print "1 ", $k->hello, "\n";
@Kid::ISA = ('P2');
print "2 ", $k->hello, "\n";
*Kid::hello = sub { "own" };
print "3 ", $k->hello, "\n";
*Kid::hello = sub { "own2" };
print "4 ", $k->hello, "\n";
print "5 ", ($k->can('hello') ? "can" : "no"), " ", ($k->isa('P2') ? "isa" : "no"), "\n";
my $r = P1->new; print "6 ", $r->hello, "\n";
bless $r, "P2";  print "7 ", $r->hello, "\n";
bless $r, "Kid"; print "8 ", $r->hello, "\n";
my $cn = "P1"; print "9 ", $cn->hello, "\n";
$cn = "P2";    print "10 ", $cn->hello, "\n";
EOF
is(run_cl($redef),
   "1 P1\n2 P2\n3 own\n4 own2\n5 can isa\n6 P1\n7 P2\n8 own2\n9 P1\n10 P2\n",
   'dispatch follows @ISA edits, glob-assigned and REdefined methods, and rebless');

# A leaf class (empty @ISA, the CLOS branch) redefined after its first call,
# and a package that exists only because something was blessed into it.
my $leaf = <<'EOF';
package Leaf; sub new { bless {}, shift } sub v { "leaf" }
package main;
my $lf = Leaf->new;
print "1 ", $lf->v, "\n";
*Leaf::v = sub { "leaf2" };
print "2 ", $lf->v, "\n";
my $only = bless {}, "OnlyBlessed";
my $e = eval { $only->nope }; print "3 ", ($@ ? "died" : "ok:$e"), "\n";
*OnlyBlessed::nope = sub { "now-here" };
print "4 ", $only->nope, "\n";
package Odd; sub new { bless {}, shift } sub push { "odd-push" } sub print { "odd-print" }
package main;
my $od = Odd->new; print "5 ", $od->push, " ", $od->print, "\n";
my $bad = eval { $od->pop }; print "6 ", ($@ ? "died" : "ok:$bad"), "\n";
EOF
is(run_cl($leaf),
   "1 leaf\n2 leaf2\n3 died\n4 now-here\n5 odd-push odd-print\n6 died\n",
   'leaf class redefined after its first call; bless-only package; builtin-named methods');

# Two levels of inheritance, shadowed at the MIDDLE level at run time, plus the
# qualified spellings that must keep bypassing the fast path.
my $deep = <<'EOF';
package A1; sub new { bless {}, shift } sub w { "A1" }
package B1; our @ISA = ('A1');
package C1; our @ISA = ('B1');
package main;
my $c1 = bless {}, "C1"; print "1 ", $c1->w, "\n";
*B1::w = sub { "B1" };   print "2 ", $c1->w, "\n";
print "3 ", ($c1->UNIVERSAL::isa('A1') ? "y" : "n"), " ", $c1->A1::w, "\n";
print "4 ", "A1"->w, " ", "main::A1"->w, "\n";
EOF
is(run_cl($deep), "1 A1\n2 B1\n3 y A1\n4 A1 A1\n",
   'inherited method shadowed at the middle level; UNIVERSAL:: and PKG:: qualified calls');

# ── 2. SUPER:: resolution order (#533) ────────────────────────────────────
my $super_universal = <<'EOF';
package Solo;
sub new { bless {}, shift }
sub tisa  { $_[0]->SUPER::isa('Solo')  ? "y" : "n" }
sub tcan  { $_[0]->SUPER::can('tisa')  ? "y" : "n" }
sub tdoes { $_[0]->SUPER::DOES('Solo') ? "y" : "n" }
sub tmiss { my $r = eval { $_[0]->SUPER::nope }; $@ ? "die" : "ok:$r" }
sub timp  { my $r = eval { $_[0]->SUPER::import }; $@ ? "die" : "ok" }
package main;
my $s = Solo->new;
print "1 ", $s->tisa, "\n";
print "2 ", $s->tcan, "\n";
print "3 ", $s->tdoes, "\n";
print "4 ", $s->tmiss, "\n";
print "5 ", $s->timp, "\n";
EOF
is(run_cl($super_universal), "1 y\n2 y\n3 y\n4 die\n5 ok\n",
   'SUPER:: from a package with no @ISA reaches UNIVERSAL (isa/can/DOES), import is a no-op, a real miss dies');

# The die is perl-shaped and trappable, and it names the CURRENT class.
my $super_die = <<'EOF';
package Base; sub new { bless {}, shift } sub hello { "base" }
package Kid2; our @ISA = ('Base');
sub hello { "kid" }
sub thello { $_[0]->SUPER::hello }
sub tisa   { $_[0]->SUPER::isa('Base') ? "y" : "n" }
sub tmiss  { eval { $_[0]->SUPER::nope }; $@ }
package main;
my $k = bless {}, "Kid2";
print "1 ", $k->thello, "\n";
print "2 ", $k->tisa, "\n";
my $err = $k->tmiss;
print "3 ", ($err =~ /^Can't locate object method "nope" via package "Kid2"/ ? "shaped" : "RAW:$err"), "\n";
EOF
is(run_cl($super_die), "1 base\n2 y\n3 shaped\n",
   'SUPER:: finds the parent, still reaches UNIVERSAL::isa, and a miss dies perl-shaped naming the current class');

# A parent's own isa outranks UNIVERSAL's; AUTOLOAD in the parent chain answers
# a missing SUPER:: method, with perl's $AUTOLOAD spelling; and UNIVERSAL's own
# @ISA is searched.
my $super_rest = <<'EOF';
package B2; sub isa { "parent-isa" }
package K2; our @ISA = ('B2'); sub new { bless {}, shift }
sub t { $_[0]->SUPER::isa('whatever') }
package B3; our $AUTOLOAD;
sub AUTOLOAD { my $n = $AUTOLOAD; return if $n =~ /DESTROY$/; "auto[$n]" }
package K3; our @ISA = ('B3'); sub new { bless {}, shift }
sub t { $_[0]->SUPER::zork }
package LastChance; sub lastditch { "last" }
package UNIVERSAL; our @ISA = ('LastChance');
package Solo2; sub new { bless {}, shift } sub t { $_[0]->SUPER::lastditch }
package main;
print "1 ", K2->new->t, "\n";
print "2 ", K3->new->t, "\n";
print "3 ", Solo2->new->t, "\n";
EOF
is(run_cl($super_rest), "1 parent-isa\n2 auto[K3::SUPER::zork]\n3 last\n",
   "a parent's own isa wins over UNIVERSAL's; parent-chain AUTOLOAD answers with \$AUTOLOAD = Current::SUPER::method; UNIVERSAL's own \@ISA is searched");
