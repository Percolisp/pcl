#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# method-cache-01.t: the OWN-CLASS METHOD CACHE (s473s, the own-class half of
# task #582) may never answer with a method the program has since changed.
#
# p-method-call caches the pair (class name as the invocant spells it, method
# name as the call site spells it) -> the SYMBOL its own-class resolution
# found.  A stale entry would silently dispatch the WRONG method, which is the
# worst failure mode in this codebase, so the cache is built so it CANNOT go
# stale rather than being invalidated at a list of sites: it stores the symbol,
# so every redefinition path writes through it, and it re-tests `fboundp`, so
# every removal path declines to the slow path.  This file is that claim, event
# by event -- eval-redefine, glob-assign of a fresh sub, glob-assign of an
# existing code ref, `undef &`, `local *` (inside AND after), a method added to
# a class that was inheriting one, two classes sharing a method name, rebless,
# and names built at run time.
#
# EVERY expected string is real perl's (5.40.3) output for the same program,
# with ONE annotated exception -- see the `undef &` note on row 1.
#
# The last row is the inverse guard: it fails on a tree without the cache.

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

plan tests => 4;

# -- 1. every event that changes what an own-class call resolves to ---------
# One SBCL launch; each numbered line is one event.
#
# Row 5 is the ONE annotated divergence: perl's `undef &Kid::m1` empties the
# CV but leaves it in the glob, so the next call dies "Undefined subroutine
# called"; PCL removes the sub, so the call INHERITS Base::m1.  That is
# PRE-EXISTING (measured on the pre-cache runtime, task #1530) and is asserted
# here as a canary -- it is still the cache requirement that matters, namely
# that the cached entry stops answering at all.  When #1530 is fixed this row
# fails, and the expectation becomes perl's "ERR:undef-sub".
my $inval = <<'EOF';
package Base; sub new { bless { v => 1 }, $_[0] } sub m1 { "Base::m1" } sub m2 { "Base::m2" }
package Kid;  our @ISA = ('Base'); sub m1 { "Kid::m1" } sub m3 { "Kid::m3" }
package Other; sub thing { "Other::thing" }
package main;
my $k = Kid->new;
my $b = Base->new;
print "1a ", $k->m1, "\n";
print "1b ", $k->m1, "\n";
{ no warnings 'redefine'; eval 'sub Kid::m1 { "Kid::m1-v2" } 1' or die $@; }
print "2 ", $k->m1, "\n";
{ no warnings 'redefine'; no strict 'refs'; *Kid::m1 = sub { "Kid::m1-glob" }; }
print "3 ", $k->m1, "\n";
{ no warnings 'redefine'; no strict 'refs'; *Kid::m1 = \&Other::thing; }
print "4 ", $k->m1, "\n";
{ no warnings 'redefine'; no strict 'refs'; *Kid::m1 = sub { "Kid::m1-tmp" }; }
{ no strict 'refs'; undef &Kid::m1; }
my $r5 = eval { $k->m1 };
print "5 ", (defined $r5 ? $r5 : "ERR:undef-sub"), "\n";
{ no warnings 'redefine'; no strict 'refs'; *Kid::m1 = sub { "Kid::m1-v3" }; }
print "6 ", $k->m1, "\n";
{
  no warnings 'redefine'; no strict 'refs';
  local *Kid::m1 = sub { "Kid::m1-local" };
  print "7a ", $k->m1, "\n";
}
print "7b ", $k->m1, "\n";
print "8a ", $k->m2, "\n";
{ no warnings 'redefine'; no strict 'refs'; *Kid::m2 = sub { "Kid::m2-new" }; }
print "8b ", $k->m2, "\n";
print "9a ", $b->m1, "\n";
print "9b ", $k->m1, "\n";
my $o = Kid->new;
print "10a ", $o->m1, "\n";
bless $o, 'Base';
print "10b ", $o->m1, "\n";
for my $i (1 .. 2) { my $n = "m"; $n .= "1"; print "11$i ", $k->$n, "\n"; }
for my $i (1 .. 2) { my $c = "Ba"; $c .= "se"; print "12$i ", $c->new->m1, "\n"; }
EOF
is(run_cl($inval),
   "1a Kid::m1\n1b Kid::m1\n2 Kid::m1-v2\n3 Kid::m1-glob\n4 Other::thing\n"
   . "5 Base::m1\n6 Kid::m1-v3\n7a Kid::m1-local\n7b Kid::m1-v3\n"
   . "8a Base::m2\n8b Kid::m2-new\n9a Base::m1\n9b Kid::m1-v3\n"
   . "10a Kid::m1-v3\n10b Base::m1\n"
   . "111 Kid::m1-v3\n112 Kid::m1-v3\n121 Base::m1\n122 Base::m1\n",
   'a cached own-class method follows every redefinition, removal, local, rebless and run-time-built name');

# -- 2. the root-stash spellings the cache keys on ------------------------
# %pcl-normalize-class-name was rewritten in the same commit (its two literal
# `string=` tests became length/char tests), and the cache probes BEFORE it --
# so a class reachable under two spellings gets two entries and both must be
# right.  `ref()` on rows 4/5 keeps the spelling the program blessed with
# (pre-existing, the #580 family), so this row asks only what DISPATCHES.
my $spell = <<'EOF';
package Foo; sub new { bless {}, $_[0] } sub who { "Foo::who" } sub two { "Foo::two" }
package main;
print "1 ", Foo->who, "\n";
print "2 ", "main::Foo"->who, "\n";
print "3 ", "::Foo"->who, "\n";
print "4 ", Foo->who, " ", "main::Foo"->two, " ", "::Foo"->two, "\n";
my $o = bless {}, "::Foo";
print "5 ", $o->who, " ", $o->who, "\n";
my $p = bless {}, "main::Foo";
print "6 ", $p->who, " ", $p->two, "\n";
EOF
is(run_cl($spell),
   "1 Foo::who\n2 Foo::who\n3 Foo::who\n4 Foo::who Foo::two Foo::two\n"
   . "5 Foo::who Foo::who\n6 Foo::who Foo::two\n",
   'the three root-stash spellings all dispatch to the same package, cached or not');

# -- 3. a method the class does NOT own is never cached -------------------
# The inherited half of #582 is still blocked on @ISA-write invalidation, so an
# own MISS must reach the walks every time: an @ISA edit between two calls has
# to be visible on the second.
my $isa = <<'EOF';
package P1; sub hello { "P1" }
package P2; sub hello { "P2" }
package Kid; sub new { bless {}, shift }
package main;
@Kid::ISA = ('P1');
my $k = Kid->new;
print "1 ", $k->hello, "\n";
print "2 ", $k->hello, "\n";
@Kid::ISA = ('P2');
print "3 ", $k->hello, "\n";
*Kid::hello = sub { "own" };
print "4 ", $k->hello, "\n";
@Kid::ISA = ('P1');
print "5 ", $k->hello, "\n";
EOF
is(run_cl($isa), "1 P1\n2 P1\n3 P2\n4 own\n5 own\n",
   'an inherited (own-miss) call is not cached: an @ISA edit shows on the next call');

# -- 4. inverse guard: the cache is present in the runtime ----------------
# Fails on a tree without s473s's change, which is what an inverse verification
# of this file has to see.
{
    open my $fh, '<', $runtime or die "open $runtime: $!";
    local $/;
    my $src = <$fh>;
    close $fh;
    my $ok = $src =~ /\Qdefvar *pcl-own-method-cache*\E/
          && $src =~ /\Q(%pcl-own-method-cached raw-class method-name)\E/
          && $src =~ /\Q(%pcl-own-method-cache-put raw-class method-name own)\E/;
    ok($ok, 'p-method-call probes and fills the own-class method cache');
}
