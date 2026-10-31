#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# amp-call-proto-01.t -- task #3859: `&NAME(LIST)` IGNORES the prototype
# (perlsub), so LIST is passed in LIST context as the flat @_ it is.
#
# Two halves, both fixed:
#   * CONTEXT: the prototype's `$` slot / reference slot imposed SCALAR context
#     on an & call's arguments (`sub g ($) {}  &g(L())` passed L's LAST value;
#     perl passes the list).  The one reading is Pl::PExpr::call_prototype,
#     which both the argument context and the emitter now ask.
#   * SPREAD: a sub the runtime defines natively with a fixed lambda list (the
#     TAP entries) received an array or a list-context call's vector as ONE
#     argument and died "invalid number of arguments" (`&is(@pair)`,
#     Pod-Simple's `&is( e "...", "..." )`).  An & call now spreads its
#     arguments at the call (p-amp-spread).
# Every expected value is perl's own: each program runs under perl AND pcl
# and its whole output is compared.

use v5.30;
use strict;
use warnings;
use Test::More;
use File::Temp qw(tempfile);
use FindBin qw($RealBin);

my $project_root = "$RealBin/../..";
# runpcl, not pcl: a FIRST `pcl` run of a Test::More program prints its plan
# line twice (#2702, the first run from a compiled file), and every run here
# is a first run.
my $pcl_bin      = "$project_root/runpcl";

plan skip_all => "pcl not found"  unless -x $pcl_bin;
plan skip_all => "sbcl not found" unless `which sbcl 2>/dev/null`;

my @progs = (
  ['context', q{
sub L { return (7,8) }
sub g ($) { print "g [@_]\n" }
sub ga (\@) { print "ga [", join(",", map { ref($_) || $_ } @_), "]\n" }
sub gc (&) { print "gc [", join(",", map { ref($_) || $_ } @_), "]\n" }
sub gs ($$) { print "gs [@_]\n" }
sub w { print "w ", (wantarray ? "list" : defined(wantarray) ? "scalar" : "void"), "\n"; 1 }
my @a = (1, 2, 3);
&g(L());
&g(@a);
&ga(@a);
&gc(sub { 1 }, 5);
&gs(L(), 9);
&g(w());
g(w());
sub outer { &g; &g(); }
outer(4, 5);
my $r = &g(@a);
sub h ($) { return scalar(@_) }
my $n = &h(L());
print "n $n\n";
my @l = (&h(@a), h(@a));
print "l @l\n";
&g((L())[0], L());
}],
  ['spread', q{
use strict; use warnings;
use Test::More tests => 4;
my @x = (1, 1, "n");
&is(@x);
sub two { (2, 2) }
&is(two(), "m");
sub e2 { die "list only\n" unless wantarray; @_ }
&is( e2("a", "a"), "e2 list");
&ok(1, "ok spread");
package A; our $AUTOLOAD; sub AUTOLOAD { print "auto $AUTOLOAD [@_]\n"; 7 }
package main;
my @q = (3, 4);
&A::nosuch(@q);
{ my sub ls { "ls[@_]" } print &ls(@q), "\n"; }
sub wr { $_[0] = "w"; $_[1] = "v" } my @t = (1, 2); &wr(@t); print "@t\n";
my %h = (k => 1); sub cnt { scalar @_ } print &cnt(%h), "\n";
}],
);

plan tests => scalar(@progs);

for my $p (@progs) {
  my ($name, $code) = @$p;
  my ($fh, $pl) = tempfile(SUFFIX => '.pl', UNLINK => 1);
  print $fh $code;
  close $fh;
  my $perl = `perl $pl 2>&1 </dev/null`;
  my $pcl  = `$pcl_bin $pl 2>&1 </dev/null`;
  is($pcl, $perl, "$name: pcl's output is perl's");
}
