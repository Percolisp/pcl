#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# braced-elem-01.t -- task #3864: `${name{KEY}}` / `${name[IDX]}` /
# `@{name[LIST]}` / `@{name{LIST}}` in CODE are the element / slice
# `$name{KEY}` ... (toke.c S_scan_ident: an identifier followed by `{` or `[`
# inside `${...}` is the subscripted variable), not a call of `name`.  PPI
# builds Word + Block / Constructor inside the deref block, so PCL emitted a
# call and died "Undefined subroutine &P::Q::L called" (Log::Log4perl's
# Logger.pm:731 `${Log::Log4perl::Level::LEVELS{$cust_prio}}`).  The fix is
# the `${name}` collapse's sibling in Pl::PExpr (_block_word_subscript).
# A quote-like NAME (`${m{q}}`) is PPI's lexing question, task #4102.
# Every expected value is perl's own: the program runs under perl AND pcl and
# each row compares its line.

use v5.30;
use strict;
use warnings;
use Test::More;
use File::Temp qw(tempfile);
use FindBin qw($RealBin);

my $project_root = "$RealBin/../..";
my $pcl_bin      = "$project_root/runpcl";

plan skip_all => "runpcl not found" unless -x $pcl_bin;
plan skip_all => "sbcl not found"   unless `which sbcl 2>/dev/null`;

my $prog = <<'PERL';
package P::Q; our %L = (5 => 'five'); our @A = (7, 8);
package main;
print "r1 ", (${P::Q::L{5}} // 'U'), "\n";
my %h = (k => 'v'); print "r2 ", ${h{k}}, "\n";
print "r3 ", ${P::Q::A[1]}, "\n";
our @a = (4, 5, 6); print "r4 ", join(",", @{a[0,1]}), "\n";
print "r5 ", ${ h{k} }, "\n";
${h{new}} = 9; print "r6 $h{new}\n";
my @l = (1, 2, 3); print "r7 ${l[2]}\n";
my $c = sub { ${h{k}} . ${l[0]} }; print "r8 ", $c->(), "\n";
print "r9 ", join(",", @{h{'k','new'}}), "\n";
my $x = 3; my $d = sub { ${x} }; print "r10 ", $d->(), "\n";
print "r11 ${h{k}}\n";
sub f { my %z = (q => 'r'); return ${z{q}} } print "r12 ", f(), "\n";
my $i = 1; print "r13 ", ${l[$i + 1]}, "\n";
our %g = (a => 1); { no strict 'refs'; print "r14 ", ${g{a}}, "\n"; }
PERL

my @labels = map { "r$_" } 1 .. 14;
plan tests => scalar(@labels);

my ($fh, $pl) = tempfile(SUFFIX => '.pl', UNLINK => 1);
print $fh $prog;
close $fh;
my $perl = `perl $pl 2>&1 </dev/null`;
my $pcl  = `$pcl_bin $pl 2>&1 </dev/null`;

sub line_of {
  my ($out, $label) = @_;
  my ($l) = $out =~ /^(\Q$label\E\b.*)$/m;
  return $l // "<no line for $label>";
}
for my $label (@labels) {
  my $want = line_of($perl, $label);
  is(line_of($pcl, $label), $want, "$label (perl: $want)");
}
