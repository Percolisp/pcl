#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# regex-x-comment-01.t -- task #3856 (RULED s529): a PATTERN COMMENT is not
# interpolated.
#
# perl's lexer (toke.c S_scan_const), in a pattern and outside a character
# class, copies `(?#...)` and -- when the op has /x -- a `#` comment to the end
# of the line through UNINTERPOLATED.  PCL's one interpolation scanner
# (Pl::InterpScan) scanned them as ordinary text, so
#   * a `${ ... }` in a comment REFUSED the whole file (Template::Parser);
#   * a variable whose VALUE holds a newline ended the comment early;
#   * `# @{[ f() ]}` CALLED f.
# The scanner now takes `x => 0|1` (the op's TRAILING modifiers only: /x, /xx,
# qr//x -- an inline (?x) does not count) and keeps toke.c's in_charclass
# state, which starts over after every interpolated variable (perl re-enters
# scan_const there).  The rows are the s529 probe set
# (~/pcl-agent-scratch/s529/probes/p3856[abc].pl), every expected value
# perl's own: the program runs under perl AND pcl and each row compares its
# line.  Row 21 of that set (`use re '/x'`) is task #4042 and is not here.

use v5.30;
use strict;
use warnings;
use Test::More;
use File::Temp qw(tempfile);
use FindBin qw($RealBin);

my $project_root = "$RealBin/../..";
my $pcl_bin      = "$project_root/pcl";

plan skip_all => "pcl not found"  unless -x $pcl_bin;
plan skip_all => "sbcl not found" unless `which sbcl 2>/dev/null`;

# Each row prints "<label> <answer>" -- the answer is perl's, read from the run.
my @rows = (
  ['filing', 'the filing: `${ ... }` in a /x comment is not a reference (the file transpiles)',
   q{print "filing ", ("q" =~ / q  # a comment naming ${ ... }
                       /x ? "ok" : "no"), "\n";}],
  ['r2', 'plain `$x` and `$h{k}` in a /x comment',
   q{print "r2 ", ("q" =~ / q  # a comment naming $x and $h{k}
                       /x ? "ok" : "no"), "\n";}],
  ['r3', '`#` inside `[...]` is literal, the variable after it interpolates',
   q{print "r3 ", ("q#Z" =~ / q [#] $z /x ? "ok" : "no"), "\n";}],
  ['r4', 'an escaped `\#` is literal, the variable after it interpolates',
   q{print "r4 ", ("q#Z" =~ / q \# $z /x ? "ok" : "no"), "\n";}],
  ['r5', '`(?# ... $x )` under /x is a comment',
   q{print "r5 ", ("q" =~ / q (?# a comment naming $x ) /x ? "ok" : "no"), "\n";}],
  ['r6', 'a variable AFTER a `(?#...)` comment interpolates',
   q{print "r6 ", ("qzzz" =~ / q (?# comment ) $x /x ? "ok" : "no"), "\n";}],
  ['r7', 'without /x a `#` is literal and `$x` interpolates',
   q{print "r7 ", ("q#zzz" =~ /q#$x/ ? "ok" : "no"), "\n";}],
  ['r8', '... and with /x the `#$x` is a comment',
   q{print "r8 ", ("q" =~ /q#$x/x ? "ok" : "no"), "\n";}],
  ['r9', '`[#x]` is a class, not a comment start',
   q{print "r9 ", ("qxZ" =~ / q [#x] $z /x ? "ok" : "no"), "\n";}],
  ['r10', 'an unknown `@nosuch` / `$nosuch` in a comment',
   q{print "r10 ", ("q" =~ / q # see @nosuch and $nosuch
                       /x ? "ok" : "no"), "\n";}],
  ['r11', 'the s/// REPLACEMENT is never a pattern; the pattern\'s comment is skipped',
   q{my $s = "a b"; (my $t = $s) =~ s/ \s # drop $x
                                 /_/x; print "r11 $t\n";}],
  ['r12', '/xx counts as /x',
   q{print "r12 ", ("q" =~ m{ q # $x
                           }xx ? "ok" : "no"), "\n";}],
  ['r13', 'qr//x counts',
   q{my $re = qr/ q # $x
           /x; print "r13 ", ("q" =~ $re ? "ok" : "no"), "\n";}],
  ['r14', 'an INLINE (?x) does not count: perl interpolates the newline value',
   q{print "r14 ", ("q" =~ /(?x) q # $nl
/ ? "ok" : "no"), "\n";}],
  ['r15', 'a newline in an interpolated VALUE does not end the comment',
   q{print "r15 ", ("q" =~ / q # $nl
/x ? "ok" : "no"), "\n";}],
  ['r16', '`@{[ f() ]}` in a comment',
   q{print "r16 ", ("q" =~ / q # calls @{[ f() ]}
/x ? "ok" : "no"), "\n";}],
  ['r17', '... is never CALLED',
   q{print "r17 calls=$calls\n";}],
  ['r18', '`(?# $nl )` is a comment',
   q{print "r18 ", ("q" =~ / q (?# $nl ) /x ? "ok" : "no"), "\n";}],
  ['r19', 'a class then a comment',
   q{print "r19 ", ("q#" =~ / q [#] # $nl
/x ? "ok" : "no"), "\n";}],
  ['r20', 'an undef variable in a comment',
   q{my $u; print "r20 ", ("q" =~ / q # $u
/x ? "ok" : "no"), "\n";}],
  ['cc', 'the class state starts over after a variable (toke.c re-enters scan_const)',
   q{print "cc ", ("ab" =~ /^[$a1#]$b1/x ? "interp" : "no"), "\n";}],
  ['cc2', '... and without the variable the class holds the `#`',
   q{print "cc2 ", ("ab" =~ /^[a#]$b1/x ? "interp" : "no"), "\n";}],
);

plan tests => scalar(@rows) + 1;

# TWO programs: the filing row was a whole-FILE refusal, so on an unfixed tree
# it would take every other row down with it -- in its own program, each
# remaining row fails or passes on its own (the inverse check is per row).
my $head = join "\n",
  'use strict; use warnings;',
  'my $x = "zzz"; my $z = "Z"; my %h = (k => "v");',
  'my $nl = "\nZ"; my $calls = 0; sub f { $calls++; "F" }',
  'my $a1 = "a"; my $b1 = "b";', '';
my ($perl, $pcl) = ('', '');
for my $group ([grep { $_->[0] eq 'filing' } @rows], [grep { $_->[0] ne 'filing' } @rows]) {
  my ($fh, $pl) = tempfile(SUFFIX => '.pl', UNLINK => 1);
  print $fh $head, join("\n", map { $_->[2] } @$group), "\n";
  close $fh;
  $perl .= `perl $pl 2>&1 </dev/null`;
  $pcl  .= `$pcl_bin $pl 2>&1 </dev/null`;
}

sub line_of {
  my ($out, $label) = @_;
  my ($l) = $out =~ /^(\Q$label\E\b.*)$/m;
  return $l // "<no line for $label>";
}

unlike($pcl, qr/cannot compile interpolated regex reference|statement dropped|refused/,
       'both programs transpile whole');
for my $r (@rows) {
  my ($label, $desc) = @$r;
  my $want = line_of($perl, $label);
  is(line_of($pcl, $label), $want, "$desc (perl: $want)");
}
