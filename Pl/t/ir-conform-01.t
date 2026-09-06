#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform-01.t — the gate row for the IR conformance corpus
# (docs/plan-speed-and-ir-s470.md Part B item B6, task #1239).
#
# `ir-conform/cases/` holds ~350 small Perl programs, each named by the
# semantics it pins, each with perl 5.40.3's answer recorded beside it.  A
# backend author for another target (JavaScript, C) runs THEIR backend over
# the `.ir` files and compares to those answers; PCL's own CL target runs the
# same comparison, which is what makes the corpus trustworthy — a case PCL
# gets wrong is a PCL bug the corpus found, listed in
# `ir-conform/known-fail.tsv` with its owning task, never edited to pass.
#
# WHAT THIS FILE COSTS, and why it is a SAMPLE.  The whole corpus is one
# transpile and one SBCL launch per case (~65 s at --jobs 2).  The gate's
# metric is a file's WALL time (CLAUDE.md rule 6), so the gate runs an evenly
# spaced sample of 20 and the FULL corpus is a WHAT-TO-RUN-WHEN entry:
#
#     tools/ir-conform --jobs 2        # after any cl/ runtime or Pl/ emission change
#
# The structural rows below are free and cover the whole corpus: they are what
# stops a case from rotting into a claim nothing evaluates (rule 12).

use v5.30;
use strict;
use warnings;
use Test::More;
use FindBin qw($RealBin);

my $root  = "$RealBin/../..";
my $cases = "$root/ir-conform/cases";
my $tool  = "$root/tools/ir-conform";

plan tests => 8;

ok(-d $cases, 'the corpus directory exists');
ok(-x $tool,  'tools/ir-conform is executable');

opendir(my $dh, $cases) or die "opendir $cases: $!";
my @all = sort readdir $dh;
closedir $dh;
my @pl = grep { /\.pl$/ } @all;
cmp_ok(scalar @pl, '>=', 100, 'the corpus holds at least 100 cases')
  or diag("found " . scalar(@pl) . " cases in $cases");

# Rule 12: a case with no oracle is a claim that cannot be evaluated.  It must
# be impossible to add one silently.
my @no_oracle = grep { my $b = $_; $b =~ s/\.pl$//; ! -f "$cases/$b.expected" } @pl;
is(scalar @no_oracle, 0, 'every case has a recorded .expected oracle')
  or diag("without an oracle: @no_oracle");

# Every oracle's header parses, and its byte count matches its body — a
# truncated oracle would otherwise read as a semantic difference.
my @bad_header;
for my $f (@pl) {
  (my $b = $f) =~ s/\.pl$//;
  open my $fh, '<:raw', "$cases/$b.expected" or do { push @bad_header, "$b: unreadable"; next };
  my $head = <$fh>;
  my $body = do { local $/; <$fh> } // '';
  close $fh;
  if (!defined $head || $head !~ /^\#!ir-conform\/expected v1 exit=(\d+) bytes=(\d+)$/) {
    push @bad_header, "$b: header"; next;
  }
  push @bad_header, "$b: bytes=$2 but " . length($body) . " on disk"
    if length($body) != $2;
}
is(scalar @bad_header, 0, 'every oracle header parses and matches its body')
  or diag(join "\n", @bad_header);

# A `.rules` file may only name a normalisation the tool implements — an
# unknown name would silently normalise nothing.
my @bad_rules;
for my $f (grep { /\.rules$/ } @all) {
  open my $fh, '<', "$cases/$f" or next;
  while (my $l = <$fh>) {
    next if $l =~ /^\s*(?:#|$)/;
    push @bad_rules, "$f: $l"
      if $l !~ /^\s*normalise\s+(?:die-location|hex-address|sort-lines)\s*$/;
  }
  close $fh;
}
is(scalar @bad_rules, 0, 'every .rules line names a known normalisation')
  or diag(join '', @bad_rules);

# Every known-fail row names a case that exists and a task that owns it.  A row
# whose case was renamed or deleted would otherwise excuse nothing forever.
my @bad_known;
{
  my $kf = "$root/ir-conform/known-fail.tsv";
  open my $fh, '<', $kf or die "cannot read $kf: $!";
  while (my $l = <$fh>) {
    next if $l =~ /^\s*(?:#|$)/;
    chomp $l;
    my ($case, $task, $cause) = split /\t/, $l, 3;
    push @bad_known, "no such case: $case"        if ! -f "$cases/$case.pl";
    push @bad_known, "$case: task is not a number" if ($task // '') !~ /^\d+$/;
    push @bad_known, "$case: no cause"             if !defined $cause || $cause eq '';
  }
  close $fh;
}
is(scalar @bad_known, 0, 'every known-fail row names a live case and a task')
  or diag(join "\n", @bad_known);

# The sample.  Any FAIL is a regression against perl; any STALE is a
# known-fail row that now passes and must be dropped with its fix.
my $out = qx{'$tool' --sample 20 2>&1};
my $rc  = $? >> 8;
my @bad = grep { /^(?:FAIL|STALE) / } split /\n/, $out;
ok($rc == 0 && !@bad, 'the 20-case sample answers exactly as perl does')
  or diag($out);
