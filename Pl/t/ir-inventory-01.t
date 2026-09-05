#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-inventory-01.t — the checked-in IR op inventory must be what the tool
# produces TODAY (task #1170, Part B item B1).
#
# `docs/ir-op-inventory.tsv` and `docs/ir-op-inventory.md` are GENERATED from
# the loaded runtime's export list by `tools/ir-inventory.pl`.  Nothing else in
# the tree notices when they fall behind: a new `p-*` export, a changed
# `Contract:` docstring tail, a family reassignment — all of them silently make
# the port list a backend author reads WRONG.
#
# So this file uses the #1072 pattern from Pl/t/artifact-staleness-01.t:
# regenerate into a temp directory with the DOCUMENTED tool (never a copy of
# the recipe, which would be free to drift from the recipe it checks) and
# compare the body.  Nothing is normalised, because nothing legitimately
# varies: the tool writes no timestamps, no paths and no addresses, and its
# only input is the runtime it loads.
#
# TO FIX A FAILING ROW HERE (do not edit this test):
#     tools/ir-inventory.pl
# and commit both files.  If the diff surprises you, the FIRST line of the
# diff head below says which name moved.
#
# Cost: one `tools/ir-inventory.pl` run = one SBCL start on the cached core
# plus a 682-row walk (~1 s warm).  It is the whole file's wall time, which is
# the gate's metric (CLAUDE.md §6).

use strict;
use warnings;
use Test::More;
use FindBin qw($RealBin);
use File::Temp qw(tempdir);

my $root = "$RealBin/../..";
my $tool = "$root/tools/ir-inventory.pl";

ok(-x $tool, 'tools/ir-inventory.pl is present and executable')
  or BAIL_OUT('the generator is missing — this test cannot check anything');

my $tmp = tempdir(CLEANUP => 1);          # never in the tree
my $fresh_tsv = "$tmp/fresh.tsv";
my $fresh_md  = "$tmp/fresh.md";

# Run with cwd = the repo root: the tool resolves everything from its own
# $RealBin, but a relative cwd is what the recipe in its header uses.
my $out = run_at_root($tool, '--quiet',
                      '--out-tsv', $fresh_tsv, '--out-md', $fresh_md);

for my $pair ([ 'docs/ir-op-inventory.tsv', $fresh_tsv ],
              [ 'docs/ir-op-inventory.md',  $fresh_md  ]) {
  my ($rel, $fresh) = @$pair;
  my $have = lines_of("$root/$rel");
  my $want = lines_of($fresh);
  my $at   = first_difference($have, $want);
  ok(!defined $at, "$rel is what tools/ir-inventory.pl emits now")
    or diag("$rel is STALE.\n"
            . "  first difference at line " . ($at + 1) . "\n"
            . "  regenerate with:  tools/ir-inventory.pl\n"
            . diff_head($have, $want, $at, 30));
}

# A vacuous pass is the failure mode a comparison test invites: if the tool
# ever emits an empty file, every line matches trivially.  Assert the shape.
my $tsv = lines_of("$root/docs/ir-op-inventory.tsv");
my @rows = grep { !/^#/ } @$tsv;
cmp_ok(scalar @rows, '>', 400,
       'the inventory has a plausible number of rows (one per :pcl export)');
like($rows[0], qr/^name\tkind\tfamily\tlambda_list\t/,
     'the TSV carries its column header');

# The families that ir-spec §10 states a rule for must all be REACHED by the
# name map — a §10 row with no member means the map and the doc have drifted.
my %fam;
$fam{ (split /\t/, $_)[2] }++ for @rows[1 .. $#rows];
for my $f (qw(numeric bitwise numeric-compare string string-compare logical
              assignment compound-assignment increment elements slice-delete
              aggregate-builtin regex compiled-regex io command-capture
              introspection context-frame declaration)) {
  cmp_ok($fam{$f} // 0, '>', 0, "ir-spec \x{a7}10 family '$f' has members in the inventory");
}

# UNCLASSIFIED is allowed to exist (the map is honest about what it does not
# cover) but it must not be where the vocabulary quietly ends up.
cmp_ok($fam{UNCLASSIFIED} // 0, '<', 20,
       'at most a handful of exports fall outside every family rule');

done_testing();

# --- helpers ---------------------------------------------------------------

sub run_at_root {
  my (@cmd) = @_;
  my $pid = open(my $ph, '-|');
  defined $pid or die "ir-inventory-01.t: fork: $!\n";
  if (!$pid) {                                   # child
    chdir $root or die "ir-inventory-01.t: chdir $root: $!\n";
    open STDERR, '>&', \*STDOUT;                 # keep noise out of TAP
    exec { $cmd[0] } @cmd
      or die "ir-inventory-01.t: exec $cmd[0]: $!\n";
  }
  my $o = do { local $/; <$ph> };
  close $ph;
  die "ir-inventory-01.t: @cmd failed (status $?)\n"
      . (defined $o && length $o ? "  output: $o" : '')
    if $?;
  return $o;
}

sub lines_of {
  my ($path) = @_;
  open my $fh, '<:raw', $path or die "ir-inventory-01.t: $path: $!\n";
  my @l = <$fh>;
  close $fh;
  return \@l;
}

sub first_difference {
  my ($have, $want) = @_;
  my $max = @$have > @$want ? @$have : @$want;
  for my $i (0 .. $max - 1) {
    my $h = $i < @$have ? $have->[$i] : undef;
    my $w = $i < @$want ? $want->[$i] : undef;
    return $i if !defined $h || !defined $w || $h ne $w;
  }
  return undef;
}

# '-' is the checked-in file, '+' what the tool emits now.  Position-by-
# position, not an LCS diff, so an inserted line reads as "everything below
# changed" — honest for a pointer at the first divergence.
sub diff_head {
  my ($have, $want, $at, $limit) = @_;
  my @out = ('  --- checked in / +++ regenerated ---');
  my $max = @$have > @$want ? @$have : @$want;
  my ($i, $agree) = ($at, 0);
  while ($i < $max && @out < $limit && $agree < 3) {
    my $h = $i < @$have ? $have->[$i] : undef;
    my $w = $i < @$want ? $want->[$i] : undef;
    if (defined $h && defined $w && $h eq $w) { push @out, '  ' . rtrim($h); $agree++ }
    else {
      push @out, '- ' . rtrim($h) if defined $h;
      push @out, '+ ' . rtrim($w) if defined $w;
      $agree = 0;
    }
    $i++;
  }
  return join("\n", @out);
}

sub rtrim { my ($s) = @_; $s //= ''; $s =~ s/\s+\z//; return $s }
