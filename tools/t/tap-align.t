#!/usr/bin/env perl
# Unit tests for PclTapAlign — the description-based TAP pairing that the suite
# runner's per-test failure log is built on (task #177).
#
# NOT part of the Pl/t gate: that gate measures the transpiler, this measures a
# measurement tool.  Run it directly:  prove tools/t/tap-align.t
use strict;
use warnings;
use Test::More;
use FindBin qw($RealBin);
use lib "$RealBin/../lib";
use PclTapAlign qw(tap_rows align_taps);

# Compact writer: "ok:desc" / "nok:desc", numbered from 1 in order.
sub tap {
  my $n = 0;
  join '', map { $n++; my ($v, $d) = split /:/, $_, 2;
                 sprintf("%s %d - %s\n", $v eq 'ok' ? 'ok' : 'not ok', $n, $d) } @_;
}
sub verdicts {   # [ "perlnum perlverb pclverb(pclnum)" ] for diverging pairs only
  my ($pairs) = @_;
  my @out;
  for my $pr (@$pairs) {
    my ($p, $c) = @$pr;
    my $cv = $c ? $c->{verb} : '(missing)';
    next if $p->{verb} eq $cv;
    push @out, "$p->{num} $p->{verb} -> $cv" . ($c && $c->{num} != $p->{num} ? "(#$c->{num})" : "");
  }
  return \@out;
}

# ---------------------------------------------------------------- baseline
{
  my $t = tap(qw(ok:a ok:b ok:c));
  my ($pairs, $extras) = align_taps(tap_rows($t), tap_rows(tap(qw(ok:a nok:b ok:c))));
  is_deeply(verdicts($pairs), ['2 ok -> not ok'], 'straight streams pair positionally');
  is_deeply($extras, [], 'no extras when the streams line up');
}

# ------------------------------------------------- the op/do.t shape (#177)
# PCL emits two EXTRA rows mid-file (fail() guards firing), so its numbering
# runs +2 ahead.  The rows AFTER the divergence must still be attributed to the
# right assertions — that is the whole bug.
{
  my $perl = tap(qw(ok:x ok:syn1 ok:syn2 ok:delete-copy ok:tail));
  my $pcl  = tap(qw(ok:x nok:called1 nok:syn1 nok:called2 nok:syn2 ok:delete-copy ok:tail));
  my ($pairs, $extras) = align_taps(tap_rows($perl), tap_rows($pcl));
  is_deeply(verdicts($pairs), ['2 ok -> not ok(#3)', '3 ok -> not ok(#5)'],
            'renumbered rows are attributed to the right assertions');
  is_deeply([map { $_->{desc} } @$extras], ['called1', 'called2'],
            'the PCL-only rows are reported individually, by description');
  # The two rows AFTER the shift pass in both streams and must NOT be accused.
  ok(!grep({ /delete-copy|tail/ } map { $_ } @{verdicts($pairs)}),
     'rows that pass on both sides after a shift are not reported as failures');
}

# ------------------------------------------------------ PCL skipping rows
{
  my $perl = tap(qw(ok:a ok:b ok:c ok:d));
  my $pcl  = tap(qw(ok:a ok:d));
  my ($pairs, $extras) = align_taps(tap_rows($perl), tap_rows($pcl));
  is_deeply(verdicts($pairs), ['2 ok -> (missing)', '3 ok -> (missing)'],
            'rows PCL never emitted are reported missing, not mis-paired');
  is_deeply($extras, [], 'and nothing is called extra');
}

# ------------------------------------------------------- INVERSE GUARDS
# Each of these is a way the aligner could be too clever and silently invent a
# pairing.  It must fall back to positional pairing instead.
{
  # 1. Descriptions that interpolate the compared value differ exactly when a
  #    test fails.  No re-sync evidence exists -> pair positionally.
  my $perl = tap('ok:got 7', 'ok:next');
  my $pcl  = tap('nok:got 8', 'ok:next');
  my ($pairs, $extras) = align_taps(tap_rows($perl), tap_rows($pcl));
  is_deeply(verdicts($pairs), ['1 ok -> not ok'],
            'value-interpolated descriptions still pair positionally');
  is_deeply($extras, [], 'and are never mistaken for insertions');
}
{
  # 2. Unnamed tests (no description) carry no evidence at all.
  my ($pairs) = align_taps(tap_rows(tap('ok:', 'ok:', 'ok:')),
                           tap_rows(tap('ok:', 'nok:', 'ok:')));
  is_deeply(verdicts($pairs), ['2 ok -> not ok'], 'unnamed rows pair positionally');
}
{
  # 3. A repeated description must not let a single match drag the alignment
  #    forward: with 4 identical rows on both sides, pairing stays 1:1.
  my ($pairs, $extras) = align_taps(tap_rows(tap(('ok:same') x 4)),
                                    tap_rows(tap('ok:same', 'nok:same', 'ok:same', 'ok:same')));
  is_deeply(verdicts($pairs), ['2 ok -> not ok'], 'repeated descriptions stay 1:1');
  is_deeply($extras, [], 'no spurious extras from repeated descriptions');
}
{
  # 4. A match too far ahead (past the window) is NOT evidence.
  local $PclTapAlign::WINDOW = 3;
  my $perl = tap(qw(ok:a ok:b));
  my $pcl  = tap('ok:a', (map { "ok:junk$_" } 1 .. 6), 'ok:b');
  my ($pairs, $extras) = align_taps(tap_rows($perl), tap_rows($pcl));
  is($pairs->[1][1]{desc}, 'junk1', 'out-of-window match is ignored (pairs positionally)');
  is(scalar @$extras, 6, 'the unmatched tail is reported as extras, not silently dropped');
}

done_testing();
