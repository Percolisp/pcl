#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

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
use PclTapAlign qw(tap_rows align_taps rowkey_desc);

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

# ------------------------------------------------- rowkey_desc (I1/#185 key)
# The BASELINE half of the same contract: a row baseline is keyed by perl's
# DESCRIPTION, so a description carrying a per-RUN token can never be blessed —
# it reads NEW + FIXED every run.  Two such tokens, both perl's own.
{
  is(rowkey_desc("correct error message for require 'tmp_JVEJ_B.ph'"),
     "correct error message for require 'tmp_TMPFILE.ph'",
     't/test.pl tempfile() names encode the PID — normalized (op/require_errors.t, 4 rows)');
  is(rowkey_desc('correct error message for require ::tmp_CIFV_B'),
     'correct error message for require ::tmp_TMPFILE',
     'the same name in every spelling the file uses');
  is(rowkey_desc('CODE(0x63ec642bcf00) is not CODE(0x63ec642bd100)'),
     'CODE(0xADDR) is not CODE(0xADDR)',
     "a reference stringification's address is normalized (comp/proto.t:77)");
  is(rowkey_desc('Foo=HASH(0xdeadbeef) blessed'), 'Foo=HASH(0xADDR) blessed',
     'a BLESSED stringification too');
  is(rowkey_desc('0x80000000 is a single character'),
     '0x80000000 is a single character',
     'a hex CONSTANT in a description is STABLE and keeps its text (op/index.t)');
  is(rowkey_desc('tmpfile is fine'), 'tmpfile is fine',
     'a word that merely starts with tmp is not a tempfile name');
  is(rowkey_desc('[at /build/perl-5.40.3/t/op/closure.t line 653]', '/build/perl-5.40.3/t'),
     '[at t/op/closure.t line 653]',
     "this machine's build path is stripped, the stable line number kept (#217)");
  is(rowkey_desc("trailing space   "), 'trailing space',
     'trailing whitespace is not part of the key');
  is(rowkey_desc(undef), '', 'an undef description keys as the empty string, never dies');
}

done_testing();
