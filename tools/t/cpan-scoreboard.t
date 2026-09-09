#!/usr/bin/perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# tools/t/cpan-scoreboard.t — the guard for the CPAN board instrument (#1502).
#
# NOT IN THE Pl/t GATE: it forks SBCL twice through tools/run-dist-t.pl.
# Measured wall time ~5 s (`prove --timer tools/t/cpan-scoreboard.t`), which is
# far too slow for a gate that runs on every change and far too cheap to skip
# when the board tooling is touched.  Run it directly, like tools/t/tap-align.t.
#
# What it pins, over the two-file fixture dist tools/t/fixture-dist:
#   * the per-FILE table is unchanged in shape and content (PASS/PARTIAL/FAIL,
#     ok, notok, rc) — the blessed board snapshots are diffed against it;
#   * the per-ROW file carries a named failure WITH its got/expected, an
#     UNNAMED failure (the #1041 key: empty description), and the synthetic
#     `*FILE*` row that is the only way a file producing no TAP at all can
#     appear in a file of failing assertions;
#   * a registered TIMEOUT ALLOWANCE is read and announced (the board had none,
#     which is why a merely slow file read as FAIL 0/0 — #1512);
#   * --diff prints NEW / FIXED / LOST and the CAUSES line, and says
#     "LOST: NOT CHECKED" rather than nothing when the per-file tables are
#     absent — an unchecked gate must never look like a passed one.

use strict;
use warnings;
use Test::More tests => 24;
use File::Temp qw(tempdir);
use FindBin;
use lib "$FindBin::RealBin/../lib";
use PCLTimeouts ();
use PCLTap ();

my $root = "$FindBin::RealBin/../..";
my $dist = "$FindBin::RealBin/fixture-dist";
my $tmp  = tempdir(CLEANUP => 1);

# ── the registry reader (shared with tools/run-perl-suite.pl) ───────────────
{
  my $reg_file = "$tmp/timeouts.tsv";
  open my $fh, '>', $reg_file or die;
  print $fh "# a comment\n\n";
  print $fh "fixture-dist/rows.t\t200\tfixture allowance\n";
  print $fh "bogus/line-without-seconds\tnot-a-number\tignored\n";
  close $fh;
  my $reg = PCLTimeouts::read_timeouts($reg_file);
  is_deeply([sort keys %$reg], ['fixture-dist/rows.t'], 'registry: comments, blanks and non-numeric rows are skipped');
  is($reg->{'fixture-dist/rows.t'}{cause}, 'fixture allowance', 'registry: the cause column is kept');
  is(PCLTimeouts::timeout_for($reg, 'fixture-dist/rows.t', 120), 200, 'allowance wins over a smaller default');
  is(PCLTimeouts::timeout_for($reg, 'fixture-dist/rows.t', 500), 500, 'a bigger --timeout still wins (max, not override)');
  is(PCLTimeouts::timeout_for($reg, 'fixture-dist/other.t', 120), 120, 'an unregistered file gets the default');
  is_deeply(PCLTimeouts::read_timeouts("$tmp/no-such-file"), {}, 'a missing registry is empty, not fatal');
}

# ── the TAP reader: the counts are the historical ones ─────────────────────
{
  my $tap = PCLTap::parse_tap(<<'TAP');
1..4
ok 1 - a
not ok 2 - b
#      got: '2'
# expected: '3'
not ok 3
not ok 4 - todo it # TODO later
TAP
  is($tap->{ok}, 1, 'ok count matches /^ok \d+/');
  is($tap->{notok}, 3, 'not-ok count counts a TODO row too (the historical rule)');
  is($tap->{rows}[0]{got} . '/' . $tap->{rows}[0]{expected}, "'2'/'3'", 'got/expected are captured');
  is($tap->{rows}[1]{desc}, '', 'an unnamed row keys on the empty description (#1041)');
  is($tap->{rows}[2]{directive}, 'TODO later', 'a directive is recorded on the row, not folded into the count');
}

# ── the run: per-file table, row file, allowance announcement ──────────────
my $reg_file = "$tmp/board-timeouts.tsv";
{
  open my $fh, '>', $reg_file or die;
  print $fh "fixture-dist/rows.t\t200\tfixture: the allowance must be announced\n";
  close $fh;
}
my ($tsv, $rows) = ("$tmp/board.tsv", "$tmp/board.rows.tsv");
my $err = "$tmp/board.err";
system("perl \Q$root/tools/cpan-scoreboard.pl\E --jobs 2 --timeout 120 "
     . "--timeouts \Q$reg_file\E --tsv \Q$tsv\E --rows \Q$rows\E \Q$dist\E "
     . ">$tmp/board.out 2>\Q$err\E") == 0 or diag("scoreboard exited $?");

my $stderr = do { local $/; open my $f, '<', $err or die; <$f> };
like($stderr, qr/timeout allowance: \s*fixture-dist\/rows\.t\s+200s\s+\(fixture: the allowance must be announced\)/,
     'the allowance in effect is announced with its cause');

my @table = do { open my $f, '<', $tsv or die; grep { /\S/ } <$f> };
chomp @table;
is_deeply(\@table,
          ["fixture-dist\trows.t\tPARTIAL\t2\t2\t0",
           "fixture-dist\tskipped.t\tFAIL\t0\t0\t0"],
          'the per-FILE table keeps its six columns and its verdicts');

my @rowlines = do { open my $f, '<', $rows or die; grep { /\S/ && !/^#/ } <$f> };
chomp @rowlines;
is(scalar(@rowlines), 3, 'three rows: the named failure, the unnamed failure, the *FILE* row');
is($rowlines[0], "fixture-dist\trows.t\t2\tfixture named fail\t'2'\t'3'",
   'a named failing assertion carries its got and expected');
is($rowlines[1], "fixture-dist\trows.t\t3\t\t\t",
   'an unnamed failing assertion keys on the empty description');
like($rowlines[2], qr/^fixture-dist\tskipped\.t\t0\t\*FILE\*\t1\.\.0 # SKIP fixture: nothing to run here \(rc=0\)\t/,
     'a file that produced NO TAP gets a *FILE* row naming why');

# ── --diff: NEW / FIXED / LOST / CAUSES ────────────────────────────────────
{
  my $base = "$tmp/base.rows.tsv";
  open my $fh, '>', $base or die;
  print $fh "# blessed\n";
  print $fh "fixture-dist\trows.t\t2\tfixture named fail\t'2'\t'3'\t#999 (a cause)\n";
  print $fh "fixture-dist\trows.t\t9\tgone now\t\t\tUNEXPLAINED\n";
  print $fh "fixture-dist\tskipped.t\t0\t*FILE*\t1..0\t\t\n";
  close $fh;
  my $out = `perl \Q$root/tools/cpan-scoreboard.pl\E --diff \Q$base\E \Q$rows\E 2>&1`;
  my $rc = $? >> 8;
  like($out, qr/^NEW\s+fixture-dist rows\.t #3\s*$/m, 'a row absent from the baseline is NEW');
  like($out, qr/^FIXED\s+fixture-dist rows\.t #9 gone now$/m, 'a baseline row the run did not produce is FIXED');
  like($out, qr/^LOST: NOT CHECKED/m, 'without the per-file tables LOST says so instead of printing nothing');
  like($out, qr/^CAUSES: 2 of 3 blessed row\(s\) have no cause/m,
       'UNEXPLAINED and an empty cause both count as cause-less');
  is($rc, 1, '--diff exits nonzero when there are NEW rows');

  # LOST needs the two per-file tables: a file that aborts earlier loses
  # passing rows without adding failing ones.
  my $board_base = "$tmp/board-base.tsv";
  open my $bf, '>', $board_base or die;
  print $bf "fixture-dist\trows.t\tPARTIAL\t5\t2\t0\n";
  print $bf "fixture-dist\tskipped.t\tFAIL\t0\t0\t0\n";
  close $bf;
  my $out2 = `perl \Q$root/tools/cpan-scoreboard.pl\E --diff \Q$base\E \Q$rows\E --board-baseline \Q$board_base\E --board-current \Q$tsv\E 2>&1`;
  like($out2, qr/^LOST\s+fixture-dist rows\.t — 3 passing row\(s\) \(5 -> 2\)$/m,
       'a file whose ok count fell reports its lost passing rows');
  like($out2, qr/^TOTAL passing: baseline 5, current 2$/m, 'the run prints the passing TOTAL both ways');
}
