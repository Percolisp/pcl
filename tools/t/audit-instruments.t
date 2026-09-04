#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# Fixture-driven tests for the PHASE-0 audit instruments (task #993,
# docs/plan-test-audit-s464.md §3): the SHORTFALL baseline (I2), the CAUSE
# column (I3) and the sweep-side gate that reads them.  The companion's ROW
# DIFF (I1) is exercised through the same shortfall/status plumbing plus a
# multiset check of its rowkeys.
#
# NOT part of the Pl/t gate: that gate measures the transpiler, this measures a
# measurement tool.  Run it directly:  prove tools/t/audit-instruments.t
use strict;
use warnings;
use Test::More;
use File::Temp qw(tempdir);
use FindBin qw($RealBin);
use lib "$RealBin/../lib";
use PCLShortfall qw(read_shortfall write_shortfall);

my $SWEEP_DIFF = "$RealBin/../sweep-diff.pl";
my $dir = tempdir(CLEANUP => 1);

sub spew {
  my ($path, @lines) = @_;
  open my $fh, '>', $path or die "write $path: $!";
  print $fh "$_\n" for @lines;
  close $fh;
  return $path;
}
sub run_diff {
  my (@args) = @_;
  my $out = `$^X \Q$SWEEP_DIFF\E @{[ map { quotemeta } @args ]} 2>&1`;
  return ($out, $? >> 8);
}

# ── PCLShortfall: read/write round trip, and the cause default ──────────────
{
  my $p = "$dir/shortfall.tsv";
  write_shortfall($p, { 'perl-tests/pack.t' => { rows => 8997, cause => '#148 utf8' },
                        't/op/coresubs.t'   => { rows => 1109, cause => 'UNEXPLAINED' },
                        'perl-tests/zero.t' => { rows => 0,    cause => 'gone' } }, 'deadbeef 2026-09-03');
  my $back = read_shortfall($p);
  is(scalar(keys %$back), 2, 'a zero-row entry is not written (a fixed row leaves the baseline)');
  is($back->{'perl-tests/pack.t'}{rows}, 8997, 'shortfall row count round-trips');
  is($back->{'t/op/coresubs.t'}{cause}, 'UNEXPLAINED', 'UNEXPLAINED survives the round trip');
  like(do { open my $fh, '<', $p or die; local $/; <$fh> }, qr/^# taken-at: deadbeef/m,
       'the writer stamps the tree it was blessed from');
  is_deeply(read_shortfall("$dir/does-not-exist.tsv"), {},
            'a missing baseline is an empty hash, never a claim that nothing is missing');
}

# ── the sweep gate: shortfall is compared per file, MORE is a failure ───────
# _status.tsv columns:
#   name status pass fail planned drops child-drops shortfall unrun note
# shortfall = planned - (pass+fail); unrun = that minus the SKIPPED rows.
{
  my $log = "$dir/faillog";
  mkdir $log;
  spew("$log/_status.tsv",
       join("\t", 'pack.t',  'OK', 5725, 89, 14722, 0, 0, 8908, 44, ''),
       join("\t", 'lc.t',    'OK',   82,  0,  2659, 0, 0, 2577, 0, ''),
       join("\t", 'clean.t', 'OK',   10,  0,    10, 0, 0, 0, 0, ''));
  spew("$log/clean.fails.tsv");
  my $base = spew("$dir/fail-base.tsv");
  my $sfp  = "$dir/row-shortfall.tsv";
  write_shortfall($sfp, { 'perl-tests/pack.t' => { rows => 8908, cause => '#148' },
                          'perl-tests/lc.t'   => { rows => 2577, cause => 'UNEXPLAINED' } });
  # sweep-diff looks for row-shortfall.tsv beside the fail baseline.
  my ($out, $rc) = run_diff('diff', $base, $log);
  like($out, qr/TOTAL planned rows not asserted: baseline 11485, current 11485 \(\+0\)/,
       'a matching shortfall reports the totals and does not fail');
  like($out, qr/UNEXPLAINED shortfall: 2577 row\(s\) in 1 file\(s\)/,
       'the UNEXPLAINED rows are counted every run — that is the audit queue');
  like($out, qr/of those, 44 row\(s\) produced no TAP at all .*; 11441 were SKIPPED/,
       'the two halves are named: a stopped file and a skipped row are not the same event');
  is($rc, 0, 'a run whose shortfall equals the baseline exits clean');

  # One MORE unproduced row than blessed = a NEW shortfall = a failing run.
  spew("$log/_status.tsv",
       join("\t", 'pack.t',  'OK', 5724, 89, 14722, 0, 0, 8909, 45, ''),
       join("\t", 'lc.t',    'OK',   82,  0,  2659, 0, 0, 2577, 0, ''),
       join("\t", 'clean.t', 'OK',   10,  0,    10, 0, 0, 0, 0, ''));
  ($out, $rc) = run_diff('diff', $base, $log);
  like($out, qr/\+ pack\.t\s+8908 -> 8909 planned row\(s\) NEVER PRODUCED/,
       'a new shortfall names the file and both numbers');
  like($out, qr/NOT clean/, 'and says the run is not clean');
  is($rc, 1, 'a NEW shortfall fails the run like a NEW failure');

  # FEWER is a fix: reported, and it does NOT fail the run.
  spew("$log/_status.tsv",
       join("\t", 'pack.t',  'OK', 5725, 89, 14722, 0, 0, 8908, 44, ''),
       join("\t", 'lc.t',    'OK', 2659,  0,  2659, 0, 0, 0, 0, ''),
       join("\t", 'clean.t', 'OK',   10,  0,    10, 0, 0, 0, 0, ''));
  ($out, $rc) = run_diff('diff', $base, $log);
  like($out, qr/- lc\.t\s+2577 -> 0 planned row\(s\) — fixed; EDIT the baseline row/,
       'a fixed shortfall says the row leaves BY EDIT');
  is($rc, 0, 'a fixed shortfall does not fail the run');
}

# ── a check that could not run must SAY so, never go quiet ─────────────────
{
  my $log = "$dir/faillog2";
  mkdir $log;
  spew("$log/_status.tsv", join("\t", 'a.t', 'OK', 1, 0, 1, 0, 0, 0, ''));
  spew("$log/a.fails.tsv");
  mkdir "$dir/no-neighbours" or die "mkdir: $!";
  spew("$dir/no-neighbours/fail-base.tsv");
  # From a cwd with no baselines/ either: sweep-diff falls back to
  # `baselines/row-shortfall.tsv` relative to cwd (the drop census's rule), and
  # running this from the repo root would find the REAL one.
  my $out = `cd \Q$dir/no-neighbours\E && $^X \Q$SWEEP_DIFF\E diff \Q$dir/no-neighbours/fail-base.tsv\E \Q$log\E 2>&1`;
  like($out, qr/SHORTFALL: NOT CHECKED/,
       'no baseline beside the fail baseline reports NOT CHECKED, not silence');
}

# ── I3: the CAUSE column is read, ignored for the join, and counted ────────
{
  my $log = "$dir/faillog3";
  mkdir $log;
  spew("$log/_status.tsv", join("\t", 'x.t', 'OK', 5, 2, 7, 0, 0, 0, ''));
  # A LIVE run's rows have FIVE fields — no cause.
  spew("$log/x.fails.tsv",
       join("\t", 'x.t', 11, 'row one', "'a'", "'b'"),
       join("\t", 'x.t', 12, 'row two', "'c'", "'d'"));
  # The BLESSED baseline has six.  Same two rows, so: 0 new, 0 fixed.
  my $base = spew("$dir/fail-base-cause.tsv",
       join("\t", 'x.t', 11, 'row one', "'a'", "'b'", '#1028 (bitwise ops numify undef)'),
       join("\t", 'x.t', 12, 'row two', "'c'", "'d'", 'UNEXPLAINED'));
  my ($out, $rc) = run_diff('diff', $base, $log);
  like($out, qr/summary: 0 new, 0 fixed/,
       'the six-column baseline still joins on (file, description)');
  like($out, qr/CAUSES: 1 of 2 blessed row\(s\) have no cause/,
       'cause-less rows are counted, so the queue cannot silently grow');
  unlike($out, qr/expected=.*#1028/,
         'the cause never leaks into the expected field (the 5-vs-6 split bug)');
  is($rc, 0, 'a cause-less row is queue, not a failure');

  # A baseline with no cause column at all must say NOT CHECKED.
  my $old = spew("$dir/fail-base-nocause.tsv",
       join("\t", 'x.t', 11, 'row one', "'a'", "'b'"),
       join("\t", 'x.t', 12, 'row two', "'c'", "'d'"));
  ($out) = run_diff('diff', $old, $log);
  like($out, qr/CAUSES: NOT CHECKED — no cause column/,
       'a baseline without the column says so instead of reporting 100% cause-less');

  # `save` must WARN before it throws attributions away.
  my $err = `$^X \Q$SWEEP_DIFF\E save \Q$log\E \Q$base\E 2>&1 >/dev/null`;
  like($err, qr/carries a CAUSE column/, '`save` warns that it would lose every attribution');
}

# ── I1: the rowkey multiset is what the row diff compares ──────────────────
# The companion's row baseline is keyed (rel, PERL's description) and compared
# as a MULTISET — a description that repeats inside a file must be registered
# as many times as it diverges.  This is run-perl-suite.pl's multiset_diff
# contract, restated here so a change to it fails a test rather than a run.
{
  my ($a, $b) = ([qw(alpha alpha beta)], [qw(alpha beta gamma)]);
  my %count;
  $count{$_}++ for @$a;
  $count{$_}-- for @$b;
  my @only_a = map { ($_) x $count{$_} } grep { $count{$_} > 0 } sort keys %count;
  my @only_b = map { ($_) x -$count{$_} } grep { $count{$_} < 0 } sort keys %count;
  is_deeply(\@only_a, ['alpha'], 'a SECOND divergence of the same description is a NEW ROW');
  is_deeply(\@only_b, ['gamma'], 'a blessed row that stopped diverging is a FIXED ROW');
}

# ── I1: the *rows-unstable* opt-out (s468be, task #1082) ───────────────────
# A file whose blessed rows are exactly ONE `*rows-unstable*` entry opts out of
# the ROW check and keeps that entry through a bless (run-perl-suite.pl's
# fails_rows_unstable + bless_fail_rows).  The predicate is restated here; the
# check that MATTERS is the one below it, over the real baseline: an opt-out
# entry sitting BESIDE ordinary rows is a silent no-op — the file would still
# be row-checked and the operator would believe it was not.
{
  my $unstable = sub { my @r = @_; return (@r == 1 && $r[0] =~ /^\*rows-unstable\*/) ? 1 : 0 };
  is($unstable->('*rows-unstable* perl interpolates its own hash order'), 1,
     'one *rows-unstable* row opts the file out');
  is($unstable->('*rows-unstable* x', 'a real row'), 0,
     'an opt-out beside real rows does NOT opt out (it would be a silent no-op)');
  is($unstable->('a real row'), 0, 'an ordinary file is row-checked');
  is($unstable->(), 0, 'a file with no blessed rows is row-checked');

  my $base = "$RealBin/../../baselines/perl-suite-fails.tsv";
 SKIP: {
    skip "no $base in this checkout", 2 unless -e $base;
    my %rows;
    open my $bf, '<', $base or die "open $base: $!";
    while (<$bf>) {
      chomp; next if /^\s*(?:#|$)/;
      my ($rel, $num, $pv, $cv, $key) = split /\t/, $_, 5;
      push @{ $rows{$rel} }, $key if defined $key;
    }
    close $bf;
    my @registered = sort grep { grep { /^\*rows-unstable\*/ } @{ $rows{$_} } } keys %rows;
    my @broken = grep { !$unstable->(@{ $rows{$_} }) } @registered;
    is_deeply(\@broken, [], 'every *rows-unstable* file in the real baseline is its ONLY row');
    cmp_ok(scalar @registered, '>=', 1,
           'the baseline carries the registrations the header documents');
  }
}

done_testing();
