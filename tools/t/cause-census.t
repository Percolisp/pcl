#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# tools/cause-census.pl — the failure-cause CLASS census (task #1782, s486a).
# Fixture baselines in a tempdir, one row per class, so the six-class rule and
# the two weightings are checked without reading the real 472k-row population.
#
# NOT part of the Pl/t gate — it measures a measurement tool.  Run it directly:
#   prove tools/t/cause-census.t
use strict;
use warnings;
use Test::More;
use File::Temp qw(tempdir);
use File::Path qw(make_path);
use FindBin qw($RealBin);

my $TOOL = "$RealBin/../cause-census.pl";
ok(-e $TOOL, 'the tool is where STATUS.md cites it');

my $root = tempdir(CLEANUP => 1);
make_path("$root/baselines", "$root/.faillog");

sub spew {
  my ($path, @lines) = @_;
  open my $fh, '>:raw', $path or die "write $path: $!";
  print $fh "$_\n" for @lines;
  close $fh;
  return $path;
}
sub run_census {
  my (@args) = @_;
  my $out = `$^X \Q$TOOL\E --root \Q$root\E @args 2>&1`;
  return ($out, $? >> 8);
}

# ── the fixture: one row per class, plus the two rows the RULE is about ─────
# The sweep baseline is keyed (file, description); the last field is the cause.
spew("$root/baselines/fail-baseline.tsv",
  '# a fixture, not a measurement',
  join("\t", 'a.t', 1, 'ns only',      "'x'", "'y'", 'NS:Warnings-gated diagnostics are absent'),
  join("\t", 'a.t', 2, 'task and ns',  "'x'", "'y'", '#221 (NS:Warnings-gated diagnostics are absent)'),
  join("\t", 'a.t', 3, 'md spelling',  "'x'", "'y'", q{not-supported.md 'mro' section - C3-only}),
  join("\t", 'a.t', 4, 'parked',       "'x'", "'y'", 'PARKED: pack/unpack (USER s485)'),
  join("\t", 'a.t', 5, 'a bug',        "'x'", "'y'", '#1452 (a) (LHS lvalues first)'),
  join("\t", 'a.t', 6, 'no cause',     "'x'", "'y'", 'UNEXPLAINED'),
  join("\t", 'a.t', 7, 'hygiene',      "'x'", "'y'", 'DECIDED "PCL has no PVBM"'),
);
spew("$root/baselines/perl-suite-fails.tsv",
  join("\t", 'op/x.t', 3, 'ok', 'not ok', 'row one', '#999 (a task)'),
  join("\t", 'op/x.t', 4, 'ok', 'not ok', 'row two', 'NS:a section'),
  join("\t", 'op/x.t', 5, 'ok', 'not ok', 'row three'),
);
spew("$root/baselines/cpan-board14-fails.tsv",
  join("\t", 'Dist-1.0', 'a.t', 2, 'named fail', '', '', '#77 (a task)'),
  join("\t", 'Dist-1.0', 'a.t', 3, 'skipped by perl too', '', '', 'PERL-SKIP: perl skips it'),
  join("\t", 'Dist-1.0', 'b.t', 0, '*FILE*', '1..0', '', 'NS:a section'),
);
spew("$root/baselines/perl-suite-expected.tsv",
  join("\t", 'mro/a.t', q{not-supported.md 'mro' section}),
);
spew("$root/baselines/perl-suite-expected-rows.tsv",
  join("\t", 'mro/a.t', '*summary*'),
  join("\t", 'mro/a.t', 'a diverging row'),
  join("\t", 'mro/orphan.t', 'a row whose FILE has no registered reason'),
);
spew("$root/baselines/row-shortfall.tsv",
  join("\t", 'perl-tests/p.t', 5, 'UNEXPLAINED'),
  join("\t", 'perl-tests/q.t', 2, 'NS:a section'),
  join("\t", 't/op/r.t', 100, '#123 (a task)'),
);

# ── KEY-weighted first: no run log, and the tool must SAY so ───────────────
{
  my ($out, $rc) = run_census();
  is($rc, 0, 'the census exits clean');
  like($out, qr/KEY-weighted/, 'with no run log the sweep is key-weighted, and says so');
  like($out, qr/perl-tests sweep\n\s+CAUSES: 6 of 7 — not-supported 3, parked 1, bug 1, other 1, unexplained 1/,
       'one row per class; BOTH the task+NS row and the not-supported.md spelling count as not-supported');
  like($out, qr/CPAN board.*\n\s+CAUSES: 3 of 3 — not-supported 1, parked 0, bug 1, other 0, perl-skip 1/s,
       'perl-skip is shown where it occurs');
  like($out, qr/shortfall: perl-tests\n\s+CAUSES: 2 of 7 /,
       'the shortfall is weighted by its COUNT column, not by files');
  like($out, qr/shortfall: perl's t\/\n\s+CAUSES: 100 of 100 /,
       "the t/ half is the rows of t/, not perl-tests/");
}

# ── the not-supported share EXCLUDES perl-skip from its denominator ────────
{
  my ($out) = run_census();
  # board: ns 1, bug 1, perl-skip 1 -> 1 of 2 = 50.0%, not 1 of 3 = 33.3%
  like($out, qr/CPAN board.*?not-supported \+ parked: 50\.0%/s,
       'perl skipping the file too means the row is not PCL\'s to answer');
}

# ── XDIFF: not-supported by construction; a row whose file has no reason ───
{
  my ($out) = run_census();
  like($out, qr/companion XDIFF rows\n\s+CAUSES: 2 of 3 — not-supported 2, parked 0, bug 0, other 0, unexplained 1/,
       'each XDIFF row carries its FILE\'s reason; a file with no reason leaves its rows unexplained');
}

# ── ROW-weighted: a run log joins on (file, description) ───────────────────
{
  spew("$root/.faillog/a.t.fails.tsv",
    join("\t", 'a.t', 1, 'ns only',   "'x'", "'y'"),
    join("\t", 'a.t', 1, 'ns only',   "'x'", "'y'"),   # the same KEY twice: two ROWS
    join("\t", 'a.t', 5, 'a bug',     "'x'", "'y'"),
    join("\t", 'a.t', 9, 'not blessed', "'x'", "'y'"), # log/baseline drift
  );
  my ($out) = run_census();
  like($out, qr/ROW-weighted/, 'with a log present the sweep is row-weighted');
  like($out, qr/perl-tests sweep\n\s+CAUSES: 3 of 3 — not-supported 2, parked 0, bug 1, other 0, unexplained 0/,
       'a key standing for two failing rows counts twice');
  like($out, qr/1 run row\(s\) have no baseline row \(log\/baseline drift\)/,
       'a run row with no blessed row is reported as drift, never folded into a class');
  like($out, qr/joined to 7 blessed lines/, 'the join says what it joined');
  like($out, qr/\d{4}-\d\d-\d\d/, 'the log DATE is printed — it is the date of the measurement');
}

# ── the registry column: NOT COUNTED unless it is there ────────────────────
{
  my ($out) = run_census();
  like($out, qr/registry: NOT COUNTED \(no \Q$root\E\/\.faillog\/_status\.tsv\)/,
       'no _status.tsv at all: NOT COUNTED, never zero');

  spew("$root/.faillog/_status.tsv",
    join("\t", 'a.t', 'OK', 10, 3, 13, 0, 0, 0, 0, ''));
  ($out) = run_census();
  like($out, qr/registry: NOT COUNTED \(no registry column/,
       'a _status.tsv without the column says so — a skip-registry relabel is a not-supported FAILURE');

  spew("$root/.faillog/_status.tsv",
    "# name\tstatus\tpass\tfail\tplanned\tdrops\tchild-drops\tshortfall\tunrun\tnote\tregistry",
    join("\t", 'a.t', 'OK', 10, 3, 13, 0, 0, 0, 0, '', 17),
    join("\t", 'b.t', 'OK', 10, 0, 10, 0, 0, 0, 0, '', 3));
  ($out) = run_census();
  like($out, qr/registry: 20 skip-registry relabel\(s\) in the sweep \(2 file\(s\)\)/,
       'a named registry column is summed, and named as not-supported failures');

  # The sweep's OWN shape (s486b): no header line; registry-skips is the
  # ELEVENTH field, after the tab-free `note`, registry-stale the twelfth.
  spew("$root/.faillog/_status.tsv",
    join("\t", 'a.t', 'OK', 10, 3, 13, 0, 0, 0, 0, '', 17, 0),
    join("\t", 'b.t', 'OK', 10, 0, 10, 0, 0, 0, 0, '', 3, 1),
    join("\t", 'c.t', 'OK', 10, 0, 10, 0, 0, 0, 0, ''));
  ($out) = run_census();
  like($out, qr/registry: 20 skip-registry relabel\(s\) in the sweep \(2 file\(s\)\)/,
       'the positional eleventh column (the runner\'s own layout) is summed; a row without it is not counted');
}

# ── --hygiene lists every `other` row, because the class must trend to zero ─
{
  # the `other` row has to be IN the run for the row-weighted census to see it
  spew("$root/.faillog/b.t.fails.tsv",
    join("\t", 'a.t', 7, 'hygiene', "'x'", "'y'"));
  my ($out) = run_census('--hygiene');
  like($out, qr/HYGIENE — 1 `other` row\(s\)/, 'the hygiene list is the `other` class');
  like($out, qr/DECIDED "PCL has no PVBM"/, 'and it quotes the cause that cites nothing citable');
}

# ── --markdown is the STATUS.md table ──────────────────────────────────────
{
  my ($out) = run_census('--markdown');
  like($out, qr/^\| population \| rows \| not-supported \|/m, 'a markdown header row');
  like($out, qr/^\|---\|---:\|/m, 'with an alignment row');
  like($out, qr/^\| perl-tests sweep \| 4 \| 2 \| 0 \| 1 \| 1 \| 0 \| 0 \| 50\.0% \|$/m,
       'one row per population, counts and share');
  like($out, qr/^\| \*\*all populations\*\* \|/m, 'and a total row');
  unlike($out, qr/CAUSES:/, 'the markdown form is a table, not the console report');
}

# ── a missing baseline is NO INFORMATION, never a clean zero ───────────────
{
  unlink "$root/baselines/cpan-board14-fails.tsv";
  my ($out) = run_census();
  like($out, qr/CPAN board \(14 dists\)\s+BASELINE ABSENT/,
       'an absent baseline says so instead of contributing zeros');
}

done_testing();
