#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# The CAUSE column on the COMPANION suite's row-level fail baseline
# (baselines/perl-suite-fails.tsv), task #1501 / #993 I3.  The sweep's
# equivalent is covered by tools/t/audit-instruments.t; this file covers the
# half that is new here: the six-field format, its ONE reader/writer
# (tools/lib/PCLCauses.pm), the CAUSES line all three runners now share, and
# the rowkey rule the column depends on — a rowkey may not contain a TAB.
#
# NOT part of the Pl/t gate: that gate measures the transpiler, this measures a
# measurement tool.  Run it directly:  prove tools/t/perl-suite-causes.t
use strict;
use warnings;
use Test::More;
use File::Temp qw(tempdir);
use FindBin qw($RealBin);
use lib "$RealBin/../lib";
use PCLCauses qw(has_cause causes_line read_fail_rows fail_row_line);
use PclTapAlign qw(rowkey_desc);

my $dir = tempdir(CLEANUP => 1);

sub spew {
  my ($path, @lines) = @_;
  open my $fh, '>:raw', $path or die "write $path: $!";
  print $fh "$_\n" for @lines;
  close $fh;
  return $path;
}
sub slurp { open my $fh, '<:raw', $_[0] or die; local $/; return <$fh> }

# ── has_cause: UNEXPLAINED is the queue's spelling, so it is NOT a cause ────
{
  ok( has_cause('#1234'),                       'a task number is a cause');
  ok( has_cause('NS:Warnings-gated diagnostics'),'a not-supported anchor is a cause');
  ok( has_cause('PARKED: pack/unpack (USER)'),  'a parking note is a cause');
  ok(!has_cause('UNEXPLAINED'),                 'UNEXPLAINED is the queue, not a cause');
  ok(!has_cause('UNEXPLAINED (see the census)'),'UNEXPLAINED with a tail is still the queue');
  ok(!has_cause(''),                            'an empty field is not a cause');
  ok(!has_cause(undef),                         'a missing column is not a cause');
}

# ── causes_line: three answers, because "0 of 0" and "N of N" differ ────────
{
  like(causes_line([], 'x.tsv'), qr/^CAUSES: NOT CHECKED — the baseline has no rows/,
       'an empty baseline says so instead of reporting a clean queue');
  like(causes_line([undef, undef], 'x.tsv'),
       qr/^CAUSES: NOT CHECKED — no cause column in x\.tsv/,
       'a baseline that predates the column says so, and names itself');
  like(causes_line(['#1', undef, 'UNEXPLAINED'], 'x.tsv'),
       qr/^CAUSES: 2 of 3 blessed row\(s\) have no cause/,
       'the queue is counted, and UNEXPLAINED counts into it');
}

# ── the six-field format: read, and its inverse ────────────────────────────
{
  my $p = spew("$dir/fails.tsv",
    '# a comment line, skipped',
    join("\t", 'op/a.t', 3, 'ok', 'not ok', 'row one'),
    join("\t", 'op/a.t', 4, 'ok', 'not ok', 'row two', '#1234 (the operator)'),
    join("\t", 'op/b.t', 9, 'ok', '(missing)', 'row three', 'UNEXPLAINED'));
  my ($meta, $cause) = read_fail_rows($p);
  is(scalar(keys %$meta), 2, 'rows group by file');
  is($meta->{'op/a.t'}[0][3], 'row one', 'the rowkey is field 5');
  is($meta->{'op/a.t'}[0][4], undef,     'a five-field row has no cause');
  is($meta->{'op/a.t'}[1][4], '#1234 (the operator)', 'a six-field row carries its cause');
  is($cause->{"op/a.t\trow two"}, '#1234 (the operator)',
     'the cause is keyed the way the ROW DIFF joins: (rel, rowkey)');
  ok(!exists $cause->{"op/a.t\trow one"}, 'an uncaused row is absent from the cause map');
  like(causes_line([ map { $_->[4] } map { @$_ } values %$meta ], $p),
       qr/CAUSES: 2 of 3 /, 'the runner counts the baseline, not the run');

  # The inverse: five fields when there is no cause, so the column's arrival
  # rewrites NOTHING.  This is the property that let the real 18,336-row
  # baseline gain the column with a 3-row diff.
  is(fail_row_line('op/a.t', [3, 'ok', 'not ok', 'row one', undef]),
     join("\t", 'op/a.t', 3, 'ok', 'not ok', 'row one') . "\n",
     'an uncaused row round-trips to FIVE fields, byte-identical');
  is(fail_row_line('op/a.t', [4, 'ok', 'not ok', 'row two', '#1234']),
     join("\t", 'op/a.t', 4, 'ok', 'not ok', 'row two', '#1234') . "\n",
     'a caused row writes six');

  my $round = join('', map { my $rel = $_; map { fail_row_line($rel, $_) }
                             @{ $meta->{$rel} } } sort keys %$meta);
  is($round, join('', grep { !/^#/ } map { "$_\n" } split /\n/, slurp($p)),
     'read then write is the identity on every row of the fixture');
}

# ── a MISSING baseline is no information, never "nothing is wrong" ─────────
{
  my ($meta, $cause) = read_fail_rows("$dir/does-not-exist.tsv");
  is_deeply($meta, {}, 'a missing baseline reads as no rows');
  is_deeply($cause, {}, 'and no causes');
}

# ── the rowkey may not contain a TAB (the column depends on it) ────────────
# Three blessed rows carried one: perl runs the re_tests HEADER line as a test.
# The five-field parse survived them only because the key was LAST.
{
  is(rowkey_desc("pat\tstring\ty/n/etc\texpr", undef),
     'pat string y/n/etc expr',
     'a TAB in a description is normalized away — it is the field separator');
  is(rowkey_desc('plain description', undef), 'plain description',
     'a description without one is untouched');
  my $key = rowkey_desc("a\tb", undef);
  my ($m) = read_fail_rows(spew("$dir/tabkey.tsv",
     join("\t", 'op/c.t', 1, 'ok', 'not ok', $key, '#9999')));
  is($m->{'op/c.t'}[0][3], 'a b',   'so the key survives a round trip with a cause after it');
  is($m->{'op/c.t'}[0][4], '#9999', 'and the cause is not eaten by the key');
}

# ── the real baselines: the format the runner will actually read ──────────
{
  my $real = "$RealBin/../../baselines/perl-suite-fails.tsv";
  SKIP: {
    skip 'no checked-in companion fail baseline', 4 unless -e $real;
    my ($meta, $cause) = read_fail_rows($real);
    my @keys = map { @$_ } values %$meta;
    ok(scalar(@keys) > 1000, 'the blessed baseline reads as thousands of rows');
    is(scalar(grep { $_->[3] =~ /\t/ } @keys), 0,
       'no blessed rowkey contains a TAB — the cause column would eat it');
    # Read the file RAW as well: a tabbed rowkey does not show up in the
    # parsed key (the six-field split hands the tail to the CAUSE instead),
    # so the invariant has to be stated about the LINE.
    my @wide = grep { !/^#/ && /\S/ && (() = /\t/g) > 5 }
               split /\n/, slurp($real);
    is(scalar(@wide), 0,
       'every blessed line has at most six fields — five plus the cause');
    like(causes_line([ map { $_->[4] } @keys ], $real),
         qr/^CAUSES: (?:\d+ of \d+ blessed row|NOT CHECKED — no cause column)/,
         'the runner can report on it');
  }
}

done_testing();
