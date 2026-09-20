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
use PCLCauses qw(has_cause causes_line read_fail_rows fail_row_line
                 cause_class class_census census_line);
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
       qr/^CAUSES: 1 of 3 — not-supported 0, parked 0, bug 1, other 0, unexplained 2/,
       'the third answer is the class split (s486a); UNEXPLAINED counts into it');
  like(causes_line(['#1', undef, 'UNEXPLAINED'], 'x.tsv'),
       qr/\(unexplained = 2 cause-less blessed row\(s\): QUEUE, not baseline/,
       'the queue claim the three runners have always made is still on the line');
}

# ── cause_class: THE RULE, one row per class (s486a) ────────────────────────
{
  is(cause_class('NS:Warnings-gated diagnostics are absent'), 'not-supported',
     'an NS: anchor is a not-supported.md section');
  is(cause_class('#221 (NS:Warnings-gated diagnostics are absent)'), 'not-supported',
     'a row naming BOTH a task and a section is not-supported — the task owns the residue');
  is(cause_class("not-supported.md \x{a7}mro \x{2014} C3-only"), 'not-supported',
     'the perl-suite-expected.tsv spelling counts too');
  is(cause_class('PARKED: pack/unpack (USER s485)'), 'parked', 'PARKED: is its own class');
  is(cause_class('#1452 (a) (LHS lvalues must be resolved first)'), 'bug', 'a bare task is the queue');
  is(cause_class('PERL-SKIP: perl skips this file too'), 'perl-skip', 'the board class');
  is(cause_class('DECIDED "PCL has no PVBM"'), 'other', 'a cause citing nothing citable is HYGIENE');
  is(cause_class('UNEXPLAINED'), 'unexplained', 'UNEXPLAINED is the queue, not a cause');
  is(cause_class(undef), 'unexplained', 'a missing column is unexplained, never other');

  my $n = class_census(['NS:x', 'PARKED: y', '#12', 'DECIDED z', undef, 'PERL-SKIP']);
  is_deeply($n, { 'not-supported' => 1, parked => 1, bug => 1, other => 1,
                  unexplained => 1, 'perl-skip' => 1 },
            'class_census reports every class, zeros included');
  is(census_line('CAUSES', $n),
     "CAUSES: 5 of 6 — not-supported 1, parked 1, bug 1, other 1, perl-skip 1, unexplained 1\n",
     'census_line is the ONE formatter; perl-skip shows only where it occurs');
  is(census_line('CAUSES', class_census([('#1') x 1234, ('UNEXPLAINED') x 2])),
     "CAUSES: 1,234 of 1,236 — not-supported 0, parked 0, bug 1,234, other 0, unexplained 2\n",
     'thousands are commified so a five-figure population stays readable');
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
       qr/CAUSES: 1 of 3 — /, 'the runner counts the baseline, not the run');

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
    skip 'no checked-in companion fail baseline', 5 unless -e $real;
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
    # A blessed key is what the ONE projection produces, or it can never
    # match: 170 rows written on 2026-09-13 carried this machine's ABSOLUTE
    # perl build path (`[at /home/…/perl-5.40.3/t/re/subst.t line 119]`)
    # where every run produces `[at t/re/subst.t line 119]`, so each was a
    # phantom NEW ROW plus a phantom FIXED ROW on every companion run for a
    # week (#1966).  Stated WITHOUT knowing any machine's t/ directory: no
    # key may hold an absolute path into a perl source tree's t/, and the
    # projection must be the identity on every blessed key.
    my @abs = grep { $_->[3] =~ m{/[^\s'"]*/t/(?:op|re|io|uni|comp|cmd|base|lib|mro|run|opbasic|porting|class|win32|bigmem|japh|perf|test_pl|benchmark)/[\w.-]+\.t\b}
                       && $_->[3] =~ m{(?:^|[\s\['"(])/} } @keys;
    my @moved = grep { rowkey_desc($_->[3], undef) ne $_->[3] } @keys;
    is(scalar(@abs) + scalar(@moved), 0,
       'every blessed rowkey is already in the projected spelling — no absolute t/ path, no unnormalised address')
      or diag(join "\n", map { $_->[3] } (@abs, @moved)[0 .. 4]);
    like(causes_line([ map { $_->[4] } @keys ], $real),
         qr/^CAUSES: (?:[\d,]+ of [\d,]+ — not-supported |NOT CHECKED — no cause column)/,
         'the runner can report on it');
  }
}

done_testing();
