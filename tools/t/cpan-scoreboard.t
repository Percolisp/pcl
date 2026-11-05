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
#   * a file that DIES after row 1, short of its plan, reads PARTIAL and gets
#     the synthetic `*PLAN*` row (#3241: it used to read PASS 1/0);
#   * a registered TIMEOUT ALLOWANCE is read and announced (the board had none,
#     which is why a merely slow file read as FAIL 0/0 — #1512);
#   * --diff prints NEW / FIXED / LOST and the CAUSES line, and says
#     "LOST: NOT CHECKED" rather than nothing when the per-file tables are
#     absent — an unchecked gate must never look like a passed one.

use strict;
use warnings;
use Test::More tests => 60;
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

  # The same file shape carries any per-file NUMERIC allowance with a cause —
  # baselines/perl-suite-heap.tsv says MB of SBCL dynamic space (task #1590) —
  # so read_allowances is the ONE parse and read_timeouts is it with the number
  # named `secs`.  Both readings of the same fixture must agree.
  my $gen = PCLTimeouts::read_allowances($reg_file);
  is_deeply([sort keys %$gen], ['fixture-dist/rows.t'], 'read_allowances: same rows, same skipping');
  is($gen->{'fixture-dist/rows.t'}{n}, 200, 'read_allowances names the number `n` (MB, seconds, ...)');
  is($gen->{'fixture-dist/rows.t'}{cause}, 'fixture allowance', 'read_allowances keeps the cause');
  is($reg->{'fixture-dist/rows.t'}{secs}, $gen->{'fixture-dist/rows.t'}{n},
     'read_timeouts is read_allowances with the number named `secs`');
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

# ── a subtest's INDENTED stream is the child's: counted nowhere (#1576) ────
# perl's TAP::Parser counts only top-level rows; the child's rows, its plan and
# its diagnostics are indented and belong to the parent's ONE row.
{
  my $tap = PCLTap::parse_tap(<<'TAP');
1..2
# Subtest: inner
    1..2
    not ok 1 - inner bad
    #      got: '1'
    ok 2 - inner good
not ok 1 - inner
# Subtest: skipper
    1..0 # SKIP why
ok 2 # skip why
TAP
  is("$tap->{ok}/$tap->{notok}/$tap->{plan}", '1/1/2', 'subtest: only top-level rows and the top-level plan count');
  is($tap->{rows}[0]{desc} . '|' . $tap->{rows}[0]{got}, 'inner|', 'subtest: the parent row is the failing row; a child diagnostic is not attached to it');
  ok(!defined $tap->{skip_all}, 'subtest: a child skip_all is not the file\'s skip_all');
}

# ── the exit status a TAP run ends with (#3651) ─────────────────────────────
# Test::Builder's END: the failures (at most 254), else 255 for a missed plan,
# else 254 for rows with no plan; a status announced as the run's OWN (a die,
# an `exit N') is never the verdict.
is(PCLTap::test_builder_exit(3, 3, 2), 2,     'TB exit: a met plan with 2 failures exits 2');
is(PCLTap::test_builder_exit(300, 300, 300), 254, 'TB exit: failures are capped at 254');
is(PCLTap::test_builder_exit(1, 3, 0), 255,   'TB exit: a missed plan exits 255');
is(PCLTap::test_builder_exit(1, undef, 0), 254, 'TB exit: rows with no plan exit 254');
is(PCLTap::test_builder_exit(2, 2, 0), 0,     'TB exit: a clean run exits 0');
ok(PCLTap::exit_is_verdict("1..3\nok 1\n", 255, 1, 3, 0), 'verdict: 255 for a short plan is the TAP verdict');
ok(!PCLTap::exit_is_verdict("ok 1\n# Looks like your test exited with 255 just after 1.\n", 255, 1, 3, 0),
   'verdict: an announced entry status is not (Test::Builder\'s words)');
ok(!PCLTap::exit_is_verdict("ok 1\n# PCL-EXIT status=255 after=1\n", 255, 1, 3, 0),
   'verdict: an announced entry status is not (the t/test.pl fact line)');
is(PCLTap::parse_tap("1..2\nnot ok 1 # TODO x\nnot ok 2\n")->{failed}, 1, 'parse_tap: failed counts the not-ok rows without a TODO');

# ── the run: per-file table, row file, allowance announcement ──────────────
my $reg_file = "$tmp/board-timeouts.tsv";
{
  open my $fh, '>', $reg_file or die;
  print $fh "fixture-dist/rows.t\t200\tfixture: the allowance must be announced\n";
  close $fh;
}
my ($tsv, $rows) = ("$tmp/board.tsv", "$tmp/board.rows.tsv");
my $err = "$tmp/board.err";
# --no-oracle: this run pins the verdicts WITHOUT a perl oracle (a file PCL
# skips reads FAIL, as it always did); the oracle runs are further down.
system("perl \Q$root/tools/cpan-scoreboard.pl\E --no-oracle --jobs 2 --timeout 120 "
     . "--timeouts \Q$reg_file\E --tsv \Q$tsv\E --rows \Q$rows\E \Q$dist\E "
     . ">$tmp/board.out 2>\Q$err\E") == 0 or diag("scoreboard exited $?");

my $stderr = do { local $/; open my $f, '<', $err or die; <$f> };
like($stderr, qr/timeout allowance: \s*fixture-dist\/rows\.t\s+200s\s+\(fixture: the allowance must be announced\)/,
     'the allowance in effect is announced with its cause');

my @table = do { open my $f, '<', $tsv or die; grep { /\S/ } <$f> };
chomp @table;
is_deeply(\@table,
          ["fixture-dist\tdies.t\tPARTIAL\t1\t0\t0",
           "fixture-dist\trows.t\tPARTIAL\t2\t2\t0",
           "fixture-dist\tskipped.t\tFAIL\t0\t0\t0"],
          'the per-FILE table keeps its six columns and its verdicts');

my @rowlines = do { open my $f, '<', $rows or die; grep { /\S/ && !/^#/ } <$f> };
chomp @rowlines;
is(scalar(@rowlines), 4, 'four rows: the *PLAN* row, the named failure, the unnamed failure, the *FILE* row');
is($rowlines[0], "fixture-dist\tdies.t\t0\t*PLAN*\tran 1 of 3, rc 255\t3 row(s)",
   'a file that dies after row 1 gets a *PLAN* row: ran M of N, rc R (#3241)');
is($rowlines[1], "fixture-dist\trows.t\t2\tfixture named fail\t'2'\t'3'",
   'a named failing assertion carries its got and expected');
is($rowlines[2], "fixture-dist\trows.t\t3\t\t\t",
   'an unnamed failing assertion keys on the empty description');
like($rowlines[3], qr/^fixture-dist\tskipped\.t\t0\t\*FILE\*\t1\.\.0 # SKIP fixture: nothing to run here \(rc=0\)\t/,
     'a file that produced NO TAP gets a *FILE* row naming why');
unlike(join("\n", @rowlines), qr/\trows\.t\t0\t\*PLAN\*/,
       'a file that met its plan gets no *PLAN* row, failing rows or not (it exits with the TAP verdict, #3651)');

# The SBCL child's WAIT STATUS is reported, so a run KILLED by a signal (the
# OOM killer on a loaded box) can be told apart from a run that finished and
# printed nothing.  Only the REPORTING half is asserted here — a killed child
# is not reproducible in a fixture; the retry it feeds is a serial re-run of
# exactly the rows whose signal is nonzero.
my $rows_out = `perl \Q$root/tools/run-dist-t.pl\E --rows \Q$dist\E t/rows.t 2>/dev/null`;
like($rows_out, qr/^SBCL\t2\t0\t1$/m, 'run-dist-t --rows reports the SBCL exit code (rows.t: 2 failures = Test::Builder\'s 2, #3651), signal and verdict');

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
  like($out, qr/^CAUSES: 1 of 3 — not-supported 0, parked 0, bug 1, other 0, unexplained 2/m,
       'UNEXPLAINED and an empty cause both count as cause-less (class split, s486a)');
  is($rc, 1, '--diff exits nonzero when there are NEW rows');

  # LOST needs the two per-file tables: a file that aborts earlier loses
  # passing rows without adding failing ones.
  my $board_base = "$tmp/board-base.tsv";
  open my $bf, '>', $board_base or die;
  print $bf "fixture-dist\tdies.t\tPASS\t1\t0\t0\n";
  print $bf "fixture-dist\trows.t\tPARTIAL\t5\t2\t0\n";
  print $bf "fixture-dist\tskipped.t\tFAIL\t0\t0\t0\n";
  close $bf;
  my $out2 = `perl \Q$root/tools/cpan-scoreboard.pl\E --diff \Q$base\E \Q$rows\E --board-baseline \Q$board_base\E --board-current \Q$tsv\E 2>&1`;
  like($out2, qr/^LOST\s+fixture-dist rows\.t — 3 passing row\(s\) \(5 -> 2\)$/m,
       'a file whose ok count fell reports its lost passing rows');
  like($out2, qr/^TOTAL passing: baseline 6, current 3$/m, 'the run prints the passing TOTAL both ways');
}

# ── --diff's UNVERIFIED bucket and the moved *PLAN* (#4382) ─────────────────
# dies.t stops after row 1 in the current run (its *PLAN* row), so a blessed
# failing row 2 there was never REACHED: not fixed.  A file the current
# per-file table does not list was not run.  A *PLAN* row whose shortfall
# text moved says so.  (The s532 round: 16 of 18 FIXED were such rows.)
{
  my $base = "$tmp/unver.rows.tsv";
  open my $fh, '>', $base or die;
  print $fh "fixture-dist\tdies.t\t0\t*PLAN*\tran 2 of 3, rc 255\t3 row(s)\t#999 (a cause)\n";
  print $fh "fixture-dist\tdies.t\t2\tdies late\t\t\t#999 (a cause)\n";
  print $fh "fixture-dist\tabsent.t\t1\tnot run\t\t\t#999 (a cause)\n";
  print $fh "fixture-dist\trows.t\t9\tgone now\t\t\t#999 (a cause)\n";
  close $fh;
  my $out = `perl \Q$root/tools/cpan-scoreboard.pl\E --diff \Q$base\E \Q$rows\E --board-baseline \Q$tsv\E --board-current \Q$tsv\E 2>&1`;
  like($out, qr/^\?\s+fixture-dist dies\.t: 1 blessed row\(s\) not reached \(the file ran 1 row\(s\)\)/m,
       'a blessed row above the row a stopped file reached is UNVERIFIED, never FIXED');
  like($out, qr/^\?\s+fixture-dist absent\.t: 1 blessed row\(s\) not reached \(file not in the current board\)/m,
       'a blessed row of a file the current board did not run is UNVERIFIED');
  like($out, qr/^MISMATCH \*PLAN\* fixture-dist dies\.t: blessed 'ran 2 of 3, rc 255', now 'ran 1 of 3, rc 255'$/m,
       'a *PLAN* row whose shortfall text moved prints a MISMATCH line');
  like($out, qr/^3 NEW \/ 1 FIXED \/ 2 UNVERIFIED \/ 0 LOST$/m,
       'only the row of a file that ran its plan is FIXED; the summary counts UNVERIFIED');
}

# ── --dist-serial (#4381): a dist whose t-files depend on an EARLIER file's
# side effect (Template-Toolkit compile2.t reads the cache compile1.t writes)
# runs its files one at a time, in order, on two workers too.  b.t passes
# only when a.t (which sleeps first) has finished before b.t starts.
{
  my $sd = "$tmp/serial-dist";
  mkdir $sd; mkdir "$sd/t";
  open my $a, '>', "$sd/t/a.t" or die;
  print $a "use Test::More tests => 1;\nsleep 3;\nopen my \$f, '>', 't/marker' or die; close \$f;\nok(1, 'a wrote the marker');\n";
  close $a;
  open my $b, '>', "$sd/t/b.t" or die;
  print $b "use Test::More tests => 1;\nok(-e 't/marker', 'a.t ran to its end first');\nunlink 't/marker';\n";
  close $b;
  my ($st, $sr) = ("$tmp/serial.tsv", "$tmp/serial.rows.tsv");
  system("perl \Q$root/tools/cpan-scoreboard.pl\E --no-oracle --jobs 2 --timeout 120 "
       . "--tsv \Q$st\E --rows \Q$sr\E --dist-serial \Q$sd\E >$tmp/serial.out 2>&1") == 0
    or diag("scoreboard exited $?");
  my @t = do { open my $f, '<', $st or die; grep { /\S/ && !/^#/ } <$f> };
  chomp @t;
  is_deeply(\@t, ["serial-dist\ta.t\tPASS\t1\t0\t0", "serial-dist\tb.t\tPASS\t1\t0\t0"],
            '--dist-serial: the dist\'s files ran one at a time, in order (b.t saw a.t\'s marker)');
  # --dist-serial=REGEX: only the matching family is a chain (TT: ^compile).
  open my $c, '>', "$sd/t/c.t" or die;
  print $c "use Test::More tests => 1;\nok(1, 'c is independent');\n";
  close $c;
  system("perl \Q$root/tools/cpan-scoreboard.pl\E --no-oracle --jobs 2 --timeout 120 "
       . "--tsv \Q$st\E --rows \Q$sr\E --dist-serial=^[ab] \Q$sd\E >$tmp/serial2.out 2>&1") == 0
    or diag("scoreboard exited $?");
  @t = do { open my $f, '<', $st or die; grep { /\S/ && !/^#/ } <$f> };
  chomp @t;
  is_deeply(\@t, ["serial-dist\ta.t\tPASS\t1\t0\t0", "serial-dist\tb.t\tPASS\t1\t0\t0",
                  "serial-dist\tc.t\tPASS\t1\t0\t0"],
            '--dist-serial=REGEX: the matching files still run in order; the rest run beside them');
}

# ── THE PERL ORACLE, the SKIP verdict, MISMATCH (s531b, #4160 / ruling 4) ──
# The oracle is tools/t/fixture-oracle-perl: perl itself, except that with
# STUB_PERL_RUNS=1 it RUNS skipped.t (two rows).  Each run has its own oracle
# cache, so one run's answer is never another's.
{
  my $stub = "$FindBin::RealBin/fixture-oracle-perl";
  my $causes = "$tmp/causes.rows.tsv";
  open my $cf, '>', $causes or die;
  print $cf "fixture-dist\tskipped.t\t0\t*FILE*\t1..0\t\tPERL-SKIP (the fixture skips itself)\n";
  print $cf "fixture-dist\trows.t\t2\tfixture named fail\t'2'\t'3'\tPERL-FAILS (perl fails it too)\n";
  print $cf "fixture-dist\trows.t\t0\tfixture named pass\t\t\tPERL-FAILS (a stale claim: perl passes it)\n";
  print $cf "fixture-dist\trows.t\t3\t\t\t\t#77 (an unnamed failure)\n";
  print $cf "fixture-dist\tdies.t\t0\t*PLAN*\tran 1 of 3, rc 255\t3 row(s)\tNS:ns-xs (a ruled whole-file row)\n";
  close $cf;
  my $board = sub {
    my ($tag, %env) = @_;
    local @ENV{keys %env} = values %env;
    local $ENV{PCL_ORACLE_CACHE} = "$tmp/oracle-$tag";
    my $out = `perl \Q$root/tools/cpan-scoreboard.pl\E --jobs 2 --oracle-perl \Q$stub\E --causes \Q$causes\E --tsv \Q$tmp/$tag.tsv\E --rows \Q$tmp/$tag.rows.tsv\E \Q$dist\E 2>&1`;
    my @t = do { open my $f, '<', "$tmp/$tag.tsv" or die; grep { /\S/ } <$f> };
    chomp @t;
    return ($out, \@t);
  };

  my ($out, $t) = $board->('skip');
  is((grep { /\tskipped\.t\t/ } @$t)[0], "fixture-dist\tskipped.t\tSKIP\t0\t0\t0",
     'SKIP: PCL prints 1..0 # SKIP and the perl oracle skips the file too');
  like($out, qr/^  -- 0 PASS \/ 2 PARTIAL \/ 0 FAIL \/ 1 SKIP of 3$/m, 'SKIP has its own column in the per-dist line');
  like($out, qr/^== TOTAL: 0 PASS \/ 2 PARTIAL \/ 0 FAIL \/ 1 SKIP of 3 file\(s\); 3 ok, 2 not ok$/m, '... and in the totals');
  my @rl = do { open my $f, '<', "$tmp/skip.rows.tsv" or die; grep { /\S/ && !/^#/ } <$f> };
  ok((grep { /\tskipped\.t\t0\t\*FILE\*\t/ } @rl), 'a SKIP file still gets its *FILE* row (the blessed PERL-SKIP row is its record)');
  like($out, qr/not ok rows: 1 ours \/ 0 ruled \/ 1 perl's \/ 0 harness \/ 0 unblessed; \+ 2 assertion\(s\) never reached behind 1 whole-file row\(s\) \(0 of them ours\)/,
       'the split: each failing row by its blessed cause; a SKIP file hides nothing; ran 1 of 3 hides 2');
  like($out, qr/^MISMATCH PERL-FAILS fixture-dist rows\.t "fixture named pass": perl's row is ok$/m,
       'MISMATCH fires for a PERL-FAILS claim perl\'s own run contradicts');
  unlike($out, qr/MISMATCH PERL-SKIP/, 'no MISMATCH for a PERL-SKIP claim perl confirms');
  like($out, qr/^ORACLE: 3 file\(s\) -- 0 cached, 3 measured/m, 'the oracle reports what it measured');

  ($out, $t) = $board->('runs', STUB_PERL_RUNS => 1);
  is((grep { /\tskipped\.t\t/ } @$t)[0], "fixture-dist\tskipped.t\tFAIL\t0\t0\t0",
     'a file PCL skips and perl RUNS reads FAIL, never SKIP');
  like($out, qr/^MISMATCH PERL-SKIP  fixture-dist skipped\.t: perl ran 2 row\(s\) \(ok 1, not ok 1\)$/m,
       'MISMATCH fires for a PERL-SKIP claim on a file perl runs');

  # --diff: the SKIP file's *FILE* row is in both files and its ok count is
  # 0 both ways, so the SKIP verdict moves neither FIXED nor LOST.
  my $bb = "$tmp/skip-base.tsv";
  open my $bf, '>', $bb or die;
  print $bf "fixture-dist\tdies.t\tPARTIAL\t1\t0\t0\nfixture-dist\trows.t\tPARTIAL\t2\t2\t0\nfixture-dist\tskipped.t\tFAIL\t0\t0\t0\n";
  close $bf;
  my $d = `perl \Q$root/tools/cpan-scoreboard.pl\E --diff \Q$causes\E \Q$tmp/skip.rows.tsv\E --board-baseline \Q$bb\E --board-current \Q$tmp/skip.tsv\E 2>&1`;
  unlike($d, qr/^(?:FIXED|LOST)\s+fixture-dist skipped\.t/m, '--diff: a SKIP file reports neither FIXED nor LOST');
}
