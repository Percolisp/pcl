#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# run-perl-suite.pl — run Perl's own core test files (t/base, t/cmd, t/comp,
# t/mro, t/class, …) through PCL and compare TAP results to real perl, with a
# crash signature.  The t/ ORIGINALS are authoritative and ALL of them run
# here, including the ones PCL also keeps a copy of in perl-tests/ (task #150
# part 1, s318): a passing corpus copy must never shadow a failing real file —
# chop/dor/not/quotemeta read as fully passing in the sweep while their t/op
# originals had 2-5 failures each.  Overlap with the sweep is harmless
# duplication; a missing t/ row is a hole in the release signal.  (task #25 /
# R1 gate; results catalogue: docs/perl-test-suite-survey.md — UPDATE that doc
# when a row changes so we don't re-investigate the same files).
#
# Usage:
#   tools/run-perl-suite.pl base/rs.t comp/our.t   # specific files (rel to t/)
#   tools/run-perl-suite.pl --dir comp             # all runnable files in t/<dir>
#   tools/run-perl-suite.pl --all                  # every runnable file in the default dirs
#   tools/run-perl-suite.pl                        # == --all
#
# Options:
#   --tdir PATH        perl build t/ tree (default: the 5.40.3 build below)
#   --dir D            add one subdir (repeatable)
#   --all              scan the default dir set (see @DEFAULT_DIRS)
#   --jobs N           parallel workers (default 8)
#   --timeout N        per-file SBCL timeout seconds (default 90); a file
#                      registered in baselines/perl-suite-timeouts.tsv gets the
#                      LARGER of its allowance and this (see that file)
#   --quick            the DEFAULT companion form (task #345): do not run the
#                      files that spend a whole timeout to produce nothing new
#                      — the #326 hang set — and do not run a file whose
#                      registered allowance exceeds the quick CAP (120 s),
#                      because it cannot finish inside it.  Every such file
#                      gets a NOT-RUN row naming which of the two reasons and
#                      its cause, so the coverage hole is visible and still
#                      fails the run; see the --quick block below.
#   --no-core          skip the saved-core fast path (source-load the runtime)
#   --tsv FILE         also write one TSV row per file (rel, P ok/notok,
#                      C ok/notok, status, signature) for diffing runs
#   --faillog DIR      per-test failure log dir (default .suitelog, cleared
#                      each run): for every DIFF/TIMEOUT file, one TSV row per
#                      diverging TAP test — num, perl-verb, pcl-verb, desc.
#                      The two streams are paired BY DESCRIPTION (PclTapAlign,
#                      task #177), never by test number: PCL can emit extra or
#                      missing rows mid-file, and a number join then blames
#                      rows that PASS (op/do.t t67/t70) while crediting rows
#                      that fail.  test# stays PERL's; PCL's own number is
#                      shown when it differs.  A PCL run that produced NO TAP
#                      writes one summary row.
#
# Expected divergences — marking not-supported dependencies:
#   baselines/perl-suite-expected.tsv maps `rel<TAB>reason` (reason should cite the
#   docs/not-supported.md section, and the plan doc when one exists).  A
#   divergent file with a row becomes status XDIFF — still runs, still prints
#   its row + reason, but does not fail the exit code.  If an expected file
#   comes back OK the row is STALE (flagged AND fails the run) so a fixed
#   feature can never hide behind an old expectation — same philosophy as the
#   sweep's skip-registry (docs/test-skip-registry.md).  Crashing files may be
#   marked only when the crash itself is the documented gap; everything
#   UNEXPLAINED stays a fix/triage target.
#
# THE THREE PHASE-0 AUDIT INSTRUMENTS (task #993, docs/plan-test-audit-s464.md
# §3).  All three answer the same question from different sides: WHAT IS THIS
# RUN NOT TELLING ME?
#
#   --bless-fails      baselines/perl-suite-fails.tsv — the ROW-level fail
#                      baseline (I1).  Until it existed the 273 DIFF files were
#                      blessed as COUNTS: #964's failing row sat inside
#                      op/sub.t's "12 not ok" for months and nothing named it.
#                      Every run prints NEW ROW / FIXED ROW / UNVERIFIED / LOST.
#   --bless-shortfall  baselines/row-shortfall.tsv — perl rows PCL never
#                      produced, per file, WITH A CAUSE (I2, shared with the
#                      sweep through tools/lib/PCLShortfall.pm).  The 10
#                      TRANSPILE files are one line each and 2,031 rows behind
#                      them; a shortfall is invisible to every other bucket.
#   --bless-stamps     baselines/perl-suite-notrun-stamps.tsv — WHEN each
#                      never-run file was last measured (I4).  "not run" says
#                      nothing about how old the hole is; the stamp does.
#
#   XDIFF IS GRANTED PER ROW, all-or-nothing (task #185), exactly like FIXTURE:
#   baselines/perl-suite-expected-rows.tsv holds the blessed multiset of diverging
#   rows for each registered file, keyed by PERL's test DESCRIPTION (#177 —
#   numbers are the unstable coordinate).  A file whose divergence grows a row
#   that is not in the baseline stays DIFF and names the intruder; a row that
#   stops diverging makes the file STALE.  Without this the file-level reason
#   excused the WHOLE file forever: a new bug landing anywhere inside op/
#   signatures.t (355 blessed rows) was indistinguishable from the blessed gap.
#   The baseline is generated — `--bless-rows` rewrites it from the current
#   run, touching only the files that run measured.
#
# Speed: like tools/prove-core, a FRESH SBCL core with the runtime compiled in
# is built once per invocation (never stale, removed on exit); each test then
# starts from the core (~0.003s) instead of recompiling the runtime (~1.2s).
#
# How the generated CL is LOADED (task #467): with pcl::p-load-with-recovery,
# one top-level form at a time, continuing past an uncaught die in a single
# form — the SAME load sweep-perl-tests.pl uses.  Both measurement runners
# must agree on this: with a plain `--load` here, one dying form ended the
# file, so the same compiler change cost the sweep 1 row and this runner 94
# (s432).  A recovered form is COUNTED and PRINTED (`aborted-forms:N` in the
# signature column) — never swallowed — so a file that dies mid-way can never
# read as OK.  Programs (./runpcl) are a plain load: recovery is measurement
# policy, not runtime semantics.
#
# Harness fixture (the shadow t/): tests using the classic idiom
# `chdir 't' if -d 't'; require './test.pl'` are RUN, not skipped.  A shadow
# copy of the t/ tree is built in the temp dir — every top-level entry of the
# real t/ symlinked in, then PCL's transpilable stubs (perl-tests/t/test.pl,
# charset_tools.pl, loc_tools.pl) overlaid on top.  Transpile and SBCL both
# run with CWD = shadow, so `require './test.pl'` resolves to the stub at
# transpile time (cwd-first resolution in _extract_file_prototypes learns the
# `is ($$@)` prototypes) and at runtime (p-require-file is cwd-relative),
# while every OTHER relative path (fixture data files, ./op/..., ./harness)
# still reaches the real tree through the symlinks.  The perl baseline runs
# in the REAL t/ against perl's own test.pl — authoritative TAP.  The TAP
# layer (cl/pcl-test.lisp: plan/is/ok/skip/...) is compiled into the saved
# core, mirroring the sweep's `--load` of it.
#
# NEED-HARNESS files — those that fiddle @INC in a BEGIN to pull build-tree
# modules from ../lib — used to be dropped from the scan here, silently.  Since
# s438 they are SCANNED like every other file (all five in perl 5.40's t/ were
# measured to produce a verdict; the scan line reports the count), and a future
# one that genuinely cannot be measured goes in %NEED_HARNESS_NOT_RUN for a
# NOT-RUN row.  There is no silent filter left.
#
# Harness-fixture artifacts — a THIRD category, and not not-support:
#   baselines/perl-suite-fixture.tsv maps `rel<TAB>rows<TAB>cause` for rows that
#   diverge because the two sides run in different trees, not because PCL
#   lacks anything.  (Worked example, task #172: the shadow t/ SYMLINKS op/,
#   and getcwd(3) returns the PHYSICAL path, so op/chdir.t's `"$Cwd/op"` can
#   never equal the post-chdir cwd — PCL and perl were verified identical on
#   every primitive involved.)  Such a file becomes status FIXTURE and stops
#   counting as UNEXPLAINED.  Deliberately NOT perl-suite-expected.tsv: that
#   file's bar is "explained by a blessed not-supported.md section", so filing
#   an artifact there would claim PCL lacks something it has.
#   Registration is per-ROW and all-or-nothing: the file's ENTIRE divergence
#   must be registered rows or it stays DIFF with the intruders named, so a
#   real bug landing in a fixture-affected file can never hide.  A registered
#   file that starts fully passing is STALE and fails the run.
#
# Output columns: P:perl_ok/notok  C:pcl_ok/notok  STATUS  [crash-signature]
# STATUS: OK (counts match) | DIFF | TRANSPILE | TIMEOUT | NOTAP (perl itself
# produced no TAP — not comparable, doesn't fail the run; PCL result shown)
# | XDIFF (expected divergence) | FIXTURE (harness artifact, see above)
# | KILLED / NOT-RUN (the RUN died before measuring this file — see below;
# also the status of a QUARANTINED file, which is never run at all).
# Exit: nonzero iff any DIFF/TRANSPILE/TIMEOUT/MISSING/NO-RESULT/KILLED/NOT-RUN.
#
# EVERY requested file gets a row, including when the run itself dies (task
# #157).  It did not use to: under memory pressure the run produced no rows,
# no summary, no --tsv and exit 0 — a total failure and a run nobody asked for
# were the same observation.  Three separate defects, all fixed here:
#   * a forked WORKER inherited the parent's SIGTERM handler and END blocks,
#     so one signalled worker `rm -rf`'d the SHARED tmpdir and unlinked the
#     shared core, after which the parent died in tempfile();
#   * `system()` in an END block overwrites $?, and $? at the end of the last
#     END block IS the process exit code — so the "Exit: nonzero" contract
#     above silently never held, for any run;
#   * nothing reported the files that never got measured.
# Verified by killing a worker and by killing the parent mid-dispatch (s319).

use strict;
use warnings;
use File::Basename qw(basename dirname);
use File::Temp qw(tempfile tempdir);
use File::Path qw(make_path);
use File::Copy qw(copy);
use Cwd qw(abs_path);
use POSIX qw(:sys_wait_h _exit);
use FindBin;
use lib "$FindBin::RealBin/lib";
# Description-based TAP pairing (task #177) — unit-tested in tools/t/tap-align.t.
use PclTapAlign qw(tap_rows align_taps);
use PCLSbcl ();   # the ONE builder of an SBCL command line (task #344)
use PCLProc qw(run_isolated reap_orphan_transpilers);   # session isolation + reaping (#367)
use PCLPaths qw(perl_suite_t);
use PCLShortfall ();   # the ONE reader/writer of the shared shortfall baseline (#993)

# Contain the whole sweep in its own memory-capped cgroup: a runaway child
# (e.g. the pl2cl eval-server ballooning on op/cond.t's 20k-nested ternary)
# then OOMs only this scope, never the desktop session.  PCL_SUITE_SCOPED
# guards re-exec recursion; skipped when systemd-run is unavailable.
if (!$ENV{PCL_SUITE_SCOPED}
    && system('systemd-run --user --scope -q -p MemoryMax=1G true 2>/dev/null') == 0) {
  $ENV{PCL_SUITE_SCOPED} = 1;
  exec('systemd-run', '--user', '--scope', '-q',
       '-p', 'MemoryMax=10G', '-p', 'MemorySwapMax=1G',
       $^X, $0, @ARGV)
    or die "systemd-run re-exec failed: $!\n";
}

my $root    = abs_path(dirname(abs_path($0)) . "/..");
my $pl2cl   = "$root/pl2cl";
my $runtime = "$root/cl/pcl-runtime.lisp";
my $testlib = "$root/cl/pcl-test.lisp";

# Dirs worth sweeping.  Excluded on purpose: porting (perl-repo hygiene),
# win32, bigmem (huge memory), perf/benchmark (timing), test_pl (tests the
# harness itself), japh (obfuscated), lib (needs the build-tree module layout).
my @DEFAULT_DIRS = qw(base cmd comp opbasic op mro class run uni re io);

# perl's own t/ — derived (PCL_PERL_SUITE_T, else the perlbrew build tree of
# the running perl), never hard-coded: task #278.  Resolved AFTER the argument
# loop, so `--tdir PATH` still works on a machine where nothing derives.
my $tdir;
my ($all, $no_core, $tsv_file);
my $jobs = 8;
my $timeout = 90;
my $faillog = "$root/.suitelog";
my $expected_tsv = "$root/baselines/perl-suite-expected.tsv";
my $expected_rows_tsv = "$root/baselines/perl-suite-expected-rows.tsv";
my $fixture_tsv  = "$root/baselines/perl-suite-fixture.tsv";
my $timeouts_tsv = "$root/baselines/perl-suite-timeouts.tsv";
# The three PHASE-0 audit instruments (task #993, docs/plan-test-audit-s464.md §3).
my $fails_tsv     = "$root/baselines/perl-suite-fails.tsv";           # I1 row bless
my $shortfall_tsv = "$root/baselines/row-shortfall.tsv";              # I2 planned-produced
my $stamps_tsv    = "$root/baselines/perl-suite-notrun-stamps.tsv";   # I4 last measured
my ($bless_rows, $bless_fails, $bless_shortfall, $bless_stamps);
my $quick;
my (@dirs, @files);
while (@ARGV) {
  my $a = shift @ARGV;
  if    ($a eq '--bless-rows')     { $bless_rows = 1 }
  elsif ($a eq '--bless-fails')     { $bless_fails = 1 }
  elsif ($a eq '--bless-shortfall') { $bless_shortfall = 1 }
  elsif ($a eq '--bless-stamps')    { $bless_stamps = 1 }
  elsif ($a eq '--tdir')           { $tdir = shift @ARGV }
  elsif ($a eq '--dir')            { push @dirs, shift @ARGV }
  elsif ($a eq '--all')            { $all = 1 }
  elsif ($a eq '--quick')          { $quick = 1 }
  elsif ($a eq '--jobs')           { $jobs = shift @ARGV }
  elsif ($a eq '--timeout')        { $timeout = shift @ARGV }
  elsif ($a eq '--no-core')        { $no_core = 1 }
  elsif ($a eq '--tsv')            { $tsv_file = shift @ARGV }
  elsif ($a eq '--faillog')        { $faillog = shift @ARGV }
  else                             { push @files, $a }
}
$tdir //= perl_suite_t();
$faillog = "$root/$faillog" unless $faillog =~ m{^/};

# Expected-divergence registry: rel -> reason (citing docs/not-supported.md).
my %expected;
if (open my $ef, '<', $expected_tsv) {
  while (<$ef>) {
    chomp;
    next if /^\s*(?:#|$)/;
    my ($rel, $reason) = split /\t/, $_, 2;
    $expected{$rel} = $reason // '';
  }
  close $ef;
}

# Expected-divergence ROW baseline (task #185): rel -> sorted arrayref of the
# rowkeys the file is allowed to diverge on.  MACHINE-maintained (--bless-rows),
# kept out of perl-suite-expected.tsv so the ~1900 generated rows never drown
# the 108 hand-written reasons there.  A rowkey is PERL's test DESCRIPTION —
# never a test number (#177: numbers are the unstable coordinate) — with three
# fallbacks: "#N" for a genuinely unnamed test, "*summary*" for the log's
# test#-0 summary rows (no-TAP / renumbered), "*no-log*" when the file produced
# no per-test log at all (TRANSPILE).  Compared as a MULTISET, so a description
# that repeats inside a file must be registered as many times as it diverges.
my %expected_rows;
if (open my $rf, '<', $expected_rows_tsv) {
  while (<$rf>) {
    chomp;
    next if /^\s*(?:#|$)/;
    my ($rel, $key) = split /\t/, $_, 2;
    push @{ $expected_rows{$rel} }, $key if defined $key;
  }
  close $rf;
  @{ $expected_rows{$_} } = sort @{ $expected_rows{$_} } for keys %expected_rows;
}

# Fixture-artifact registry: rel -> { rows => {n=>1}, cause => text }.
# A DIFFERENT category from %expected — see baselines/perl-suite-fixture.tsv and
# the FIXTURE paragraph in the header comment.  Keyed per ROW, not per file.
my %fixture;
if (open my $ff, '<', $fixture_tsv) {
  while (<$ff>) {
    chomp;
    next if /^\s*(?:#|$)/;
    my ($rel, $rows, $cause) = split /\t/, $_, 3;
    $fixture{$rel} = { rows  => { map { $_ => 1 } grep { length } split /\s*,\s*/, ($rows // '') },
                       cause => $cause // '' };
  }
  close $ff;
}
# Per-file TIMEOUT ALLOWANCE registry: rel -> { secs, cause }.  A file that
# TIMEOUTs contributes NO rows, so a file which merely needs longer than the
# default reads as a total loss and its passing rows evaporate invisibly —
# the #176 pack.t lesson, which cost the sweep a whole file's visibility.
# The sweep answers it with a blind retry at 3x; here the need is KNOWN per
# file and belongs written down with its cause, so the default run honours it
# and the allowance is reviewable.  The effective timeout is the MAX of the
# registry value and --timeout, so raising --timeout still works.
my %file_timeout;
if (open my $tf, '<', $timeouts_tsv) {
  while (<$tf>) {
    chomp;
    next if /^\s*(?:#|$)/;
    my ($rel, $secs, $cause) = split /\t/, $_, 3;
    next unless defined $secs && $secs =~ /^\d+$/;
    $file_timeout{$rel} = { secs => $secs, cause => $cause // '' };
  }
  close $tf;
}
sub timeout_for {
  my ($rel) = @_;
  my $e = $file_timeout{$rel} or return $timeout;
  return $e->{secs} > $timeout ? $e->{secs} : $timeout;
}

# ── I1: the ROW-level fail baseline (task #993, plan-test-audit §3) ──────────
# baselines/perl-suite-fails.tsv — the companion's answer to the sweep's
# baselines/fail-baseline.tsv.  Until this existed the 273 DIFF files were blessed
# as COUNTS only: #964's failing row sat inside op/sub.t's "12 not ok" and
# nothing named it, so a semantic difference could live for months while the
# file's numbers never moved.  Keyed exactly like the #185 expected-rows
# baseline — (rel, PERL's test DESCRIPTION), compared as a MULTISET — because
# test NUMBERS are the unstable coordinate (#177) and because the two
# baselines must not disagree about what a row IS.
#
# Files registered in perl-suite-expected.tsv or perl-suite-fixture.tsv are
# NOT in here: their rows are already gated, per row, by those registries.
# One gate per file, or a moved row reads twice and is fixed once.
my %blessed_fails;      # rel -> sorted arrayref of rowkeys
my %blessed_fail_meta;  # rel -> arrayref of [num, perl_verb, pcl_verb, rowkey]
# Whether the baseline existed AT STARTUP, not at report time: a --bless-fails
# run WRITES the file before the report, so `-e` there would answer yes and the
# report would list every blessed row as a NEW ROW.  It did exactly that on the
# first bless (s465az).  Every registry answer must come from the state the run
# was MEASURED against.
my $fails_tsv_existed = -e $fails_tsv;
if (open my $bf, '<', $fails_tsv) {
  while (<$bf>) {
    chomp;
    next if /^\s*(?:#|$)/;
    my ($rel, $num, $pv, $cv, $key) = split /\t/, $_, 5;
    next unless defined $key;
    push @{ $blessed_fails{$rel} }, $key;
    push @{ $blessed_fail_meta{$rel} }, [$num, $pv, $cv, $key];
  }
  close $bf;
  @{ $blessed_fails{$_} } = sort @{ $blessed_fails{$_} } for keys %blessed_fails;
}

# ── I2: the ROW-SHORTFALL baseline, SHARED with the sweep ────────────────────
# baselines/row-shortfall.tsv — rows the population expected and PCL never
# produced, per file, with a CAUSE.  The drop census's shape (rows leave by
# EDIT; more than blessed fails the run) applied to a different blindness:
# a drop is a statement that vanished, a shortfall is a whole TAIL of rows that
# never ran.  "OK" used to mean "no previously-passing row lost", so pack.t
# read OK while 8,997 of its 14,722 planned rows were never produced.
# Keys are population-prefixed exactly like the drop census: `t/<rel>` here,
# `perl-tests/<name>` for the sweep.
my %shortfall_base = %{ PCLShortfall::read_shortfall($shortfall_tsv) };

# ── I4: WHEN was a not-run file last measured? ───────────────────────────────
# baselines/perl-suite-notrun-stamps.tsv — one row per file this runner does
# not measure in its default form (QUARANTINED, QUICK-SKIP, QUICK-CAPPED,
# NEED-HARNESS) and per snapshot row no `--all` scan refreshes.  A hole that is
# only ever printed as "not run" says nothing about how OLD it is: op/list.t
# has been quarantined since s320 and the hang set is only reached by a full
# `--all`, which happened once in 40 sessions.  The stamp makes the age
# countable.  Machine-maintained: `--bless-stamps` rewrites it from a run.
my %stamp;   # rel -> { session, date, note }
if (open my $tf2, '<', $stamps_tsv) {
  while (<$tf2>) {
    chomp;
    next if /^\s*(?:#|$)/;
    my ($rel, $sess, $date, $note) = split /\t/, $_, 4;
    next unless defined $date;
    $stamp{$rel} = { session => $sess, date => $date, note => $note // '' };
  }
  close $tf2;
}

# PCL_SUITE_KEEP=DIR — copy a file's run artifacts out of the (deleted)
# tempdir so they can be READ: the perl side's raw TAP, the emitted CL, the
# transpile stderr, and the PCL side's raw output.  The last is the one that
# pays: the report joins the two sides by TAP description and keeps only ok /
# not ok, so a `diag` a failing row printed — which is often the whole
# measurement — never leaves the tempdir.  Named `<safe-rel>.<what>`.
sub keep_artifacts {
  my ($safe, @src) = @_;
  my $dir = $ENV{PCL_SUITE_KEEP} or return;
  make_path($dir) unless -d $dir;
  for my $s (@src) {
    next unless defined $s && -f $s;
    (my $what = $s) =~ s{^.*/\Q$safe\E}{};
    copy($s, "$dir/$safe" . ($what eq '' ? '.out' : $what));
  }
  return;
}

-d $tdir or die "perl t/ tree not found: $tdir (pass --tdir)\n";
$all = 1 if !@files && !@dirs;
push @dirs, @DEFAULT_DIRS if $all;

# Enumerate self-contained files in each requested dir.
#
# NO copied-file filter (task #150 part 1, decided s317 fable-answers §6i).
# This scan used to skip any t/ file whose basename+head matched a
# perl-tests/ corpus file, on the theory that the sweep already owned it.
# That let a PASSING corpus copy shadow a FAILING real t/ file: the four
# drifted copies (chop/dor/not/quotemeta) read as fully passing in the sweep
# while the t/op originals had 2-5 failures each — a misleading signal for a
# release gate.  The t/ ORIGINALS are authoritative, so they always run and
# always report; a file being in both this run and the sweep is harmless
# duplication.  (Re-syncing the drifted copies themselves is part 2, post-R1:
# it churns the blessed fail-baseline.)
# NEED-HARNESS FILES ARE SCANNED TOO, since s438 (ruled fable-answers-s437 §2
# ask 1(b), "narrowed by measurement").  A file that fiddles @INC in a BEGIN
# was SKIPPED here on the theory that it needs perl's build tree — silently,
# which is how five of them ended up with snapshot rows that nothing could ever
# refresh (s431 spliced them in by hand; s434 printed the hole from both
# sides).  All five were then MEASURED by naming them, and all five produce a
# verdict against real perl (comp/line_debug.t DIFF 1/24, op/goto.t
# TRANSPILE-FAIL, op/lex.t DIFF 13/39, op/require_errors.t DIFF 3/68,
# run/dtrace.t NOTAP), so the exclusion was costing coverage and buying
# nothing.  They now join the scan and the file count is 528, not 523.
#
# The rule is kept as DATA, not deleted: a need-harness file that genuinely
# cannot be measured belongs in %NEED_HARNESS_NOT_RUN below, where it gets a
# NOT-RUN row naming the rule (the #345 shape) and stays UNEXPLAINED — counted
# on every run instead of inferred from a file count.  The registry is empty
# today because the measurement says it should be.
my %need_harness;
for my $d (@dirs) {
  my ($n_all, $n_harness) = (0, 0);
  for my $f (sort glob "$tdir/$d/*.t") {
    $n_all++;
    my $base = basename($f);
    open my $fh, '<', $f or next;
    local $/; my $src = <$fh>; close $fh;
    # Files pulling build-tree modules via @INC fiddling in BEGIN.
    # (`require './test.pl'` and `chdir 't'` files run via the shadow t/.)
    if ($src =~ m{BEGIN[^\n]*\@INC}) { $n_harness++; $need_harness{"$d/$base"} = 1 }
    push @files, "$d/$base";
  }
  printf STDERR "scan t/%-8s %3d files: %3d self-contained, %3d need-harness (run too, since s438)\n",
    $d, $n_all, $n_all - $n_harness, $n_harness;
}
@files or die "no files (give t-relative paths, --dir <subdir>, or --all)\n";

# ------------------------------------- files this run deliberately does NOT run
#
# QUARANTINE (task #160, ruled s320): files that take the whole MACHINE down,
# not just themselves.  op/list.t alone consumed a 10 GB cgroup in 53 s under
# --jobs 1 on an idle box and got OOM-killed, taking the run with it and
# leaving op/pack.t unmeasured behind it in the queue.  The transpiler is
# innocent (pl2cl: 1.23 s, 64 MB, 564 lines of CL) — the blowup is SBCL-side,
# diagnosis is POST-R1 and needs the user to re-authorize running these.
my %QUARANTINE = (
  'op/list.t' => 'task #160 — 10 GB SBCL blowup OOM-kills the run',
  'op/pack.t' => 'task #160 — never measured; was queued behind op/list.t when it OOMed',
);

# --quick (task #345, ruled docs/fable-answers-s400.md §8) — the DEFAULT
# companion form, and the reason the companion suite is affordable per change.
#
# Roughly HALF a full run's wall time is spent on the same known-bad tail every
# time, and it produces NOTHING: the #326 files stop dead at the same TAP row
# whatever the budget (793+112 at 150 s AND at 400 s — measured, task #326), and
# a file whose registered allowance is minutes cannot finish inside a quick
# run's budget at all.  Quick mode does not run either kind.
#
# It does NOT hide them.  Each gets a NOT-RUN row carrying WHICH rule fired and
# its cause — an UNEXPLAINED status, exactly like QUARANTINE, so the file still
# fails the run and still shows in the tsv as a hole.  That is the difference
# between "not measured this run" and "fine": the hole stays countable.
#
# Why NOT-RUN rather than "run it with a smaller budget": a truncated TAP stream
# is not a cheaper measurement, it is a DIFFERENT one — C_ok then means "how far
# it got before the cutoff" (the s325/#195 lesson, see the snapshot header), so
# the file's verdict would differ between quick and full runs for no reason but
# the clock.  Quick's bar is that every file which runs in BOTH forms gets the
# SAME verdict; a capped-and-truncated file would break exactly that.
#
# MEMBERSHIP IS MEASURED, never assumed: a file belongs here only when a
# LARGER budget returns the SAME rows (task #326's own test).  A file that
# merely needs longer belongs in baselines/perl-suite-timeouts.tsv instead — that
# registry's promise is "give it the time and it finishes", and a file which
# never finishes would make the promise false.
my %QUICK_SKIP = (
  # The six drivers over t/re/re_tests that share one hang (task #326).  They
  # burn the whole per-file timeout to re-emit the same ~905 rows.
  're/regexp.t'             => 'task #326 — hangs at TAP row ~905 (identical counts at 150 s and 400 s)',
  're/regexp_noamp.t'       => 'task #326 — same driver/data as re/regexp.t, same stall',
  're/regexp_notrie.t'      => 'task #326 — same driver/data as re/regexp.t, same stall',
  're/regexp_qr.t'          => 'task #326 — same driver/data as re/regexp.t, same stall',
  're/regexp_qr_embed.t'    => 'task #326 — same driver/data as re/regexp.t, same stall',
  're/regexp_trielist.t'    => 'task #326 — same driver/data as re/regexp.t, same stall',
  # Two of the three files task #345 called "the tail", measured s404 at 90 s,
  # 300 s and 900 s: 10x the default budget buys nothing, so an allowance would
  # be a lie.  (The third, re/pat_psycho.t, IS merely slow — it completed at
  # 300 s with 11 rows — so it is registered in baselines/perl-suite-timeouts.tsv
  # instead, and the allowance cap below keeps it out of a quick run anyway.)
  're/overload.t'           => 'HANG measured s404 — 3 of perl\'s 87 rows at 90 s, at 300 s (s398) and at 900 s',
  're/speed.t'              => 'HANG measured s404 — 1 of perl\'s 59 rows at 300 s AND at 900 s (pathological patterns it times; #326 family)',
);
my $QUICK_CAP = 120;   # seconds; a registered allowance above this is not run

# A need-harness file (BEGIN-@INC, see the scan above) that cannot be measured
# at all.  EMPTY by measurement, s438: all five in perl 5.40's t/ produce a
# verdict.  A future one goes here with WHAT was measured, never on suspicion.
my %NEED_HARNESS_NOT_RUN = (
);

my %not_run;
for my $rel (@files) {
  if ($QUARANTINE{$rel}) { $not_run{$rel} = "QUARANTINED: $QUARANTINE{$rel}"; next }
  if ($need_harness{$rel} && $NEED_HARNESS_NOT_RUN{$rel}) {
    $not_run{$rel} = "NEED-HARNESS: $NEED_HARNESS_NOT_RUN{$rel}"; next
  }
  next unless $quick;
  if ($QUICK_SKIP{$rel}) { $not_run{$rel} = "QUICK-SKIP: $QUICK_SKIP{$rel}"; next }
  my $e = $file_timeout{$rel} or next;
  $not_run{$rel} = sprintf("QUICK-CAPPED: registered allowance %ds > quick cap %ds — %s",
                           $e->{secs}, $QUICK_CAP, $e->{cause})
    if $e->{secs} > $QUICK_CAP;
}
my @quarantined    = grep { ($not_run{$_} // '') =~ /^QUARANTINED/  } @files;
my @need_harness_nr= grep { ($not_run{$_} // '') =~ /^NEED-HARNESS/ } @files;
printf STDERR "need-harness: %d file(s) scanned, %d not run — listed NOT-RUN below\n",
  scalar(grep { $need_harness{$_} } @files), scalar(@need_harness_nr)
  if %need_harness;
my @quick_skipped  = grep { ($not_run{$_} // '') =~ /^QUICK-SKIP/   } @files;
my @quick_capped   = grep { ($not_run{$_} // '') =~ /^QUICK-CAPPED/ } @files;
# I4 (task #993): the files the runner's DEFAULT form does not measure, whether
# or not THIS invocation is that form.  Membership must not depend on --quick:
# the whole point of the stamp is that a full `--all` MEASURES the hang set, and
# that measurement is what a later quick run's "last measured" line reports.
# Computed from the tables, never from %not_run, which is this run's view.
my %stamp_tracked;
for my $rel (@files) {
  $stamp_tracked{$rel} = "QUARANTINED: $QUARANTINE{$rel}" if $QUARANTINE{$rel};
  $stamp_tracked{$rel} = "QUICK-SKIP: $QUICK_SKIP{$rel}"  if $QUICK_SKIP{$rel};
  $stamp_tracked{$rel} = "NEED-HARNESS: $NEED_HARNESS_NOT_RUN{$rel}"
    if $need_harness{$rel} && $NEED_HARNESS_NOT_RUN{$rel};
  $stamp_tracked{$rel} = sprintf("QUICK-CAPPED: registered allowance %ds > quick cap %ds — %s",
                                 $file_timeout{$rel}{secs}, $QUICK_CAP, $file_timeout{$rel}{cause})
    if $file_timeout{$rel} && $file_timeout{$rel}{secs} > $QUICK_CAP;
}
printf STDERR "quick mode: %d file(s) not run (%d hang-set, %d allowance > %ds) — listed NOT-RUN below\n",
  scalar(@quick_skipped) + scalar(@quick_capped),
  scalar(@quick_skipped), scalar(@quick_capped), $QUICK_CAP if $quick;

# ------------------------------------------------- crash-honest reporting
# A run that DIES must never look like a run that was never asked for
# (task #157).  It used to: an OOM under memory pressure produced no row,
# no summary, no --tsv and a zero exit — indistinguishable from "nobody
# asked", and a release gate reads that as nothing-to-see.
#
# TWO mechanisms, because they cover different deaths:
#
#  1. The report lives in an END block, so a SIGTERM (what a cgroup OOM
#     stop actually sends — see the systemd-run wrapper above), a die, or
#     a Ctrl-C still prints a row for EVERY file: KILLED for the ones in
#     flight, NOT-RUN for the ones never started.  Both count as bad, so
#     the exit code is nonzero and the tsv is complete.
#  2. SIGKILL runs nothing, so rows are ALSO appended to a journal as they
#     arrive.  A killed run leaves the rows it did get plus a missing
#     "# complete" trailer, i.e. visibly partial rather than absent.
#
# $MAIN_PID guards it (and the cleanup ENDs below): a forked WORKER that
# exits via die/exit would otherwise run the parent's END blocks and
# `rm -rf` the shared tmpdir out from under its siblings — which is
# precisely how the whole run's evidence used to vanish at once.
my $MAIN_PID = $$;
my (%children, %results);
# Task #366: how many snapshot-differing files get a serial re-run before the
# report.  Capped so a broken tree cannot double the wall time; the cap is
# PRINTED when it bites, never silent.
my $RERUN_CAP = 40;
my $rerun_movers_done = 0;
my ($dispatch_started, $reported) = (0, 0);
END {
  return if $$ != $MAIN_PID || !$dispatch_started || $reported;
  my $bad = emit_report();
  $? = 1 if $bad && !$?;
}

# Fresh saved core (tools/prove-core pattern): runtime compiled in ONCE,
# rebuilt every invocation so it can never be stale; removed on exit.
my $core = "";
unless ($no_core) {
  (undef, $core) = tempfile("pcl-suite-core.XXXXXX", TMPDIR => 1, OPEN => 0);
  print STDERR "run-perl-suite: building fresh core from cl/pcl-runtime.lisp ...\n";
  if (system("sbcl --noinform --non-interactive --load \Q$runtime\E --load \Q$testlib\E "
           . "--eval '(sb-ext:save-lisp-and-die \"$core\" :executable nil)' "
           . ">/dev/null 2>&1") != 0) {
    print STDERR "run-perl-suite: core build FAILED — falling back to source-load\n";
    unlink $core; $core = "";
  }
}
# `local $?` or the cleanup EATS THE EXIT STATUS: whatever $? holds when the
# LAST END block finishes becomes the process exit code, and system()/`` set
# $?.  That is why every run exited 0 no matter how many files diverged, i.e.
# the "Exit: nonzero iff ..." contract above never actually held (task #157).
END { local $?; unlink $core if $core && $$ == $MAIN_PID }
# The prefix (stack size, banner flags, --core placement) is built by
# tools/lib/PCLSbcl.pm — the ONE place all five runners share (task #344).
# It is what made #324 possible: this runner used to hand-write the option
# string and was the only one WITHOUT --control-stack-size, so the companion
# suite ran PCL with SBCL's 2 MB default, a stack 256× smaller than the gate's.
# Deep-recursion files then died `control-stack-exhausted` HERE and nowhere
# else, and the truncation was read as a PCL crash: re/pat_rt_report.t stopped
# at 2431 of 2514 rows and its snapshot row blamed `(?{ CODE })`.
#
# The stack size stays overridable HERE because this runner spawns a PERL side
# and a PCL side per worker, so `--jobs 8` reserves 8x this much stack at once
# — see the measurement in task #324 before lowering it.
my $stack_mb = $ENV{PCL_SUITE_STACK_MB} // $PCLSbcl::STACK_MB;
my $sbcl = $core
  ? PCLSbcl::sbcl_prefix_str(core => $core, stack_mb => $stack_mb)
  : PCLSbcl::sbcl_prefix_str(runtime => $runtime, stack_mb => $stack_mb)
      . " --load \Q$testlib\E";

my $tmpdir = tempdir(CLEANUP => 0);
END { local $?; system("rm -rf \Q$tmpdir\E") if $tmpdir && -d $tmpdir && $$ == $MAIN_PID }

# FIXTURE SANITY (s318, task #151).  The PERL side runs with CWD = the REAL
# $tdir, so `require './test.pl'` there must find PERL's harness — the 2000-line
# t/test.pl, not PCL's ~400-line transpilable stub.  In s316v a `cp` followed a
# symlink out of the shadow and overwrote the real one with the stub; real perl
# then could not even COMPILE files that say `plan tests => N` (the stub has no
# prototype for plan), so the perl side emitted zero TAP and a whole run came
# back NOTAP — rows that look like data but are only a broken fixture.  NOTAP is
# labelled "says nothing about PCL", which made the damage quiet.  Two cheap
# identifying checks, and we die instead of producing a misleading run.
{
  my $real = "$tdir/test.pl";
  -f $real or die "run-perl-suite: perl's t/test.pl is MISSING at $real\n";
  open my $fh, '<', $real or die "run-perl-suite: open $real: $!\n";
  my $head = do { local $/; <$fh> };
  close $fh;
  my $lines = () = $head =~ /\n/g;
  $head =~ /most of Test::More functionality/ && $lines > 1000
    or die "run-perl-suite: $real is NOT perl's harness ($lines lines).\n"
         . "  It was probably overwritten by PCL's stub (perl-tests/t/test.pl).\n"
         . "  Every perl-side run would produce no TAP and the whole sweep would\n"
         . "  read NOTAP.  Restore it from the perl source tarball before rerunning.\n";
}

# Shadow t/ (see header): real tree symlinked entry-by-entry, PCL stubs on top.
my $shadow = "$tmpdir/t";
mkdir $shadow or die "mkdir $shadow: $!\n";
for my $e (glob "$tdir/*") {
  symlink $e, "$shadow/" . basename($e)
    or die "symlink $e: $!\n";
}
for my $stub (qw(test.pl charset_tools.pl loc_tools.pl)) {
  my $src = "$root/perl-tests/t/$stub";
  -f $src or die "PCL stub missing: $src\n";
  unlink "$shadow/$stub";
  symlink $src, "$shadow/$stub" or die "symlink $src: $!\n";
}
# The shadow's PARENT mirrors the perl source root (everything except t/,
# which IS the shadow) so `catfile(updir, ...)` fixture reads work — e.g.
# op/signatures.t reads ../regen/keywords.pl, porting tests read ../MANIFEST.
# Perl's build MODULE trees (lib/ ext/ dist/ cpan/) are NOT mirrored: the
# boilerplate `@INC = '../lib'` means "the modules matching the executing
# perl", which for PCL is the core-shim require fallback — mirroring them
# shadowed that fallback with real 5.40.3 modules, so Config.pm died on its
# version check (run/runenv* 0/0) and XS-backed modules (List::Util) died in
# XSLoader with no loadable object (re/regexp_* truncated).  Task #136.
# `../lib` is still created as an EMPTY directory: some tests need it to
# exist as a filesystem fixture (op/stat_errors.t opendirs it), and an empty
# dir on @INC finds nothing, so the shim fallback still serves every module.
for my $e (glob "$tdir/../*") {
  my $base = basename($e);
  next if $base eq 't' || $base =~ /^(?:lib|ext|dist|cpan)$/;
  symlink $e, "$tmpdir/$base";   # EEXIST is fine (tempdir is fresh per run)
}
mkdir "$tmpdir/lib";

# Fresh per-test failure log each run (mirrors the sweep's .faillog).
system("rm -rf \Q$faillog\E");
mkdir $faillog;

# Append-as-you-go journal (task #157): the only record that survives a
# SIGKILL of this process, and it makes a long run watchable from another
# shell (`tail -f`).  A journal without its "# complete" trailer means the
# run died — the rows above it are still valid, the missing ones are not
# "nothing to report", they are unmeasured.
my $journal_file = "$faillog/run-journal.tsv";
open my $JOURNAL, '>', $journal_file or die "write $journal_file: $!\n";
$JOURNAL->autoflush(1);
printf $JOURNAL "# run-perl-suite %s: %d files, jobs=%d timeout=%d%s\n",
  scalar(localtime), scalar(@files), $jobs, $timeout,
  ($quick ? sprintf(" QUICK (cap %ds; %d file(s) not run — see the NOT-RUN rows)",
                    $QUICK_CAP, scalar(@quick_skipped) + scalar(@quick_capped))
          : '');
print $JOURNAL "# file  P_ok  P_notok  C_ok  C_notok  status  sig   [P=PERL, C=PCL]\n";
# Timeout allowances in effect for files in THIS run — printed to both the
# journal and the terminal, so a long-running file's allowance is never a
# silent property of a registry nobody reads.
for my $rel (sort grep { $file_timeout{$_} && !$not_run{$_} } @files) {
  my $e = $file_timeout{$rel};
  printf $JOURNAL "# timeout-allowance\t%s\t%d\t%s\n", $rel, timeout_for($rel), $e->{cause};
  printf STDERR "timeout allowance: %-24s %4ds  (%s)\n", $rel, timeout_for($rel), $e->{cause};
}
print $JOURNAL "# queued\t$_\n" for @files;


# ---------------------------------------------------------------- worker
sub run_one {
  my ($rel, $result_file) = @_;
  my ($p_ok, $p_notok, $c_ok, $c_notok, $status, $sig) = (0, 0, 0, 0, 'OK', '');
  # DROPS (task #343, ruled fable-answers-s400.md §6.5): statements the
  # compiler could not lower and replaced with nil in this file's CL.  They are
  # invisible in the TAP comparison — the statement simply is not there — so
  # the count rides along with the transpile this run already does, and is
  # compared per file against baselines/parse-error-drop-census-s399.tsv in the
  # summary.  -1 = NOT MEASURED (no CL produced), never 0.
  my $drops = -1;
  my $f = "$tdir/$rel";

  unless (-f $f) {
    ($status, $sig) = ('MISSING', '');
    goto WRITE;
  }

  # ulimit -v: fork-heavy tests can orphan children past the timeout; the cap
  # stops any single perl from eating the machine (a 6 GB leak OOM-killed the
  # whole desktop session twice before group-reaping was added).
  (my $safe0 = $rel) =~ s{/}{_}g;
  my $poutf = "$tmpdir/$safe0.perlout";
  my $orphans = 0;
  (undef, my $k) = run_isolated("ulimit -v 4194304 2>/dev/null; cd \Q$tdir\E && "
                              . "timeout 30 perl \Q$f\E > \Q$poutf\E 2>/dev/null");
  $orphans += $k;
  my $perl = do { local $/; my $fh; open($fh, '<', $poutf) ? (<$fh> // '') : '' };
  $p_ok    = () = $perl =~ /^ok /mg;
  $p_notok = () = $perl =~ /^not ok /mg;

  my $to = timeout_for($rel);   # per-file allowance (baselines/perl-suite-timeouts.tsv)
  (my $safe = $rel) =~ s{/}{_}g;
  my $lisp = "$tmpdir/$safe.lisp";
  # Transpile with CWD = shadow so `require './test.pl'` prototype extraction
  # (cwd-first) reads the PCL stub, not perl's real harness.
  (my $terr, $k) = run_isolated("ulimit -v 4194304 2>/dev/null; cd \Q$shadow\E && "
                  . "timeout -k 10 $to perl -I\Q$root\E \Q$pl2cl\E --no-cache --lenient-ppi \Q$f\E "
                  . "> \Q$lisp\E 2>\Q$lisp\E.err");
  $orphans += $k;
  my $pcl = "";
  my $sbcl_exit = 0;
  if ($terr == 0) {
    if (open my $lf, '<', $lisp) {
      $drops = 0;
      while (my $l = <$lf>) { $drops++ while $l =~ /;; PARSE ERROR:/g }
      close $lf;
    }
    # CWD = shadow t/ (fixture files resolve through the symlinks, test.pl to
    # the stub); timeout(1) actually kills a hung SBCL (alarm in the parent
    # would leave an orphan).
    my $out = "$tmpdir/$safe.out";
    # PCLPERL: fresh_perl_*/runperl children in the PCL stub test.pl run
    # under PCL (tools/pclperl-for-tests) instead of the real perl; the
    # fresh core doubles as the children's startup image.
    my $childenv = "PCLPERL=\Q$root\E/tools/pclperl-for-tests"
                 . ($core ? " PCL_TEST_CORE=\Q$core\E" : "");
    # -k: an SBCL wedged in a runaway compile ignores/defers TERM and lives on
    # PAST the run (s316h: two escaped SBCLs + their orphaned 6 GB pl2cl
    # --server eval process); SIGKILL 10s after the TERM guarantees reaping.
    # RECOVERY LOAD, not `--load` (task #467): evaluate the generated file one
    # top-level form at a time and continue past an uncaught die in any single
    # form, exactly as sweep-perl-tests.pl does.  The two runners used to
    # DISAGREE on this axis, and the disagreement is not visible in either
    # report: measured s432, the SAME compiler change cost the sweep ONE row
    # (the form that died) and cost this runner 94 (op/method.t 96 -> 44,
    # op/sort.t 181 -> 142, op/lexsub.t 9 -> 6), every one of them a row AFTER
    # the dying form in a file that already crashed.  So a per-file count from
    # here was not comparable to one from the sweep for any change that makes
    # something die — the same class of trap as #324 (one runner measuring on a
    # 2 MB stack for months), and the reason the five runners share ONE SBCL
    # command builder.  The load is the second axis they must share.
    # Users (./runpcl) stay a plain load: recovery is a MEASUREMENT policy — it
    # buys rows after a failure, which a harness wants and a program must not.
    my $sbcl_cmd = "$sbcl --eval \Q(pcl::p-load-with-recovery \"$lisp\")\E";
    print STDERR "SBCL[run-perl-suite]: $sbcl_cmd\n" if $ENV{PCL_SHOW_SBCL};
    (my $rc, $k) = run_isolated("cd \Q$shadow\E && $childenv timeout -k 10 $to $sbcl_cmd"
                              . " > \Q$out\E 2>&1");
    $orphans += $k;
    $sbcl_exit = $rc >> 8;
    $pcl = do { local $/; my $fh; open($fh, '<', $out) ? (<$fh> // '') : '' };
    $c_ok    = () = $pcl =~ /^ok /mg;
    $c_notok = () = $pcl =~ /^not ok /mg;
  }

  # PCL_SUITE_KEEP=DIR — keep this file's four artifacts for reading (task
  # #694).  The runner joins the two sides by TAP description and reports only
  # the verdict, so a file's own `diag` (the value a row PRINTS when it fails)
  # is thrown away with $tmpdir — and for a row like op/exec.t's `$!` sanity
  # check after an aborted form, that diag IS the measurement.  Inert unless
  # the variable is set; four plain copies, no other behaviour, so a run WITH
  # it must produce the same verdicts as one without.
  keep_artifacts($safe, $poutf, $lisp, "$lisp.err", "$tmpdir/$safe.out")
    if $ENV{PCL_SUITE_KEEP};

  $sig = (($terr >> 8) == 124 ? "transpile-timeout" : "TRANSPILE-FAIL") if $terr != 0;
  $sig ||= "timeout"          if $sbcl_exit == 124;
  $sig ||= "unbound:$1"       if $pcl =~ /The variable (\S+) is unbound/;
  $sig ||= "undef-fn:$1"      if $pcl =~ /The function (\S+) is undefined/;
  $sig ||= "parse-error"      if $pcl =~ /PARSE ERROR/;
  $sig ||= "crash:$1"         if $pcl =~ /Unhandled ([^\s:]+(?::[^\s]+)?)/;
  $sig ||= "crash:$1"         if $pcl =~ /debugger invoked on a (\S+)/;
  # Generic crash classes (SIMPLE-ERROR etc.) subgroup by their message: first
  # line after the condition header, numbers/addresses normalized to N.
  if ($sig =~ /^crash:/
      && $pcl =~ /(?:Unhandled \S+|debugger invoked on a \S+).*?>:\s*\n\s+([^\n]+(?:\n[^\n]+){0,4})/s) {
    my $msg = $1;
    $msg =~ s/\n\s*Backtrace.*//s;
    $msg =~ s/[0-9]+/N/g;
    $msg =~ s/\s+/ /g;
    $msg =~ s/\t/ /g;
    $sig .= ": " . substr($msg, 0, 90);
  }

  # RECOVERED top-level forms (task #467), the second half of the recovery
  # load above.  A die the recovery caught leaves no "Unhandled" / "debugger
  # invoked" header, so the two crash rules above cannot see it — the
  # `unbound:`/`undef-fn:` rules still fire (they read the condition TEXT,
  # which the recovery prints), but a file that aborted a form would otherwise
  # come back with no signature at all.  Count them, name the first, and
  # ALWAYS append: a file that could not evaluate a top-level form has failed
  # at something even when its remaining TAP happens to match perl's, and that
  # is what keeps such a file from reading as OK.  Deliberately BEFORE the
  # status decision — unlike the orphan note below, which is an observation
  # about the RUN (#367) and must not turn a matching file into a DIFF.
  my $aborted = () = $pcl =~ /; PCL recovery: top-level form aborted/g;
  if ($aborted) {
    # Up to three lines, like the crash subgroup above: SBCL's commonest
    # report here — "The function\nMAIN::PL-NEW\nis undefined." — is wrapped
    # over three, and a one-line capture reads "The function" and names
    # nothing.  Stop at whatever the file printed next (TAP, or the next
    # recovery line), or the signature swallows a test row.
    my ($first) = $pcl =~ /; PCL recovery: top-level form aborted \(recovered\): ([^\n]*(?:\n[^\n]*){0,2})/;
    $first //= '';
    $first =~ s/\n(?:ok |not ok |1\.\.|#|;).*//s;
    $first =~ s/[0-9]+/N/g;
    $first =~ s/\s+/ /g;
    # Collapse a DEEP absolute path to its basename.  The signature is 90
    # characters and exists to NAME the condition; since the s435 flip the
    # commonest one starts
    #   "PCL: statement not supported at /home/…/perl-N.N.N/t/op/substr.t line N: …"
    # and the directory prefix alone overran the budget — every drop-caused
    # abort came back as "…/perl-N.N.N/t/o", naming nothing.  Two or more
    # directory components, and only where a path can START (whitespace or an
    # opening quote/paren), so a substitution in the quoted SOURCE TEXT
    # (`s/a/b/`) and a shallow "/dev/tty" are left alone.  Cosmetic by
    # construction: read_snapshot compares status + counts, never $sig.
    $first =~ s{(?<=[\s"'(])/(?:[^\s/"']+/){2,}}{}g;
    $sig = join('; ', grep { length } $sig,
                sprintf("aborted-forms:%d%s", $aborted,
                        length $first ? ": " . substr($first, 0, 90) : ''));
  }
  # The recovery's OTHER exit: the READER gave up, so everything past that
  # point in the file was never even read (the #419 shape — one `>0x10FFFF`
  # literal makes the rest of an emitted file unreadable).  A plain `--load`
  # reported that as a crash; the recovery prints and returns, so without this
  # rule such a file would come back with no signature at all — the exact
  # blind spot this block exists to close.
  if ($pcl =~ /; PCL recovery: unreadable form, stopping: ([^\n]*)/) {
    my $why = $1;
    $why =~ s/[0-9]+/N/g;
    $why =~ s/\s+/ /g;
    $sig = join('; ', grep { length } $sig, "unreadable-form: " . substr($why, 0, 90));
  }

  $status = $terr != 0                                   ? 'TRANSPILE'
          : $sbcl_exit == 124                            ? 'TIMEOUT'
          : ($p_ok + $p_notok) == 0                      ? 'NOTAP'
          : ($p_ok == $c_ok && $p_notok == $c_notok && !$sig) ? 'OK'
          :                                                'DIFF';

  # #367: say when this file left descendants behind — AFTER the status is
  # decided.  Reaping an orphan is an observation about the RUN, never a
  # divergence: putting it in $sig before this line turned op/alarm.t (5/0 vs
  # perl's 5/0, and 8 orphans) into a DIFF.
  $sig = ($sig ? "$sig; " : "") . "reaped $orphans orphan(s)" if $orphans;

  # Per-test failure log: pair the two TAP streams BY DESCRIPTION (align_taps
  # — never by test number, see its comment) and record every diverging test.
  # This is the triage input for marking not-supported rows, and the input the
  # FIXTURE registry matches against, so a mis-pairing here costs real time.
  if ($status eq 'DIFF' || $status eq 'TIMEOUT') {
    my ($prows, $crows) = (tap_rows($perl), tap_rows($pcl));
    if (open my $lf, '>', "$faillog/$safe.fails.tsv") {
      print $lf "# file  test#  perl_result  pcl_result  description   [test# = PERL's number; rows paired by description, so PCL's own number may differ — it is shown when it does.  description = PERL's test line, values interpolated by perl — not PCL's output]\n";
      if (!@$crows) {
        print $lf join("\t", $rel, 0, 'ok*', '(no TAP)',
                       "PCL produced no TAP output" . ($sig ? " ($sig)" : "")), "\n";
      } else {
        my ($pairs, $extras) = align_taps($prows, $crows);
        my $shifted = grep { $_->[1] && $_->[1]{num} != $_->[0]{num} } @$pairs;
        print $lf join("\t", $rel, 0, '(none)', 'renumbered',
                       "PCL's TAP numbering is offset from perl's for $shifted row(s)"
                       . " — rows below are paired by description, not by number"), "\n"
          if $shifted;
        # The 500-row cap is old, and it was INVISIBLE: a file with 4,204
        # diverging rows wrote 500 and said nothing, so the #993 row baseline
        # built from this log would silently be a partial multiset.  Count
        # first, then say so in a COMMENT line — comments are skipped by every
        # reader (read_diverging_rows, diverging_rowkeys, sweep-diff's load),
        # so the blessed #185 row multisets are byte-identical to before.
        my @diverge;
        for my $pair (@$pairs) {
          my ($p, $c) = @$pair;
          my $cv = $c ? $c->{verb} : '(missing)';
          next if $p->{verb} eq $cv;
          $cv .= " [PCL #$c->{num}]" if $c && $c->{num} != $p->{num};
          push @diverge, [$rel, $p->{num}, $p->{verb}, $cv, $p->{desc}];
        }
        printf $lf "# TRUNCATED: %d diverging row(s), only the first 500 are listed"
                 . " — any row baseline built from this file is PARTIAL\n", scalar(@diverge)
          if @diverge > 500;
        splice(@diverge, 500) if @diverge > 500;
        print $lf join("\t", @$_), "\n" for @diverge;
        # PCL-only rows: named individually now.  They are evidence in their
        # own right — do.t's two extras ARE the principle-9 divergence firing.
        print $lf join("\t", $rel, 0, '(none)', "extra [PCL #$_->{num}]",
                       $_->{desc}), "\n" for @$extras;
      }
      close $lf;
    }
  }

WRITE:
  open my $rf, '>', $result_file or _exit(1);
  print $rf join("\t", $rel, $p_ok, $p_notok, $c_ok, $c_notok, $status, $sig, $drops), "\n";
  close $rf;
  # Reap everything this worker's process group spawned — timeout(1) kills
  # only its direct child, so fork-heavy tests leave orphaned grandchildren.
  # This kills the worker itself too; the parent reads only the result file
  # (already written), never the exit status.
  kill 'KILL', -$$;
}

# ---------------------------------------------------- parallel dispatch
# Files whose run needs most of the machine by itself get a solo phase after
# the parallel bulk drains, so their peak doesn't stack on 7 other SBCLs.
my %HEAVY = map { $_ => 1 } (
  'op/cond.t',   # 20k-nested-ternary eval: pl2cl server peaks ~6.6 GB
);
my @heavy = grep { $HEAVY{$_} } @files;

# The deliberately-not-run files (QUARANTINE / --quick, decided above): one
# NOT-RUN row each, carrying WHICH rule fired and its cause.  Recorded here,
# after the journal is open, so they reach the journal like any other row.
for my $rel (grep { $not_run{$_} } @files) {
  # The tsv/journal row keeps the WHOLE cause (it is the record); the terminal
  # line is truncated, because a registry cause is a paragraph and a wall of
  # text is how a reader stops reading the report.
  record_result([$rel, 0, 0, 0, 0, 'NOT-RUN', $not_run{$rel}, -1]);
  printf "%-24s %s\n", $rel, "NOT-RUN (" . short_cause($not_run{$rel}) . ")";
}
@files = grep { !$not_run{$_} } @files;
@heavy = grep { !$not_run{$_} } @heavy;
# Workers sit in their own process groups, so terminal SIGINT no longer
# reaches them — forward it.  exit() still runs the END blocks, which is
# now what PRINTS the report for everything unfinished (task #157); a
# cgroup OOM stop arrives as SIGTERM, so this is the path an out-of-memory
# run actually takes.
for my $s ('INT', 'TERM') {
  $SIG{$s} = sub {
    print "\n-- caught SIG$_[0] — killing workers and reporting what we have\n";
    kill 'KILL', map { -$_ } keys %children;
    exit 130;
  };
}
$dispatch_started = 1;
for my $phase ([[grep { !$HEAVY{$_} } @files], $jobs], [\@heavy, 1]) {
my ($phase_files, $slots) = @$phase;
my @queue = @$phase_files;
print "-- solo phase: @queue\n" if @queue && $slots == 1 && @heavy;
while (@queue || %children) {
  while (@queue && keys(%children) < $slots) {
    my $rel = shift @queue;
    my (undef, $result_file) = tempfile(DIR => $tmpdir, SUFFIX => '.res', OPEN => 0);
    my $pid = fork();
    die "fork: $!" unless defined $pid;
    # Each worker leads its own process group so the group (worker + every
    # grandchild spawned via system/backticks) can be killed as a unit.
    # A WORKER must never run the parent's END blocks or its signal
    # handler: `exit`/`die` in a child would `rm -rf` the SHARED tmpdir and
    # unlink the shared core, destroying every SIBLING's result, and the
    # inherited handler would KILL the siblings outright.  A scope-wide
    # SIGTERM (cgroup OOM) hits workers and parent alike, so this used to
    # erase the whole run's evidence at once — task #157.  $MAIN_PID guards
    # the END blocks; DEFAULT here makes a signalled worker just die, which
    # the parent then reports as NO-RESULT.
    if ($pid == 0) {
      setpgrp(0, 0);
      $SIG{$_} = 'DEFAULT' for 'INT', 'TERM';
      %children = ();
      run_one($rel, $result_file);
      _exit(0);
    }
    $children{$pid} = { rel => $rel, result_file => $result_file, start => time() };
  }
  for my $pid (keys %children) {
    next unless waitpid($pid, WNOHANG) == $pid;
    my $info = delete $children{$pid};
    my $line = '';
    if (open my $rf, '<', $info->{result_file}) { chomp($line = <$rf> // ''); close $rf }
    my @r = split /\t/, $line, 8;   # …, sig, drops (task #343)
    @r = ($info->{rel}, 0, 0, 0, 0, 'NO-RESULT', '', -1) if @r < 6;
    $r[7] = -1 unless defined $r[7] && $r[7] =~ /^-?\d+$/;
    # Expected-divergence registry: divergent+expected -> XDIFF (doesn't fail
    # the run); OK+expected -> STALE (fails the run: remove the stale row).
    #
    # XDIFF is granted per ROW, all-or-nothing (task #185), exactly like
    # FIXTURE: the file's diverging rows must MATCH the blessed multiset in
    # baselines/perl-suite-expected-rows.tsv.  An unregistered diverging row keeps
    # the file DIFF and is named — so a new bug landing inside a file whose
    # feature gap is blessed can never hide behind the file-level reason.  A
    # registered row that stopped diverging is STALE, same as a whole file
    # that starts passing.
    classify_result(\@r);
    record_result(\@r);
    printf "%-24s P:%4d/%-3d C:%4d/%-4d %-7s %s\n", @r[0 .. 5], $r[6] // '';
    STDOUT->flush();
  }
  # Hard-kill stragglers the in-child timeout somehow missed.
  for my $pid (keys %children) {
    my $info = $children{$pid};
    next unless time() - $info->{start} > timeout_for($info->{rel}) + 40;
    kill 'KILL', -$pid; waitpid($pid, 0);
    record_result([$info->{rel}, 0, 0, 0, 0, 'TIMEOUT', '(killed)', -1]);
    printf "%-24s %s\n", $info->{rel}, 'TIMEOUT (killed)';
    delete $children{$pid};
  }
  select(undef, undef, undef, 0.1) if @queue || %children;
}
}

rerun_movers_serially();
# The blesses run BEFORE the report on purpose: every report section compares
# against the registries as they were AT STARTUP, so a blessing run still
# prints the honest "what moved" (or "NOT CHECKED") verdict for the tree it
# measured, instead of comparing a file against itself.
bless_expected_rows() if $bless_rows;
bless_fail_rows()     if $bless_fails;
bless_shortfall()     if $bless_shortfall;
bless_stamps()        if $bless_stamps;
exit(emit_report() ? 1 : 0);

# ---- A moved row is re-run ALONE before it is believed (task #366) ---------
#
# s406 measured 22 of 36 companion differences as pure CONTENTION: a file that
# spawns fresh_perl/runperl children loses rows when the machine is busy, and
# every one of them reproduced the snapshot when re-run by itself.  That made
# "re-run a mover alone" a rule the operator had to remember; this is the
# runner doing it, the way the sweep already re-runs a LOST file serially
# (#215).  BOTH values are printed and the SERIAL one is the verdict — a file
# that differs in both runs really moved.
sub rerun_movers_serially {
  return if $rerun_movers_done++;
  my %snap = read_snapshot();
  return if !%snap;
  # A file this run did NOT measure is not a mover: a --quick NOT-RUN row (the
  # #326 hang set, the >120 s allowances) differs from the snapshot trivially,
  # and re-running it here ALONE, at its full allowance, would spend the ~40
  # minutes --quick exists to save AND overwrite the NOT-RUN row with a serial
  # verdict, un-quicking the report (measured s409: eleven such re-runs).
  # KILLED is the same kind of row.  Only a measured verdict can move.
  my @movers = grep {
    my $s = $snap{$_};
    my $r = $results{$_};
    $s && $r && $r->[5] !~ /^(?:NOT-RUN|KILLED)$/
      && ($r->[5] ne $s->{status} || $r->[3] != $s->{c_ok} || $r->[4] != $s->{c_notok});
  } sort keys %results;
  return if !@movers;
  my $capped = 0;
  if (@movers > $RERUN_CAP) {
    $capped = @movers - $RERUN_CAP;
    @movers = @movers[0 .. $RERUN_CAP - 1];
  }
  printf "\n-- %d file(s) differ from the snapshot — re-running each ALONE (task #366)%s\n",
    scalar(@movers), ($capped ? "; $capped more NOT re-run (cap $RERUN_CAP)" : '');
  for my $rel (@movers) {
    my $par = $results{$rel};
    my (undef, $result_file) = tempfile(DIR => $tmpdir, SUFFIX => '.res', OPEN => 0);
    my $pid = fork();
    die "fork: $!" if !defined $pid;
    if (!$pid) {
      setpgrp(0, 0);
      $SIG{$_} = 'DEFAULT' for 'INT', 'TERM';
      %children = ();
      run_one($rel, $result_file);
      _exit(0);
    }
    waitpid($pid, 0);
    my $line = '';
    if (open my $rf, '<', $result_file) { chomp($line = <$rf> // ''); close $rf }
    my @r = split /\t/, $line, 8;
    next if @r < 6;                        # no result: keep the parallel one
    $r[7] = -1 if !defined $r[7] || $r[7] !~ /^-?\d+$/;
    classify_result(\@r);
    my $agrees = ($r[5] eq $par->[5] && $r[3] == $par->[3] && $r[4] == $par->[4]);
    my $sn = $snap{$rel};
    my $serial_is_snap = ($r[5] eq $sn->{status} && $r[3] == $sn->{c_ok} && $r[4] == $sn->{c_notok});
    record_result(\@r);
    # Say which of the THREE values agree; never assert a match that was not
    # checked (s409: the label used to claim "serial matches the snapshot"
    # for every serial != parallel row, including three-way disagreements).
    my $verdict = $agrees         ? 'REAL MOVE (both runs agree)'
                : $serial_is_snap ? 'contention — serial matches the snapshot'
                : sprintf('THREE-WAY: snapshot %s %d/%d — rows unstable? (serial recorded)',
                          $sn->{status}, $sn->{c_ok}, $sn->{c_notok});
    printf "   %-24s parallel %s %d/%-4d  serial %s %d/%-4d  %s\n",
      $rel, $par->[5], $par->[3], $par->[4], $r[5], $r[3], $r[4], $verdict;
  }
  return;
}

# baselines/perl-suite-run.tsv, the blessed per-file snapshot: name -> counts.
sub read_snapshot {
  my %s;
  open my $fh, '<', "$root/baselines/perl-suite-run.tsv" or return ();
  while (my $l = <$fh>) {
    next if $l =~ /^#/ || $l !~ /\S/;
    chomp $l;
    my @f = split /\t/, $l;
    next if @f < 6;
    $s{$f[0]} = { c_ok => $f[3], c_notok => $f[4], status => $f[5] };
  }
  close $fh;
  return %s;
}

# ----------------------------------------------------------- summary
# One row per file requested, ALWAYS — see the crash-honest reporting note
# near the top.  Called on the normal path and from the END block; the
# $reported latch keeps it single-shot either way.
# Which TAP test numbers diverged, read back from the per-test failure log the
# worker just wrote.  Returns () when there is no log (e.g. PCL produced no TAP
# at all — the summary-row case, which must never read as "nothing diverged").
sub read_diverging_rows {
  my ($rel) = @_;
  (my $safe = $rel) =~ s{/}{_}g;
  open my $lf, '<', "$faillog/$safe.fails.tsv" or return ();
  my @rows;
  while (<$lf>) {
    next if /^\s*#/;
    my (undef, $n) = split /\t/, $_, 3;
    next unless defined $n;
    # test# 0 is the log's summary row — "PCL produced no TAP" or "N PCL-only
    # test numbers".  Both are divergences, and neither can ever match a
    # numeric registry row, so they correctly keep such a file out of FIXTURE.
    push @rows, ($n =~ /^[1-9][0-9]*$/ ? $n : "summary-row($n)");
  }
  close $lf;
  return @rows;
}

# The ROW-level identity of a file's divergence, for the #185 expected-rows
# baseline.  Keys are PERL's descriptions, never numbers (#177) — see the
# %expected_rows comment for the three fallbacks.  Returns a SORTED list, so
# callers can compare it as a multiset.
# The per-test failure log of ONE file, read back as rows: [num, perl_verb,
# pcl_verb, rowkey].  ONE reader (task #993): diverging_rowkeys is the key
# projection of it and the I1 bless is the row projection, so the two can
# never disagree about what a row IS.  Returns (\@rows, $sentinel) where
# $sentinel is '*no-log*' (no log written — TRANSPILE, or a file that never
# diverged), '*empty-log*' (a log with no rows), or undef.  Also reports
# whether the log said it was TRUNCATED at the 500-row cap.
sub diverging_rows_full {
  my ($rel) = @_;
  (my $safe = $rel) =~ s{/}{_}g;
  open my $lf, '<', "$faillog/$safe.fails.tsv" or return ([], '*no-log*', 0);
  my (@rows, $truncated);
  while (<$lf>) {
    if (/^\s*#/) { $truncated = 1 if /^#\s*TRUNCATED:/; next }
    chomp;
    my (undef, $n, $pv, $cv, $desc) = split /\t/, $_, 5;
    next unless defined $n;
    $desc = '' unless defined $desc;
    $desc =~ s/\s+\z//;
    $desc =~ s{\Q$tdir\E/}{t/}g;
    # test# 0 is not a paired test: either a summary row (no TAP at all / PCL
    # renumbered), whose text carries an unstable crash signature and so
    # normalizes to one sentinel, or a PCL-ONLY row, which is real evidence
    # (do.t's two extras ARE the principle-9 divergence) and keeps its text.
    my $key;
    if ($n !~ /^[1-9][0-9]*$/) {
      $key = (defined $cv && $cv =~ /^extra\b/)
             ? '*extra* ' . (length $desc ? $desc : '(unnamed)')
             : '*summary*';
    } else {
      # An UNNAMED test's description is perl's "[at <file> line N]" marker
      # (the $tdir strip above keeps the stable line number and drops this
      # machine's absolute perl-build path — the task #217 family: a generated
      # artifact must not bake in build paths).
      $key = length $desc ? $desc : "#$n";
    }
    push @rows, [$n, $pv // '', $cv // '', $key];
  }
  close $lf;
  return (\@rows, (@rows ? undef : '*empty-log*'), ($truncated ? 1 : 0));
}

sub diverging_rowkeys {
  my ($rel) = @_;
  my ($rows, $sentinel) = diverging_rows_full($rel);
  return ($sentinel) if defined $sentinel;
  return (sort map { $_->[3] } @$rows);
}

# (in A not in B, in B not in A) over two SORTED lists, counting duplicates.
sub multiset_diff {
  my ($a, $b) = @_;
  my %count;
  $count{$_}++ for @$a;
  $count{$_}-- for @$b;
  my (@only_a, @only_b);
  for my $k (sort keys %count) {
    push @only_a, ($k) x  $count{$k} if $count{$k} > 0;
    push @only_b, ($k) x -$count{$k} if $count{$k} < 0;
  }
  return (\@only_a, \@only_b);
}

sub short_cause {
  my ($text, $max) = @_;
  $text //= '';
  $max  //= 110;
  $text =~ s/\s+/ /g;
  return length($text) > $max ? substr($text, 0, $max - 3) . '...' : $text;
}

sub row_list_excerpt {
  my ($rows) = @_;
  my @show = @$rows > 3 ? (@$rows[0 .. 2], sprintf("+%d more", @$rows - 3)) : @$rows;
  return join('; ', map { my $s = $_; $s =~ s/\s+/ /g;
                          length($s) > 60 ? substr($s, 0, 57) . '...' : $s } @show);
}

# --bless-rows: rewrite baselines/perl-suite-expected-rows.tsv from THIS run.  Only
# files this run actually measured are touched — a partial run must never erase
# the baseline for files it did not look at (same reasoning as the sweep's
# save-status).  A registered file that came back OK loses its rows here, which
# is what clears a STALE row after a fix.
sub bless_expected_rows {
  my %rows;
  if (open my $rf, '<', $expected_rows_tsv) {
    while (<$rf>) {
      chomp;
      next if /^\s*(?:#|$)/;
      my ($rel, $key) = split /\t/, $_, 2;
      push @{ $rows{$rel} }, $key if defined $key;
    }
    close $rf;
  }
  my $touched = 0;
  for my $rel (keys %results) {
    next unless exists $expected{$rel};
    my $st = $results{$rel}[5];
    next unless $st =~ /^(?:DIFF|TRANSPILE|TIMEOUT|XDIFF|STALE|OK)$/;
    # A hand-placed *rows-unstable* opt-out survives re-blessing: it is a
    # measured claim about the file (its row set is nondeterministic), not
    # something a single run may overwrite with that run's rows.
    next if @{ $rows{$rel} || [] } == 1 && $rows{$rel}[0] eq '*rows-unstable*';
    delete $rows{$rel};
    $touched++;
    next if $st eq 'OK';
    my @keys = diverging_rowkeys($rel);
    $rows{$rel} = \@keys if @keys;
  }
  open my $out, '>', $expected_rows_tsv or die "write $expected_rows_tsv: $!";
  print $out <<'HDR';
# perl-suite-expected-rows.tsv — the ROW baseline behind XDIFF (task #185).
# GENERATED: `tools/run-perl-suite.pl --bless-rows` rewrites the files that run
# measured.  Do not hand-edit; edit baselines/perl-suite-expected.tsv (the reasons)
# and re-bless.
#
# One line per DIVERGING ROW of a registered file: <rel-path><TAB><rowkey>.
# rowkey = PERL's test description (never a test number — task #177 proved
# numbers are the unstable coordinate), with four fallbacks: "#N" for a
# genuinely unnamed test, "*extra* <desc>" for a PCL-ONLY TAP row, "*summary*"
# for the log's test#-0 summary rows, and "*no-log*" for a file that produced
# no per-test log at all (TRANSPILE).
#
# ENFORCEMENT: the multiset here must EQUAL the run's diverging rows.  An extra
# row keeps the file DIFF and names the intruder (a new bug cannot hide inside
# a blessed feature gap); a row that stopped diverging makes the file STALE.
#
# *rows-unstable* — a file whose ONLY entry is this opts OUT of the row check
# (the file-level reason covers it wholesale, as before #185).  It must be
# hand-placed and justified by a MEASUREMENT that the row set is
# nondeterministic; --bless-rows never invents it and never overwrites it.
HDR
  for my $rel (sort keys %rows) {
    print $out "$rel\t$_\n" for @{ $rows{$rel} };
  }
  close $out;
  my $n = 0; $n += scalar @{ $rows{$_} } for keys %rows;
  printf "blessed expected-rows: %d rows over %d files (%d files re-measured) -> %s\n",
    $n, scalar(keys %rows), $touched, $expected_rows_tsv;
  return;
}

# ── I1/I2/I4: which files each instrument owns, and the blesses ─────────────
#
# A file is ROW-GATED here unless another registry already gates its rows:
# perl-suite-expected.tsv (XDIFF) and perl-suite-fixture.tsv (FIXTURE) are
# both per-ROW and all-or-nothing, so a file in either would be gated twice
# and a moved row would read twice and be fixed once.
sub row_gated_here { my ($rel) = @_; return !$expected{$rel} && !$fixture{$rel} }

# Did this run produce a COMPARABLE TAP stream for this file?  A file that
# never got to its assertions cannot verify anything about its blessed rows —
# the same rule as sweep-diff.pl's ran_clean(), and the reason a TRANSPILE this
# run reports its 50 vanished blessed rows as UNVERIFIED, never as 50 FIXED.
sub row_verifiable {
  my ($st) = @_;
  return $st !~ /^(?:TRANSPILE|TIMEOUT|KILLED|NOT-RUN|MISSING|NO-RESULT)$/;
}

# This session's identity for a stamp.  PCL_SESSION when the operator sets it
# (e.g. s465), else the short sha — never a guess that reads like a session.
sub stamp_session {
  return $ENV{PCL_SESSION} if $ENV{PCL_SESSION};
  my $sha = `git -C \Q$root\E rev-parse --short HEAD 2>/dev/null`;
  chomp $sha;
  return length $sha ? $sha : 'unknown';
}
sub stamp_today { my @t = localtime; return sprintf("%04d-%02d-%02d", $t[5]+1900, $t[4]+1, $t[3]) }

# --bless-fails: rewrite baselines/perl-suite-fails.tsv from THIS run.  Only
# files this run MEASURED are touched (the bless_expected_rows rule: a partial
# run must never erase rows for files it did not look at).
sub bless_fail_rows {
  my %rows;   # rel -> arrayref of [num, pv, cv, key]
  %rows = %blessed_fail_meta;
  my ($touched, $truncated) = (0, 0);
  for my $rel (keys %results) {
    my $st = $results{$rel}[5];
    if (!row_gated_here($rel)) { delete $rows{$rel}; next }
    next unless row_verifiable($st);       # unmeasured: keep what is blessed
    delete $rows{$rel};
    $touched++;
    my ($rr, undef, $trunc) = diverging_rows_full($rel);
    $truncated++ if $trunc;
    $rows{$rel} = $rr if @$rr;
  }
  open my $out, '>', $fails_tsv or die "write $fails_tsv: $!";
  print $out <<'HDR';
# perl-suite-fails.tsv — the ROW-level fail baseline for tools/run-perl-suite.pl
# (task #993 / docs/plan-test-audit-s464.md §3 I1).  The companion suite's
# answer to baselines/fail-baseline.tsv: until this file existed the 273 DIFF
# files were blessed as COUNTS only, so #964's failing row sat inside op/sub.t's
# "12 not ok" for months with nothing naming it.
#
# One line per DIVERGING TAP row:
#   <rel> <TAB> <PERL's test#> <TAB> <perl verb> <TAB> <PCL verb> <TAB> <rowkey>
# rowkey = PERL's test DESCRIPTION (test NUMBERS are the unstable coordinate,
# task #177), with the same fallbacks as perl-suite-expected-rows.tsv: "#N" for
# an unnamed test, "*extra* <desc>" for a PCL-ONLY row, "*summary*" for the
# log's test#-0 summary rows.  The JOIN KEY is (rel, rowkey), compared as a
# MULTISET.  got/expected are NOT here: perl's TAP does not carry them on the
# ok line (they are in the diag, which the runner does not keep).
#
# GENERATED: `tools/run-perl-suite.pl --all --bless-fails`.  A run only rewrites
# the files it MEASURED; a file that produced no comparable TAP this run keeps
# its blessed rows (they are UNVERIFIED, never "fixed").
#
# NOT IN HERE: files registered in perl-suite-expected.tsv (XDIFF) or
# perl-suite-fixture.tsv (FIXTURE) — their rows are already gated per row by
# those registries, and one row must have exactly one gate.
#
# TRUNCATION: a file with more than 500 diverging rows writes only its first
# 500 to the per-test log, so its multiset here is PARTIAL and a new failure
# past row 500 is invisible.  The log says so in a `# TRUNCATED:` line and the
# runner counts such files in every report.
HDR
  printf $out "# taken-at: %s %s\n", stamp_session(), stamp_today();
  my $n = 0;
  for my $rel (sort keys %rows) {
    for my $r (sort { $a->[3] cmp $b->[3] || ($a->[0] // 0) <=> ($b->[0] // 0) } @{ $rows{$rel} }) {
      print $out join("\t", $rel, @$r), "\n";
      $n++;
    }
  }
  close $out;
  printf "blessed fail-rows: %d row(s) over %d file(s) (%d file(s) re-measured, %d truncated) -> %s\n",
    $n, scalar(keys %rows), $touched, $truncated, $fails_tsv;
  return;
}

# --bless-shortfall: rewrite this population's rows in the SHARED
# baselines/row-shortfall.tsv.  The sweep's rows (perl-tests/…) are copied
# through untouched — one file, two populations, exactly like the drop census.
sub bless_shortfall {
  my %rows = map { $_ => { %{ $shortfall_base{$_} } } } keys %shortfall_base;
  my $touched = 0;
  for my $rel (sort keys %results) {
    my $r = $results{$rel};
    my $key = "t/$rel";
    next if $r->[5] =~ /^(?:NOT-RUN|KILLED|MISSING|NO-RESULT)$/;   # unmeasured
    $touched++;
    my $short = ($r->[1] + $r->[2]) - ($r->[3] + $r->[4]);
    $short = 0 if $short < 0;
    if (!$short) { delete $rows{$key}; next }
    # A file already REGISTERED as an expected divergence (XDIFF) or a harness
    # FIXTURE artifact has a cause by construction: the registration covers its
    # whole divergence, all-or-nothing (#185), and rows perl produced that PCL
    # did not are part of that divergence.  Calling those UNEXPLAINED would put
    # ~325,000 uniprops rows into the audit's queue, which is the opposite of
    # what the queue is for.  An EXISTING hand-written cause always wins.
    my $cause = $shortfall_base{$key} ? $shortfall_base{$key}{cause} : 'UNEXPLAINED';
    if ($cause =~ /^UNEXPLAINED/) {
      $cause = 'XDIFF: '   . short_cause($expected{$rel}, 90)     if $expected{$rel};
      $cause = 'FIXTURE: ' . short_cause($fixture{$rel}{cause}, 90) if $fixture{$rel};
    }
    $rows{$key} = { rows => $short, cause => $cause };
  }
  PCLShortfall::write_shortfall($shortfall_tsv, \%rows,
                                stamp_session() . ' ' . stamp_today());
  my $sum = 0; $sum += $rows{$_}{rows} for keys %rows;
  printf "blessed shortfall: %d row(s) over %d file(s) (%d companion file(s) re-measured) -> %s\n",
    $sum, scalar(keys %rows), $touched, $shortfall_tsv;
  return;
}

# --bless-stamps: rewrite baselines/perl-suite-notrun-stamps.tsv.  A file this
# run MEASURED gets this session's stamp; a file it did not keeps the old one
# (that is the whole point — the stamp is the AGE of the last measurement).
sub bless_stamps {
  my %out = map { $_ => { %{ $stamp{$_} } } } keys %stamp;
  my ($sess, $date) = (stamp_session(), stamp_today());
  for my $rel (sort keys %stamp_tracked) {
    # Every tracked file gets a row.  A file this run did NOT measure keeps the
    # stamp it had (that IS the age); one it measured gets this session's.
    $out{$rel} //= { session => 'NEVER', date => '-', note => '' };
    my $r = $results{$rel};
    if ($r && $r->[5] !~ /^(?:NOT-RUN|KILLED|MISSING|NO-RESULT)$/) {
      $out{$rel} = { session => $sess, date => $date,
                     note => sprintf("%s — measured %s %d/%d",
                                     short_cause($stamp_tracked{$rel}, 60),
                                     $r->[5], $r->[3], $r->[4]) };
    } elsif (($out{$rel}{session} // 'NEVER') eq 'NEVER') {
      $out{$rel}{note} = short_cause($stamp_tracked{$rel}, 90);
    }
  }
  open my $fh, '>', $stamps_tsv or die "write $stamps_tsv: $!";
  print $fh <<'HDR';
# perl-suite-notrun-stamps.tsv — WHEN was each not-run file last measured?
# (task #993 / docs/plan-test-audit-s464.md §3 I4)
#
#   <rel> <TAB> <session> <TAB> <date> <TAB> <note>
#
# One row per file tools/run-perl-suite.pl does not measure in its default
# form: QUARANTINED (task #160), QUICK-SKIP (the #326 hang set), QUICK-CAPPED
# (a registered allowance above the 120 s quick cap), NEED-HARNESS.  A hole
# that is only ever printed as "not run" says nothing about how OLD it is —
# op/list.t has been quarantined since s320 and the hang set is reached only by
# a full `--all`.  The stamp makes the age countable, and every run prints it.
#
# session `NEVER` = this file has no measurement on record at all.
# GENERATED: `tools/run-perl-suite.pl --all --bless-stamps` (a file measured by
# that run gets the run's session; one it did not measure keeps its old stamp).
HDR
  printf $fh "# taken-at: %s %s\n", $sess, $date;
  print $fh join("\t", $_, $out{$_}{session}, $out{$_}{date}, $out{$_}{note} // ''), "\n"
    for sort keys %out;
  close $fh;
  printf "blessed stamps: %d file(s) -> %s\n", scalar(keys %out), $stamps_tsv;
  return;
}

# The registry classification of ONE result row (expected-divergence,
# fixture-artifact).  EXTRACTED from the dispatch loop (task #366) so the
# serial re-run of moved rows classifies identically — two copies of this
# would be two verdicts for the same file.
sub classify_result {
  my ($r) = @_;
  if (my $reason = $expected{$r->[0]}) {
    if ($r->[5] =~ /^(?:DIFF|TRANSPILE|TIMEOUT)$/) {
      my @actual = diverging_rowkeys($r->[0]);
      my @reg    = @{ $expected_rows{$r->[0]} || [] };
      # A file whose row set is MEASURED nondeterministic (see the
      # *rows-unstable* paragraph in the rows baseline) opts out of the row
      # check — the file-level reason covers it wholesale, as before #185.
      # One entry, never mixed with real rows, and it has to be put there by
      # hand: --bless-rows will not invent it.
      my ($new, $gone) = (@reg == 1 && $reg[0] eq '*rows-unstable*')
                         ? ([], []) : multiset_diff(\@actual, \@reg);
      if (@$new) {
        $r->[6] = join(' | ', grep { length } $r->[6] // '',
                     sprintf("NOT fully registered: %d unregistered diverging row(s): %s",
                             scalar(@$new), row_list_excerpt($new)),
                     $reason);
      } elsif (@$gone) {
        $r->[5] = 'STALE';
        $r->[6] = sprintf("%d registered row(s) no longer diverge (%s) — re-bless with --bless-rows",
                        scalar(@$gone), row_list_excerpt($gone));
      } else {
        $r->[5] = 'XDIFF';
        $r->[6] = join(' | ', grep { length } $r->[6] // '',
                     (@reg == 1 && $reg[0] eq '*rows-unstable*'
                      ? 'rows NOT checked (*rows-unstable*)' : ()),
                     $reason);
      }
    } elsif ($r->[5] eq 'OK') {
      $r->[5] = 'STALE';
      $r->[6] = "expected-divergence row now PASSES — remove it from baselines/perl-suite-expected.tsv";
    }
  }
  # Fixture-artifact registry (task #172): per-ROW, and only ever applied to
  # a file whose ENTIRE divergence is registered rows.  One unregistered row
  # and the file stays DIFF — naming the intruders, so a real bug appearing
  # inside a fixture-affected file can never hide behind the registration.
  elsif (my $fx = $fixture{$r->[0]}) {
    if ($r->[5] eq 'DIFF') {
      my @diverging = read_diverging_rows($r->[0]);
      my @unreg = grep { !$fx->{rows}{$_} } @diverging;
      if (!@diverging) {
        $r->[6] = join(' | ', grep { length } $r->[6] // '',
                     'fixture rows registered but no per-row log — left DIFF');
      } elsif (@unreg) {
        $r->[6] = join(' | ', grep { length } $r->[6] // '',
                     "NOT a pure fixture artifact: unregistered failing rows @unreg");
      } else {
        $r->[5] = 'FIXTURE';
        $r->[6] = join(' | ', grep { length } $r->[6] // '', $fx->{cause});
      }
    } elsif ($r->[5] eq 'OK') {
      $r->[5] = 'STALE';
      $r->[6] = "fixture-artifact row now PASSES — remove it from baselines/perl-suite-fixture.tsv";
    }
  }
  return;
}

# ── I1: the ROW DIFF (task #993) ────────────────────────────────────────────
# The buckets are sweep-diff.pl's, for the same reasons:
#   NEW ROW    a diverging row this run produced that is not blessed
#   FIXED ROW  a blessed row that stopped diverging, in a file that ran
#   UNVERIFIED a blessed row absent because the file produced nothing
#              comparable this run (TRANSPILE/TIMEOUT/KILLED) — NEVER "fixed"
#   LOST       passing rows (C_ok) this run did not produce, vs the snapshot
# Printed on EVERY run, including the clean ones, and it SAYS SO when it could
# not run: a check that goes quiet when it cannot run is indistinguishable
# from one that passed.
sub report_row_diff {
  print "\n";
  if (!%blessed_fails && !$fails_tsv_existed) {
    print "ROW DIFF: NOT CHECKED — no $fails_tsv"
        . " (bless one with: tools/run-perl-suite.pl --all --bless-fails)\n";
    return;
  }
  my (@new, @fixed, @unver, @trunc, @unstable);
  my %seen;
  for my $rel (sort keys %results) {
    $seen{$rel} = 1;
    next unless row_gated_here($rel);
    my $st = $results{$rel}[5];
    my @reg = @{ $blessed_fails{$rel} || [] };
    my ($rows, undef, $tr) = diverging_rows_full($rel);
    push @trunc, $rel if $tr;
    my @actual = sort map { $_->[3] } @$rows;
    my ($only_now, $only_reg) = multiset_diff(\@actual, \@reg);
    if (row_verifiable($st)) {
      push @new,   [$rel, $_] for @$only_now;
      push @fixed, [$rel, $_] for @$only_reg;
    } else {
      push @unstable, [$rel, $_] for @$only_now;
      push @unver,    [$rel, $_, $st] for @$only_reg;
    }
  }
  # A blessed file this run never looked at (a --dir run, or a file gone from
  # t/): its rows are unverified, exactly like sweep-diff's DID NOT RUN.  Kept
  # as ONE number — for a --dir run this is every other file in the baseline,
  # and 25,000 lines of "not looked at" would bury the four rows that moved.
  my ($absent_rows, $absent_files) = (0, 0);
  for my $rel (sort keys %blessed_fails) {
    next if $seen{$rel};
    $absent_files++;
    $absent_rows += scalar @{ $blessed_fails{$rel} };
  }
  # LOST: the sweep's fourth bucket, read from the count snapshot — the row
  # buckets above all read FAILING rows, so none of them can see a file that
  # simply stopped producing PASSING ones.
  my %snap = read_snapshot();
  my @lost_rows;
  for my $rel (sort keys %results) {
    my $s = $snap{$rel} or next;
    my $r = $results{$rel};
    next if $r->[5] =~ /^(?:NOT-RUN|KILLED)$/;
    push @lost_rows, [$rel, $s->{c_ok} - $r->[3], $s->{status}, $r->[5]]
      if $r->[3] < $s->{c_ok};
  }
  printf "ROW DIFF vs %s: %d NEW ROW, %d FIXED ROW, %d UNVERIFIED, %d LOST%s\n",
    $fails_tsv, scalar(@new), scalar(@fixed), scalar(@unver), scalar(@lost_rows),
    (@new ? '  <-- a NEW failing row: this run is NOT clean' : '');
  printf "  + %-24s %s\n", $_->[0], short_cause($_->[1], 90) for @new[0 .. ($#new > 39 ? 39 : $#new)];
  print  "  ... and " . (@new - 40) . " more NEW ROW(s)\n" if @new > 40;
  printf "  - %-24s %s\n", $_->[0], short_cause($_->[1], 90) for @fixed[0 .. ($#fixed > 39 ? 39 : $#fixed)];
  print  "  ... and " . (@fixed - 40) . " more FIXED ROW(s)\n" if @fixed > 40;
  if (@unver) {
    my %by; $by{ $_->[0] }++ for @unver;
    printf "  ? %-24s %d blessed row(s) unverified — %s\n", $_, $by{$_},
      ($results{$_} ? $results{$_}[5] : 'NOT RUN') for sort keys %by;
  }
  if (@unstable) {
    my %by; $by{ $_->[0] }++ for @unstable;
    printf "  ~ %-24s %d new row(s) in a file that produced no comparable TAP — noise, not a regression\n",
      $_, $by{$_} for sort keys %by;
  }
  printf "  ! %-24s -%d passing row(s) (snapshot %s, now %s)\n", @$_[0,1,2,3] for @lost_rows;
  printf "  %d blessed row(s) in %d file(s) this run did not look at — NOT verified, not fixed\n",
    $absent_rows, $absent_files if $absent_rows;
  printf "  %d file(s) exceed the 500-row log cap — their row baselines are PARTIAL: %s\n",
    scalar(@trunc), join(', ', @trunc) if @trunc;
  return;
}

# ── I2: the SHORTFALL, this population's half (task #993) ───────────────────
sub report_shortfall {
  print "\n";
  # Declared one at a time on purpose: `my (@up, @down, $x) = ((), (), 0)`
  # FLATTENS — @up swallows the whole right-hand side and every scalar comes
  # back undef, which cost this report a die at its first shortfall row.
  my (@up, @down);
  my $sum = 0;
  my $unexplained = 0;
  my $unexplained_files = 0;
  for my $rel (sort keys %results) {
    my $r = $results{$rel};
    next if $r->[5] =~ /^(?:NOT-RUN|KILLED|MISSING|NO-RESULT)$/;
    my $short = ($r->[1] + $r->[2]) - ($r->[3] + $r->[4]);
    $short = 0 if $short < 0;
    $sum += $short;
    my $b = $shortfall_base{"t/$rel"};
    my $was = $b ? $b->{rows} : 0;
    push @up,   [$rel, $was, $short] if $short > $was;
    push @down, [$rel, $was, $short] if $short < $was;
    if ($short && (!$b || $b->{cause} eq 'UNEXPLAINED')) {
      $unexplained += $short;
      $unexplained_files++;
    }
  }
  if (!%shortfall_base) {
    printf "SHORTFALL: NOT CHECKED — no %s (bless one with --bless-shortfall);"
         . " this run: %d row(s) perl produced and PCL did not\n", $shortfall_tsv, $sum;
    return;
  }
  printf "SHORTFALL (perl rows PCL never produced): %d in this run%s\n", $sum,
    (@up ? '  <-- a file lost rows it used to produce: this run is NOT clean' : '');
  printf "  + %-24s %d -> %d row(s) NEVER PRODUCED — NEW shortfall\n", @$_[0,1,2] for @up;
  printf "  - %-24s %d -> %d row(s) — fixed; EDIT the baseline row\n", @$_[0,1,2] for @down;
  printf "  UNEXPLAINED: %d row(s) in %d file(s) have no cause — that is the audit's queue (#993)\n",
    $unexplained, $unexplained_files;
  return;
}

# ── I4: HOW OLD is each not-run hole? (task #993) ───────────────────────────
sub report_stamps {
  print "\n";
  my @holes = sort keys %stamp_tracked;
  if (!@holes) { print "NOT-RUN STAMPS: no tracked hole among this run's files\n"; return }
  my @unmeasured = grep { !$results{$_} || $results{$_}[5] =~ /^(?:NOT-RUN|KILLED)$/ } @holes;
  printf "NOT-RUN STAMPS: %d file(s) the DEFAULT form does not measure; %d of them"
       . " were not measured by THIS run — last measured when?\n",
    scalar(@holes), scalar(@unmeasured);
  my $never = 0;
  for my $rel (@holes) {
    my $s = $stamp{$rel};
    my $measured_now = $results{$rel} && $results{$rel}[5] !~ /^(?:NOT-RUN|KILLED)$/;
    $never++ if !$measured_now && (!$s || ($s->{session} // 'NEVER') eq 'NEVER');
    printf "  %-24s last measured %-10s %-12s %s\n", $rel,
      ($measured_now ? '(this run)' : $s ? $s->{session} : 'NEVER'),
      ($measured_now ? stamp_today() : $s ? $s->{date} : '-'),
      short_cause($stamp_tracked{$rel}, 70);
  }
  printf "  %d of them have NO measurement on record at all\n", $never if $never;
  print  "  (re-stamp with: PCL_SESSION=sNNN tools/run-perl-suite.pl --all --bless-stamps)\n";
  return;
}

sub record_result {
  my ($r) = @_;
  $results{$r->[0]} = $r;
  print $JOURNAL join("\t", @$r), "\n" if $JOURNAL;
  reap_orphan_transpilers();
  return;
}

# Per-file SESSION isolation (task #367) + orphaned-transpiler reaping live in
# tools/lib/PCLProc.pm — ONE copy shared with sweep-perl-tests.pl (s413, #387
# family 6); the long notes on WHY the session is the handle are there.

sub emit_report {
  return 0 if $reported++;
  # Also at the END: the per-file reap in record_result cannot catch a server
  # orphaned by the LAST file's kill (measured s396 — op/cond.t runs last, in
  # the solo phase, and left a 5.6 GB server behind after the run finished).
  reap_orphan_transpilers();
  # Files with no row: the run died before it got to them (task #157).
  # KILLED/NOT-RUN are UNEXPLAINED statuses, so a died run exits nonzero and
  # its tsv is complete — the release gate can see the hole instead of
  # reading absence as "nothing to report".
  my %inflight = map { $_->{rel} => 1 } values %children;
  my @lost = grep { !$results{$_} } @files;
  for my $rel (@lost) {
    my ($st, $why) = $inflight{$rel}
      ? ('KILLED',  '(run died with this file in flight)')
      : ('NOT-RUN', '(run died before this file started)');
    record_result([$rel, 0, 0, 0, 0, $st, $why, -1]);
  }
  if (@lost) {
    printf "\n!! RUN DID NOT COMPLETE — %d of %d files have no measurement.\n"
         . "!! Most likely out of memory: re-run with --jobs 2-4 on a quiet\n"
         . "!! machine.  Rows below are marked KILLED/NOT-RUN, never OK.\n",
      scalar(@lost), scalar(@files);
  }

  my %by_status;
  push @{ $by_status{ $results{$_}[5] } }, $_ for keys %results;
  print "----\n";
  for my $st (sort keys %by_status) {
    my @f = sort @{ $by_status{$st} };
    printf "%-8s %3d%s\n", $st, scalar @f,
      ($st eq 'OK' ? '' : ':  ' . join(', ', @f));
  }
  my $n_bad = grep { $results{$_}[5] !~ /^(?:OK|NOTAP|XDIFF|FIXTURE)$/ } keys %results;
  printf "%d files: %d OK, %d NOTAP, %d XDIFF (expected, see baselines/perl-suite-expected.tsv), %d FIXTURE (harness artifact, see baselines/perl-suite-fixture.tsv), %d UNEXPLAINED\n",
    scalar(keys %results), scalar(@{ $by_status{OK} // [] }),
    scalar(@{ $by_status{NOTAP} // [] }), scalar(@{ $by_status{XDIFF} // [] }),
    scalar(@{ $by_status{FIXTURE} // [] }), $n_bad;
  # Quarantined files are NOT-RUN by construction, so they are already counted
  # as UNEXPLAINED above — say so, so nobody reads the number as new breakage.
  # #366: the serial re-run happens after the dispatch loop, so an interrupted
  # run (op/cond.t's memory guard SIGTERMs the parent about half the time)
  # never reaches it.  SAY so — an unmeasured mover that reads as measured is
  # the same failure as a silently skipped file.
  print "NOTE: the serial re-run of snapshot-differing files (task #366) did NOT run —\n"
      . "  this run was interrupted.  Re-run any moved file ALONE before believing it.\n"
    if !$rerun_movers_done;
  printf "%d of those UNEXPLAINED are QUARANTINED (never run this session): %s\n",
    scalar(@quarantined), join(', ', map { "$_ ($QUARANTINE{$_})" } @quarantined)
    if @quarantined;
  # Same for the --quick holes: the whole point of quick mode is that what it
  # does not measure is COUNTED and NAMED here, never inferred from an absence.
  if ($quick) {
    printf "--quick did not run %d file(s): %d hang-set (task #326), %d registered allowance > %ds\n",
      scalar(@quick_skipped) + scalar(@quick_capped),
      scalar(@quick_skipped), scalar(@quick_capped), $QUICK_CAP;
    printf "  QUICK-SKIP    %-24s %s\n", $_, short_cause($QUICK_SKIP{$_}) for @quick_skipped;
    printf "  QUICK-CAPPED  %-24s allowance %ds (%s)\n",
      $_, $file_timeout{$_}{secs}, short_cause($file_timeout{$_}{cause}) for @quick_capped;
    print  "  re-run without --quick to measure them (baselines/perl-suite-timeouts.tsv holds the allowances)\n";
  }
  print "failure log: $faillog/*.fails.tsv\n" if grep { -f $_ } glob "$faillog/*.fails.tsv";

  # SUITE FILES WITH NO SNAPSHOT ROW (s431 side finding, ruled s433 §A.4).
  # baselines/perl-suite-run.tsv carried 523 rows for 528 files and nobody noticed
  # for months: five files were simply ABSENT — not quarantined, not
  # registered, just missing — so a regression in any of them could never read
  # as a mover, because rerun_movers_serially() compares against the snapshot
  # and a file with no row has nothing to differ from.  That is the #176
  # family: a hole inferred from an absence.  It was found by COUNTING two
  # numbers by hand; it must not need counting again, so every run names it.
  # Printed, never fatal: this is a fact about the BASELINE, not a measurement
  # this run failed.  A row is added by splicing this run's FIRST measurement
  # in with a `# sNNN first measurement` marker (see the snapshot's header).
  {
    my %snap = read_snapshot();
    if (!%snap) {
      print "SNAPSHOT: NOT CHECKED — no baselines/perl-suite-run.tsv\n";
    } else {
      my @norow = grep { !$snap{$_} } sort keys %results;
      printf "SNAPSHOT: %d of %d file(s) measured here have NO row in baselines/perl-suite-run.tsv%s\n",
        scalar(@norow), scalar(keys %results), (@norow ? ':' : ' — every file is covered');
      printf "  no-snapshot-row  %s\n", $_ for @norow;
      print "  splice each with its FIRST measurement (marker: # sNNN first measurement)"
          . " — until then it can never read as a mover (#176 family)\n" if @norow;
      # ... and the SAME hole from the other side, on a full scan only: a
      # snapshot row for a file `--all` never even considers.  Found s434:
      # the five rows s431 spliced in are all need-harness files (`BEGIN` +
      # @INC), which the dir scan filters out — so they were measured by
      # NAMING them and no default run has measured them since.  A row nothing
      # refreshes cannot move either; it is a snapshot of one day, forever.
      # Only on $all, because for a --dir run every other row is trivially
      # "not covered" and the line would be pure noise.
      if ($all) {
        my @unrefreshed = grep { !$results{$_} } sort keys %snap;
        printf "SNAPSHOT: %d row(s) for files this --all scan does not run%s\n",
          scalar(@unrefreshed), (@unrefreshed ? ' (need-harness, or gone from t/):' : '');
        printf "  never-refreshed  %s\n", $_ for @unrefreshed;
        print "  re-measure by NAMING the file (tools/run-perl-suite.pl <rel>) — the scan\n"
            . "  filters files that fiddle \@INC in BEGIN, so --all can never move these rows\n"
          if @unrefreshed;
      }
    }
  }

  # DROPS vs the census (task #343, ruled §6.5).  The companion half of the
  # sweep's DROPS bucket: this population owns 63 of the census's 72 files, and
  # a dropped statement is invisible in the TAP comparison above — the row it
  # would have produced simply does not exist.  MORE than the census = a new
  # silent drop; FEWER = a fix, and the census row leaves by EDIT.
  {
    my %census;
    my $census_path = "$root/baselines/parse-error-drop-census-s399.tsv";
    if (open my $cf, '<', $census_path) {
      while (my $l = <$cf>) {
        chomp $l;
        next if !length $l || $l =~ /^#/;
        my ($rel, $n) = split /\t/, $l;
        $census{$rel} = $n if defined $n && $n =~ /^\d+$/;
      }
      close $cf;
    }
    if (!%census) {
      print "DROPS: NOT CHECKED — no census at $census_path\n";
    } else {
      my ($sum, $unmeasured, @up, @down) = (0, 0);
      for my $rel (sort keys %results) {
        my $now = $results{$rel}[7];
        if (!defined $now || $now < 0) { $unmeasured++; next }
        $sum += $now;
        my $was = $census{"t/$rel"} // 0;
        push @up,   [$rel, $was, $now] if $now > $was;
        push @down, [$rel, $was, $now] if $now < $was;
      }
      printf "DROPS: %d dropped statement(s) in this run%s\n", $sum,
        ($unmeasured ? " ($unmeasured file(s) NOT MEASURED — no CL)" : '');
      printf "  + %-24s %d -> %d  NEW silent drop\n", @$_[0,1,2] for @up;
      printf "  - %-24s %d -> %d  fixed; EDIT the census row\n", @$_[0,1,2] for @down;
    }
  }

  report_row_diff();
  report_shortfall();
  report_stamps();

  if ($JOURNAL) {
    printf $JOURNAL "# %s\n", @lost ? "INCOMPLETE: @{[scalar @lost]} files unmeasured"
                                    : "complete";
    close $JOURNAL;
    print "journal: $journal_file\n" if @lost;
  }
  if ($tsv_file) {
    open my $tf, '>', $tsv_file or die "write $tsv_file: $!\n";
    # Legend at the point of use — this has been misread twice (s316v).
    print $tf "# file  P_ok  P_notok  C_ok  C_notok  status  sig  drops   [P=PERL, C=PCL]\n";
    print $tf "# drops = statements the compiler could not lower, replaced by nil in the\n";
    print $tf "#   emitted CL (#138 family, task #343); -1 = NOT MEASURED (no CL produced).\n";
    print $tf "#   Baseline: baselines/parse-error-drop-census-s399.tsv.  Rows written before s402\n";
    print $tf "#   have no drops column — that is UNKNOWN, not zero.\n";
    print $tf "# NOTAP = PERL produced no TAP (row not comparable; says nothing bad about PCL)\n";
    print $tf "# XDIFF = expected divergence, baselines/perl-suite-expected.tsv (a blessed not-supported.md gap)\n";
    print $tf "# FIXTURE = harness artifact, baselines/perl-suite-fixture.tsv (the MEASUREMENT differs, not PCL)\n";
    print $tf "# NOT-RUN with QUARANTINED = deliberately not run this session; UNMEASURED, never passing\n";
    print $tf "# INCOMPLETE RUN — KILLED/NOT-RUN rows are unmeasured, not passing\n" if @lost;
    print $tf join("\t", @{ $results{$_} }), "\n" for sort keys %results;
    close $tf;
    print "wrote $tsv_file\n";
  }
  return $n_bad;
}
