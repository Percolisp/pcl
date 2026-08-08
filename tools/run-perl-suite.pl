#!/usr/bin/env perl

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
#                      registered in docs/perl-suite-timeouts.tsv gets the
#                      LARGER of its allowance and this (see that file)
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
#   docs/perl-suite-expected.tsv maps `rel<TAB>reason` (reason should cite the
#   docs/not-supported.md section, and the plan doc when one exists).  A
#   divergent file with a row becomes status XDIFF — still runs, still prints
#   its row + reason, but does not fail the exit code.  If an expected file
#   comes back OK the row is STALE (flagged AND fails the run) so a fixed
#   feature can never hide behind an old expectation — same philosophy as the
#   sweep's skip-registry (docs/test-skip-registry.md).  Crashing files may be
#   marked only when the crash itself is the documented gap; everything
#   UNEXPLAINED stays a fix/triage target.
#
#   XDIFF IS GRANTED PER ROW, all-or-nothing (task #185), exactly like FIXTURE:
#   docs/perl-suite-expected-rows.tsv holds the blessed multiset of diverging
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
# Still skipped as need-harness: files that fiddle @INC in BEGIN — they pull
# build-tree modules from ../lib that PCL cannot load.  That is now the ONLY
# filter, and dir scans report its count, so coverage is visible.
#
# Harness-fixture artifacts — a THIRD category, and not not-support:
#   docs/perl-suite-fixture.tsv maps `rel<TAB>rows<TAB>cause` for rows that
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
use Cwd qw(abs_path);
use POSIX qw(:sys_wait_h _exit);
use FindBin;
use lib "$FindBin::RealBin/lib";
# Description-based TAP pairing (task #177) — unit-tested in tools/t/tap-align.t.
use PclTapAlign qw(tap_rows align_taps);

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

my $tdir = "/home/bernt/perl5/perlbrew/build/perl-5.40.3/perl-5.40.3/t";
my ($all, $no_core, $tsv_file);
my $jobs = 8;
my $timeout = 90;
my $faillog = "$root/.suitelog";
my $expected_tsv = "$root/docs/perl-suite-expected.tsv";
my $expected_rows_tsv = "$root/docs/perl-suite-expected-rows.tsv";
my $fixture_tsv  = "$root/docs/perl-suite-fixture.tsv";
my $timeouts_tsv = "$root/docs/perl-suite-timeouts.tsv";
my $bless_rows;
my (@dirs, @files);
while (@ARGV) {
  my $a = shift @ARGV;
  if    ($a eq '--bless-rows')     { $bless_rows = 1 }
  elsif ($a eq '--tdir')           { $tdir = shift @ARGV }
  elsif ($a eq '--dir')            { push @dirs, shift @ARGV }
  elsif ($a eq '--all')            { $all = 1 }
  elsif ($a eq '--jobs')           { $jobs = shift @ARGV }
  elsif ($a eq '--timeout')        { $timeout = shift @ARGV }
  elsif ($a eq '--no-core')        { $no_core = 1 }
  elsif ($a eq '--tsv')            { $tsv_file = shift @ARGV }
  elsif ($a eq '--faillog')        { $faillog = shift @ARGV }
  else                             { push @files, $a }
}
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
# A DIFFERENT category from %expected — see docs/perl-suite-fixture.tsv and
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
for my $d (@dirs) {
  my ($n_all, $n_harness) = (0, 0);
  for my $f (sort glob "$tdir/$d/*.t") {
    $n_all++;
    my $base = basename($f);
    open my $fh, '<', $f or next;
    local $/; my $src = <$fh>; close $fh;
    # Skip files pulling build-tree modules via @INC fiddling in BEGIN.
    # (`require './test.pl'` and `chdir 't'` files run via the shadow t/.)
    if ($src =~ m{BEGIN[^\n]*\@INC}) { $n_harness++; next }
    push @files, "$d/$base";
  }
  printf STDERR "scan t/%-8s %3d files: %3d runnable, %3d need-harness\n",
    $d, $n_all, $n_all - $n_harness, $n_harness;
}
@files or die "no files (give t-relative paths, --dir <subdir>, or --all)\n";

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
# --core must precede all other toplevel sbcl options.
my $sbcl = $core ? "sbcl --core \Q$core\E --noinform --non-interactive"
                 : "sbcl --noinform --non-interactive --load \Q$runtime\E --load \Q$testlib\E";

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
printf $JOURNAL "# run-perl-suite %s: %d files, jobs=%d timeout=%d\n",
  scalar(localtime), scalar(@files), $jobs, $timeout;
print $JOURNAL "# file  P_ok  P_notok  C_ok  C_notok  status  sig   [P=PERL, C=PCL]\n";
# Timeout allowances in effect for files in THIS run — printed to both the
# journal and the terminal, so a long-running file's allowance is never a
# silent property of a registry nobody reads.
for my $rel (sort grep { $file_timeout{$_} } @files) {
  my $e = $file_timeout{$rel};
  printf $JOURNAL "# timeout-allowance\t%s\t%d\t%s\n", $rel, timeout_for($rel), $e->{cause};
  printf STDERR "timeout allowance: %-24s %4ds  (%s)\n", $rel, timeout_for($rel), $e->{cause};
}
print $JOURNAL "# queued\t$_\n" for @files;


# ---------------------------------------------------------------- worker
sub run_one {
  my ($rel, $result_file) = @_;
  my ($p_ok, $p_notok, $c_ok, $c_notok, $status, $sig) = (0, 0, 0, 0, 'OK', '');
  my $f = "$tdir/$rel";

  unless (-f $f) {
    ($status, $sig) = ('MISSING', '');
    goto WRITE;
  }

  # ulimit -v: fork-heavy tests can orphan children past the timeout; the cap
  # stops any single perl from eating the machine (a 6 GB leak OOM-killed the
  # whole desktop session twice before group-reaping was added).
  my $perl = `ulimit -v 4194304 2>/dev/null; cd \Q$tdir\E && timeout 30 perl \Q$f\E 2>/dev/null`;
  $p_ok    = () = $perl =~ /^ok /mg;
  $p_notok = () = $perl =~ /^not ok /mg;

  my $to = timeout_for($rel);   # per-file allowance (docs/perl-suite-timeouts.tsv)
  (my $safe = $rel) =~ s{/}{_}g;
  my $lisp = "$tmpdir/$safe.lisp";
  # Transpile with CWD = shadow so `require './test.pl'` prototype extraction
  # (cwd-first) reads the PCL stub, not perl's real harness.
  my $terr = system("ulimit -v 4194304 2>/dev/null; cd \Q$shadow\E && "
                  . "timeout -k 10 $to perl -I\Q$root\E \Q$pl2cl\E --no-cache --lenient-ppi \Q$f\E "
                  . "> \Q$lisp\E 2>\Q$lisp\E.err");
  my $pcl = "";
  my $sbcl_exit = 0;
  if ($terr == 0) {
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
    system("cd \Q$shadow\E && $childenv timeout -k 10 $to $sbcl --load \Q$lisp\E > \Q$out\E 2>&1");
    $sbcl_exit = $? >> 8;
    $pcl = do { local $/; my $fh; open($fh, '<', $out) ? (<$fh> // '') : '' };
    $c_ok    = () = $pcl =~ /^ok /mg;
    $c_notok = () = $pcl =~ /^not ok /mg;
  }

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

  $status = $terr != 0                                   ? 'TRANSPILE'
          : $sbcl_exit == 124                            ? 'TIMEOUT'
          : ($p_ok + $p_notok) == 0                      ? 'NOTAP'
          : ($p_ok == $c_ok && $p_notok == $c_notok && !$sig) ? 'OK'
          :                                                'DIFF';

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
        my $rows = 0;
        for my $pair (@$pairs) {
          my ($p, $c) = @$pair;
          my $cv = $c ? $c->{verb} : '(missing)';
          next if $p->{verb} eq $cv;
          $cv .= " [PCL #$c->{num}]" if $c && $c->{num} != $p->{num};
          print $lf join("\t", $rel, $p->{num}, $p->{verb}, $cv, $p->{desc}), "\n";
          last if ++$rows >= 500;
        }
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
  print $rf join("\t", $rel, $p_ok, $p_notok, $c_ok, $c_notok, $status, $sig), "\n";
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

# QUARANTINE (task #160, ruled s320): files that take the whole MACHINE down,
# not just themselves.  op/list.t alone consumed a 10 GB cgroup in 53 s under
# --jobs 1 on an idle box and got OOM-killed, taking the run with it and
# leaving op/pack.t unmeasured behind it in the queue.  The transpiler is
# innocent (pl2cl: 1.23 s, 64 MB, 564 lines of CL) — the blowup is SBCL-side,
# diagnosis is POST-R1 and needs the user to re-authorize running these.
#
# Quarantine means NOT RUN, never "skipped": each gets a NOT-RUN row carrying
# the reason, which is an UNEXPLAINED status, so the file still fails the run
# and still shows up in the tsv as a hole in the release signal.  Hiding it
# would be the one thing worse than not measuring it.
my %QUARANTINE = (
  'op/list.t' => 'task #160 — 10 GB SBCL blowup OOM-kills the run',
  'op/pack.t' => 'task #160 — never measured; was queued behind op/list.t when it OOMed',
);
my @quarantined = grep { $QUARANTINE{$_} } @files;
for my $rel (@quarantined) {
  record_result([$rel, 0, 0, 0, 0, 'NOT-RUN', "QUARANTINED: $QUARANTINE{$rel}"]);
  printf "%-24s %s\n", $rel, "NOT-RUN (QUARANTINED: $QUARANTINE{$rel})";
}
@files = grep { !$QUARANTINE{$_} } @files;
@heavy = grep { !$QUARANTINE{$_} } @heavy;
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
    my @r = split /\t/, $line, 7;
    @r = ($info->{rel}, 0, 0, 0, 0, 'NO-RESULT', '') if @r < 6;
    # Expected-divergence registry: divergent+expected -> XDIFF (doesn't fail
    # the run); OK+expected -> STALE (fails the run: remove the stale row).
    #
    # XDIFF is granted per ROW, all-or-nothing (task #185), exactly like
    # FIXTURE: the file's diverging rows must MATCH the blessed multiset in
    # docs/perl-suite-expected-rows.tsv.  An unregistered diverging row keeps
    # the file DIFF and is named — so a new bug landing inside a file whose
    # feature gap is blessed can never hide behind the file-level reason.  A
    # registered row that stopped diverging is STALE, same as a whole file
    # that starts passing.
    if (my $reason = $expected{$r[0]}) {
      if ($r[5] =~ /^(?:DIFF|TRANSPILE|TIMEOUT)$/) {
        my @actual = diverging_rowkeys($r[0]);
        my @reg    = @{ $expected_rows{$r[0]} || [] };
        # A file whose row set is MEASURED nondeterministic (see the
        # *rows-unstable* paragraph in the rows baseline) opts out of the row
        # check — the file-level reason covers it wholesale, as before #185.
        # One entry, never mixed with real rows, and it has to be put there by
        # hand: --bless-rows will not invent it.
        my ($new, $gone) = (@reg == 1 && $reg[0] eq '*rows-unstable*')
                           ? ([], []) : multiset_diff(\@actual, \@reg);
        if (@$new) {
          $r[6] = join(' | ', grep { length } $r[6] // '',
                       sprintf("NOT fully registered: %d unregistered diverging row(s): %s",
                               scalar(@$new), row_list_excerpt($new)),
                       $reason);
        } elsif (@$gone) {
          $r[5] = 'STALE';
          $r[6] = sprintf("%d registered row(s) no longer diverge (%s) — re-bless with --bless-rows",
                          scalar(@$gone), row_list_excerpt($gone));
        } else {
          $r[5] = 'XDIFF';
          $r[6] = join(' | ', grep { length } $r[6] // '',
                       (@reg == 1 && $reg[0] eq '*rows-unstable*'
                        ? 'rows NOT checked (*rows-unstable*)' : ()),
                       $reason);
        }
      } elsif ($r[5] eq 'OK') {
        $r[5] = 'STALE';
        $r[6] = "expected-divergence row now PASSES — remove it from docs/perl-suite-expected.tsv";
      }
    }
    # Fixture-artifact registry (task #172): per-ROW, and only ever applied to
    # a file whose ENTIRE divergence is registered rows.  One unregistered row
    # and the file stays DIFF — naming the intruders, so a real bug appearing
    # inside a fixture-affected file can never hide behind the registration.
    elsif (my $fx = $fixture{$r[0]}) {
      if ($r[5] eq 'DIFF') {
        my @diverging = read_diverging_rows($r[0]);
        my @unreg = grep { !$fx->{rows}{$_} } @diverging;
        if (!@diverging) {
          $r[6] = join(' | ', grep { length } $r[6] // '',
                       'fixture rows registered but no per-row log — left DIFF');
        } elsif (@unreg) {
          $r[6] = join(' | ', grep { length } $r[6] // '',
                       "NOT a pure fixture artifact: unregistered failing rows @unreg");
        } else {
          $r[5] = 'FIXTURE';
          $r[6] = join(' | ', grep { length } $r[6] // '', $fx->{cause});
        }
      } elsif ($r[5] eq 'OK') {
        $r[5] = 'STALE';
        $r[6] = "fixture-artifact row now PASSES — remove it from docs/perl-suite-fixture.tsv";
      }
    }
    record_result(\@r);
    printf "%-24s P:%4d/%-3d C:%4d/%-4d %-7s %s\n", @r[0 .. 5], $r[6] // '';
    STDOUT->flush();
  }
  # Hard-kill stragglers the in-child timeout somehow missed.
  for my $pid (keys %children) {
    my $info = $children{$pid};
    next unless time() - $info->{start} > timeout_for($info->{rel}) + 40;
    kill 'KILL', -$pid; waitpid($pid, 0);
    record_result([$info->{rel}, 0, 0, 0, 0, 'TIMEOUT', '(killed)']);
    printf "%-24s %s\n", $info->{rel}, 'TIMEOUT (killed)';
    delete $children{$pid};
  }
  select(undef, undef, undef, 0.1) if @queue || %children;
}
}

bless_expected_rows() if $bless_rows;
exit(emit_report() ? 1 : 0);

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
sub diverging_rowkeys {
  my ($rel) = @_;
  (my $safe = $rel) =~ s{/}{_}g;
  open my $lf, '<', "$faillog/$safe.fails.tsv" or return ('*no-log*');
  my @keys;
  while (<$lf>) {
    next if /^\s*#/;
    chomp;
    my (undef, $n, undef, $cv, $desc) = split /\t/, $_, 5;
    next unless defined $n;
    $desc = '' unless defined $desc;
    $desc =~ s/\s+\z//;
    $desc =~ s{\Q$tdir\E/}{t/}g;
    # test# 0 is not a paired test: either a summary row (no TAP at all / PCL
    # renumbered), whose text carries an unstable crash signature and so
    # normalizes to one sentinel, or a PCL-ONLY row, which is real evidence
    # (do.t's two extras ARE the principle-9 divergence) and keeps its text.
    if ($n !~ /^[1-9][0-9]*$/) {
      push @keys, (defined $cv && $cv =~ /^extra\b/)
                  ? '*extra* ' . (length $desc ? $desc : '(unnamed)')
                  : '*summary*';
      next;
    }
    # An UNNAMED test's description is perl's "[at <file> line N]" marker (the
    # $tdir strip above keeps the stable line number and drops this machine's
    # absolute perl-build path — the task #217 family: a generated artifact
    # must not bake in build paths).
    push @keys, (length $desc ? $desc : "#$n");
  }
  close $lf;
  return @keys ? (sort @keys) : ('*empty-log*');
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

sub row_list_excerpt {
  my ($rows) = @_;
  my @show = @$rows > 3 ? (@$rows[0 .. 2], sprintf("+%d more", @$rows - 3)) : @$rows;
  return join('; ', map { my $s = $_; $s =~ s/\s+/ /g;
                          length($s) > 60 ? substr($s, 0, 57) . '...' : $s } @show);
}

# --bless-rows: rewrite docs/perl-suite-expected-rows.tsv from THIS run.  Only
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
# measured.  Do not hand-edit; edit docs/perl-suite-expected.tsv (the reasons)
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

sub record_result {
  my ($r) = @_;
  $results{$r->[0]} = $r;
  print $JOURNAL join("\t", @$r), "\n" if $JOURNAL;
  return;
}

sub emit_report {
  return 0 if $reported++;
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
    record_result([$rel, 0, 0, 0, 0, $st, $why]);
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
  printf "%d files: %d OK, %d NOTAP, %d XDIFF (expected, see docs/perl-suite-expected.tsv), %d FIXTURE (harness artifact, see docs/perl-suite-fixture.tsv), %d UNEXPLAINED\n",
    scalar(keys %results), scalar(@{ $by_status{OK} // [] }),
    scalar(@{ $by_status{NOTAP} // [] }), scalar(@{ $by_status{XDIFF} // [] }),
    scalar(@{ $by_status{FIXTURE} // [] }), $n_bad;
  # Quarantined files are NOT-RUN by construction, so they are already counted
  # as UNEXPLAINED above — say so, so nobody reads the number as new breakage.
  printf "%d of those UNEXPLAINED are QUARANTINED (never run this session): %s\n",
    scalar(@quarantined), join(', ', map { "$_ ($QUARANTINE{$_})" } @quarantined)
    if @quarantined;
  print "failure log: $faillog/*.fails.tsv\n" if grep { -f $_ } glob "$faillog/*.fails.tsv";

  if ($JOURNAL) {
    printf $JOURNAL "# %s\n", @lost ? "INCOMPLETE: @{[scalar @lost]} files unmeasured"
                                    : "complete";
    close $JOURNAL;
    print "journal: $journal_file\n" if @lost;
  }
  if ($tsv_file) {
    open my $tf, '>', $tsv_file or die "write $tsv_file: $!\n";
    # Legend at the point of use — this has been misread twice (s316v).
    print $tf "# file  P_ok  P_notok  C_ok  C_notok  status  sig   [P=PERL, C=PCL]\n";
    print $tf "# NOTAP = PERL produced no TAP (row not comparable; says nothing bad about PCL)\n";
    print $tf "# XDIFF = expected divergence, docs/perl-suite-expected.tsv (a blessed not-supported.md gap)\n";
    print $tf "# FIXTURE = harness artifact, docs/perl-suite-fixture.tsv (the MEASUREMENT differs, not PCL)\n";
    print $tf "# NOT-RUN with QUARANTINED = deliberately not run this session; UNMEASURED, never passing\n";
    print $tf "# INCOMPLETE RUN — KILLED/NOT-RUN rows are unmeasured, not passing\n" if @lost;
    print $tf join("\t", @{ $results{$_} }), "\n" for sort keys %results;
    close $tf;
    print "wrote $tsv_file\n";
  }
  return $n_bad;
}
