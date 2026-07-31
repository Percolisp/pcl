#!/usr/bin/env perl

# run-perl-suite.pl — run Perl's own core test files (t/base, t/cmd, t/comp,
# t/mro, t/class, …) through PCL and compare TAP results to real perl, with a
# crash signature.  These are the distribution tests that were NOT copied into
# PCL's perl-tests/ sweep corpus (which is almost entirely t/op/), so this is
# the companion sweep for everything the sweep doesn't cover (task #25 / R1
# gate; results catalogue: docs/perl-test-suite-survey.md — UPDATE that doc
# when a row changes so we don't re-investigate the same files).
#
# Usage:
#   tools/run-perl-suite.pl base/rs.t comp/our.t   # specific files (rel to t/)
#   tools/run-perl-suite.pl --dir comp             # all runnable files in t/<dir>
#   tools/run-perl-suite.pl --all                  # every default dir, NOT-copied files only
#   tools/run-perl-suite.pl                        # == --all
#
# Options:
#   --tdir PATH        perl build t/ tree (default: the 5.40.3 build below)
#   --dir D            add one subdir (repeatable); implies the copied-file filter
#   --all              scan the default dir set (see @DEFAULT_DIRS)
#   --include-copied   with --all/--dir: also run files whose basename exists
#                      in perl-tests/ (default: skip them — the sweep owns those)
#   --jobs N           parallel workers (default 8)
#   --timeout N        per-file SBCL timeout seconds (default 90)
#   --no-core          skip the saved-core fast path (source-load the runtime)
#   --tsv FILE         also write one TSV row per file (rel, P ok/notok,
#                      C ok/notok, status, signature) for diffing runs
#   --faillog DIR      per-test failure log dir (default .suitelog, cleared
#                      each run): for every DIFF/TIMEOUT file, one TSV row per
#                      diverging TAP test — num, perl-verb, pcl-verb, desc —
#                      by joining the two TAP streams on test number.  A PCL
#                      run that produced NO TAP writes one summary row.
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
# build-tree modules from ../lib that PCL cannot load.  Dir scans report how
# many files each filter dropped, so coverage is visible.
#
# Output columns: P:perl_ok/notok  C:pcl_ok/notok  STATUS  [crash-signature]
# STATUS: OK (counts match) | DIFF | TRANSPILE | TIMEOUT | NOTAP (perl itself
# produced no TAP — not comparable, doesn't fail the run; PCL result shown).
# Exit: nonzero iff any DIFF/TRANSPILE/TIMEOUT/MISSING/NO-RESULT.

use strict;
use warnings;
use File::Basename qw(basename dirname);
use File::Temp qw(tempfile tempdir);
use Cwd qw(abs_path);
use POSIX qw(:sys_wait_h _exit);

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
my ($all, $include_copied, $no_core, $tsv_file);
my $jobs = 8;
my $timeout = 90;
my $faillog = "$root/.suitelog";
my $expected_tsv = "$root/docs/perl-suite-expected.tsv";
my (@dirs, @files);
while (@ARGV) {
  my $a = shift @ARGV;
  if    ($a eq '--tdir')           { $tdir = shift @ARGV }
  elsif ($a eq '--dir')            { push @dirs, shift @ARGV }
  elsif ($a eq '--all')            { $all = 1 }
  elsif ($a eq '--include-copied') { $include_copied = 1 }
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
-d $tdir or die "perl t/ tree not found: $tdir (pass --tdir)\n";
$all = 1 if !@files && !@dirs;
push @dirs, @DEFAULT_DIRS if $all;

# Files already in the sweep corpus — dir scans skip them by default.  Keyed
# by basename PLUS matching head content (first 300 bytes): several t/ files
# share a basename with a corpus file copied from a DIFFERENT dir (cmd/for.t
# vs op/for.t, class/method.t vs op/method.t, most of uni/) and must still run
# here.  A true copy whose head was locally edited would merely run in both
# sweeps — harmless.
my %corpus_head;
for my $f (glob "$root/perl-tests/*.t") {
  open my $fh, '<', $f or next;
  read $fh, my $head, 300;
  $corpus_head{ basename($f) } = $head // '';
  close $fh;
}
sub in_corpus {
  my ($f) = @_;
  my $head = $corpus_head{ basename($f) };
  return 0 unless defined $head;
  open my $fh, '<', $f or return 0;
  read $fh, my $h, 300;
  close $fh;
  return ($h // '') eq $head;
}

# Enumerate self-contained files in each requested dir.
for my $d (@dirs) {
  my ($n_all, $n_harness, $n_copied) = (0, 0, 0);
  for my $f (sort glob "$tdir/$d/*.t") {
    $n_all++;
    my $base = basename($f);
    if (!$include_copied && in_corpus($f)) { $n_copied++; next }
    open my $fh, '<', $f or next;
    local $/; my $src = <$fh>; close $fh;
    # Skip files pulling build-tree modules via @INC fiddling in BEGIN.
    # (`require './test.pl'` and `chdir 't'` files run via the shadow t/.)
    if ($src =~ m{BEGIN[^\n]*\@INC}) { $n_harness++; next }
    push @files, "$d/$base";
  }
  printf STDERR "scan t/%-8s %3d files: %3d runnable, %3d need-harness%s\n",
    $d, $n_all, $n_all - $n_harness - $n_copied, $n_harness,
    $n_copied ? ", $n_copied in sweep corpus" : "";
}
@files or die "no files (give t-relative paths, --dir <subdir>, or --all)\n";

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
END { unlink $core if $core }
# --core must precede all other toplevel sbcl options.
my $sbcl = $core ? "sbcl --core \Q$core\E --noinform --non-interactive"
                 : "sbcl --noinform --non-interactive --load \Q$runtime\E --load \Q$testlib\E";

my $tmpdir = tempdir(CLEANUP => 0);
END { system("rm -rf \Q$tmpdir\E") if $tmpdir && -d $tmpdir }

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

# TAP stream -> { test-number => [verb, description] }.
# Horizontal whitespace ONLY after the number: \s there matches the newline of
# a description-less line ("ok 3\n"), swallowing the NEXT TAP line as the
# description and reporting its test number as (missing) — 421 phantom rows
# on op/signatures.t (s316o).
sub tap_map {
  my ($out) = @_;
  my %m;
  while ($out =~ /^(not ok|ok)[ \t]+(\d+)[ \t]*[-#]?[ \t]*([^\n]*)$/mg) {
    $m{$2} = [$1, $3];
  }
  return \%m;
}

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

  (my $safe = $rel) =~ s{/}{_}g;
  my $lisp = "$tmpdir/$safe.lisp";
  # Transpile with CWD = shadow so `require './test.pl'` prototype extraction
  # (cwd-first) reads the PCL stub, not perl's real harness.
  my $terr = system("ulimit -v 4194304 2>/dev/null; cd \Q$shadow\E && "
                  . "timeout -k 10 $timeout perl -I\Q$root\E \Q$pl2cl\E --no-cache --lenient-ppi \Q$f\E "
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
    system("cd \Q$shadow\E && $childenv timeout -k 10 $timeout $sbcl --load \Q$lisp\E > \Q$out\E 2>&1");
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

  # Per-test failure log: join the two TAP streams on test number and record
  # every diverging test — the triage input for marking not-supported rows.
  if ($status eq 'DIFF' || $status eq 'TIMEOUT') {
    my ($pm, $cm) = (tap_map($perl), tap_map($pcl));
    if (open my $lf, '>', "$faillog/$safe.fails.tsv") {
      if (!%$cm) {
        print $lf join("\t", $rel, 0, 'ok*', '(no TAP)',
                       "PCL produced no TAP output" . ($sig ? " ($sig)" : "")), "\n";
      } else {
        my $rows = 0;
        for my $n (sort { $a <=> $b } keys %$pm) {
          my $pv = $pm->{$n}[0];
          my $cv = $cm->{$n} ? $cm->{$n}[0] : '(missing)';
          next if $pv eq $cv;
          print $lf join("\t", $rel, $n, $pv, $cv, $pm->{$n}[1]), "\n";
          last if ++$rows >= 500;
        }
        my $extra = grep { !$pm->{$_} } keys %$cm;
        print $lf join("\t", $rel, 0, '(none)', 'extra',
                       "$extra PCL-only test numbers"), "\n" if $extra;
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
my (%children, %results);
# Workers sit in their own process groups, so terminal SIGINT no longer
# reaches them — forward it (exit() still runs the END tmpdir cleanup).
for my $s ('INT', 'TERM') {
  $SIG{$s} = sub { kill 'KILL', map { -$_ } keys %children; exit 130 };
}
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
    if ($pid == 0) { setpgrp(0, 0); run_one($rel, $result_file); _exit(0) }
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
    if (my $reason = $expected{$r[0]}) {
      if ($r[5] =~ /^(?:DIFF|TRANSPILE|TIMEOUT)$/) {
        $r[5] = 'XDIFF';
        $r[6] = join(' | ', grep { length } $r[6] // '', $reason);
      } elsif ($r[5] eq 'OK') {
        $r[5] = 'STALE';
        $r[6] = "expected-divergence row now PASSES — remove it from docs/perl-suite-expected.tsv";
      }
    }
    $results{$info->{rel}} = \@r;
    printf "%-24s P:%4d/%-3d C:%4d/%-4d %-7s %s\n", @r[0 .. 5], $r[6] // '';
    STDOUT->flush();
  }
  # Hard-kill stragglers the in-child timeout somehow missed.
  for my $pid (keys %children) {
    my $info = $children{$pid};
    next unless time() - $info->{start} > $timeout + 40;
    kill 'KILL', -$pid; waitpid($pid, 0);
    $results{$info->{rel}} = [$info->{rel}, 0, 0, 0, 0, 'TIMEOUT', '(killed)'];
    printf "%-24s %s\n", $info->{rel}, 'TIMEOUT (killed)';
    delete $children{$pid};
  }
  select(undef, undef, undef, 0.1) if @queue || %children;
}
}

# ----------------------------------------------------------- summary
my %by_status;
push @{ $by_status{ $results{$_}[5] } }, $_ for keys %results;
print "----\n";
for my $st (sort keys %by_status) {
  my @f = sort @{ $by_status{$st} };
  printf "%-8s %3d%s\n", $st, scalar @f,
    ($st eq 'OK' ? '' : ':  ' . join(', ', @f));
}
my $n_bad = grep { $results{$_}[5] !~ /^(?:OK|NOTAP|XDIFF)$/ } keys %results;
printf "%d files: %d OK, %d NOTAP, %d XDIFF (expected, see docs/perl-suite-expected.tsv), %d UNEXPLAINED\n",
  scalar(keys %results), scalar(@{ $by_status{OK} // [] }),
  scalar(@{ $by_status{NOTAP} // [] }), scalar(@{ $by_status{XDIFF} // [] }), $n_bad;
print "failure log: $faillog/*.fails.tsv\n" if grep { -f $_ } glob "$faillog/*.fails.tsv";

if ($tsv_file) {
  open my $tf, '>', $tsv_file or die "write $tsv_file: $!\n";
  print $tf join("\t", @{ $results{$_} }), "\n" for sort keys %results;
  close $tf;
  print "wrote $tsv_file\n";
}
exit($n_bad ? 1 : 0);
