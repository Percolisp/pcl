#!/usr/bin/env perl
#
# Parallel sweep of all Perl test files.
# Usage: ./sweep-perl-tests.pl [--jobs N] [--timeout N] [--no-retry] [file.t ...]
#
# TIMEOUT RETRY (task #176).  A file that TIMEOUTs contributes NOTHING: no
# pass rows, no fail rows, no baseline rows — so it is invisible to
# tools/sweep-diff.pl and a regression inside it cannot be seen.  pack.t sat
# in exactly that hole for the whole project's life (~89 failing rows that
# were neither blessed nor "new"), because it is simply SLOWER than the
# default timeout, not hung.  So a TIMEOUT is now retried ONCE at 3x the
# timeout, alone at the end of the queue (by then the machine is quiet, which
# is the other half of why a slow file times out: see task #180).  A truly
# hung file costs 4x its timeout once and is still reported TIMEOUT.
#
# Each child writes results to a temp file; parent reads after wait.
# Skips heredoc.t (137/138 tests are fresh_perl_is no-ops, produces no TAP output).
# local.t and reverse.t were previously skipped (Tie::Array hang) but now run.
# bop.t was previously skipped (hung), now runs fine.

use strict;
use warnings;
use File::Basename;
use File::Temp qw(tempfile tempdir);
use Cwd qw(abs_path getcwd);
use POSIX qw(:sys_wait_h _exit);

my $JOBS    = 8;
my $TIMEOUT = 90;  # seconds per test (first attempt)
my $RETRY   = 3;   # TIMEOUT is retried once at $RETRY x $TIMEOUT; 0 = no retry
# heredoc.t: 137/138 tests are fresh_perl_is no-ops (no TAP output)
# list.t: builds 100k-nested "(1,(1,...))" string then evals it — PPI parse is O(n²+)
#         on deeply-nested expressions; takes >8 min at 100% CPU (not OOM, just slow)
# lfs.t: large-file support — needs 64-bit lseek offsets + sparse files (XS/platform);
#        self-skips (1..0) anyway. Permanently skipped (not-supported, no recoverable tests).
my @SKIP    = qw(heredoc.t list.t lfs.t);

my $GATE    = 1;   # a FULL sweep ends by running tools/sweep-diff.pl (--no-gate off)
my @test_files;
while (@ARGV) {
    my $arg = shift;
    if ($arg eq '--jobs')    { $JOBS    = shift; next; }
    if ($arg eq '--timeout') { $TIMEOUT = shift; next; }
    if ($arg eq '--no-retry'){ $RETRY   = 0;     next; }
    if ($arg eq '--no-gate') { $GATE    = 0;     next; }
    push @test_files, $arg;
}
my $full_sweep = @test_files ? 0 : 1;   # no file arguments = the whole corpus

my $project_root = abs_path(dirname($0));
my $pl2cl      = "$project_root/pl2cl";
my $runtime    = "$project_root/cl/pcl-runtime.lisp";
my $testlib    = "$project_root/cl/pcl-test.lisp";
my $registry   = "$project_root/cl/skip-registry.lisp";
my $perl_tests = "$project_root/perl-tests";

unless (@test_files) {
    @test_files = glob("$perl_tests/*.t");
}

my %skip = map { $_ => 1 } @SKIP;
@test_files = grep { !$skip{basename($_)} } @test_files;
@test_files = sort { basename($a) cmp basename($b) } @test_files;

my $total = scalar @test_files;
# CLEANUP => 0 because child processes inherit the cleanup handler
# and would delete the dir when they exit. We clean up manually at end.
my $tmpdir = tempdir(CLEANUP => 0);
# $MAIN_PID: workers _exit(0) to skip END blocks, but an UNCAUGHT die in a
# child would still run this one and `rm -rf` the tmpdir out from under every
# sibling.  That is exactly how run-perl-suite lost whole runs under memory
# pressure (task #157) — same guard here so it cannot happen at all.
# `local $?`: system() overwrites $?, and $? after the last END block is the
# process exit status, so an unguarded cleanup silently zeroes any exit code.
my $MAIN_PID = $$;
END { local $?; system("rm -rf \Q$tmpdir\E") if $tmpdir && -d $tmpdir && $$ == $MAIN_PID }

# Structured failure log (consumed by tools/sweep-diff.pl and tools/triage.pl).
# Set PCL_TEST_LOG_DIR so child SBCL processes append one TSV line per FAILING
# assertion to <dir>/<file>.fails.tsv. Cleared each run for a fresh DB.
my $log_dir = $ENV{PCL_TEST_LOG_DIR} || "$project_root/.faillog";
# MUST be absolute: SBCL is run from perl-tests/ and many tests `chdir 't'`, so a
# relative log dir (e.g. PCL_TEST_LOG_DIR=.faillog) would resolve against the test's
# cwd and the open would die SB-INT:SIMPLE-FILE-ERROR, killing the whole file.
$log_dir = "$project_root/$log_dir" unless $log_dir =~ m{^/};
system("rm -rf \Q$log_dir\E"); mkdir $log_dir;
$ENV{PCL_TEST_LOG_DIR} = $log_dir;   # inherited by all forked children / system() calls

# Fresh saved core for the fresh_perl_*/runperl CHILDREN (see PCLPERL below):
# without it every child source-loads the runtime (~1.2s+), and files with
# many fresh_perl calls would blow their timeout.  Same fresh-every-run
# policy as tools/prove-core — a stale core would test old runtime code.
my $child_core = "$tmpdir/pclperl.core";
print "Building child core for fresh_perl/runperl (pclperl-for-tests)...\n";
if (system("sbcl --noinform --non-interactive --load \Q$runtime\E --load \Q$testlib\E "
         . "--eval '(sb-ext:save-lisp-and-die \"$child_core\" :executable nil)' "
         . ">/dev/null 2>&1") != 0) {
    print "  core build FAILED - children will source-load (slow)\n";
    $child_core = "";
}
$ENV{PCLPERL} = "$project_root/tools/pclperl-for-tests"
    unless ($ENV{PCL_FRESH_PERL} // '') eq 'real';
$ENV{PCL_TEST_CORE} = $child_core if $child_core;

print "Running $total tests with $JOBS parallel jobs (timeout=${TIMEOUT}s"
    . ($RETRY > 1 ? ", TIMEOUT retried once at ${\($TIMEOUT * $RETRY)}s" : ", no retry") . ")\n";
print "Skipping: ", join(', ', @SKIP), "\n";
print "Failure log: $log_dir/*.fails.tsv\n\n";

# Worker function — runs in child process, writes results to a file
sub run_one_test {
    my ($file, $result_file, $timeout) = @_;
    my $name = basename($file);
    $timeout ||= $TIMEOUT;

    my $pass = 0; my $fail = 0; my $skip = 0; my $planned = -1; my $status = 'OK'; my $snippet = '';

    eval {
        local $SIG{ALRM} = sub { die "TIMEOUT\n" };
        alarm($timeout);

        my $orig = getcwd();
        chdir $perl_tests or die "chdir: $!\n";

        my ($err_fh, $err_file) = tempfile(SUFFIX => '.err', DIR => $tmpdir, UNLINK => 0);
        close $err_fh;

        my $cl_code = `perl -I$project_root $pl2cl --no-cache --lenient-ppi $name 2>$err_file`;
        if ($? != 0) {
            my $err = do { local $/; open(my $f, '<', $err_file) or die; <$f> };
            chdir $orig; alarm(0);
            die "TRANSPILE_FAIL\n" . (split /\n/, $err)[0] . "\n";
        }

        my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', DIR => $tmpdir, UNLINK => 0);
        print $cl_fh $cl_code;
        close $cl_fh;

        # Run SBCL from $perl_tests (NOT from $orig) — tests do `chdir 't'`
        # which requires CWD = perl-tests/ so that perl-tests/t/ is found.
        # Disable module cache to avoid FASL corruption from parallel processes.
        # Use 'timeout' so SBCL is actually killed if it hangs (alarm() only
        # interrupts the parent Perl process, leaving SBCL running as orphan).
        # Write to a temp file (not backtick pipe) so SBCL's block-buffered
        # output is not lost when timeout sends SIGKILL.
        my $tmp_out = "/tmp/pcl-sweep-$$.out";
        # Load the generated test via p-load-with-recovery (NOT plain --load): it
        # evaluates the file one top-level form at a time and recovers from an
        # uncaught die in any single form, so one not-supported statement (e.g.
        # `pack "P"`, or `die if $@` after an unsatisfiable string eval) no longer
        # aborts the whole file and silently swallows every test after it.
        system("timeout $timeout sbcl --control-stack-size 512 --noinform --non-interactive --load $runtime --eval \"(setf pcl::*pcl-skip-cache* t)\" --load $testlib --load $registry --eval \"(setf pcl::*current-test-file* \\\"$name\\\")\" --eval \"(pcl::p-load-with-recovery \\\"$cl_file\\\")\" >$tmp_out 2>&1");
        my $sbcl_exit = $? >> 8;
        my $out = do { local $/; my $f; open($f, '<', $tmp_out) ? do { my $c = <$f>; $c // '' } : '' };
        unlink $tmp_out;
        if ($sbcl_exit == 124) { die "TIMEOUT\n" }
        alarm(0);

        chdir $orig;

        $out =~ s/^;.*\n//gm;
        $out =~ s/^\s*\n//gm;
        $out =~ s/PCL Runtime loaded\n?//g;
        $out =~ s/STYLE-WARNING[^\n]*\n//g;

        # Parse TAP plan line (1..N) to detect if SBCL crashed mid-file
        ($planned) = ($out =~ /^1\.\.(\d+)/m);
        $planned //= -1;

        # Count: `not ok N` = fail; `ok N # skip ...` = skip (test-own or registry);
        # `# TODO` = expected outcome, never a failure (a failing TODO is a known-
        # broken-in-Perl test, counted as skip like prove does; a passing TODO is
        # an unexpected success, counted as pass); plain `ok N` = pass.
        while ($out =~ /^(not ok|ok) \d+([^\n]*)$/gm) {
            my ($verb, $rest) = ($1, $2);
            if    ($rest =~ /#\s*skip/i) { $skip++ }
            elsif ($rest =~ /#\s*todo/i) { $verb eq 'not ok' ? $skip++ : $pass++ }
            elsif ($verb eq 'not ok')    { $fail++ }
            else                         { $pass++ }
        }

        # Detect abnormal termination
        if ($sbcl_exit != 0) {
            $status = 'CRASH';
        } elsif ($planned >= 0 && ($pass + $fail + $skip) < $planned) {
            # SBCL exited cleanly but fewer tests ran than planned — crashed mid-file
            $status = 'PARTIAL';
        }

        if ($fail > 0 || $pass == 0 || $status ne 'OK') {
            # Crash localization. The harness emits a neutral fact when it ran
            # fewer tests than planned: "# PCL-INCOMPLETE last=N planned=M desc=D".
            # We phrase it by exit code: a nonzero exit (CRASH) is a true mid-file
            # abort whose crash site is the next assertion (~test N+1); a clean
            # exit (PARTIAL) reached EOF but under-counted (tests dropped/skipped).
            my $loc = '';
            if (my ($n, $pl, $d) = ($out =~ /^#\s*PCL-INCOMPLETE last=(\d+) planned=(\S+) desc=(.*)$/m)) {
                $loc = ($status eq 'CRASH')
                    ? "CRASH after test $n ($d) -- crash site ~test @{[$n+1]}"
                    : "INCOMPLETE: ran $n of $pl, last test $n ($d)";
            }
            # Prefer a genuine SBCL crash line over a test description that merely
            # contains the word "error".
            my ($errline) =
                ($out =~ /^[^\n]*(Unhandled [^\n]*|debugger invoked[^\n]*|fatal error[^\n]*|UNBOUND-VARIABLE[^\n]*|is not of type[^\n]*|undefined function[^\n]*)/im);
            ($errline) = ($out =~ /^(.*?(?:error|UNBOUND|unbound|undefined|Backtrace)[^\n]*)/im)
                unless defined $errline;
            $errline //= (split /\n/, $out)[0] // '';
            $errline =~ s/^\s+//;
            $snippet = $loc ? ($status eq 'CRASH' ? "$loc | $errline" : $loc) : $errline;
            $snippet = substr($snippet, 0, 160);
        }
    };
    alarm(0);

    if ($@) {
        chomp(my $err = $@);
        if ($err eq 'TIMEOUT') {
            ($status, $snippet) = ('TIMEOUT', "(killed after ${timeout}s)");
        } elsif ($err =~ /^TRANSPILE_FAIL\n(.*)/s) {
            ($status, $snippet) = ('TRANSPILE_FAIL', (split /\n/, $1)[0] // '');
        } else {
            ($status, $snippet) = ('ERROR', substr($err, 0, 100));
        }
        ($pass, $fail, $skip) = (0, 0, 0);
    }

    # Write tab-separated result (skip + planned added between fail and status)
    open(my $rf, '>', $result_file) or die;
    print $rf join("\t", $name, $pass, $fail, $skip, $planned, $status, $snippet) . "\n";
    close $rf;
}

# Min MemAvailable seen during the run (task #215).  A LOST report has two
# possible causes — a real regression, or the machine going short of memory and
# killing/starving a worker — and the number that tells them apart is free
# memory at the time.  Sampled once a second from /proc/meminfo; undef on a
# platform without it, which is reported as "not measured", never as "fine".
my $min_mem_kb;
my $last_mem_sample = 0;
sub mem_available_kb {
    open my $mfh, '<', '/proc/meminfo' or return undef;
    while (my $l = <$mfh>) { return $1 if $l =~ /^MemAvailable:\s+(\d+)/ }
    return undef;
}
sub sample_mem {
    my $now = time();
    return if $now == $last_mem_sample;
    $last_mem_sample = $now;
    my $kb = mem_available_kb();
    return if !defined $kb;
    $min_mem_kb = $kb if !defined $min_mem_kb || $kb < $min_mem_kb;
}
sub mem_report {
    return "min MemAvailable during the run: not measured (no /proc/meminfo)"
        if !defined $min_mem_kb;
    return sprintf("min MemAvailable during the run: %.1f GB", $min_mem_kb / 1048576);
}

# Parallel dispatch.  A queue entry is [file, timeout]: a file that TIMEOUTs is
# re-queued once at $RETRY x its timeout (task #176).  Because the retry goes on
# the END of the queue it also runs on a quieter machine, which is the other
# half of why a merely-slow file times out.
my %children;  # pid => { name, result_file, start, timeout, retried }
my @queue = map { [$_, $TIMEOUT, 0] } @test_files;
my %results;
my $retries = 0;

my $started = 0;
my $finished = 0;

# WARM-FIRST (task #215, ruled fable-answers-s337.md §5c).  The FIRST file runs
# ALONE; the fan-out starts only once it has finished.  Eight workers starting
# on a cold ~/.pcl-cache all transpile-and-write the same module entries at the
# same moment, and s337c measured the result: do.t loaded a half-written module
# and aborted after 6 of 73 rows, costing 60 passing rows with the failure diff
# still reading "0 new".  One file's wall time buys a populated cache for the
# common modules.  (The write itself is atomic since s339 — see p-load-module-
# cached — so this is belt-and-braces, and it also stops N workers duplicating
# the same transpile work.)
my $warm_first = ($JOBS > 1);
if ($warm_first) {
    print "Warm-first: running " . basename($test_files[0])
        . " alone to populate the module cache, then fanning out to $JOBS jobs\n";
}

while (@queue || %children) {
    sample_mem();
    # Launch up to $JOBS children — but only ONE until the warm-first file is done
    while (@queue && keys(%children) < ($warm_first ? 1 : $JOBS)) {
        my ($file, $timeout, $retried) = @{ shift @queue };
        my $name = basename($file);
        my ($rf, $result_file) = tempfile(DIR => $tmpdir, SUFFIX => '.res', UNLINK => 0);
        close $rf;

        $started++ unless $retried;
        my $pid = fork();
        die "fork: $!" unless defined $pid;

        if ($pid == 0) {
            # Child — use _exit to skip END blocks (avoids tmpdir cleanup)
            run_one_test($file, $result_file, $timeout);
            _exit(0);
        }

        $children{$pid} = { name => $name, file => $file, result_file => $result_file,
                            start => time(), timeout => $timeout, retried => $retried };
        if ($retried) { printf "[retry  ] %-22s started (timeout=${timeout}s)\n", $name }
        else          { printf "[%3d/%3d] %-22s started\n", $started, $total, $name }
        STDOUT->flush();
    }

    # Reap finished children
    my @done_pids;
    for my $pid (keys %children) {
        my $kid = waitpid($pid, WNOHANG);
        push @done_pids, $pid if $kid == $pid;
    }

    for my $pid (@done_pids) {
        my $info = $children{$pid};
        $warm_first = 0;   # the cache is populated (or the first file failed) — fan out

        # Read result file
        my $r = { pass => 0, fail => 0, skip => 0, planned => -1, status => 'NO_RESULT', snippet => '' };
        if (open my $rf, '<', $info->{result_file}) {
            chomp(my $line = <$rf>);
            close $rf;
            my ($n, $p, $f, $sk, $pl, $s, $snip) = split /\t/, $line, 7;
            $r = { pass => $p // 0, fail => $f // 0, skip => $sk // 0, planned => $pl // -1,
                   status => $s // 'OK', snippet => $snip // '' };
        }

        # TIMEOUT, first attempt: re-queue at $RETRY x the timeout instead of
        # recording a result.  The killed run may already have appended rows to
        # its .fails.tsv; drop that partial log so the retry's is the only one.
        if ($r->{status} eq 'TIMEOUT' && $RETRY > 1 && !$info->{retried}) {
            unlink "$log_dir/$info->{name}.fails.tsv";
            push @queue, [$info->{file}, $info->{timeout} * $RETRY, 1];
            $retries++;
            printf "  TIMEOUT %-22s after %ds - re-queued at %ds\n",
                   $info->{name}, $info->{timeout}, $info->{timeout} * $RETRY;
            STDOUT->flush();
            delete $children{$pid};
            next;
        }

        $finished++;
        $results{$info->{name}} = $r;

        my $elapsed = time() - $info->{start};
        my $plan_info = $r->{planned} >= 0 ? "$r->{planned}" : '?';
        my $skip_info = $r->{skip} ? " skip=$r->{skip}" : '';
        my $line_result =
            $r->{status} eq 'TIMEOUT'        ? "TIMEOUT"                                          :
            $r->{status} eq 'TRANSPILE_FAIL' ? "TRANSPILE FAIL  $r->{snippet}"                   :
            $r->{status} eq 'CRASH'          ? "CRASH ($r->{pass}+$r->{fail}/$plan_info ran)  $r->{snippet}" :
            $r->{status} eq 'PARTIAL'        ? "PARTIAL ($r->{pass}+$r->{fail}/$plan_info ran)  $r->{snippet}" :
            $r->{status} eq 'ERROR'          ? "ERROR  $r->{snippet}"                             :
            $r->{fail} == 0 && $r->{pass} == 0 ? "NO OUTPUT  $r->{snippet}"                      :
            $r->{fail} == 0                  ? "PASS  ($r->{pass}/$plan_info$skip_info)"          :
                                               "pass=$r->{pass} fail=$r->{fail}$skip_info planned=$plan_info";

        printf "  done  %-22s %s  [%ds]\n", $info->{name}, $line_result, $elapsed;
        STDOUT->flush();

        delete $children{$pid};
    }

    # Kill children that outlived their OWN timeout (the child's alarm should
    # have fired first; this is the backstop) — and retry them like any TIMEOUT.
    my $now = time();
    for my $pid (keys %children) {
        my $info = $children{$pid};
        if ($now - $info->{start} > $info->{timeout} + 5) {
            kill 'KILL', $pid;
            waitpid($pid, 0);
            delete $children{$pid};
            $warm_first = 0;   # do not stay single-file behind a hung warm-up
            if ($RETRY > 1 && !$info->{retried}) {
                unlink "$log_dir/$info->{name}.fails.tsv";
                push @queue, [$info->{file}, $info->{timeout} * $RETRY, 1];
                $retries++;
                printf "  KILLED %-22s TIMEOUT - re-queued at %ds\n",
                       $info->{name}, $info->{timeout} * $RETRY;
            } else {
                $results{$info->{name}} = { pass => 0, fail => 0, skip => 0, planned => -1,
                                            status => 'TIMEOUT', snippet => "(killed)" };
                printf "  KILLED %-22s TIMEOUT\n", $info->{name};
                $finished++;
            }
            STDOUT->flush();
        }
    }

    select(undef, undef, undef, 0.1) if @queue || %children;
}

# Summary
print "\n" . "=" x 72 . "\n";
print "RESULTS SUMMARY\n";
print "=" x 72 . "\n\n";

printf "%-26s %5s %5s %5s  %s\n", "Test", "Pass", "Fail", "Skip", "Notes";
printf "%s\n", "-" x 72;

my ($total_pass, $total_fail, $total_skip) = (0, 0, 0);
my (@fully_passing, @crashed_files, @partial_files, @zero_pass, @timeouts);

for my $name (sort { ($results{$b}{pass} <=> $results{$a}{pass}) || ($a cmp $b) } keys %results) {
    my $r = $results{$name};
    $total_pass += $r->{pass};
    $total_fail += $r->{fail};
    $total_skip += ($r->{skip} // 0);

    my $note = ($r->{status} ne 'OK') ? "$r->{status} $r->{snippet}" : $r->{snippet};
    $note = substr($note // '', 0, 40);

    my $plan_info = $r->{planned} >= 0 ? "/$r->{planned}" : '';
    printf "%-26s %5d %5d %5d%s  %s\n", $name, $r->{pass}, $r->{fail}, ($r->{skip} // 0), $plan_info, $note;

    # "Fully passing" requires: clean exit, no failures, at least all planned tests ran
    # (pass+fail > planned is OK — subtests or done_testing() can cause minor over-count)
    my $all_ran = ($r->{planned} < 0 || ($r->{pass} + $r->{fail} + ($r->{skip} // 0)) >= $r->{planned});
    push @fully_passing, $name
        if $r->{status} eq 'OK' && $r->{fail} == 0 && $r->{pass} > 0 && $all_ran;

    push @crashed_files, "$name($r->{pass}+$r->{fail}/${\($r->{planned} >= 0 ? $r->{planned} : '?')})"
        if $r->{status} eq 'CRASH';
    push @partial_files, "$name($r->{pass}+$r->{fail}/$r->{planned})"
        if $r->{status} eq 'PARTIAL';
    push @zero_pass, $name if $r->{pass} == 0;
    push @timeouts,  $name if $r->{status} eq 'TIMEOUT';
}

my $file_count = scalar(keys %results);
print "\n" . "=" x 72 . "\n";
printf "TOTAL: %d passing, %d failing, %d skipped across %d files (+ %d files skipped)\n",
    $total_pass, $total_fail, $total_skip, $file_count, scalar(@SKIP);
print "\nFully passing   (" . scalar(@fully_passing) . "): " . join(', ', sort @fully_passing) . "\n";
if (@crashed_files) {
    print "\nCrashed (SBCL)  (" . scalar(@crashed_files) . "): " . join(', ', sort @crashed_files) . "\n";
}
if (@partial_files) {
    print "\nPartial (early stop) (" . scalar(@partial_files) . "): " . join(', ', sort @partial_files) . "\n";
}
print "\nZero passing    (" . scalar(@zero_pass)     . "): " . join(', ', sort @zero_pass)     . "\n";
if (@timeouts) {
    print "\nTimeouts        (" . scalar(@timeouts)   . "): " . join(', ', sort @timeouts)      . "\n";
}
if ($retries) {
    printf "\nRetried after TIMEOUT (%d): re-run at %ds each; see the per-file lines above\n",
        $retries, $TIMEOUT * $RETRY;
}
print "\nSkipped (known hang): " . join(', ', @SKIP) . "\n";

# Per-file run status, written alongside the failure log so tools/sweep-diff.pl
# can tell a file that CRASHED/PARTIAL/TIMEOUT (and therefore did not run its
# remaining assertions) apart from one that genuinely passed.  Without this, a
# flaky -j8 crash (e.g. pack.t's transient SIMPLE-FILE-ERROR) makes every
# baseline failure in that file look "FIXED".  One line per file:
#   name <TAB> status <TAB> pass <TAB> fail <TAB> planned <TAB> note
# where `note` carries the crash-localization snippet (# ABORTED after test N ...)
# for CRASH/PARTIAL files.
sub write_status_file {
    open my $sf, '>', "$log_dir/_status.tsv" or return;
    for my $name (sort keys %results) {
        my $r = $results{$name};
        my $note = ($r->{status} // 'OK') eq 'OK' ? '' : ($r->{snippet} // '');
        $note =~ s/[\t\n]/ /g;
        print $sf join("\t", $name, $r->{status} // 'OK',
                       $r->{pass} // 0, $r->{fail} // 0, $r->{planned} // -1, $note) . "\n";
    }
    close $sf;
}
write_status_file();

print "\n" . mem_report() . "\n";

# ── The gate runs itself (task #204) ────────────────────────────────────────
# A FULL sweep ends by diffing against the blessed baselines and EXITS WITH
# THAT VERDICT.  Leaving it to the operator made two things possible that both
# happened: reading "0 new / 0 fixed" as "clean" while the TOTAL fell (s328's
# 88 evaporated state.t rows), and not running the comparison at all.  A
# partial sweep (explicit files) is not comparable to a whole-corpus baseline,
# so it stays informational.
#
# LOAD-NOISE POLICY (task #215, ruled fable-answers-s334.md §s333-4).  A LOST
# report has two possible causes that look identical on paper: a real
# regression, or the machine going short of memory and starving a worker
# (s333's mechanism).  So a LOST file is RE-RUN SERIALLY — alone, on a quiet
# machine — and the serial verdict REPLACES the parallel one.  The report shows
# BOTH, next to the min MemAvailable seen during the parallel phase, so nothing
# is quietly overwritten.  One round only; a file that is still LOST serially is
# a regression, not noise.
sub run_gate {
    my ($fail_base, $differ, $label) = @_;
    print "\n" . ("=" x 72) . "\nGATE$label: tools/sweep-diff.pl vs docs/fail-baseline.tsv + docs/pass-baseline.tsv\n"
        . ("=" x 72) . "\n";
    my $cmd = join(' ', map { quotemeta } ($^X, $differ, 'diff', $fail_base, $log_dir));
    my $out = `$cmd 2>&1`;
    my $code = $? == -1 ? 127 : ($? >> 8);
    print $out;
    my @lost = ($out =~ /^  ! (\S+)\s+-\d+/mg);
    return ($code, \@lost);
}

sub rerun_serially {
    my (@names) = @_;
    my %by_name = map { basename($_) => $_ } @test_files;
    for my $name (@names) {
        my $file = $by_name{$name};
        if (!defined $file) {
            print "  (cannot re-run $name — not in this sweep's file list)\n";
            next;
        }
        unlink "$log_dir/$name.fails.tsv";
        my ($rf, $result_file) = tempfile(DIR => $tmpdir, SUFFIX => '.res', UNLINK => 0);
        close $rf;
        my $start = time();
        my $pid = fork();
        die "fork: $!" unless defined $pid;
        if ($pid == 0) { run_one_test($file, $result_file, $TIMEOUT * $RETRY); _exit(0) }
        waitpid($pid, 0);
        my $r = { pass => 0, fail => 0, skip => 0, planned => -1, status => 'NO_RESULT', snippet => '' };
        if (open my $in, '<', $result_file) {
            chomp(my $line = <$in>);
            close $in;
            my ($n, $p, $f, $sk, $pl, $s, $snip) = split /\t/, $line, 7;
            $r = { pass => $p // 0, fail => $f // 0, skip => $sk // 0, planned => $pl // -1,
                   status => $s // 'OK', snippet => $snip // '' };
        }
        printf "  serial %-22s pass=%d fail=%d planned=%s status=%s  [%ds]  (parallel run: pass=%d status=%s)\n",
               $name, $r->{pass}, $r->{fail},
               ($r->{planned} >= 0 ? $r->{planned} : '?'), $r->{status}, time() - $start,
               $results{$name}{pass} // 0, $results{$name}{status} // '?';
        $results{$name} = $r;
    }
    write_status_file();
}

if ($full_sweep && $GATE) {
    my $fail_base = "$project_root/docs/fail-baseline.tsv";
    my $differ    = "$project_root/tools/sweep-diff.pl";
    if (-e $fail_base && -x $differ) {
        my ($code, $lost) = run_gate($fail_base, $differ, '');
        if (@$lost) {
            print "\nLOST files re-run SERIALLY (task #215) — " . mem_report() . "\n";
            print "The serial verdict REPLACES the parallel one; both are shown.\n";
            rerun_serially(@$lost);
            ($code, $lost) = run_gate($fail_base, $differ, ' (after serial re-run)');
            print "\nStill LOST after a serial re-run: " . (@$lost ? join(', ', @$lost)
                  . " — NOT load noise\n" : "none — the parallel LOST was load noise\n");
        }
        print $code == 0 ? "GATE: clean\n" : "GATE: NOT CLEAN (sweep-diff exit $code)\n";
        exit $code;
    }
    print "\nGATE: NOT RUN — missing $fail_base or $differ\n";
}
