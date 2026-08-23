#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

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
use FindBin;
use lib "$FindBin::RealBin/tools/lib";
use PCLSbcl ();   # the ONE builder of an SBCL command line (task #344)
use PCLProc qw(run_isolated reap_orphan_transpilers);   # session isolation + reaping (#367)

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
    # DROPS (task #343, ruled fable-answers-s400.md §6.5): how many statements
    # the compiler could not lower and replaced with nil in THIS file's emitted
    # CL.  A drop is invisible at run time — the statement simply is not there,
    # and perl-tests/bless.t's is a test row that never runs in a file this
    # sweep reports as passing — so the count rides along with every run that
    # already transpiles the file, and tools/sweep-diff.pl compares it against
    # the blessed census.  -1 means NOT MEASURED (no CL was produced), never 0.
    my $drops = -1;
    # CHILD DROPS (task #472): drops in the programs this file's
    # fresh_perl_is/runperl rows spawn.  A child is transpiled from a STRING at
    # run time (tools/pclperl-for-tests), so its emission is never a .lisp file
    # and the `drops` count above -- which reads THIS file's CL -- cannot see
    # it.  Two are known and BOTH were rows that passed for years on nothing
    # (split.t:682, bop.t:701), found only when the s435 flip made the child
    # die instead of silently printing nothing.  The compiler appends one line
    # per drop to PCL_DROP_LOG; we set it around the RUN only, never around
    # this file's own transpile, or the file's drops would be counted twice.
    my $child_drops = 0;
    my $child_log = "$log_dir/$name.childdrops";
    unlink $child_log;

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
        $drops = () = $cl_code =~ /;; PARSE ERROR:/g;

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
        # quote => 0: these paths were never shell-quoted here, and the #344
        # move is byte-identical by construction (see tools/lib/PCLSbcl.pm).
        my $sbcl_cmd = PCLSbcl::sbcl_prefix_str(runtime => $runtime, quote => 0)
            . " --eval \"(setf pcl::*pcl-skip-cache* t)\" --load $testlib --load $registry --eval \"(setf pcl::*current-test-file* \\\"$name\\\")\" --eval \"(pcl::p-load-with-recovery \\\"$cl_file\\\")\"";
        print STDERR "SBCL[sweep]: $sbcl_cmd\n" if $ENV{PCL_SHOW_SBCL};
        # Own SESSION per file (task #367): `timeout` kills the process GROUP,
        # but anything SBCL starts through run-program is put in a NEW group
        # and escapes it — measured, a spinning child outlived the run and
        # burned a core for the rest of the session.  setpgrp does not change
        # the SESSION, so that is the handle that still reaches every
        # descendant.  `timeout` stays inside, so the timing is unchanged.
        # PCL_DROP_LOG reaches the child transpiles by inheritance: the sweep
        # -> /bin/sh -> SBCL -> run-program -> pclperl-for-tests -> pl2cl.  It
        # is NOT set around the transpile above, so only CHILD drops land here.
        # (A test file that clears %ENV for a child hides its own children from
        # this instrument -- an undercount, never a miscount.)
        my ($rc, $reaped) = do {
            local $ENV{PCL_DROP_LOG} = $child_log;
            run_isolated("timeout $timeout $sbcl_cmd >$tmp_out 2>&1");
        };
        warn "sweep: $name left $reaped orphan(s), reaped\n" if $reaped;
        my $sbcl_exit = $rc >> 8;
        my $out = do { local $/; my $f; open($f, '<', $tmp_out) ? do { my $c = <$f>; $c // '' } : '' };
        unlink $tmp_out;
        if ($sbcl_exit == 124) { die "TIMEOUT\n" }
        alarm(0);
        $child_drops = count_child_drops($child_log);

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

    # A file that TIMEOUTs or crashes still ran children up to that point, and
    # what they dropped is a fact about the corpus, not about the verdict.
    $child_drops = count_child_drops($child_log) if !$child_drops;

    # Write tab-separated result (skip + planned added between fail and status;
    # drops after planned, child-drops after drops — the free-form snippet
    # stays last)
    open(my $rf, '>', $result_file) or die;
    print $rf join("\t", $name, $pass, $fail, $skip, $planned, $drops,
                   $child_drops, $status, $snippet) . "\n";
    close $rf;
}

# One line per drop, `FILE<TAB>LINE<TAB>TEXT<TAB>REASON`, appended by
# Pl::Parser::_announce_dropped_statement.  The file is left in place: the
# COUNT is the instrument, but the identities are what a census of this
# population is made from, and re-running one file to get them back costs a
# whole SBCL run.
sub count_child_drops {
    my ($path) = @_;
    open my $lh, '<', $path or return 0;
    my $n = 0;
    $n++ while <$lh>;
    close $lh;
    return $n;
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

# Per-file SESSION isolation (task #367) + orphaned-transpiler reaping live in
# tools/lib/PCLProc.pm — ONE copy shared with tools/run-perl-suite.pl (s413,
# #387 family 6); the long notes on WHY the session is the handle are there.

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
        my $r = { pass => 0, fail => 0, skip => 0, planned => -1, drops => -1, child_drops => 0, status => 'NO_RESULT', snippet => '' };
        if (open my $rf, '<', $info->{result_file}) {
            chomp(my $line = <$rf>);
            close $rf;
            my ($n, $p, $f, $sk, $pl, $dr, $cdr, $s, $snip) = split /\t/, $line, 9;
            $r = { pass => $p // 0, fail => $f // 0, skip => $sk // 0, planned => $pl // -1,
                   drops => $dr // -1, child_drops => $cdr // 0,
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
        reap_orphan_transpilers();

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
                $results{$info->{name}} = { pass => 0, fail => 0, skip => 0, planned => -1, drops => -1, child_drops => 0,
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

# DROPS headline (task #343): the count is worthless if nobody reads it, and
# the per-file comparison against the blessed census happens in sweep-diff.pl.
# Here we only say how many statements this corpus lost at transpile time, and
# in how many files, so a jump is visible even on a run with --no-gate.
{
    my @with = grep { ($results{$_}{drops} // -1) > 0 } keys %results;
    my $sum = 0; $sum += $results{$_}{drops} for @with;
    my @unmeasured = grep { ($results{$_}{drops} // -1) < 0 } keys %results;
    printf "\nDROPS (statements the compiler could not lower): %d in %d file(s)%s\n",
        $sum, scalar(@with),
        (@unmeasured ? sprintf("; %d file(s) NOT MEASURED (no CL produced)", scalar(@unmeasured)) : '');
    printf "  %-22s %d\n", $_, $results{$_}{drops} for sort { $results{$b}{drops} <=> $results{$a}{drops} || $a cmp $b } @with;
}

# CHILD DROPS headline (task #472).  THE SIXTH POPULATION: everything the
# compiler transpiles DURING the run — the programs fresh_perl_is/runperl
# spawn (from a STRING, so their emission is never a .lisp file) and the
# modules the running program loads (perl-tests/t/test.pl itself, re-transpiled
# per file because the sweep runs with *pcl-skip-cache*).  Neither is in any
# population tools/drop-census.pl reads, so until this instrument nothing could
# count them: the two known cases were rows that had passed for years on
# nothing.  Reported, NOT gated — the ruling (fable-answers-s437 §2 ask 5) is
# measure first, bless the rows by hand, gate after one blessed run.
#
# TWO NUMBERS, because one alone lies in a different direction each way.  The
# per-file COUNT (also the _status.tsv column) says what that file's run lost;
# it double-counts a drop in the harness, which every file re-transpiles.  The
# SITE list says what is distinct across the corpus — that is the census — with
# the number of test files that reached it, so a harness drop reads as one
# site in many files instead of as many drops.
{
    my @with = grep { ($results{$_}{child_drops} // 0) > 0 } keys %results;
    my $sum = 0; $sum += $results{$_}{child_drops} for @with;
    printf "\nCHILD DROPS (statements lost in transpiles during the run): %d in %d file(s)\n",
        $sum, scalar(@with);
    printf "  %-22s %d\n", $_, $results{$_}{child_drops}
        for sort { $results{$b}{child_drops} <=> $results{$a}{child_drops} || $a cmp $b } @with;

    # The distinct sites.  A fresh_perl child's source is a temp file whose
    # NAME changes every run, so the site key drops the directory for those —
    # the identity of that drop is its text, not the path it was written to.
    my (%site, %site_files);
    for my $name (sort keys %results) {
        open my $lh, '<', "$log_dir/$name.childdrops" or next;
        my %here;
        while (my $line = <$lh>) {
            chomp $line;
            my ($file, $lno, $text, $reason) = split /\t/, $line, 4;
            next unless defined $reason;
            # A fresh_perl child is a temp file whose name changes every run,
            # so the path is not part of that drop's identity — its TEXT is.
            $file = '(child program)' if $file =~ m{/pcl_fp_\d+\.pl$}
                                      || $file =~ m{/pcl_rc_};
            $file =~ s{^\Q$project_root\E/}{};
            $file =~ s{/\./}{/}g;
            my $key = "$file:$lno\t$text\t$reason";
            $site{$key}++;
            $here{$key} = 1;
        }
        close $lh;
        $site_files{$_}{$name} = 1 for keys %here;
    }
    printf "  -- %d distinct site(s):\n", scalar(keys %site) if %site;
    for my $key (sort { keys(%{$site_files{$b}}) <=> keys(%{$site_files{$a}}) || $a cmp $b }
                 keys %site) {
        my ($where, $text, $reason) = split /\t/, $key, 3;
        printf "     %-46s in %2d file(s)  %s -- %s\n",
            $where, scalar(keys %{$site_files{$key}}),
            (length($text) > 60 ? substr($text, 0, 57) . '...' : $text), $reason;
    }
}

# Per-file run status, written alongside the failure log so tools/sweep-diff.pl
# can tell a file that CRASHED/PARTIAL/TIMEOUT (and therefore did not run its
# remaining assertions) apart from one that genuinely passed.  Without this, a
# flaky -j8 crash (e.g. pack.t's transient SIMPLE-FILE-ERROR) makes every
# baseline failure in that file look "FIXED".  One line per file:
#   name <TAB> status <TAB> pass <TAB> fail <TAB> planned <TAB> drops
#        <TAB> child-drops <TAB> note
# where `note` carries the crash-localization snippet (# ABORTED after test N ...)
# for CRASH/PARTIAL files, and `drops` is the #138-family count (task #343):
# how many statements the compiler replaced with nil in this file's CL, or -1
# when no CL was produced.  The pass baseline blessed by `sweep-diff.pl
# save-status` has only the first five columns — a reader that finds no drops
# column must treat it as UNKNOWN, never as zero.  `child-drops` (task #472)
# is the same count for the programs this file's fresh_perl/runperl rows
# spawn; 0 there means "no child dropped anything", including "this file
# spawns no child", and it is REPORTED, not gated (sweep-diff.pl reads the
# first six columns and ignores it).
sub write_status_file {
    open my $sf, '>', "$log_dir/_status.tsv" or return;
    for my $name (sort keys %results) {
        my $r = $results{$name};
        my $note = ($r->{status} // 'OK') eq 'OK' ? '' : ($r->{snippet} // '');
        $note =~ s/[\t\n]/ /g;
        print $sf join("\t", $name, $r->{status} // 'OK',
                       $r->{pass} // 0, $r->{fail} // 0, $r->{planned} // -1,
                       $r->{drops} // -1, $r->{child_drops} // 0, $note) . "\n";
    }
    close $sf;
}
write_status_file();

# Also at the END: the per-file reap cannot catch a server orphaned by the LAST
# file's kill (measured s396 — a 5.6 GB one survived a finished run).
reap_orphan_transpilers();

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
    # FORMAT COUPLING: this regex reads sweep-diff.pl's LOST report lines
    # (printf "  ! %-14s -%d  (%s)").  If that printf is reformatted, this
    # match comes up empty and the serial re-run silently stops firing — the
    # printf site carries the mirror comment naming this consumer.
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
        my $r = { pass => 0, fail => 0, skip => 0, planned => -1, drops => -1, child_drops => 0, status => 'NO_RESULT', snippet => '' };
        if (open my $in, '<', $result_file) {
            chomp(my $line = <$in>);
            close $in;
            my ($n, $p, $f, $sk, $pl, $dr, $cdr, $s, $snip) = split /\t/, $line, 9;
            $r = { pass => $p // 0, fail => $f // 0, skip => $sk // 0, planned => $pl // -1,
                   drops => $dr // -1, child_drops => $cdr // 0,
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
