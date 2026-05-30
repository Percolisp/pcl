#!/usr/bin/env perl
#
# Parallel sweep of all Perl test files.
# Usage: ./sweep-perl-tests.pl [--jobs N] [--timeout N] [file.t ...]
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
my $TIMEOUT = 90;  # seconds per test
# heredoc.t: 137/138 tests are fresh_perl_is no-ops (no TAP output)
# list.t: builds 100k-nested "(1,(1,...))" string then evals it — PPI parse is O(n²+)
#         on deeply-nested expressions; takes >8 min at 100% CPU (not OOM, just slow)
my @SKIP    = qw(heredoc.t list.t);

my @test_files;
while (@ARGV) {
    my $arg = shift;
    if ($arg eq '--jobs')    { $JOBS    = shift; next; }
    if ($arg eq '--timeout') { $TIMEOUT = shift; next; }
    push @test_files, $arg;
}

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
END { system("rm -rf \Q$tmpdir\E") if $tmpdir && -d $tmpdir }

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

print "Running $total tests with $JOBS parallel jobs (timeout=${TIMEOUT}s)\n";
print "Skipping: ", join(', ', @SKIP), "\n";
print "Failure log: $log_dir/*.fails.tsv\n\n";

# Worker function — runs in child process, writes results to a file
sub run_one_test {
    my ($file, $result_file) = @_;
    my $name = basename($file);

    my $pass = 0; my $fail = 0; my $skip = 0; my $planned = -1; my $status = 'OK'; my $snippet = '';

    eval {
        local $SIG{ALRM} = sub { die "TIMEOUT\n" };
        alarm($TIMEOUT);

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
        system("timeout $TIMEOUT sbcl --control-stack-size 512 --noinform --non-interactive --load $runtime --eval \"(setf pcl::*pcl-skip-cache* t)\" --load $testlib --load $registry --eval \"(setf pcl::*current-test-file* \\\"$name\\\")\" --load $cl_file >$tmp_out 2>&1");
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
        # plain `ok N` = pass.
        while ($out =~ /^(not ok|ok) \d+([^\n]*)$/gm) {
            if    ($1 eq 'not ok')      { $fail++ }
            elsif ($2 =~ /#\s*skip/i)   { $skip++ }
            else                        { $pass++ }
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
            ($status, $snippet) = ('TIMEOUT', "(killed after ${TIMEOUT}s)");
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

# Parallel dispatch
my %children;  # pid => { name, result_file, start }
my @queue = @test_files;
my %results;

my $started = 0;
my $finished = 0;

while (@queue || %children) {
    # Launch up to $JOBS children
    while (@queue && keys(%children) < $JOBS) {
        my $file = shift @queue;
        my $name = basename($file);
        my ($rf, $result_file) = tempfile(DIR => $tmpdir, SUFFIX => '.res', UNLINK => 0);
        close $rf;

        $started++;
        my $pid = fork();
        die "fork: $!" unless defined $pid;

        if ($pid == 0) {
            # Child — use _exit to skip END blocks (avoids tmpdir cleanup)
            run_one_test($file, $result_file);
            _exit(0);
        }

        $children{$pid} = { name => $name, result_file => $result_file, start => time() };
        printf "[%3d/%3d] %-22s started\n", $started, $total, $name;
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
        $finished++;

        # Read result file
        my $r = { pass => 0, fail => 0, skip => 0, planned => -1, status => 'NO_RESULT', snippet => '' };
        if (open my $rf, '<', $info->{result_file}) {
            chomp(my $line = <$rf>);
            close $rf;
            my ($n, $p, $f, $sk, $pl, $s, $snip) = split /\t/, $line, 7;
            $r = { pass => $p // 0, fail => $f // 0, skip => $sk // 0, planned => $pl // -1,
                   status => $s // 'OK', snippet => $snip // '' };
        }
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

    # Kill timed-out children
    my $now = time();
    for my $pid (keys %children) {
        my $info = $children{$pid};
        if ($now - $info->{start} > $TIMEOUT + 5) {
            kill 'KILL', $pid;
            waitpid($pid, 0);
            $results{$info->{name}} = { pass => 0, fail => 0, status => 'TIMEOUT', snippet => "(killed)" };
            printf "  KILLED %-22s TIMEOUT\n", $info->{name};
            STDOUT->flush();
            $finished++;
            delete $children{$pid};
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
print "\nSkipped (known hang): " . join(', ', @SKIP) . "\n";

# Per-file run status, written alongside the failure log so tools/sweep-diff.pl
# can tell a file that CRASHED/PARTIAL/TIMEOUT (and therefore did not run its
# remaining assertions) apart from one that genuinely passed.  Without this, a
# flaky -j8 crash (e.g. pack.t's transient SIMPLE-FILE-ERROR) makes every
# baseline failure in that file look "FIXED".  One line per file:
#   name <TAB> status <TAB> pass <TAB> fail <TAB> planned <TAB> note
# where `note` carries the crash-localization snippet (# ABORTED after test N ...)
# for CRASH/PARTIAL files.
if (open my $sf, '>', "$log_dir/_status.tsv") {
    for my $name (sort keys %results) {
        my $r = $results{$name};
        my $note = ($r->{status} // 'OK') eq 'OK' ? '' : ($r->{snippet} // '');
        $note =~ s/[\t\n]/ /g;
        print $sf join("\t", $name, $r->{status} // 'OK',
                       $r->{pass} // 0, $r->{fail} // 0, $r->{planned} // -1, $note) . "\n";
    }
    close $sf;
}
