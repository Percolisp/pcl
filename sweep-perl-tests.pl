#!/usr/bin/env perl
#
# Parallel sweep of all Perl test files.
# Usage: ./sweep-perl-tests.pl [--jobs N] [--timeout N] [file.t ...]
#
# Each child writes results to a temp file; parent reads after wait.
# Skips bop.t, heredoc.t (known to hang), local.t (tie hang), reverse.t (Tie::Array infinite loop).

use strict;
use warnings;
use File::Basename;
use File::Temp qw(tempfile tempdir);
use Cwd qw(abs_path getcwd);
use POSIX qw(:sys_wait_h _exit);

my $JOBS    = 8;
my $TIMEOUT = 60;  # seconds per test
my @SKIP    = qw(bop.t heredoc.t local.t reverse.t);

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

print "Running $total tests with $JOBS parallel jobs (timeout=${TIMEOUT}s)\n";
print "Skipping: ", join(', ', @SKIP), "\n\n";

# Worker function — runs in child process, writes results to a file
sub run_one_test {
    my ($file, $result_file) = @_;
    my $name = basename($file);

    my $pass = 0; my $fail = 0; my $status = 'OK'; my $snippet = '';

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
        my $out = `timeout $TIMEOUT sbcl --noinform --non-interactive --load $runtime --eval "(setf pcl::*pcl-skip-cache* t)" --load $testlib --load $cl_file 2>&1`;
        if (($? >> 8) == 124) { die "TIMEOUT\n" }
        alarm(0);

        chdir $orig;

        $out =~ s/^;.*\n//gm;
        $out =~ s/^\s*\n//gm;
        $out =~ s/PCL Runtime loaded\n?//g;
        $out =~ s/STYLE-WARNING[^\n]*\n//g;

        while ($out =~ /^(not )?ok \d+/gm) { $1 ? $fail++ : $pass++ }

        if ($fail > 0 || $pass == 0) {
            ($snippet) = ($out =~ /^(.*?(?:error|UNBOUND|unbound|undefined|Backtrace)[^\n]*)/im);
            $snippet //= (split /\n/, $out)[0] // '';
            $snippet =~ s/^\s+//;
            $snippet = substr($snippet, 0, 100);
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
        ($pass, $fail) = (0, 0);
    }

    # Write tab-separated result
    open(my $rf, '>', $result_file) or die;
    print $rf join("\t", $name, $pass, $fail, $status, $snippet) . "\n";
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
        my $r = { pass => 0, fail => 0, status => 'NO_RESULT', snippet => '' };
        if (open my $rf, '<', $info->{result_file}) {
            chomp(my $line = <$rf>);
            close $rf;
            my ($n, $p, $f, $s, $snip) = split /\t/, $line, 5;
            $r = { pass => $p // 0, fail => $f // 0, status => $s // 'OK', snippet => $snip // '' };
        }
        $results{$info->{name}} = $r;

        my $elapsed = time() - $info->{start};
        my $line_result =
            $r->{status} eq 'TIMEOUT'        ? "TIMEOUT"                                     :
            $r->{status} eq 'TRANSPILE_FAIL' ? "TRANSPILE FAIL  $r->{snippet}"               :
            $r->{status} eq 'ERROR'          ? "ERROR  $r->{snippet}"                        :
            $r->{fail} == 0 && $r->{pass} == 0 ? "NO OUTPUT  $r->{snippet}"                 :
            $r->{fail} == 0                  ? "PASS  ($r->{pass})"                          :
                                               "pass=$r->{pass} fail=$r->{fail}";

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

printf "%-26s %5s %5s  %s\n", "Test", "Pass", "Fail", "Notes";
printf "%s\n", "-" x 72;

my ($total_pass, $total_fail) = (0, 0);
my (@fully_passing, @zero_pass, @timeouts);

for my $name (sort { ($results{$b}{pass} <=> $results{$a}{pass}) || ($a cmp $b) } keys %results) {
    my $r = $results{$name};
    $total_pass += $r->{pass};
    $total_fail += $r->{fail};

    my $note = ($r->{status} ne 'OK') ? $r->{status} : $r->{snippet};
    $note = substr($note // '', 0, 40);

    printf "%-26s %5d %5d  %s\n", $name, $r->{pass}, $r->{fail}, $note;

    push @fully_passing, $name if $r->{fail} == 0 && $r->{pass} > 0;
    push @zero_pass,     $name if $r->{pass} == 0;
    push @timeouts,      $name if $r->{status} eq 'TIMEOUT';
}

my $file_count = scalar(keys %results);
print "\n" . "=" x 72 . "\n";
printf "TOTAL: %d passing, %d failing across %d files (+ %d skipped)\n",
    $total_pass, $total_fail, $file_count, scalar(@SKIP);
print "\nFully passing (" . scalar(@fully_passing) . "): " . join(', ', sort @fully_passing) . "\n";
print "\nZero passing  (" . scalar(@zero_pass)     . "): " . join(', ', sort @zero_pass)     . "\n";
if (@timeouts) {
    print "\nTimeouts      (" . scalar(@timeouts)   . "): " . join(', ', sort @timeouts)      . "\n";
}
print "\nSkipped (known hang): " . join(', ', @SKIP) . "\n";
