#!/usr/bin/env perl
# sweep-diff.pl — regression watchdog for the perl-tests sweep.
#
# Consumes the structured failure log written by cl/pcl-test.lisp's test-ok when
# PCL_TEST_LOG_DIR is set (the sweep sets it automatically to $project_root/.faillog).
# Each log line is:  file <TAB> num <TAB> description <TAB> got <TAB> expected
#
# Failures are keyed on (file, description) — NOT the test number — so the diff is
# robust to the TAP-number shifts PCL hits when its test count drifts from Perl's.
#
# Usage:
#   tools/sweep-diff.pl <current>                 # summarize a fail DB (per-file counts)
#   tools/sweep-diff.pl diff <baseline> <current> # NEW (regressions) + FIXED (newly passing)
#   tools/sweep-diff.pl save <current> <dest.tsv>  # write a sorted baseline to commit
#
# <current>/<baseline> may each be a directory (globs *.fails.tsv) or a single .tsv file.
# `diff` exits nonzero if there are NEW failures (regressions) — usable as a CI gate.

use strict;
use warnings;

# Read the per-file run status the sweep writes to <dir>/_status.tsv
# (name <TAB> status <TAB> pass <TAB> fail <TAB> planned).  Returns a hash
# file => status.  Only meaningful for a directory (a live .faillog); a single
# committed baseline .tsv has no status, so we treat every file as having run OK.
sub load_status {
    my ($path) = @_;
    my %st;
    return \%st unless defined $path && -d $path;
    my $sf = "$path/_status.tsv";
    return \%st unless -e $sf;
    open my $fh, '<', $sf or return \%st;
    while (my $line = <$fh>) {
        chomp $line;
        next unless length $line;
        my ($file, $status) = split /\t/, $line;
        $st{$file} = $status // 'OK' if defined $file;
    }
    close $fh;
    return \%st;
}

sub load {
    my ($path) = @_;
    die "no such path: $path\n" unless -e $path;
    my @files = -d $path ? glob("$path/*.fails.tsv") : ($path);
    my %rec;   # "file\tdesc" => { file, desc, got, expected, num }
    for my $f (@files) {
        open my $fh, '<', $f or die "open $f: $!\n";
        while (my $line = <$fh>) {
            chomp $line;
            next unless length $line;
            my ($file, $num, $desc, $got, $exp) = split /\t/, $line, 5;
            next unless defined $desc;
            $rec{"$file\t$desc"} = { file => $file, desc => $desc,
                                     got => $got // '', expected => $exp // '',
                                     num => $num // '' };
        }
        close $fh;
    }
    return \%rec;
}

my $mode = (@ARGV && ($ARGV[0] eq 'diff' || $ARGV[0] eq 'save')) ? shift @ARGV : 'summary';

if ($mode eq 'summary') {
    my $cur = shift @ARGV or die "usage: $0 <current>\n";
    my $rec = load($cur);
    my %by_file;
    $by_file{$_->{file}}++ for values %$rec;
    printf "%-22s %5s\n", 'file', 'fails';
    printf "%-22s %5d\n", $_, $by_file{$_} for sort { $by_file{$b} <=> $by_file{$a} } keys %by_file;
    printf "%-22s %5d\n", 'TOTAL', scalar(keys %$rec);
    exit 0;
}

if ($mode eq 'save') {
    my $cur  = shift @ARGV or die "usage: $0 save <current> <dest.tsv>\n";
    my $dest = shift @ARGV or die "usage: $0 save <current> <dest.tsv>\n";
    my $rec = load($cur);
    open my $out, '>', $dest or die "open $dest: $!\n";
    for my $k (sort keys %$rec) {
        my $r = $rec->{$k};
        print $out join("\t", $r->{file}, $r->{num}, $r->{desc}, $r->{got}, $r->{expected}), "\n";
    }
    close $out;
    printf "saved %d failures -> %s\n", scalar(keys %$rec), $dest;
    exit 0;
}

# diff
my $base_path = shift @ARGV or die "usage: $0 diff <baseline> <current>\n";
my $cur_path  = shift @ARGV or die "usage: $0 diff <baseline> <current>\n";
my $base = load($base_path);
my $cur  = load($cur_path);
my $cur_status = load_status($cur_path);   # file => OK/CRASH/PARTIAL/TIMEOUT/...

# A baseline failure that is absent from the current run is only genuinely FIXED
# if its file actually finished (status OK).  If the file CRASHED/PARTIAL'd/timed
# out this run, its later assertions never executed — so its "missing" failures
# are unverified, NOT fixes.  This is exactly the pack.t flaky-crash trap.
sub ran_clean {
    my ($file) = @_;
    my $s = $cur_status->{$file};
    return 1 unless defined $s;   # no status info (single-file baseline) → assume ran
    return $s eq 'OK';
}

my @new_all = sort grep { !exists $base->{$_} } keys %$cur;
# A NEW failure is only a genuine regression if its file finished cleanly this
# run.  A file that CRASHED/PARTIAL'd has a nondeterministic tail of assertions
# above its abort point (the abort site can shift run-to-run under parallel
# load); the extra described failures it emits there are noise, NOT regressions.
# This is the symmetric twin of the @notrun guard on the FIXED side, and is what
# stops a flaky crash in a known crash/PARTIAL file (bop.t, eval.t, …) from
# masquerading as "NEW failures (regressions)" and forcing a manual stash/compare.
my @new      = grep {  ran_clean($cur->{$_}{file}) } @new_all;  # file ran clean → real regression
my @new_unstable = grep { !ran_clean($cur->{$_}{file}) } @new_all;  # crash/partial file → noise
my @fixed_all = sort grep { !exists $cur->{$_} } keys %$base;
my @fixed   = grep {  ran_clean($base->{$_}{file}) } @fixed_all;  # file ran → real fix
my @notrun  = grep { !ran_clean($base->{$_}{file}) } @fixed_all;  # file did not run

if (@new) {
    print "NEW failures (regressions): ", scalar(@new), "\n";
    for my $k (@new) {
        my $r = $cur->{$k};
        printf "  + %-14s %s\n", $r->{file}, $r->{desc};
        printf "      got=%s expected=%s\n", $r->{got}, $r->{expected}
            if length($r->{got}) || length($r->{expected});
    }
    print "\n";
}
if (@new_unstable) {
    # Group by file — these are in crash/PARTIAL files this run, so they are
    # unverified noise (the file's abort point can shift), NOT regressions.
    my %by_file;
    $by_file{ $cur->{$_}{file} }++ for @new_unstable;
    print "UNSTABLE new fails (file crashed/partial this run — NOT counted as regressions): ",
        scalar(@new_unstable), "\n";
    for my $file (sort keys %by_file) {
        printf "  ~ %-14s %d new fail(s) above abort point — %s\n",
            $file, $by_file{$file}, ($cur_status->{$file} // 'NOT RUN');
    }
    print "\n";
}
if (@fixed) {
    print "FIXED (newly passing): ", scalar(@fixed), "\n";
    for my $k (@fixed) {
        my $r = $base->{$k};
        printf "  - %-14s %s\n", $r->{file}, $r->{desc};
    }
    print "\n";
}
if (@notrun) {
    # Group by file for a compact, honest report — these are NOT fixes.
    my %by_file;
    $by_file{ $base->{$_}{file} }++ for @notrun;
    print "DID NOT RUN (file crashed/partial this run — baseline fails UNVERIFIED, not fixed): ",
        scalar(@notrun), "\n";
    for my $file (sort keys %by_file) {
        printf "  ? %-14s %d baseline fail(s) absent — %s\n",
            $file, $by_file{$file}, ($cur_status->{$file} // 'NOT RUN');
    }
    print "\n";
}
printf "summary: %d new, %d fixed%s%s (baseline %d fails, current %d fails)\n",
    scalar(@new), scalar(@fixed),
    (@new_unstable ? sprintf(", %d unstable (crash-file noise)", scalar(@new_unstable)) : ''),
    (@notrun ? sprintf(", %d unverified (did not run)", scalar(@notrun)) : ''),
    scalar(keys %$base), scalar(keys %$cur);

exit(@new ? 1 : 0);
