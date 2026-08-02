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
#   tools/sweep-diff.pl diff <baseline> <current> # NEW + FIXED + LOST (see below)
#   tools/sweep-diff.pl save <current> <dest.tsv>  # write a sorted baseline to commit
#   tools/sweep-diff.pl save-status <current-dir> <dest.tsv>   # bless the PASS baseline
#
# <current>/<baseline> may each be a directory (globs *.fails.tsv) or a single .tsv file.
#
# THE FOURTH BUCKET — LOST (task #204, ruled in docs/fable-answers-s328.md §4).
# This tool compares FAILING rows, so a change that makes a file abort EARLIER
# removes PASSING rows without adding failing ones: the headline reads
# "0 new, 0 fixed" while the run silently lost coverage.  Measured live in s328
# (a die in `goto LABEL` cost perl-tests/state.t 88 verified rows, 157 -> 69,
# with a clean diff), so the total passing count is now a MACHINE-CHECKED gate,
# not an operator habit:
#
#   LOST = per file, baseline passing rows the current run did not produce.
#   Non-empty LOST = the run is NOT clean, same severity as NEW.
#
# The pass baseline is a blessed copy of a clean run's <dir>/_status.tsv
# (name <TAB> status <TAB> pass <TAB> fail <TAB> planned).  It is found, in
# order: --pass-baseline PATH; <baseline>/_status.tsv when <baseline> is a
# directory; else pass-baseline.tsv beside the fail baseline.  When none
# exists the LOST check SAYS SO on its own line — an unchecked gate must never
# look like a passed one.
#
# `diff` exits nonzero if there are NEW failures or any LOST rows — usable as a
# CI gate.

use strict;
use warnings;

# Read a per-file run status table: one line of
#   name <TAB> status <TAB> pass <TAB> fail <TAB> planned [<TAB> note]
# as the sweep writes to <dir>/_status.tsv and as `save-status` blesses.
# Returns file => { status, pass, fail, planned }.  An absent table is an empty
# hash, and every caller treats "no entry" as "no information", never as zero.
sub read_status_file {
    my ($sf) = @_;
    my %st;
    return \%st unless defined $sf && -e $sf;
    open my $fh, '<', $sf or return \%st;
    while (my $line = <$fh>) {
        chomp $line;
        next unless length $line;
        next if $line =~ /^#/;
        my ($file, $status, $pass, $fail, $planned) = split /\t/, $line;
        next unless defined $file;
        $st{$file} = { status  => $status  // 'OK',
                       pass    => defined $pass    ? $pass    : -1,
                       fail    => defined $fail    ? $fail    : -1,
                       planned => defined $planned ? $planned : -1 };
    }
    close $fh;
    return \%st;
}

# The status of the run under <path>: only a directory (a live .faillog) has one.
sub load_status {
    my ($path) = @_;
    return {} unless defined $path && -d $path;
    return read_status_file("$path/_status.tsv");
}

# Where the BASELINE's pass counts live.  Returns (path, records) — path is
# undef when no table could be found, which the LOST bucket reports out loud.
sub load_pass_baseline {
    my ($base_path, $explicit) = @_;
    # An EXPLICIT path that does not exist is an operator error, never a
    # reason to quietly use a different file: falling back would answer a
    # question nobody asked and call the run clean.
    if (defined $explicit) {
        die "--pass-baseline: no such file: $explicit\n" unless -e $explicit;
        return ($explicit, read_status_file($explicit));
    }
    my @try;
    push @try, "$base_path/_status.tsv" if -d $base_path;
    if (!-d $base_path) {
        my $dir = $base_path;
        $dir =~ s{/[^/]*$}{};
        $dir = '.' if $dir eq $base_path;
        push @try, "$dir/pass-baseline.tsv";
    }
    for my $p (@try) {
        return ($p, read_status_file($p)) if -e $p;
    }
    return (undef, {});
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
            next if $line =~ /^#/;   # legend/comment lines
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

my $pass_baseline_opt;
{   # --pass-baseline PATH may appear anywhere
    my @keep;
    while (@ARGV) {
        my $a = shift @ARGV;
        if ($a eq '--pass-baseline') { $pass_baseline_opt = shift @ARGV; next }
        if ($a =~ /^--pass-baseline=(.*)$/) { $pass_baseline_opt = $1; next }
        push @keep, $a;
    }
    @ARGV = @keep;
}

my %modes = map { $_ => 1 } qw(diff save save-status);
my $mode = (@ARGV && $modes{$ARGV[0]}) ? shift @ARGV : 'summary';

if ($mode eq 'save-status') {
    # Bless a clean run's per-file pass counts as the LOST-bucket baseline.
    # The sweep's `note` column (crash-localization snippet) is DROPPED: it
    # changes run to run and this file is a gate, not a report.
    my $cur  = shift @ARGV or die "usage: $0 save-status <current-dir> <dest.tsv>\n";
    my $dest = shift @ARGV or die "usage: $0 save-status <current-dir> <dest.tsv>\n";
    my $st = load_status($cur);
    die "no _status.tsv under $cur (a live .faillog directory is required)\n" unless %$st;
    open my $out, '>', $dest or die "open $dest: $!\n";
    my $total = 0;
    for my $f (sort keys %$st) {
        my $r = $st->{$f};
        print $out join("\t", $f, $r->{status}, $r->{pass}, $r->{fail}, $r->{planned}), "\n";
        $total += $r->{pass} if $r->{pass} > 0;
    }
    close $out;
    printf "saved %d files / %d passing rows -> %s\n", scalar(keys %$st), $total, $dest;
    exit 0;
}

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
    return $s->{status} eq 'OK';
}
sub cur_status_name {
    my ($file) = @_;
    my $s = $cur_status->{$file};
    return defined $s ? $s->{status} : 'NOT RUN';
}

# ── LOST: baseline PASSING rows this run did not produce (task #204) ─────────
# The three buckets above all read FAILING rows, so none of them can see a file
# that simply stopped earlier.  This one compares per-file pass counts.
my ($pass_base_path, $pass_base) = load_pass_baseline($base_path, $pass_baseline_opt);
my @lost;              # [file, lost_rows, why]
my ($base_total, $cur_total) = (0, 0);
if (%$pass_base && %$cur_status) {
    for my $file (sort keys %$pass_base) {
        my $pb = $pass_base->{$file}{pass};
        next if $pb < 0;                       # no count recorded → nothing to check
        $base_total += $pb;
        my $cs = $cur_status->{$file};
        if (!defined $cs) {
            push @lost, [$file, $pb, 'DID NOT RUN this sweep'] if $pb > 0;
            next;
        }
        my $pc = $cs->{pass};
        $cur_total += $pc if $pc > 0;
        next if $pc < 0 || $pc >= $pb;
        push @lost, [$file, $pb - $pc,
                     sprintf("%d -> %d passing, status %s (was %s)",
                             $pb, $pc, $cs->{status}, $pass_base->{$file}{status})];
    }
    # Files the current run produced that the baseline never had still count
    # toward the current total, so the headline compares like with like.
    for my $file (keys %$cur_status) {
        next if exists $pass_base->{$file};
        $cur_total += $cur_status->{$file}{pass} if $cur_status->{$file}{pass} > 0;
    }
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
            $file, $by_file{$file}, cur_status_name($file);
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
            $file, $by_file{$file}, cur_status_name($file);
    }
    print "\n";
}
if (@lost) {
    my $rows = 0; $rows += $_->[1] for @lost;
    print "LOST passing rows (baseline rows this run did not produce): $rows\n";
    for my $l (sort { $b->[1] <=> $a->[1] || $a->[0] cmp $b->[0] } @lost) {
        printf "  ! %-14s -%d  (%s)\n", $l->[0], $l->[1], $l->[2];
    }
    print "\n";
}

# The TOTAL line is the gate itself: it must be printed on EVERY run, including
# the runs where nothing was lost, and it must say so when it could not be
# computed.  A check that goes quiet when it cannot run is indistinguishable
# from a check that passed — which is the failure this whole bucket exists for.
if (!%$pass_base) {
    print "LOST: NOT CHECKED — no pass baseline found",
          (defined $pass_baseline_opt ? " at $pass_baseline_opt" : ''),
          " (bless one with: $0 save-status .faillog docs/pass-baseline.tsv)\n";
} elsif (!%$cur_status) {
    print "LOST: NOT CHECKED — the current run has no _status.tsv (needs a live .faillog directory)\n";
} else {
    printf "TOTAL passing: baseline %d, current %d (%+d)%s\n",
        $base_total, $cur_total, $cur_total - $base_total,
        (@lost ? '  <-- LOST is non-empty: this run is NOT clean' : '');
}

printf "summary: %d new, %d fixed%s%s%s (baseline %d fails, current %d fails)\n",
    scalar(@new), scalar(@fixed),
    (@lost ? sprintf(", %d LOST", scalar(@lost)) : ''),
    (@new_unstable ? sprintf(", %d unstable (crash-file noise)", scalar(@new_unstable)) : ''),
    (@notrun ? sprintf(", %d unverified (did not run)", scalar(@notrun)) : ''),
    scalar(keys %$base), scalar(keys %$cur);

exit((@new || @lost) ? 1 : 0);
