#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

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
#   tools/sweep-diff.pl save-shortfall <current-dir> <dest.tsv> # bless the SHORTFALL baseline
#
# THE BASELINE'S SIXTH COLUMN — the CAUSE (task #993, docs/plan-test-audit-s464.md
# §3 I3).  baselines/fail-baseline.tsv rows are
#   file <TAB> num <TAB> description <TAB> got <TAB> expected <TAB> cause
# where cause is a task number, a docs/not-supported.md anchor, or UNEXPLAINED.
# The JOIN KEY is unchanged — (file, description) — and a LIVE .faillog row has
# five fields, so the column is read when present and ignored otherwise.  Every
# `diff` prints how many blessed rows are cause-less, because a cause-less row
# is QUEUE, not baseline, and a queue nobody counts grows unnoticed: 229 of the
# 708 rows (bop.t, one mechanism, #1028) had sat unattributed for months.
# `save` cannot write causes — it reads a RUN — so it WARNS when the
# destination has them.  Baseline rows leave BY EDIT.
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
# THE FIFTH BUCKET — DROPS (task #343, ruled docs/fable-answers-s400.md §6.5).
# A statement the compiler could not lower is replaced by nil and the program
# runs on (the #138 family), which no bucket above can see: the row simply is
# not there.  The sweep now records a per-file `drops` count in _status.tsv and
# this tool compares it against the blessed census
# baselines/parse-error-drop-census-s399.tsv (override: --drop-census PATH).  A file
# with MORE drops than the census fails the run like a NEW failure; FEWER is a
# fix, and the census row leaves by EDIT — never by re-blessing the file.
#
# THE SIXTH BUCKET — SHORTFALL (task #993, plan-test-audit-s464.md §3 I2).
# A verdict of OK means "no previously-passing row was lost", never "the plan
# was produced": perl-tests/pack.t is OK with 8,997 of its 14,722 planned rows
# never produced, lc.t with 2,577.  Those rows are invisible to every bucket
# above, because the row is not failing — it is ABSENT.  The sweep records a
# per-file `shortfall` column in _status.tsv (planned - (pass+fail+skip)) and
# this tool compares it against baselines/row-shortfall.tsv, blessed per file
# WITH A CAUSE.  MORE than blessed fails the run like a NEW failure; FEWER is a
# fix and the row leaves BY EDIT.  Same contract as the drop census, and the
# same file shape — see tools/lib/PCLShortfall.pm.
#
# `diff` exits nonzero if there are NEW failures, any LOST rows, any new
# dropped statements, or any new shortfall — usable as a CI gate.

use strict;
use warnings;
use FindBin;
use lib "$FindBin::RealBin/lib";
use PCLShortfall ();   # the ONE reader of the shared shortfall baseline (#993)

# Read a per-file run status table: one line of
#   name <TAB> status <TAB> pass <TAB> fail <TAB> planned [<TAB> drops <TAB> note]
# as the sweep writes to <dir>/_status.tsv and as `save-status` blesses.
# Returns file => { status, pass, fail, planned, drops }.  An absent table is an
# empty hash, and every caller treats "no entry" as "no information", never as
# zero — same for a missing `drops` column (the blessed pass baseline has five
# columns and predates it), which reads back as -1 = NOT MEASURED.
sub read_status_file {
    my ($sf) = @_;
    my %st;
    return \%st unless defined $sf && -e $sf;
    open my $fh, '<', $sf or return \%st;
    while (my $line = <$fh>) {
        chomp $line;
        next unless length $line;
        next if $line =~ /^#/;
        my ($file, $status, $pass, $fail, $planned, $drops, $child, $short, $unrun)
            = split /\t/, $line;
        next unless defined $file;
        $st{$file} = { status  => $status  // 'OK',
                       pass    => defined $pass    ? $pass    : -1,
                       fail    => defined $fail    ? $fail    : -1,
                       planned => defined $planned ? $planned : -1,
                       drops   => (defined $drops && $drops =~ /^-?\d+$/) ? $drops : -1,
                       # SHORTFALL (task #993): planned - produced, the eighth
                       # column.  -1 = NOT MEASURED, exactly like drops — the
                       # blessed pass baseline has five columns and every reader
                       # must treat a missing column as unknown, never as zero.
                       short   => (defined $short && $short =~ /^-?\d+$/) ? $short : -1,
                       # the half of the shortfall that produced NO row at all
                       # (the file stopped); the rest of it was SKIPPED
                       unrun   => (defined $unrun && $unrun =~ /^-?\d+$/) ? $unrun : -1 };
    }
    close $fh;
    return \%st;
}

# ── The DROPS baseline: the #138-family census (task #343, ruled §6.5) ───────
# baselines/parse-error-drop-census-s399.tsv, whose rows are
#   <rel-path> <TAB> <drops> <TAB> <the compiler's own message(s)>
# over BOTH populations.  The sweep only measures perl-tests/*.t, so only those
# rows are comparable here; the companion half is compared by
# tools/run-perl-suite.pl.  The census IS the baseline: a drop that is fixed
# leaves it by EDIT, exactly like a fail-baseline row.
sub load_drop_census {
    my ($base_path, $explicit) = @_;
    my @try;
    if (defined $explicit) {
        die "--drop-census: no such file: $explicit\n" unless -e $explicit;
        @try = ($explicit);
    } else {
        my $dir = -d $base_path ? $base_path : ($base_path =~ s{/[^/]*$}{}r);
        $dir = '.' if !length $dir;
        @try = ("$dir/parse-error-drop-census-s399.tsv",
                'baselines/parse-error-drop-census-s399.tsv');
    }
    for my $p (@try) {
        next unless -e $p;
        my %c;
        open my $fh, '<', $p or next;
        while (my $line = <$fh>) {
            chomp $line;
            next if !length $line || $line =~ /^#/;
            my ($rel, $n) = split /\t/, $line;
            next unless defined $n && $n =~ /^\d+$/;
            $c{$rel} = $n;
        }
        close $fh;
        return ($p, \%c);
    }
    return (undef, {});
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
            # SIX fields since task #993: the blessed baseline carries a CAUSE
            # column (a task number, a not-supported.md anchor, or UNEXPLAINED).
            # The JOIN KEY is unchanged — (file, description) — and a live
            # .faillog row has five fields, so the sixth reads back undef there.
            # Splitting at 5 would have glued the cause onto `expected` and made
            # every blessed row's got/expected report wrong.
            my ($file, $num, $desc, $got, $exp, $cause) = split /\t/, $line, 6;
            next unless defined $desc;
            $rec{"$file\t$desc"} = { file => $file, desc => $desc,
                                     got => $got // '', expected => $exp // '',
                                     num => $num // '', cause => $cause };
        }
        close $fh;
    }
    return \%rec;
}

my ($pass_baseline_opt, $drop_census_opt);
{   # --pass-baseline PATH / --drop-census PATH may appear anywhere
    my @keep;
    while (@ARGV) {
        my $a = shift @ARGV;
        if ($a eq '--pass-baseline') { $pass_baseline_opt = shift @ARGV; next }
        if ($a =~ /^--pass-baseline=(.*)$/) { $pass_baseline_opt = $1; next }
        if ($a eq '--drop-census') { $drop_census_opt = shift @ARGV; next }
        if ($a =~ /^--drop-census=(.*)$/) { $drop_census_opt = $1; next }
        push @keep, $a;
    }
    @ARGV = @keep;
}

my %modes = map { $_ => 1 } qw(diff save save-status save-shortfall);
my $mode = (@ARGV && $modes{$ARGV[0]}) ? shift @ARGV : 'summary';

if ($mode eq 'save-shortfall') {
    # Bless THIS POPULATION's rows in the shared shortfall baseline (task #993).
    # The companion's rows (`t/…`) are copied through untouched — one file, two
    # populations, exactly like baselines/parse-error-drop-census-s399.tsv.  A
    # writer that only knows its own half must never erase the other's.
    my $cur  = shift @ARGV or die "usage: $0 save-shortfall <current-dir> <dest.tsv>\n";
    my $dest = shift @ARGV or die "usage: $0 save-shortfall <current-dir> <dest.tsv>\n";
    my $st = load_status($cur);
    die "no _status.tsv under $cur (a live .faillog directory is required)\n" unless %$st;
    my $rows = PCLShortfall::read_shortfall($dest);
    my $touched = 0;
    for my $file (sort keys %$st) {
        my $now = $st->{$file}{short};
        next if !defined $now || $now < 0;         # not measured: keep the row
        $touched++;
        my $key = "perl-tests/$file";
        if (!$now) { delete $rows->{$key}; next }
        $rows->{$key} = { rows  => $now,
                          cause => ($rows->{$key} ? $rows->{$key}{cause} : 'UNEXPLAINED') };
    }
    my $sha = `git rev-parse --short HEAD 2>/dev/null`;
    chomp $sha;
    my @t = localtime;
    PCLShortfall::write_shortfall($dest, $rows,
        sprintf("%s %04d-%02d-%02d", (length $sha ? $sha : 'unknown'), $t[5]+1900, $t[4]+1, $t[3]));
    my $sum = 0; $sum += $rows->{$_}{rows} for keys %$rows;
    printf "saved %d shortfall row(s) over %d file(s) (%d sweep file(s) re-measured) -> %s\n",
        $sum, scalar(keys %$rows), $touched, $dest;
    exit 0;
}

if ($mode eq 'save-status') {
    # Bless a clean run's per-file pass counts as the LOST-bucket baseline.
    # The sweep's `note` column (crash-localization snippet) is DROPPED: it
    # changes run to run and this file is a gate, not a report.
    my $cur  = shift @ARGV or die "usage: $0 save-status <current-dir> <dest.tsv>\n";
    my $dest = shift @ARGV or die "usage: $0 save-status <current-dir> <dest.tsv>\n";
    my $st = load_status($cur);
    die "no _status.tsv under $cur (a live .faillog directory is required)\n" unless %$st;
    open my $out, '>', $dest or die "open $dest: $!\n";
    # Provenance stamp (task #223, ruled fable-answers-s339.md §5b): a generated
    # baseline must say WHICH tree it was taken from, so a later "+8 drift" can be
    # bisected instead of guessed at.  Readers skip `#` lines (read_status_file,
    # load).  A missing/failing git is not fatal — the stamp degrades to unknown.
    my $sha = `git rev-parse --short HEAD 2>/dev/null`;
    chomp $sha;
    $sha = 'unknown' unless length $sha;
    my @t = localtime;
    printf $out "# taken-at: %s %04d-%02d-%02d\n", $sha, $t[5]+1900, $t[4]+1, $t[3];
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
    # A RUN has no causes to write, so this mode writes FIVE columns.  Say so
    # loudly when the destination already has a cause column: overwriting
    # baselines/fail-baseline.tsv from a run would throw away every attribution
    # AND absorb whatever else moved — which is why its rows leave BY EDIT.
    if (-e $dest) {
        my $has_cause = 0;
        if (open my $dh, '<', $dest) {
            while (my $l = <$dh>) {
                next if $l =~ /^#/ || $l !~ /\S/;
                chomp $l;
                $has_cause = 1 if (split /\t/, $l, 6) >= 6;
                last;
            }
            close $dh;
        }
        warn "WARNING: $dest carries a CAUSE column and `save` cannot write one —\n"
           . "  every attribution in it will be LOST.  A blessed baseline's rows leave\n"
           . "  BY EDIT (task #993 / #223); use `save` only for a NEW baseline file.\n"
            if $has_cause;
    }
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

# ── DROPS: statements this run's transpiles lost, vs the blessed census ──────
# Silent at run time, so no other bucket can see them: a dropped statement is
# simply not in the emitted CL (perl-tests/bless.t's is a test row that never
# runs, in a file this sweep calls passing).  MORE drops than the census = the
# run is NOT clean; FEWER = a fix, and the census row leaves by EDIT.
my ($census_path, $census) = load_drop_census($base_path, $drop_census_opt);
my (@drop_up, @drop_down);
if (%$census && %$cur_status) {
    for my $file (sort keys %$cur_status) {
        my $now = $cur_status->{$file}{drops};
        next if !defined $now || $now < 0;         # not measured this run
        my $was = $census->{"perl-tests/$file"} // 0;
        push @drop_up,   [$file, $was, $now] if $now > $was;
        push @drop_down, [$file, $was, $now] if $now < $was;
    }
}

# ── SIXTH BUCKET — SHORTFALL: rows the PLAN promised that never ran (#993) ───
# A file's verdict here is "no previously-passing row was lost", which says
# nothing about the rows that never ran at all: perl-tests/pack.t is OK with
# 8,997 of its 14,722 planned rows never produced, lc.t with 2,577.  Those are
# invisible to every bucket above — the row is not failing, it is ABSENT — so
# they are blessed per file WITH A CAUSE in baselines/row-shortfall.tsv and
# compared here.  MORE than blessed = the run is NOT clean; FEWER = a fix, and
# the row leaves BY EDIT.  Same contract as the drop census.
my $shortfall_path = do {
    my $dir = -d $base_path ? $base_path : ($base_path =~ s{/[^/]*$}{}r);
    $dir = '.' if !length $dir;
    my ($p) = grep { -e $_ } ("$dir/row-shortfall.tsv", 'baselines/row-shortfall.tsv');
    $p;
};
my $shortfall_base = $shortfall_path ? PCLShortfall::read_shortfall($shortfall_path) : {};
my (@short_up, @short_down, $short_now, $short_unexplained, $short_unexplained_files);
($short_now, $short_unexplained, $short_unexplained_files) = (0, 0, 0);
if (%$shortfall_base && %$cur_status) {
    for my $file (sort keys %$cur_status) {
        my $now = $cur_status->{$file}{short};
        next if !defined $now || $now < 0;         # not measured this run
        $short_now += $now;
        my $b   = $shortfall_base->{"perl-tests/$file"};
        my $was = $b ? $b->{rows} : 0;
        push @short_up,   [$file, $was, $now] if $now > $was;
        push @short_down, [$file, $was, $now] if $now < $was;
        if ($now && (!$b || $b->{cause} =~ /^UNEXPLAINED/)) {
            $short_unexplained += $now;
            $short_unexplained_files++;
        }
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
        # FORMAT CONSUMER: sweep-perl-tests.pl's run_gate() parses these lines
        # (/^  ! (\S+)\s+-\d+/) to pick the files for its serial re-run.  If
        # you reformat this printf, update that regex or the re-run silently
        # stops firing.
        printf "  ! %-14s -%d  (%s)\n", $l->[0], $l->[1], $l->[2];
    }
    print "\n";
}

if (@drop_up || @drop_down) {
    print "DROPPED STATEMENTS vs the census (", ($census_path // '?'), "):\n";
    printf "  + %-14s %d -> %d dropped statement(s) — NEW silent drop\n", @$_[0,1,2]
        for sort { $a->[0] cmp $b->[0] } @drop_up;
    printf "  - %-14s %d -> %d dropped statement(s) — fixed; EDIT the census row\n", @$_[0,1,2]
        for sort { $a->[0] cmp $b->[0] } @drop_down;
    print "\n";
}
# Same rule as the TOTAL line below: a check that goes quiet when it could not
# run is indistinguishable from one that passed.
if (!%$census) {
    print "DROPS: NOT CHECKED — no drop census found",
          (defined $drop_census_opt ? " at $drop_census_opt" : ''),
          " (re-bless with: tools/drop-census.pl . baselines/parse-error-drop-census-s399.tsv)\n";
} elsif (!%$cur_status) {
    print "DROPS: NOT CHECKED — the current run has no _status.tsv (needs a live .faillog directory)\n";
} else {
    my ($cur_d, $base_d, $unmeasured) = (0, 0, 0);
    for my $file (sort keys %$cur_status) {
        my $now = $cur_status->{$file}{drops};
        if (!defined $now || $now < 0) { $unmeasured++; next }
        $cur_d  += $now;
        $base_d += $census->{"perl-tests/$file"} // 0;
    }
    printf "TOTAL dropped statements: census %d, current %d (%+d)%s%s\n",
        $base_d, $cur_d, $cur_d - $base_d,
        ($unmeasured ? sprintf("  [%d file(s) not measured]", $unmeasured) : ''),
        (@drop_up ? '  <-- a NEW drop landed: this run is NOT clean' : '');
}

if (@short_up || @short_down) {
    print "SHORTFALL vs the baseline (", ($shortfall_path // '?'), "):\n";
    printf "  + %-14s %d -> %d planned row(s) NEVER PRODUCED — NEW shortfall\n", @$_[0,1,2]
        for sort { $a->[0] cmp $b->[0] } @short_up;
    printf "  - %-14s %d -> %d planned row(s) — fixed; EDIT the baseline row\n", @$_[0,1,2]
        for sort { $a->[0] cmp $b->[0] } @short_down;
    print "\n";
}
# Same rule as every other bucket: a check that goes quiet when it could not
# run is indistinguishable from one that passed.
if (!%$shortfall_base) {
    print "SHORTFALL: NOT CHECKED — no baselines/row-shortfall.tsv"
        . " (bless one with: $0 save-shortfall .faillog baselines/row-shortfall.tsv)\n";
} elsif (!%$cur_status) {
    print "SHORTFALL: NOT CHECKED — the current run has no _status.tsv (needs a live .faillog directory)\n";
} else {
    my $base_short = 0;
    for my $file (sort keys %$cur_status) {
        next if ($cur_status->{$file}{short} // -1) < 0;
        $base_short += ($shortfall_base->{"perl-tests/$file"} || {})->{rows} // 0;
    }
    my $unrun_now = 0;
    for my $file (sort keys %$cur_status) {
        my $u = $cur_status->{$file}{unrun};
        $unrun_now += $u if defined $u && $u > 0;
    }
    printf "TOTAL planned rows not asserted: baseline %d, current %d (%+d)%s\n",
        $base_short, $short_now, $short_now - $base_short,
        (@short_up ? '  <-- a NEW shortfall landed: this run is NOT clean' : '');
    # The two halves are very different in character and a single number hides
    # it: pack.t's 8,997 are almost all SKIPPED rows (blessed or the file's
    # own directives), while caller.t's 47 are rows that never ran because the
    # file aborted.  Both are shortfall; only one is a crash.
    printf "  of those, %d row(s) produced no TAP at all (the file stopped);"
         . " %d were SKIPPED\n", $unrun_now, $short_now - $unrun_now
        if $short_now > 0;
    printf "  UNEXPLAINED shortfall: %d row(s) in %d file(s) — that is the audit's queue (#993)\n",
        $short_unexplained, $short_unexplained_files if $short_unexplained;
}

# ── The CAUSE column (task #993 I3) ─────────────────────────────────────────
# A blessed failing row carries a CAUSE (task number / not-supported.md anchor);
# a cause-less row is QUEUE, not baseline.  Counted on every run so the queue
# can never silently grow — the same reason the UNEXPLAINED suite verdicts are
# counted rather than inferred from an absence.
{
    my ($have, $none) = (0, 0);
    for my $k (keys %$base) {
        my $c = $base->{$k}{cause};
        if (defined $c && length $c && $c !~ /^UNEXPLAINED/) { $have++ } else { $none++ }
    }
    if (!$have && !$none) {
        print "CAUSES: NOT CHECKED — the baseline has no rows\n";
    } elsif (!$have) {
        printf "CAUSES: NOT CHECKED — no cause column in %s (add one: docs/plan-test-audit-s464.md §3 I3)\n",
            $base_path;
    } else {
        printf "CAUSES: %d of %d blessed row(s) have no cause — a cause-less row is QUEUE, not baseline (#993)\n",
            $none, $have + $none;
    }
}

# The TOTAL line is the gate itself: it must be printed on EVERY run, including
# the runs where nothing was lost, and it must say so when it could not be
# computed.  A check that goes quiet when it cannot run is indistinguishable
# from a check that passed — which is the failure this whole bucket exists for.
if (!%$pass_base) {
    print "LOST: NOT CHECKED — no pass baseline found",
          (defined $pass_baseline_opt ? " at $pass_baseline_opt" : ''),
          " (bless one with: $0 save-status .faillog baselines/pass-baseline.tsv)\n";
} elsif (!%$cur_status) {
    print "LOST: NOT CHECKED — the current run has no _status.tsv (needs a live .faillog directory)\n";
} else {
    printf "TOTAL passing: baseline %d, current %d (%+d)%s\n",
        $base_total, $cur_total, $cur_total - $base_total,
        (@lost ? '  <-- LOST is non-empty: this run is NOT clean' : '');
}

printf "summary: %d new, %d fixed%s%s%s%s%s (baseline %d fails, current %d fails)\n",
    scalar(@new), scalar(@fixed),
    (@lost ? sprintf(", %d LOST", scalar(@lost)) : ''),
    (@drop_up ? sprintf(", %d file(s) with NEW drops", scalar(@drop_up)) : ''),
    (@short_up ? sprintf(", %d file(s) with NEW shortfall", scalar(@short_up)) : ''),
    (@new_unstable ? sprintf(", %d unstable (crash-file noise)", scalar(@new_unstable)) : ''),
    (@notrun ? sprintf(", %d unverified (did not run)", scalar(@notrun)) : ''),
    scalar(keys %$base), scalar(keys %$cur);

exit((@new || @lost || @drop_up || @short_up) ? 1 : 0);
