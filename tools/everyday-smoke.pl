#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# everyday-smoke.pl — how many ORDINARY Perl programs does PCL answer exactly
# like perl?  (task #2099; USER, s493: "Yes, run on demand", and at the end of
# s494: build it early and STEER BY ITS NUMBER — the goal is > 90 %.)
#
# The suites this project runs measure the language in ISOLATION and in bulk:
# perl's own t/ and perl-tests/ rank exotic gaps first, because that is what
# they contain.  What an ordinary script meets shows up only when ORDINARY
# Perl is compared with perl, byte for byte — which is what the s491/s492/s493
# batteries did by hand (103 programs, 68 identical).  This is that
# measurement made repeatable:
#
#     tools/everyday-smoke.pl
#     …
#     EVERYDAY: 85 of 122 identical to perl (69.7 %) -- 87e4e4b3 gen v2-1740
#
# THE LAST LINE IS THE NUMBER.  It is stable and greppable on purpose: every
# batch report quotes it before and after.
#
# WHAT IT IS NOT: a compatibility percentage.  The programs were written to
# COVER GROUND, not sampled from real code, and the denominator grows as
# programs are added.  It answers one question — "is ordinary Perl getting
# better batch by batch?" — and no other.  docs/everyday-battery.md says this
# at length; do not quote the number without it.
#
# ---------------------------------------------------------------- the design
#
# A RUN NEEDS NO PERL.  Every program's expected STDOUT is checked in beside
# it (`<name>.expect`, bytes) together with its exit status when that is not 0
# (`<name>.rc`), so a run is reproducible on a machine with no perl build and
# cannot drift with the oracle.  perl is needed in exactly two places, both
# opt-in: `--bless-expect` (derive an expectation) and `--verify-with-perl`
# (re-derive everything and report DRIFT).
#
# A PROGRAM IS ADMITTED ONLY IF PERL'S ANSWER IS A FACT OF THE PROGRAM, not of
# the run.  `--bless-expect` runs the program three times under perl — twice in
# place and once as a COPY under a different absolute path with a different cwd
# — and refuses to write an expectation unless all three agree in STDOUT bytes
# and status.  That catches `$0` / `__FILE__`, cwd-relative files, the clock,
# `$$`, `rand`, and unsorted `keys` (perl randomizes its hash seed per run, so
# three plain runs are a real hash-order test).
#
# THE WORLD IS THE SAME ON BOTH SIDES (one sub, `child_env`, builds it for the
# perl side and the PCL side alike): cwd is a fresh empty directory per
# program, TZ=UTC, LC_ALL=C, LANG=C, and PERL5LIB / PERL5OPT / PERL_HASH_SEED /
# PERL_PERTURB_KEYS are removed.
#
# THE BASELINE IS A LIST OF WHAT IS WRONG (baselines/everyday-baseline.tsv),
# like baselines/fail-baseline.tsv: a `same` program has NO row.  Every row
# carries a CAUSE — a task number or a docs/not-supported.md anchor — that
# explains THE FIRST DIFFERING LINE, and rows leave BY EDIT.  There is
# deliberately no --bless-baseline: a baseline you can re-bless is a baseline
# that absorbs regressions silently.
#
#   NEW          not `same`, no baseline row                    => exit 1
#   FIXED        a row whose program is now `same`              => edit it out
#   MOVED        same row, different first-diff line/verdict    => re-attribute
#   UNEXPLAINED  a row with an empty cause                      => exit 1
#   STALE        a row naming a program that does not exist     => exit 1
#
# USAGE
#   tools/everyday-smoke.pl [OPTIONS] [AREA | FILE …]
#
#   --jobs N            programs in parallel (default 2)
#   --timeout S         seconds per program (default 60; `# timeout: S` in a
#                       program's header raises it for that one)
#   --pcl CMD           the command under test (default <root>/pcl)
#   --corpus DIR        the corpus (default <root>/everyday)
#   --baseline FILE     the baseline (default <root>/baselines/everyday-baseline.tsv;
#                       `--baseline none` = verdicts only, no buckets)
#   --outdir DIR        where results and work directories go (default <root>/.everyday)
#   --record            append one row to baselines/everyday-history.tsv
#                       (whole-corpus runs of the default corpus on a clean tree only)
#   --bless-expect      derive .expect/.rc for the named programs (needs perl)
#   --verify-with-perl  re-derive everything and report DRIFT / NEEDS (needs perl)
#   --list              print the selected programs and exit
#
# EXIT  0 all explained · 1 NEW / UNEXPLAINED / STALE / DRIFT · 2 usage, or the
#       tree or the command under test was not found.
use strict;
use warnings;
use Getopt::Long qw(:config no_ignore_case);
use File::Basename qw(basename dirname);
use File::Path qw(make_path remove_tree);
use File::Temp qw(tempdir);
use File::Copy qw(copy);
use POSIX qw(:sys_wait_h);
use Text::ParseWords qw(shellwords);
use Cwd qw(abs_path);
use FindBin qw($RealBin);
use lib "$RealBin/lib";
use PCLPaths ();   # the ONE root resolver (task #1302)

my $LINE_WIDTH = 160;   # every per-program line is cut to one terminal line

# ------------------------------------------------------------------ options
my %opt = (jobs => 2, timeout => 60);
GetOptions(\%opt, qw(jobs=i timeout=i pcl=s corpus=s baseline=s outdir=s
                     record bless-expect verify-with-perl list help|h))
    or usage(2);
usage(0) if $opt{help};

my $ROOT = eval { PCLPaths::root($0) };
if (!defined $ROOT) { print STDERR $@; exit 2 }

my $CORPUS_DEFAULT = "$ROOT/everyday";
my $CORPUS   = defined $opt{corpus} ? _rstrip_slash($opt{corpus}) : $CORPUS_DEFAULT;
my $OUTDIR   = defined $opt{outdir} ? _rstrip_slash($opt{outdir}) : "$ROOT/.everyday";
my $HISTORY  = "$ROOT/baselines/everyday-history.tsv";
my $BASELINE = defined $opt{baseline} ? $opt{baseline}
                                      : "$ROOT/baselines/everyday-baseline.tsv";
my $DEFAULT_CORPUS = (abs_path($CORPUS) // $CORPUS) eq (abs_path($CORPUS_DEFAULT) // $CORPUS_DEFAULT);

if (!-d $CORPUS) { print STDERR "everyday-smoke: no corpus directory $CORPUS\n"; exit 2 }

# The command under test.  Split like a shell word list so `--pcl 'perl -X'`
# works; the default is the tree's own driver (NOT runpcl: script arguments
# must reach the program).
my @PCL = $opt{pcl} ? shellwords($opt{pcl}) : ("$ROOT/pcl");
if (!$opt{'bless-expect'} && !$opt{'verify-with-perl'} && !-x $PCL[0]) {
    print STDERR "everyday-smoke: $PCL[0] is not executable (--pcl CMD overrides it)\n";
    exit 2;
}

# ------------------------------------------------------------ program lookup
#
# A program is <corpus>/<area>/<name>.pl.  A positional argument is an AREA
# (a directory under the corpus) or a path to a .pl file; with none, the whole
# corpus is selected and the run may quote its number as THE number.
my @ALL = all_programs($CORPUS);
if (!@ALL) { print STDERR "everyday-smoke: no programs under $CORPUS\n"; exit 2 }

my ($SELECTED, $IS_SUBSET) = select_programs(\@ALL, \@ARGV);
if (!@$SELECTED) { print STDERR "everyday-smoke: nothing selected\n"; exit 2 }

if ($opt{list}) { print "$_->{key}\n" for @$SELECTED; exit 0 }

exit bless_expect($SELECTED)      if $opt{'bless-expect'};
exit verify_with_perl($SELECTED)  if $opt{'verify-with-perl'};
exit run_corpus($SELECTED, $IS_SUBSET);

# =========================================================================
# the corpus
# =========================================================================

# Every <area>/<name>.pl under DIR, sorted, as records the rest of the file
# uses: key, area, name, path, and the header facts (args, needs, timeout,
# expect-rc, stdin).
sub all_programs {
    my ($dir) = @_;
    my @out;
    opendir(my $dh, $dir) or return ();
    for my $area (sort grep { $_ !~ /^\./ } readdir $dh) {
        next unless -d "$dir/$area";
        opendir(my $ah, "$dir/$area") or next;
        for my $f (sort grep { /\.pl$/ } readdir $ah) {
            (my $name = $f) =~ s/\.pl$//;
            push @out, program_record($dir, $area, $name);
        }
        closedir $ah;
    }
    closedir $dh;
    return @out;
}

sub program_record {
    my ($dir, $area, $name) = @_;
    my $path = "$dir/$area/$name.pl";
    my $p = {
        key => "$area/$name", area => $area, name => $name, path => $path,
        base => "$dir/$area/$name",
        args => [], needs => [], timeout => undef, expect_rc => undef,
    };
    read_headers($p);
    $p->{stdin} = -f "$p->{base}.stdin" ? "$p->{base}.stdin" : '/dev/null';
    return $p;
}

# The header comments the runner reads.  They are ordinary Perl comments, so a
# program still runs by hand:
#   # args: alpha beta        script arguments
#   # needs: Module::Name     perl must have it to DERIVE the expectation
#   # timeout: 120            this one is slow
#   # expect-rc: 2            perl is expected to exit non-zero
#   # origin: …               where the program came from (informational)
sub read_headers {
    my ($p) = @_;
    open my $fh, '<:raw', $p->{path} or return;
    while (defined(my $l = <$fh>)) {
        last if $l !~ /^\s*#/ && $l =~ /\S/;   # headers live in the leading comments
        if    ($l =~ /^#\s*args:\s*(.*?)\s*$/)      { $p->{args}      = [ shellwords($1) ] }
        elsif ($l =~ /^#\s*needs:\s*(.*?)\s*$/)     { push @{ $p->{needs} }, split ' ', $1 }
        elsif ($l =~ /^#\s*timeout:\s*(\d+)\s*$/)   { $p->{timeout}   = $1 }
        elsif ($l =~ /^#\s*expect-rc:\s*(\d+)\s*$/) { $p->{expect_rc} = $1 }
    }
    close $fh;
    return;
}

sub select_programs {
    my ($all, $argv) = @_;
    return ($all, 0) if !@$argv;
    my %by_key  = map { $_->{key} => $_ } @$all;
    my %by_path = map { (abs_path($_->{path}) // $_->{path}) => $_ } @$all;
    my (@sel, %seen);
    for my $a (@$argv) {
        my @hit;
        if (-f $a) {
            my $ap = abs_path($a) // $a;
            @hit = grep { defined } ($by_path{$ap});
            if (!@hit) { print STDERR "everyday-smoke: $a is not in $CORPUS\n"; exit 2 }
        }
        elsif ($by_key{$a}) { @hit = ($by_key{$a}) }
        else {
            (my $area = $a) =~ s{/+$}{};
            @hit = grep { $_->{area} eq $area } @$all;
            if (!@hit) { print STDERR "everyday-smoke: no area or program '$a'\n"; exit 2 }
        }
        for my $h (@hit) { push @sel, $h unless $seen{ $h->{key} }++ }
    }
    return (\@sel, scalar(@sel) != scalar(@$all));
}

# =========================================================================
# the world both sides run in
# =========================================================================

# ONE definition of the environment, used by the perl side (expectation
# derivation) and the PCL side (the run).  If these two ever differ, every
# expectation is a lie about a world the run does not have.
sub child_env {
    my %e = %ENV;
    delete @e{ qw(PERL5LIB PERL5OPT PERL_HASH_SEED PERL_PERTURB_KEYS) };
    $e{TZ} = 'UTC'; $e{LC_ALL} = 'C'; $e{LANG} = 'C';
    return \%e;
}

# Run COMMAND on a program in a fresh WORK directory, with its stdin, stdout
# and stderr as files.  Returns (exit-status, timed-out).  The child gets its
# own session so a timeout kills the WHOLE tree: a runaway SBCL survives a
# SIGTERM to the driver alone (measured s493), and a leaked child would then
# hold the next program's files open.
sub spawn {
    my ($cmd, $prog, $work, $out, $err) = @_;
    remove_tree($work) if -d $work;
    make_path($work);
    my $env = child_env();
    my $pid = fork();
    die "everyday-smoke: fork: $!\n" if !defined $pid;
    if ($pid == 0) {
        POSIX::setsid();
        %ENV = %$env;
        chdir $work           or POSIX::_exit(126);
        open STDIN,  '<', $prog->{stdin} or POSIX::_exit(126);
        open STDOUT, '>', $out           or POSIX::_exit(126);
        open STDERR, '>', $err           or POSIX::_exit(126);
        exec { $cmd->[0] } @$cmd or POSIX::_exit(127);
    }
    return $pid;
}

sub status_of { my ($w) = @_; return ($w & 127) ? 128 + ($w & 127) : ($w >> 8) }

# Run one program to completion (used by the perl side, which is serial).
sub run_serial {
    my ($cmd, $prog, $work, $out, $err, $timeout) = @_;
    my $pid = spawn($cmd, $prog, $work, $out, $err);
    my $start = time();
    while (1) {
        my $got = waitpid($pid, WNOHANG);
        return (status_of($?), 0) if $got == $pid;
        if (time() - $start > $timeout) {
            kill 'KILL', -$pid; kill 'KILL', $pid;
            waitpid($pid, 0);
            return (-1, 1);
        }
        select(undef, undef, undef, 0.02);
    }
}

# =========================================================================
# comparison
# =========================================================================

sub slurp { my ($p) = @_; open my $h, '<:raw', $p or return undef; local $/; my $t = <$h>; close $h; return defined $t ? $t : '' }
sub spew  { my ($p, $t) = @_; open my $h, '>:raw', $p or die "everyday-smoke: $p: $!\n"; print $h $t; close $h }

# The 1-based number of the first STDOUT line that differs.  When one side is
# a prefix of the other, that is the first line the shorter one does not have.
sub first_diff_line {
    my ($want, $got) = @_;
    my @w = split /(?<=\n)/, $want;
    my @g = split /(?<=\n)/, $got;
    my $n = @w < @g ? @w : @g;
    for my $i (0 .. $n - 1) { return $i + 1 if $w[$i] ne $g[$i] }
    return $n + 1;
}

sub expected_rc { my ($p) = @_; my $t = slurp("$p->{base}.rc"); return defined $t && $t =~ /(\d+)/ ? $1 : 0 }

sub verdict_for {
    my ($want, $want_rc, $got, $got_rc, $timed_out) = @_;
    return ('TIMEOUT', '-') if $timed_out;
    return ('DIFF', first_diff_line($want, $got)) if $got ne $want;
    return ('RC', '-') if $got_rc != $want_rc;
    return ('same', '-');
}

# PCL's first REAL complaint: the banner, the compiler's notes and SBCL's
# style warnings are not one.
sub first_complaint {
    my ($text) = @_;
    return '' if !defined $text;
    for my $l (split /\n/, $text) {
        next if $l !~ /\S/;
        next if $l =~ /^PCL Runtime loaded/ || $l =~ /^;/ || $l =~ /^WARNING/ || $l =~ /STYLE-WARNING/;
        $l =~ s/\s+$//;
        return $l;
    }
    return '';
}

# =========================================================================
# the run
# =========================================================================

sub run_corpus {
    my ($progs, $is_subset) = @_;

    # Pre-flight: an expectation missing is a corpus error, not a verdict.
    my @noexpect = grep { !-f "$_->{base}.expect" } @$progs;
    if (@noexpect) {
        print STDERR "everyday-smoke: no expectation for:\n";
        print STDERR "    $_->{key}\n" for @noexpect;
        print STDERR "Derive it with:  tools/everyday-smoke.pl --bless-expect FILE…\n";
        exit 2;
    }

    my %res;   # key => { verdict, line, rc, complaint }
    my %kid;   # pid => record
    my @queue  = @$progs;
    while (@queue || %kid) {
        while (@queue && keys(%kid) < $opt{jobs}) {
            my $p = shift @queue;
            my $dir = "$OUTDIR/$p->{area}";
            make_path($dir);
            my $work = "$OUTDIR/work/$p->{area}/$p->{name}";
            my $pid  = spawn([ @PCL, $p->{path}, @{ $p->{args} } ], $p, $work,
                             "$dir/$p->{name}.out", "$dir/$p->{name}.err");
            $kid{$pid} = { prog => $p, start => time(), work => $work, dir => $dir,
                           timeout => $p->{timeout} || $opt{timeout} };
        }
        my $reaped = 0;
        for my $pid (keys %kid) {
            my $got = waitpid($pid, WNOHANG);
            if ($got == $pid) { finish($kid{$pid}, status_of($?), 0, \%res); delete $kid{$pid}; $reaped = 1; next }
            if (time() - $kid{$pid}{start} > $kid{$pid}{timeout}) {
                kill 'KILL', -$pid; kill 'KILL', $pid;
                waitpid($pid, 0);
                finish($kid{$pid}, -1, 1, \%res);
                delete $kid{$pid};
                $reaped = 1;
            }
        }
        select(undef, undef, undef, 0.05) if !$reaped && (%kid || @queue);
    }

    return report($progs, \%res, $is_subset);
}

sub finish {
    my ($k, $rc, $timed_out, $res) = @_;
    my $p = $k->{prog};
    spew("$k->{dir}/$p->{name}.rc", "$rc\n");
    my $got  = slurp("$k->{dir}/$p->{name}.out");
    my $want = slurp("$p->{base}.expect");
    my ($v, $line) = verdict_for($want, expected_rc($p), $got, $rc, $timed_out);
    # A program that matched leaves nothing behind; one that did not keeps its
    # work directory, because what it wrote is usually the evidence.
    remove_tree($k->{work}) if $v eq 'same' && -d $k->{work};
    $res->{ $p->{key} } = { verdict => $v, line => $line, rc => $rc,
                            complaint => first_complaint(slurp("$k->{dir}/$p->{name}.err")) };
    return;
}

# =========================================================================
# the report
# =========================================================================

sub report {
    my ($progs, $res, $is_subset) = @_;
    my ($rows, $row_err) = read_baseline($BASELINE);
    if ($row_err) { print STDERR $row_err; exit 2 }

    # one line per NON-same program
    for my $p (@$progs) {
        my $r = $res->{ $p->{key} };
        next if $r->{verdict} eq 'same';
        my $b = $rows->{ $p->{key} };
        my $cause = $b ? $b->{cause} : '(no baseline row)';
        my $line = sprintf("%-7s %-38s %-8s %-22s %s",
                           $r->{verdict}, $p->{key},
                           $r->{line} eq '-' ? '' : "line $r->{line}",
                           $cause, $r->{complaint});
        $line =~ s/\s+$//;
        print substr($line, 0, $LINE_WIDTH), "\n";
    }

    # per-area counts
    my (%same, %tot);
    for my $p (@$progs) { $tot{ $p->{area} }++; $same{ $p->{area} }++ if $res->{ $p->{key} }{verdict} eq 'same' }
    print "\n";
    printf("  %-12s %3d/%-3d same\n", $_, $same{$_} // 0, $tot{$_}) for sort keys %tot;

    my $exit = 0;
    if (defined $BASELINE && $BASELINE ne 'none') {
        $exit = buckets($progs, $res, $rows);
    }

    my $n = 0; $n += $same{$_} // 0 for keys %tot;
    my $m = 0; $m += $tot{$_} for keys %tot;
    my @qual;
    push @qual, "corpus $CORPUS" if !$DEFAULT_CORPUS;
    push @qual, "subset"         if $is_subset;
    my ($sha, $dirty) = git_state();
    printf("\n%s: %d of %d identical to perl (%.1f %%) -- %s%s gen %s\n",
           @qual ? "EVERYDAY (" . join(', ', @qual) . ")" : "EVERYDAY",
           $n, $m, $m ? 100 * $n / $m : 0, $sha, $dirty ? '+dirty' : '', generation());

    record_history($n, $m, \%same, \%tot, $is_subset, $sha, $dirty) if $opt{record};
    return $exit;
}

# NEW / FIXED / MOVED / UNEXPLAINED / STALE.  NEW, UNEXPLAINED and STALE fail
# the run; FIXED and MOVED are printed, because a row leaves or is
# re-attributed BY HAND.
sub buckets {
    my ($progs, $res, $rows) = @_;
    my %have = map { $_->{key} => 1 } @$progs;
    my (@new, @fixed, @moved, @unexplained, @stale);

    for my $p (@$progs) {
        my $r = $res->{ $p->{key} };
        my $b = $rows->{ $p->{key} };
        if ($r->{verdict} eq 'same') { push @fixed, $p->{key} if $b; next }
        if (!$b) { push @new, sprintf("%s  %s%s  %s", $p->{key}, $r->{verdict},
                                      $r->{line} eq '-' ? '' : " line $r->{line}", $r->{complaint}); next }
        if ($b->{verdict} ne $r->{verdict}) {
            push @moved, "$p->{key}  $b->{verdict} -> $r->{verdict}";
        }
        elsif ($b->{line} ne $r->{line}) {
            push @moved, "$p->{key}  line $b->{line} -> $r->{line}";
        }
    }
    for my $k (sort keys %$rows) {
        push @stale, $k if !$have{$k} && !$IS_SUBSET;
        push @unexplained, $k if $rows->{$k}{cause} !~ /\S/;
    }

    my $exit = 0;
    if (@new) {
        print "\nNEW (", scalar @new, ") -- a regression, or a program added without its baseline row:\n";
        print "  $_\n" for @new;
        $exit = 1;
    }
    if (@fixed) {
        print "\nFIXED (", scalar @fixed, ") -- edit these rows OUT of $BASELINE, citing what fixed them:\n";
        print "  $_\n" for sort @fixed;
    }
    if (@moved) {
        print "\nMOVED (", scalar @moved, ") -- the first difference changed; re-attribute the row:\n";
        print "  $_\n" for sort @moved;
    }
    if (@unexplained) {
        print "\nUNEXPLAINED (", scalar @unexplained, ") -- a baseline row with no cause:\n";
        print "  $_\n" for @unexplained;
        $exit = 1;
    }
    if (@stale) {
        print "\nSTALE (", scalar @stale, ") -- a baseline row naming a program that does not exist:\n";
        print "  $_\n" for @stale;
        $exit = 1;
    }
    print "\nbuckets: NEW ", scalar @new, ", FIXED ", scalar @fixed, ", MOVED ", scalar @moved,
          ", UNEXPLAINED ", scalar @unexplained, ", STALE ", scalar @stale, "\n";
    return $exit;
}

# area/name <TAB> verdict <TAB> first-diff-line <TAB> cause
sub read_baseline {
    my ($file) = @_;
    return ({}, undef) if !defined $file || $file eq 'none';
    open my $fh, '<:raw', $file or return ({}, "everyday-smoke: cannot read baseline $file: $!\n");
    my %rows;
    my $n = 0;
    while (defined(my $l = <$fh>)) {
        $n++;
        next if $l =~ /^\s*#/ || $l !~ /\S/;
        chomp $l;
        my @f = split /\t/, $l, 4;
        return ({}, "everyday-smoke: $file line $n: expected 4 tab-separated fields\n") if @f < 3;
        $rows{ $f[0] } = { verdict => $f[1], line => $f[2], cause => defined $f[3] ? $f[3] : '' };
    }
    close $fh;
    return (\%rows, undef);
}

# =========================================================================
# provenance: which tree, which emission
# =========================================================================

# The sha of the tree being measured and whether it is dirty.  scratch/ is an
# agent's notebook, never an input to a run, so an untracked scratch/ does not
# make a tree dirty; anything else does.  Outside a checkout the answer is
# ('unknown', 0) — an installed tree has no git, and that is not a defect.
sub git_state {
    my $out = qx{git -C "$ROOT" rev-parse --short HEAD 2>/dev/null};
    return ('unknown', 0) if $? != 0 || $out !~ /(\S+)/;
    my $sha = $1;
    my @dirty = grep { /\S/ && !m{^\?\? scratch/} } split /\n/, qx{git -C "$ROOT" status --porcelain 2>/dev/null};
    return ($sha, scalar @dirty);
}

sub generation {
    my $t = slurp("$ROOT/cl/pcl-runtime.lisp");
    return 'unknown' if !defined $t;
    return $t =~ /\*pcl-cache-generation\*\s+"([^"]+)"/ ? $1 : 'unknown';
}

# One row per whole-corpus measurement: the trend the project steers by.  A
# SUBSET, a non-default corpus or a dirty tree is refused, because a row that
# does not identify what was measured is worse than no row.
sub record_history {
    my ($n, $m, $same, $tot, $is_subset, $sha, $dirty) = @_;
    my $why = $is_subset        ? "it measured a SUBSET ($n of $m), not the whole corpus"
            : !$DEFAULT_CORPUS  ? "it measured $CORPUS, not the default corpus"
            : $dirty            ? "the tree is dirty ($dirty change(s) besides scratch/); the sha would not identify what was measured"
            : $sha eq 'unknown' ? undef
            :                     undef;
    if (defined $why) { print "\n--record REFUSED: $why\n"; return }
    my @t = gmtime(time);
    my $stamp = sprintf("%04d-%02d-%02dT%02d:%02d:%02dZ", $t[5] + 1900, $t[4] + 1, @t[3, 2, 1, 0]);
    my $by = join ' ', map { "$_=" . ($same->{$_} // 0) . "/" . $tot->{$_} } sort keys %$tot;
    my $new = !-e $HISTORY;
    make_path(dirname($HISTORY));
    open my $fh, '>>:raw', $HISTORY or die "everyday-smoke: $HISTORY: $!\n";
    print $fh "# tools/everyday-smoke.pl --record: one row per whole-corpus run.\n",
              "# M grows as programs are added, so N alone is not the trend -- read N/M.\n",
              "# date\tsha\tgen\tsame\ttotal\tby-area\n" if $new;
    print $fh join("\t", $stamp, $sha, generation(), $n, $m, $by), "\n";
    close $fh;
    print "\n--record: appended to $HISTORY\n";
    return;
}

# =========================================================================
# perl: expectations
# =========================================================================

# Modules a program declares with `# needs:` that THIS perl does not have.
sub missing_needs {
    my ($p) = @_;
    my @missing;
    for my $m (@{ $p->{needs} }) {
        next if $m !~ /^[A-Za-z_][A-Za-z0-9_:]*$/;   # a header typo is not a module
        system("$^X -e 'require $m; 1' >/dev/null 2>&1") == 0 or push @missing, $m;
    }
    return @missing;
}

# Run PROGRAM under perl in a throwaway world; returns (stdout, status).
sub perl_run {
    my ($p, $tmp, $tag, $path) = @_;
    my $work = "$tmp/work-$tag";
    my $out  = "$tmp/$tag.out";
    my $err  = "$tmp/$tag.err";
    my ($rc, $to) = run_serial([ $^X, $path // $p->{path}, @{ $p->{args} } ], $p, $work, $out, $err,
                               $p->{timeout} || $opt{timeout});
    return (slurp($out), $rc, $to, $err);
}

# THE ADMISSION TEST.  Three runs: twice in place, once as a copy under a
# different absolute path with a different cwd.  Returns (stdout, status) or
# (undef, reason).
sub admit {
    my ($p) = @_;
    my $tmp = tempdir(CLEANUP => 1);
    my ($o1, $rc1, $to1) = perl_run($p, $tmp, 'a');
    return (undef, undef, "perl timed out") if $to1;
    if ($rc1 != 0 && !defined $p->{expect_rc}) {
        return (undef, undef, "perl exited $rc1 — a program perl rejects is an invalid probe "
                            . "(declare a deliberate exit with `# expect-rc: $rc1`)");
    }
    if (defined $p->{expect_rc} && $rc1 != $p->{expect_rc}) {
        return (undef, undef, "perl exited $rc1 but the program declares `# expect-rc: $p->{expect_rc}`");
    }
    my ($o2, $rc2) = perl_run($p, $tmp, 'b');
    return (undef, undef, "run 2 differs from run 1 at stdout line " . first_diff_line($o1, $o2)
                        . " (the answer depends on the RUN: the clock, \$\$, rand, or hash order)")
        if $o2 ne $o1;
    return (undef, undef, "run 2 exited $rc2, run 1 exited $rc1") if $rc2 != $rc1;

    # run 3: a copy at another absolute path, run from another cwd
    my $alt = "$tmp/alt";
    make_path($alt);
    copy($p->{path}, "$alt/$p->{name}.pl") or return (undef, undef, "cannot copy the program: $!");
    my ($o3, $rc3) = perl_run($p, $tmp, 'c', "$alt/$p->{name}.pl");
    return (undef, undef, "the COPY at another path differs from run 1 at stdout line "
                        . first_diff_line($o1, $o3)
                        . " (the answer depends on WHERE the program is: \$0, __FILE__, or a cwd-relative file)")
        if $o3 ne $o1;
    return (undef, undef, "the COPY exited $rc3, run 1 exited $rc1") if $rc3 != $rc1;
    return ($o1, $rc1, undef);
}

sub bless_expect {
    my ($progs) = @_;
    my $bad = 0;
    for my $p (@$progs) {
        my @missing = missing_needs($p);
        if (@missing) { printf("NEEDS   %-38s this perl lacks %s\n", $p->{key}, join(' ', @missing)); $bad = 1; next }
        my ($out, $rc, $why) = admit($p);
        if (!defined $out) { printf("REFUSED %-38s %s\n", $p->{key}, $why); $bad = 1; next }
        spew("$p->{base}.expect", $out);
        if ($rc) { spew("$p->{base}.rc", "$rc\n") } else { unlink "$p->{base}.rc" }
        printf("blessed %-38s %d bytes%s\n", $p->{key}, length $out, $rc ? ", rc $rc" : '');
    }
    print "\nperl: $^X ($^V)\n";
    return $bad;
}

# Re-derive every expectation and say where the checked-in one no longer
# matches.  DRIFT is never fixed by rewriting the expectation on the spot: it
# means the program or the perl changed, and which one it was matters.
sub verify_with_perl {
    my ($progs) = @_;
    my (@drift, @needs, @noexpect);
    for my $p (@$progs) {
        if (!-f "$p->{base}.expect") { push @noexpect, $p->{key}; next }
        my @missing = missing_needs($p);
        if (@missing) { push @needs, "$p->{key}  (this perl lacks " . join(' ', @missing) . ")"; next }
        my $tmp = tempdir(CLEANUP => 1);
        my ($out, $rc, $to) = perl_run($p, $tmp, 'v');
        my $want = slurp("$p->{base}.expect");
        if ($to)             { push @drift, "$p->{key}  perl timed out" }
        elsif ($out ne $want) { push @drift, "$p->{key}  stdout differs at line " . first_diff_line($want, $out) }
        elsif ($rc != expected_rc($p)) { push @drift, "$p->{key}  exit status $rc, expected " . expected_rc($p) }
    }
    print "DRIFT (", scalar @drift, ") -- the checked-in expectation is not what this perl prints:\n";
    print "  $_\n" for @drift;
    print "NEEDS (", scalar @needs, ") -- not verified, this perl lacks the module:\n";
    print "  $_\n" for @needs;
    if (@noexpect) { print "NO EXPECTATION (", scalar @noexpect, "):\n"; print "  $_\n" for @noexpect }
    printf("\nverified %d programs with %s (%s)\n", scalar(@$progs) - @needs - @noexpect, $^X, sprintf("%vd", $^V));
    return (@drift || @noexpect) ? 1 : 0;
}

# =========================================================================

sub _rstrip_slash { my ($p) = @_; $p =~ s{/+$}{}; return $p }

sub usage {
    my ($code) = @_;
    my $fh = $code ? *STDERR : *STDOUT;
    open my $me, '<', $0 or exit $code;
    my $started = 0;
    while (defined(my $l = <$me>)) {
        last if $l =~ /^use strict/;
        next if $l =~ /^#!/;
        $l =~ s/^# ?//;
        next if $l =~ /^(?:Copyright \(c\)|This is free software|same terms as the Perl|SPDX-License-Identifier)/;
        next if !$started && $l !~ /\S/;
        $started = 1;
        print $fh $l;
    }
    close $me;
    exit $code;
}
