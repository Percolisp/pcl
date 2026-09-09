#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# cpan-scoreboard.pl — run every t/*.t of one or more CPAN dists through PCL
# (via tools/run-dist-t.pl) and print a per-file + per-dist PASS/PARTIAL/FAIL
# scoreboard.  This is the "CPAN suites vs baselines" half of the R1 gate
# (task #25 / docs/cpan-release-plan.md phase 0): rerun after compiler changes
# and diff the counts against the recorded baseline (session log / memory).
#
# Usage:
#   tools/cpan-scoreboard.pl ~/.cpan/build/Try-Tiny-0.32-0 ...
#   tools/cpan-scoreboard.pl --no-dist-lib ~/.cpan/build/Scalar-List-Utils-1.70-0
#   tools/cpan-scoreboard.pl --jobs 8 --timeout 120 DIST...
#   tools/cpan-scoreboard.pl --tsv baselines/cpan-scoreboard.tsv DIST...
#   tools/cpan-scoreboard.pl --rows /tmp/board.rows.tsv DIST...
#   tools/cpan-scoreboard.pl --diff baselines/cpan-board14-fails.tsv /tmp/board.rows.tsv \
#        [--board-baseline baselines/cpan-board14-s474.tsv --board-current /tmp/board.tsv]
#
# --tsv writes a machine-diffable PER-FILE baseline: one sorted line per t-file,
#   dist <TAB> file <TAB> status <TAB> ok <TAB> notok <TAB> rc
#
# --rows writes the PER-ROW file (task #1502).  The per-file table alone is too
# coarse to be a regression gate in the OTHER direction too: a file can lose a
# row and gain a row and read unchanged, and no row carries a cause.  Shape —
# deliberately the sweep's baselines/fail-baseline.tsv shape, one column wider:
#
#   dist <TAB> t-file <TAB> num <TAB> description <TAB> got <TAB> expected [<TAB> cause]
#
# one line per FAILING assertion, plus one synthetic `*FILE*` row for a t-file
# that produced NO TAP rows at all — 50 of the 14-dist board's files are in
# that state (transpile failure, load crash, timeout, or a perl-side skip), and
# a row file that could not represent them would leave a third of the board
# uncountable.  The JOIN KEY is (dist, file, description), never the test
# number, which drifts whenever PCL's row count diverges from perl's; an
# unnamed row has the empty description and #1041's rule then makes one key
# stand for all of a file's unnamed rows.
#
# The blessed row baseline is baselines/cpan-board14-fails.tsv and its seventh
# column is the CAUSE (a task number, an `NS:<section>` of docs/not-supported.md,
# `PERL-SKIP` for a file real perl skips too, or UNEXPLAINED).  A live --rows
# file has six columns; causes are added BY EDIT, exactly like the sweep's.
#
# --diff compares two row files and prints NEW / FIXED / LOST and the CAUSES
# line, the way tools/sweep-diff.pl does for the perl-tests sweep.  LOST needs
# the two PER-FILE tables (a file that aborts earlier loses passing rows
# without adding failing ones); without them it says NOT CHECKED rather than
# printing nothing, because an unchecked gate must never look like a passed one.
#
# --no-dist-lib applies to every dist AFTER the flag (see run-dist-t.pl's
# caveat: XS-stubbed dists like Scalar-List-Utils must NOT put their lib/ on
# pl2cl's @INC or the dist copy shadows PCL's shim).
#
# TIMEOUT ALLOWANCES: baselines/cpan-board-timeouts.tsv, read through the
# shared tools/lib/PCLTimeouts.pm (the perl-suite runner's registry, second
# population).  The effective per-file timeout is max(registry, --timeout) and
# the allowances in effect are printed per run.  Without it a merely SLOW file
# reads as FAIL with zero rows and its passing rows vanish invisibly — which is
# exactly what Text-Balanced 05_extmul.t did (#1512).
#
# Classification per t-file (TAP counted by run-dist-t.pl --rows):
#   PASS    = at least one ok, zero not-ok, clean exit
#   PARTIAL = at least one ok, but not-ok rows or a non-zero exit (late crash)
#   FAIL    = zero ok (transpile failure, load crash, timeout, no TAP)

use strict;
use warnings;
use File::Basename qw(basename dirname);
use File::Temp qw(tempdir);
use Cwd qw(abs_path);
use FindBin;
use lib "$FindBin::RealBin/lib";
use PCLTimeouts ();   # the ONE reader of a per-file timeout-allowance registry

my $root = abs_path(dirname(abs_path($0)) . "/..");

# ─────────────────────────────────────────────────────── the row file: I/O ──
# Rows are keyed (dist, file, description) — see the header.  A blessed file
# has a seventh CAUSE column; a live one has six, and the reader treats the
# missing column as "no cause", never as an empty cause that would pass the
# CAUSES gate.
sub load_rows {
    my ($path) = @_;
    open my $fh, '<', $path or die "open $path: $!\n";
    my %rec;
    while (my $line = <$fh>) {
        chomp $line;
        next if !length $line || $line =~ /^#/;
        my ($dist, $file, $num, $desc, $got, $exp, $cause) = split /\t/, $line, 7;
        next unless defined $desc;
        $rec{"$dist\t$file\t$desc"} =
          { dist => $dist, file => $file, num => $num // '', desc => $desc,
            got => $got // '', expected => $exp // '', cause => $cause };
    }
    close $fh;
    return \%rec;
}

# The per-FILE table (--tsv shape), used by --diff for the LOST bucket.
sub load_board {
    my ($path) = @_;
    my %st;
    return \%st unless defined $path;
    open my $fh, '<', $path or die "open $path: $!\n";
    while (my $line = <$fh>) {
        chomp $line;
        next if !length $line || $line =~ /^#/;
        my ($dist, $file, $status, $ok, $notok, $rc) = split /\t/, $line;
        next unless defined $notok;
        $st{"$dist\t$file"} = { status => $status, ok => $ok, notok => $notok, rc => $rc };
    }
    close $fh;
    return \%st;
}

# ────────────────────────────────────────────────────────────── --diff mode ──
if (@ARGV && $ARGV[0] eq '--diff') {
    shift @ARGV;
    my ($board_base, $board_cur);
    my @pos;
    while (@ARGV) {
        my $a = shift @ARGV;
        if    ($a eq '--board-baseline') { $board_base = shift @ARGV }
        elsif ($a eq '--board-current')  { $board_cur  = shift @ARGV }
        else                             { push @pos, $a }
    }
    @pos == 2 or die "usage: $0 --diff <baseline-rows.tsv> <current-rows.tsv> "
                   . "[--board-baseline F --board-current F]\n";
    my $base = load_rows($pos[0]);
    my $cur  = load_rows($pos[1]);

    my @new   = sort grep { !$base->{$_} } keys %$cur;
    my @fixed = sort grep { !$cur->{$_}  } keys %$base;
    for my $k (@new) {
        my $r = $cur->{$k};
        printf "NEW    %s %s #%s %s\n         got %s / expected %s\n",
               $r->{dist}, $r->{file}, $r->{num}, $r->{desc}, $r->{got}, $r->{expected};
    }
    for my $k (@fixed) {
        my $r = $base->{$k};
        printf "FIXED  %s %s #%s %s\n", $r->{dist}, $r->{file}, $r->{num}, $r->{desc};
    }

    # LOST — baseline PASSING rows the current run did not produce.  Row files
    # hold failures only, so this is the per-FILE tables' question.
    my $lost = 0;
    if (defined $board_base && defined $board_cur) {
        my $b = load_board($board_base);
        my $c = load_board($board_cur);
        for my $k (sort keys %$b) {
            my $now = $c->{$k};
            if (!$now) { printf "LOST   %s — file not in the current board\n", join(' ', split /\t/, $k);
                         $lost += $b->{$k}{ok}; next }
            next unless $now->{ok} < $b->{$k}{ok};
            printf "LOST   %s — %d passing row(s) (%d -> %d)\n",
                   join(' ', split /\t/, $k), $b->{$k}{ok} - $now->{ok}, $b->{$k}{ok}, $now->{ok};
            $lost += $b->{$k}{ok} - $now->{ok};
        }
        my ($bok, $cok) = (0, 0);
        $bok += $_->{ok} for values %$b;
        $cok += $_->{ok} for values %$c;
        printf "TOTAL passing: baseline %d, current %d\n", $bok, $cok;
    } else {
        print "LOST: NOT CHECKED — pass --board-baseline/--board-current (the per-file .tsv tables)\n";
    }

    # A cause-less row is QUEUE, not baseline (#993, the sweep's rule).
    my $nocause = grep { !defined $_->{cause} || $_->{cause} !~ /\S/
                         || $_->{cause} =~ /^UNEXPLAINED/ } values %$base;
    printf "CAUSES: %d of %d blessed row(s) have no cause — a cause-less row is QUEUE, not baseline (#993)\n",
           $nocause, scalar(keys %$base);
    printf "\n%d NEW / %d FIXED / %d LOST\n", scalar(@new), scalar(@fixed), $lost;
    exit(@new || $lost ? 1 : 0);
}

# ────────────────────────────────────────────────────────────────── the run ──
my $jobs = 8;
my $timeout = 120;
my @dists;           # [dir, no_dist_lib]
my $no_dist_lib = 0;
my ($tsv, $rows_file);
my $timeouts_tsv = "$root/baselines/cpan-board-timeouts.tsv";
my @argv_copy = @ARGV;
while (@ARGV) {
  my $a = shift @ARGV;
  if    ($a eq '--jobs')        { $jobs = shift @ARGV }
  elsif ($a eq '--timeout')     { $timeout = shift @ARGV }
  elsif ($a eq '--tsv')         { $tsv = shift @ARGV }
  elsif ($a eq '--rows')        { $rows_file = shift @ARGV }
  elsif ($a eq '--timeouts')    { $timeouts_tsv = shift @ARGV }
  elsif ($a eq '--no-dist-lib') { $no_dist_lib = 1 }
  else                          { push @dists, [abs_path($a), $no_dist_lib] }
}
@dists or die "usage: $0 [--jobs N] [--timeout S] [--no-dist-lib] [--tsv F] [--rows F] <dist-dir>...\n";

my %file_timeout = %{ PCLTimeouts::read_timeouts($timeouts_tsv) };

# Work list: one entry per t-file.
my @work;
for my $d (@dists) {
  my ($dir, $ndl) = @$d;
  -d "$dir/t" or die "no t/ in $dir\n";
  for my $t (sort glob "$dir/t/*.t") {
    my $rel = basename($dir) . "/" . basename($t);
    push @work, { dist => $dir, ndl => $ndl, t => $t, rel => $rel,
                  to => PCLTimeouts::timeout_for(\%file_timeout, $rel, $timeout) };
  }
}
# The allowances in effect for THIS run, printed — a long-running file's
# allowance is never a silent property of a registry nobody reads.
for my $w (grep { $file_timeout{ $_->{rel} } } @work) {
  printf STDERR "timeout allowance: %-40s %4ds  (%s)\n",
    $w->{rel}, $w->{to}, $file_timeout{ $w->{rel} }{cause};
}

# Row payloads can run to hundreds of lines per file, so they go to a per-work
# temp file and only the one-line verdict crosses the pipe: a >PIPE_BUF write
# from a forked child is not atomic and would interleave with a sibling's.
my $tmp = tempdir("cpan-board-XXXXXX", TMPDIR => 1, CLEANUP => 1);
$work[$_]{idx} = $_ for 0 .. $#work;

# Fork pool: each child runs one t-file, writes one result line to a pipe.
my %kids;
my @queue = @work;
pipe(my $rd, my $wr) or die "pipe: $!";
my @rows;

sub run_one {
  my ($w) = @_;
  my @cmd = ("timeout", $w->{to}, "$root/tools/run-dist-t.pl", "--rows");
  push @cmd, "--no-dist-lib" if $w->{ndl};
  push @cmd, $w->{dist}, $w->{t};
  my $out = do {
    open(my $p, '-|', @cmd) or return undef;
    local $/; my $o = <$p> // ''; close $p; $o;
  };
  my $rc = $? >> 8;
  my ($ok, $notok) = $out =~ /pass=(\d+) fail=(\d+)/ ? ($1, $2) : (0, 0);
  my $class = $ok == 0             ? 'FAIL'
            : ($notok || $rc != 0) ? 'PARTIAL'
            :                        'PASS';
  $class = 'FAIL' if $rc == 124;   # timeout
  my ($plan, $skip) = ('', '');
  ($plan, $skip) = ($1, $2) if $out =~ /^PLAN\t([^\t]*)\t([^\n]*)$/m;
  # The SBCL child's wait status, reported by run-dist-t.pl --rows.  A nonzero
  # SIGNAL means the run was KILLED (the OOM killer on a loaded box), and a
  # killed run's empty TAP must never be published as a verdict.
  my $sig = ($out =~ /^SBCL\t\d+\t(\d+)$/m) ? $1 : 0;
  open my $rf, '>', "$tmp/$w->{idx}.rows" or return undef;
  print $rf "$_\n" for ($out =~ /^ROW\t([^\n]*)$/mg);
  close $rf;
  return [$w->{dist}, basename($w->{t}), $class, $ok, $notok, $rc,
          $w->{idx}, $plan, $skip, $sig];
}

sub spawn_one {
  my ($w) = @_;
  my $pid = fork() // die "fork: $!";
  if ($pid == 0) {
    close $rd;
    my $r = run_one($w) or exit 9;
    print $wr join("\t", @$r), "\n";
    exit 0;
  }
  $kids{$pid} = 1;
}

close_wr_when_done: {
  spawn_one(shift @queue) while @queue && keys %kids < $jobs;
  close $wr if !@queue && !%kids;
}
# Reader loop: reap + refill as lines arrive.
my $expected = @work;
while (@rows < $expected) {
  my $line = <$rd>;
  defined $line or last;
  chomp $line;
  push @rows, [split /\t/, $line, 10];
  my $done = wait();
  delete $kids{$done} if $done > 0;
  spawn_one(shift @queue) if @queue;
}
close $rd;
1 while wait() > 0;

# A SIGNAL-KILLED run is not a verdict — re-run it SERIALLY at the end of the
# queue, once, and take the serial reading (the sweep's #176 retry, narrowed to
# the one condition that is provably not the file's own doing).  Normally this
# list is empty and costs nothing; on a loaded box it is the difference between
# a blessed row file and load noise.
{
  my %by_idx = map { $_->{idx} => $_ } @work;
  for my $r (@rows) {
    next unless $r->[9];
    printf STDERR "retry: %s %s — SBCL killed by signal %d (verdict was %s %d/%d); re-running serially\n",
                  basename($r->[0]), $r->[1], $r->[9], $r->[2], $r->[3], $r->[4];
    my $again = run_one($by_idx{ $r->[6] }) or next;
    printf STDERR "retry: %s %s — serial verdict %s %d/%d%s\n",
                  basename($r->[0]), $r->[1], $again->[2], $again->[3], $again->[4],
                  $again->[9] ? " (killed AGAIN, signal $again->[9])" : '';
    @$r = @$again unless $again->[9];
  }
}

# Report.
my %by_dist;
for my $r (sort { $a->[0] cmp $b->[0] || $a->[1] cmp $b->[1] } @rows) {
  push @{ $by_dist{ $r->[0] } }, $r;
}
my $grand_bad = 0;
for my $dist (sort keys %by_dist) {
  my %n = (PASS => 0, PARTIAL => 0, FAIL => 0);
  print "== ", basename($dist), "\n";
  for my $r (@{ $by_dist{$dist} }) {
    my (undef, $file, $class, $ok, $notok, $rc) = @$r;
    $n{$class}++;
    printf "  %-8s %-28s ok=%-4d notok=%-4d rc=%d\n",
           $class, $file, $ok, $notok, $rc;
  }
  my $total = @{ $by_dist{$dist} };
  printf "  -- %d PASS / %d PARTIAL / %d FAIL of %d\n",
         $n{PASS}, $n{PARTIAL}, $n{FAIL}, $total;
  $grand_bad += $n{FAIL};
}

# Machine-diffable baseline: dist basename (NOT the path — it carries a build
# dir), file, status, ok, notok, rc.  Sorted, so diff(1) is the whole gate.
if (defined $tsv) {
  open my $fh, '>', $tsv or die "open $tsv: $!\n";
  for my $dist (sort keys %by_dist) {
    for my $r (@{ $by_dist{$dist} }) {
      my (undef, $file, $class, $ok, $notok, $rc) = @$r;
      print $fh join("\t", basename($dist), $file, $class, $ok, $notok, $rc), "\n";
    }
  }
  close $fh;
  print "\nwrote $tsv\n";
}

# The ROW file (task #1502).  Six columns; the blessed baseline's seventh is
# the cause, added by edit.
if (defined $rows_file) {
  my $sha = `git -C \Q$root\E rev-parse --short HEAD 2>/dev/null`;
  chomp $sha;
  my @t = localtime;
  open my $fh, '>', $rows_file or die "open $rows_file: $!\n";
  printf $fh "# cpan-board ROW-level failures — one line per FAILING assertion, plus one\n"
           . "#   `*FILE*` row per t-file that produced no TAP rows at all.\n"
           . "# Columns: dist, t-file, num, description, got, expected [, cause]\n"
           . "# Join key: (dist, t-file, description) — never the number (#1041).\n"
           . "# Written by tools/cpan-scoreboard.pl --rows, tree %s, %04d-%02d-%02d, perl %vd\n"
           . "# Command: %s\n",
           (length $sha ? $sha : 'unknown'), $t[5]+1900, $t[4]+1, $t[3], $^V,
           join(' ', $0, @argv_copy);
  my $nrows = 0;
  for my $r (sort { $a->[0] cmp $b->[0] || $a->[1] cmp $b->[1] } @rows) {
    my ($dist, $file, $class, $ok, $notok, $rc, $idx, $plan, $skip) = @$r;
    my @out;
    if (open my $rf, '<', "$tmp/$idx.rows") {
      while (my $l = <$rf>) { chomp $l; push @out, $l }
      close $rf;
    }
    if ($ok == 0 && $notok == 0) {
      # No TAP at all: the file's story is "it did not run", and without a
      # synthetic row it would be invisible in a file of failing assertions.
      my $what = length($skip // '') ? "1..0 $skip (rc=$rc)" : "no TAP rows (rc=$rc)";
      unshift @out, join("\t", 0, '*FILE*', $what,
                         (defined $plan && length $plan) ? "$plan row(s)" : 'TAP rows');
    }
    for my $l (@out) {
      my ($num, $desc, $got, $exp, $dir) = split /\t/, $l, 5;
      $desc = '' unless defined $desc;
      $desc .= " # $dir" if defined $dir && length $dir;   # TODO/SKIP directive
      print $fh join("\t", basename($dist), $file, $num, $desc,
                           $got // '', $exp // ''), "\n";
      $nrows++;
    }
  }
  close $fh;
  print "wrote $rows_file ($nrows row(s))\n";
}
exit 0;
