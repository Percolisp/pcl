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
# line, the way tools/sweep-diff.pl does for the perl-tests sweep -- and its
# `?` UNVERIFIED bucket (#4382): a blessed row in a file the current run
# stopped BEFORE reaching (its *PLAN* row says "ran M", or it has a *FILE*
# row, or --board-current does not list it) is never FIXED.  A *PLAN* row
# whose shortfall text moved prints a MISMATCH *PLAN* line.  LOST needs
# the two PER-FILE tables (a file that aborts earlier loses passing rows
# without adding failing ones); without them it says NOT CHECKED rather than
# printing nothing, because an unchecked gate must never look like a passed one.
#
# --no-dist-lib applies to every dist AFTER the flag (see run-dist-t.pl's
# caveat: XS-stubbed dists like Scalar-List-Utils must NOT put their lib/ on
# pl2cl's @INC or the dist copy shadows PCL's shim).
#
# --dist-inc=DIR,DIR puts extra directories on @INC for the NEXT dist only
# (tools/run-dist-t.pl --inc): a dependency's lib/ fetched under ~/.cpan/build
# and never installed, or the dist's own inc/ its `make test` adds (s518e,
# Type-Tiny + Exporter-Tiny).  t-files are found RECURSIVELY; a row's file name
# is its path under t/.
#
# TIMEOUT ALLOWANCES: baselines/cpan-board-timeouts.tsv, read through the
# shared tools/lib/PCLTimeouts.pm (the perl-suite runner's registry, second
# population).  The effective per-file timeout is max(registry, --timeout) and
# the allowances in effect are printed per run.  Without it a merely SLOW file
# reads as FAIL with zero rows and its passing rows vanish invisibly — which is
# exactly what Text-Balanced 05_extmul.t did (#1512).
#
# Classification per t-file (TAP counted by run-dist-t.pl --rows):
#   PASS    = at least one ok, zero not-ok, the PLAN MET and SBCL's exit 0
#   PARTIAL = at least one ok, but not-ok rows, a SHORTFALL, or a non-zero exit
#   FAIL    = zero ok (transpile failure, load crash, timeout, no TAP)
#   SKIP    = PCL printed `1..0 # SKIP` AND the perl oracle skips the file too
#             (#4160 / s531b): not a failure, its own column; PCL skipping a
#             file perl RUNS stays FAIL.  `*FILE*` rows are still written for
#             a SKIP file (the blessed PERL-SKIP row is its record).
#
# THE PERL ORACLE (s531b, Fable ruling s531 #4): every t-file is also run
# under real perl, from the dist root, with the same @INC rule
# (tools/run-dist-t.pl --oracle-only, CACHED under ~/.pcl-cache/oracle/, so it
# costs a perl run per file only the first time; --refresh-oracle re-measures,
# --no-oracle turns it off).  It is its own timed call, so it never eats the
# PCL run's timeout.  With --causes <blessed rows> the run also prints, per
# dist and in total, the not-ok rows split by their blessed cause -- ours /
# ruled / perl's / harness (PCLCauses::class_group) -- and the assertions
# hidden behind whole-file rows; and a MISMATCH line for every blessed
# PERL-SKIP / PERL-FAILS claim perl's own run contradicts (a report, never a
# gate: perl's answer depends on the box's installed modules).
#
# THE SHORTFALL (#3241): a file whose program stops before its plan is met is
# not a PASS, whatever rows it printed first.  Before #3241 the verdict read
# run-dist-t.pl's OWN exit (0 whenever it ran) and never compared the plan, so
# a file that died after row 1 of 25,119 read PASS 1/0.  The rule is prove's:
# the rows that ran (ok + not ok) must equal the plan (`1..N` up front or
# `done_testing`'s trailer) and SBCL must exit 0.  (#3651: or exit with the TAP layer's own
# VERDICT, Test::Builder's status for the rows -- the failures; run-dist-t.pl
# --rows says which in the SBCL line's fourth field.)  A file that ran rows and
# misses either condition gets ONE synthetic row, so the row gate can hold it
# with a cause:
#
#   dist <TAB> t-file <TAB> 0 <TAB> *PLAN* <TAB> ran M of N, rc R <TAB> N row(s)
#
# (`of no plan` / `a plan` when no plan line was printed; R = SBCL's exit, or
# 124 for a timeout).  A file that ran NO rows keeps its `*FILE*` row instead.

use strict;
use warnings;
use File::Basename qw(basename dirname);
use File::Temp qw(tempdir);
use Cwd qw(abs_path);
use FindBin;
use lib "$FindBin::RealBin/lib";
use PCLTimeouts ();   # the ONE reader of a per-file timeout-allowance registry
use PCLCauses ();     # the ONE reading of the CAUSE column (#993 I3)

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
    my $bcur = defined $board_cur ? load_board($board_cur) : undef;

    my @new   = sort grep { !$base->{$_} } keys %$cur;
    my ($fixed, $unver) = split_fixed($base, $cur, $bcur);
    for my $k (@new) {
        my $r = $cur->{$k};
        printf "NEW    %s %s #%s %s\n         got %s / expected %s\n",
               $r->{dist}, $r->{file}, $r->{num}, $r->{desc}, $r->{got}, $r->{expected};
    }
    for my $k (@$fixed) {
        my $r = $base->{$k};
        printf "FIXED  %s %s #%s %s\n", $r->{dist}, $r->{file}, $r->{num}, $r->{desc};
    }
    my $nunver = 0;
    for my $f (sort keys %$unver) {
        my ($n, $why) = (scalar @{ $unver->{$f}{rows} }, $unver->{$f}{why});
        $nunver += $n;
        printf "?      %s %s: %d blessed row(s) not reached (%s) — not verified, not fixed\n",
               (split /\t/, $f), $n, $why;
    }
    # A *PLAN* row in both files whose shortfall text MOVED (ran 8 -> ran 1):
    # neither NEW nor FIXED, and it is the line that says a file stops earlier.
    for my $k (sort keys %$base) {
        my ($b, $c) = ($base->{$k}, $cur->{$k});
        next unless $c && $b->{desc} eq '*PLAN*' && $c->{got} ne $b->{got};
        printf "MISMATCH *PLAN* %s %s: blessed '%s', now '%s'\n",
               $b->{dist}, $b->{file}, $b->{got}, $c->{got};
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
    # ONE reading, three populations (tools/lib/PCLCauses.pm).
    print PCLCauses::causes_line([ map { $_->{cause} } values %$base ], $pos[0]);
    printf "\n%d NEW / %d FIXED / %d UNVERIFIED / %d LOST\n",
           scalar(@new), scalar(@$fixed), $nunver, $lost;
    exit(@new || $lost ? 1 : 0);
}

# A blessed failing row the current run did not produce is FIXED only if the
# file got that far (#4382, the sweep's `?` bucket): a file that STOPPED short
# -- the current run has its *FILE* row (no rows at all) or its *PLAN* row
# ("ran M of N, rc R", the shortfall text) -- never reached a blessed row
# numbered above M, and with --board-current a file missing from the current
# per-file table was not run at all.  Those rows are UNVERIFIED.  Returns
# ([FIXED keys], {"dist\tfile" => {why, rows => [keys]}}).
sub split_fixed {
    my ($base, $cur, $bcur) = @_;
    my %ran;
    for my $r (values %$cur) {
        my $f = "$r->{dist}\t$r->{file}";
        if ($r->{desc} eq '*FILE*') { $ran{$f} = 0 }
        elsif ($r->{desc} eq '*PLAN*' && $r->{got} =~ /^ran (\d+) of /) { $ran{$f} //= $1 }
    }
    my (@fixed, %unver);
    for my $k (sort grep { !$cur->{$_} } keys %$base) {
        my $r = $base->{$k};
        my $f = "$r->{dist}\t$r->{file}";
        my $why;
        if ($bcur && !$bcur->{$f}) { $why = 'file not in the current board' }
        elsif (defined $ran{$f}
               && ($ran{$f} == 0 || ($r->{num} =~ /^\d+\z/ && $r->{num} > $ran{$f}))) {
            $why = "the file ran $ran{$f} row(s)";
        }
        if (defined $why) { $unver{$f}{why} //= $why; push @{ $unver{$f}{rows} }, $k }
        else              { push @fixed, $k }
    }
    return (\@fixed, \%unver);
}

# ────────────────────────────────────────────────────────────────── the run ──
my $jobs = 8;
my $timeout = 120;
my @dists;           # [dir, no_dist_lib, [extra @INC dirs], serial]
my @pending_inc;     # --dist-inc=DIR,DIR: extra @INC for the NEXT dist only
my $pending_serial = 0;   # --dist-serial[=REGEX]: the NEXT dist's files (those whose t-name matches REGEX) run one at a time, in order (#4381)
my $only_rx;         # --only=REGEX: run only the t-files whose DIST/NAME matches
my $no_dist_lib = 0;
my ($tsv, $rows_file);
my $timeouts_tsv = "$root/baselines/cpan-board-timeouts.tsv";
# THE PERL ORACLE (s531b, Fable ruling s531 #4): on by default; cached by
# tools/run-dist-t.pl, so it costs a perl run per t-file only the first time.
my ($oracle, $refresh_oracle, $oracle_perl, $oracle_to) = (1, 0, undef, 120);
my $causes_file;     # --causes: the blessed row baseline, for the split + MISMATCH
my @argv_copy = @ARGV;
while (@ARGV) {
  my $a = shift @ARGV;
  if    ($a eq '--no-oracle')      { $oracle = 0 }
  elsif ($a eq '--refresh-oracle') { $refresh_oracle = 1 }
  elsif ($a eq '--oracle-perl')    { $oracle_perl = shift @ARGV }
  elsif ($a eq '--oracle-timeout') { $oracle_to = shift @ARGV }
  elsif ($a eq '--causes')         { $causes_file = shift @ARGV }
  elsif ($a eq '--jobs')        { $jobs = shift @ARGV }
  elsif ($a eq '--timeout')     { $timeout = shift @ARGV }
  elsif ($a eq '--tsv')         { $tsv = shift @ARGV }
  elsif ($a eq '--rows')        { $rows_file = shift @ARGV }
  elsif ($a eq '--timeouts')    { $timeouts_tsv = shift @ARGV }
  elsif ($a eq '--no-dist-lib') { $no_dist_lib = 1 }
  elsif ($a =~ /^--dist-inc=(.+)$/) { @pending_inc = split /,/, $1 }
  elsif ($a eq '--dist-serial')     { $pending_serial = 1 }
  elsif ($a =~ /^--dist-serial=(.+)$/) { $pending_serial = qr/$1/ }
  elsif ($a =~ /^--only=(.+)$/)     { $only_rx = qr/$1/ }   # a census in CHUNKS (s518e)
  else { push @dists, [abs_path($a), $no_dist_lib, [@pending_inc], $pending_serial];
         @pending_inc = (); $pending_serial = 0 }
}
@dists or die "usage: $0 [--jobs N] [--timeout S] [--no-dist-lib] [--tsv F] [--rows F] <dist-dir>...\n";

my %file_timeout = %{ PCLTimeouts::read_timeouts($timeouts_tsv) };

# Work list: one entry per t-file.
my @work;
for my $d (@dists) {
  my ($dir, $ndl, $inc, $serial) = @$d;
  -d "$dir/t" or die "no t/ in $dir\n";
  for my $t (t_files("$dir/t")) {
    my $rel = basename($dir) . "/" . t_name($t, $dir);
    next if $only_rx && $rel !~ $only_rx;
    push @work, { dist => $dir, ndl => $ndl, inc => $inc, t => $t, rel => $rel,
                  serial => (ref $serial ? scalar(t_name($t, $dir) =~ $serial) : $serial) ? 1 : 0,
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
my %kids;        # pid -> the work item it runs
my %dist_busy;   # a SERIAL dist -> how many of its files run now (0 or 1)
# A SERIAL dist's chain is the longest single thread of work, so it STARTS
# first (its items lead the queue, in their own order) and the other workers
# take the other dists beside it -- at the end of the queue it would run alone.
my @queue = ((grep { $_->{serial} } @work), (grep { !$_->{serial} } @work));
pipe(my $rd, my $wr) or die "pipe: $!";
my @rows;

# The oracle is its OWN timed call, before the PCL run: a slow or hanging perl
# must never eat the PCL run's timeout allowance, or verdicts would move for an
# instrument reason.  Its PERLROW lines go to a per-work temp file, like ROW.
sub run_oracle {
  my ($w) = @_;
  my @o = (undef) x 6;   # perl plan, perl skip, perl ok, perl notok, how, seconds
  return @o unless $oracle;
  my @cmd = ("timeout", "-k", 5, $oracle_to + 15, "$root/tools/run-dist-t.pl", "--oracle-only",
             "--oracle-timeout", $oracle_to);
  push @cmd, "--refresh-oracle" if $refresh_oracle;
  push @cmd, "--oracle-perl", $oracle_perl if defined $oracle_perl;
  push @cmd, "--no-dist-lib" if $w->{ndl};
  push @cmd, map { ("--inc", $_) } @{ $w->{inc} || [] };
  push @cmd, $w->{dist}, $w->{t};
  my $out = do {
    open(my $p, '-|', @cmd) or return @o;
    local $/; my $o = <$p> // ''; close $p; $o;
  };
  @o[4, 5] = $out =~ /^ORACLE\t(\w+)\t([\d.]+)$/m ? ($1, $2) : ('timeout', $oracle_to);
  @o[0 .. 3] = ($1, $2, $3, $4) if $out =~ /^PERL\t([^\t]*)\t([^\t]*)\t(\d+)\t(\d+)\t/m;
  if (open my $pf, '>', "$tmp/$w->{idx}.perl") {
    print $pf "$_\n" for ($out =~ /^PERLROW\t([^\n]*)$/mg);
    close $pf;
  }
  return @o;
}

sub run_one {
  my ($w) = @_;
  my @perl = run_oracle($w);
  my @cmd = ("timeout", "-k", 5, $w->{to}, "$root/tools/run-dist-t.pl", "--rows");
  push @cmd, "--no-dist-lib" if $w->{ndl};
  push @cmd, map { ("--inc", $_) } @{ $w->{inc} || [] };
  push @cmd, $w->{dist}, $w->{t};
  my $out = do {
    open(my $p, '-|', @cmd) or return undef;
    local $/; my $o = <$p> // ''; close $p; $o;
  };
  my $rc = $? >> 8;
  my ($ok, $notok) = $out =~ /pass=(\d+) fail=(\d+)/ ? ($1, $2) : (0, 0);
  my ($plan, $skip) = ('', '');
  ($plan, $skip) = ($1, $2) if $out =~ /^PLAN\t([^\t]*)\t([^\n]*)$/m;
  # The SBCL child's exit and wait status, reported by run-dist-t.pl --rows.
  # A nonzero SIGNAL means the run was KILLED (the OOM killer on a loaded box),
  # and a killed run's empty TAP must never be published as a verdict.
  # The fourth field (#3651) says whether that exit is the TAP layer's VERDICT
  # (Test::Builder's status: the failures, or 255 for a missed plan) -- a file
  # that runs its whole plan with failures exits F and is PARTIAL by its rows,
  # not short.
  my ($sbcl_rc, $sig, $verdict) =
    ($out =~ /^SBCL\t(\d+)\t(\d+)(?:\t(\d))?$/m) ? ($1, $2, $3 // 0) : (undef, 0, 0);
  my $short = shortfall($ok + $notok, $plan, $skip,
                        $rc == 124 ? 124 : ($sbcl_rc // $rc), $rc != 124 && $verdict);
  my $class = $ok == 0                         ? 'FAIL'
            : ($notok || $rc != 0 || length $short) ? 'PARTIAL'
            :                                    'PASS';
  # SKIP (#4160): a file perl skips TOO is not a failure -- and only then:
  # PCL's skip_all alone is FAIL, because PCL skipping a file perl runs is a
  # divergence (a module PCL cannot load, a feature probe answering wrong).
  $class = 'SKIP' if $class eq 'FAIL' && $ok + $notok == 0 && length($skip)
                     && defined $perl[1] && length $perl[1];
  $class = 'FAIL' if $rc == 124;   # timeout
  open my $rf, '>', "$tmp/$w->{idx}.rows" or return undef;
  print $rf "$_\n" for ($out =~ /^ROW\t([^\n]*)$/mg);
  close $rf;
  return [$w->{dist}, t_name($w->{t}, $w->{dist}), $class, $ok, $notok, $rc,
          $w->{idx}, $plan, $skip, $sig, $short, map { $_ // '' } @perl];
}
use constant NFIELDS => 17;   # the result line's fields (11 + the oracle's 6)

# A dist's t-files, RECURSIVELY and sorted (Type-Tiny keeps 371 of its 375
# in subdirectories, s518e), and the NAME a row carries: the path under t/
# (`20-modules/Type-Tiny/basic.t`), so two `basic.t` in different directories
# are two keys.  For a flat t/ both are what they always were.
sub t_files {
  my ($tdir) = @_;
  my @out;
  my @todo = ($tdir);
  while (defined(my $d = shift @todo)) {
    opendir(my $dh, $d) or next;
    for my $e (sort grep { !/^\./ } readdir $dh) {
      my $p = "$d/$e";
      if (-d $p) { push @todo, $p }
      elsif ($e =~ /\.t\z/ && -f $p) { push @out, $p }
    }
  }
  return sort @out;
}
sub t_name {
  # The path under the DIST's t/ -- never under the first `/t/` of the whole
  # path: tools/t/fixture-dist/t/x.t is x.t (the fixture dist lives under tools/t).
  my ($t, $dist) = @_;
  $t =~ s{^\Q$dist\E/t/}{} or $t =~ s{^.*?/t/}{};
  return $t;
}

# The shortfall text for a file that RAN rows (see the header, #3241): '' when
# the plan is met and the exit is clean, else "ran M of N, rc R".
sub shortfall {
  my ($ran, $plan, $skip, $exit, $verdict) = @_;
  return '' if $ran == 0;                       # the *FILE* row's case
  my $planned = (defined $plan && length $plan) ? $plan : undef;
  return '' if defined $planned && $ran == $planned && (!$exit || $verdict);
  return '' if length($skip // '');             # 1..0 # SKIP after rows: not a plan
  return sprintf 'ran %d of %s, rc %d', $ran,
                 defined $planned ? $planned : 'no plan', $exit // 0;
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
  $kids{$pid} = $w;
  $dist_busy{ $w->{dist} }++ if $w->{serial};
}

# A SERIAL dist (--dist-serial, #4381) runs one file at a time, in its sorted
# order (with --dist-serial=REGEX only the files whose t-name matches: the
# dependent family, so the rest of the dist keeps its parallel workers):
# its next serial file is not READY while one of its files runs.  The other
# workers take other dists' files meanwhile -- the queue is scanned for the
# first ready item, so a serial dist never holds the pool up.
sub next_ready {
  for my $i (0 .. $#queue) {
    next if $queue[$i]{serial} && $dist_busy{ $queue[$i]{dist} };
    return splice @queue, $i, 1;
  }
  return undef;
}

sub spawn_ready {
  while (@queue && keys %kids < $jobs) {
    my $w = next_ready() // last;
    spawn_one($w);
  }
}

close_wr_when_done: {
  spawn_ready();
  close $wr if !@queue && !%kids;
}
# Reader loop: reap + refill as lines arrive.
my $expected = @work;
while (@rows < $expected) {
  my $line = <$rd>;
  defined $line or last;
  chomp $line;
  push @rows, [split /\t/, $line, NFIELDS];
  my $done = wait();
  if ($done > 0 && (my $w = delete $kids{$done})) {
    $dist_busy{ $w->{dist} }-- if $w->{serial};
  }
  spawn_ready();
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


# ── the rows of one t-file, as the row file writes them ────────────────────
# A description is the JOIN KEY, so it is built in ONE place for PCL's rows,
# the blessed baseline's and perl's (the oracle's PERLROW lines):
#   * a TODO/SKIP directive is kept on the description (`desc # TODO why`);
#   * a temp-file name changes every run, so a random `/tmp/<name>` component
#     is written as `/tmp/XXXX` (RULED s519 #2, DECIDED ## s519; Path-Tiny
#     filesystem.t `Read contents of /tmp/MuB2nxVyim correctly`).
sub row_desc {
  my ($desc, $dir) = @_;
  $desc = '' unless defined $desc;
  $desc .= " # $dir" if defined $dir && length $dir;
  $desc =~ s{/tmp/[A-Za-z0-9_]{6,}(?![A-Za-z0-9_.-])}{/tmp/XXXX}g;
  return $desc;
}

# [num, desc, got, expected] per row of a result, the synthetic `*FILE*` /
# `*PLAN*` row first -- what --rows writes and what the split counts.
sub file_rows {
  my ($r) = @_;
  my (undef, undef, undef, $ok, $notok, $rc, $idx, $plan, $skip, undef, $short) = @$r;
  my @out;
  if (open my $rf, '<', "$tmp/$idx.rows") {
    while (my $l = <$rf>) { chomp $l; push @out, [ split /\t/, $l, 5 ] }
    close $rf;
  }
  if ($ok == 0 && $notok == 0) {
    # No TAP at all: the file's story is "it did not run", and without a
    # synthetic row it would be invisible in a file of failing assertions.
    # A SKIP file keeps its row too: the blessed PERL-SKIP row is its record,
    # and dropping the row would read as FIXED in every --diff.
    my $what = length($skip // '') ? "1..0 $skip (rc=$rc)" : "no TAP rows (rc=$rc)";
    unshift @out, [0, '*FILE*', $what, (defined $plan && length $plan) ? "$plan row(s)" : 'TAP rows'];
  } elsif (length($short // '')) {
    # It ran rows and stopped short of its plan, or exited non-zero (#3241).
    unshift @out, [0, '*PLAN*', $short, (defined $plan && length $plan) ? "$plan row(s)" : 'a plan'];
  }
  return map { [ $_->[0], row_desc($_->[1], $_->[4]), $_->[2] // '', $_->[3] // '' ] } @out;
}

# perl's NOT-OK rows for a result (the oracle's PERLROW lines), keyed by the
# row description exactly as PCL's rows are.
sub perl_notok {
  my ($r) = @_;
  my %d;
  if (open my $pf, '<', "$tmp/$r->[6].perl") {
    while (my $l = <$pf>) { chomp $l; my (undef, $desc, $dir) = split /\t/, $l, 3; $d{ row_desc($desc, $dir) } = 1 }
    close $pf;
  }
  return \%d;
}

# ── the blessed causes (--causes): the split and the MISMATCH check ────────
my %cause;           # "dist\tfile\tdesc" -> cause
if (defined $causes_file) {
  my $b = load_rows($causes_file);
  $cause{$_} = $b->{$_}{cause} for keys %$b;
}

# Report.
my %by_dist;
for my $r (sort { $a->[0] cmp $b->[0] || $a->[1] cmp $b->[1] } @rows) {
  push @{ $by_dist{ $r->[0] } }, $r;
}
my @VERDICTS = qw(PASS PARTIAL FAIL SKIP);
my @SPLIT = ('ours', 'ruled', "perl's", 'harness', 'unread', 'unblessed');
my (%grand, %gsplit, %ghid);
my $grand_bad = 0;
my @mismatch;

# One file's contribution to the split: every FAILING assertion row by the
# group of its blessed cause (PCLCauses::class_group -- ours / ruled / perl's /
# harness; `unblessed` = no blessed row), and every whole-file row (`*FILE*`,
# `*PLAN*`) with the assertions it hides -- `ran M of N` hides N - M, a file
# that printed nothing hides perl's count (the oracle), a SKIP file hides none.
sub split_file {
  my ($r, $split, $hid) = @_;
  my ($dist, $file, $class) = @$r;
  my $base = basename($dist);
  for my $row (file_rows($r)) {
    my $c = $cause{"$base\t$file\t$row->[1]"};
    my $g = defined $c ? PCLCauses::class_group(PCLCauses::cause_class($c)) : 'unblessed';
    if ($row->[1] eq '*FILE*' || $row->[1] eq '*PLAN*') {
      next if $class eq 'SKIP';
      my $n = 0;
      if ($row->[2] =~ /^ran (\d+) of (\d+)/) { $n = $2 - $1 }
      elsif ($row->[1] eq '*FILE*' && length($r->[13] // '')) { $n = $r->[13] + $r->[14] }
      $hid->{files}++; $hid->{rows} += $n > 0 ? $n : 0;
      $hid->{ours}++ if $g eq 'ours';
      next;
    }
    $split->{$g}++;
  }
}

# MISMATCH: a blessed PERL-SKIP / PERL-FAILS claim the oracle contradicts.  A
# report line of the board run, never a gate: perl's answer depends on the
# modules installed on the box (ruling 4).  A blessed row of another class
# whose description perl ALSO fails is printed as a PERL-FAILS candidate (c).
sub mismatch_file {
  my ($r) = @_;
  my ($dist, $file) = @$r;
  return unless %cause && length($r->[15] // '') && $r->[15] ne 'timeout';
  my $base = basename($dist);
  my $perl_ran = ($r->[13] || 0) + ($r->[14] || 0);
  my $pn = perl_notok($r);
  for my $k (grep { index($_, "$base\t$file\t") == 0 } keys %cause) {
    my $cl = PCLCauses::cause_class($cause{$k});
    my $desc = (split /\t/, $k, 3)[2];
    if ($cl eq 'perl-skip' && $perl_ran) {
      push @mismatch, "MISMATCH PERL-SKIP  $base $file: perl ran $perl_ran row(s) (ok $r->[13], not ok $r->[14])";
    } elsif ($cl eq 'perl-fails' && !$pn->{$desc}) {
      push @mismatch, "MISMATCH PERL-FAILS $base $file \"$desc\": perl's row is ok";
    } elsif ($cl ne 'perl-fails' && $cl ne 'perl-skip' && $pn->{$desc}) {
      push @mismatch, "PERL-FAILS CANDIDATE $base $file \"$desc\": perl fails it too (blessed as $cl)";
    }
  }
}

sub split_text {
  my ($s, $h) = @_;
  my $t = join(' / ', map { sprintf '%d %s', $s->{$_} // 0, $_ } grep { $_ ne 'unread' || $s->{$_} } @SPLIT);
  return sprintf "not ok rows: %s; + %d assertion(s) never reached behind %d whole-file row(s) (%d of them ours)",
                 $t, $h->{rows} // 0, $h->{files} // 0, $h->{ours} // 0;
}

for my $dist (sort keys %by_dist) {
  my %n = map { ($_ => 0) } @VERDICTS;
  my (%split, %hid);
  print "== ", basename($dist), "\n";
  for my $r (@{ $by_dist{$dist} }) {
    my (undef, $file, $class, $ok, $notok, $rc) = @$r;
    $n{$class}++;
    printf "  %-8s %-28s ok=%-4d notok=%-4d rc=%d\n",
           $class, $file, $ok, $notok, $rc;
    split_file($r, \%split, \%hid) if %cause;
    mismatch_file($r);
    $grand{ok} += $ok; $grand{notok} += $notok;
  }
  my $total = @{ $by_dist{$dist} };
  printf "  -- %d PASS / %d PARTIAL / %d FAIL / %d SKIP of %d\n",
         $n{PASS}, $n{PARTIAL}, $n{FAIL}, $n{SKIP}, $total;
  printf "     %s\n", split_text(\%split, \%hid) if %cause;
  $grand{$_} += $n{$_} for @VERDICTS;
  $grand{files} += $total;
  $gsplit{$_} += $split{$_} // 0 for @SPLIT;
  $ghid{$_} += $hid{$_} // 0 for qw(rows files ours);
  $grand_bad += $n{FAIL};   # FAIL alone: a SKIP file is not a failure
}
printf "\n== TOTAL: %d PASS / %d PARTIAL / %d FAIL / %d SKIP of %d file(s); %d ok, %d not ok\n",
       (map { $grand{$_} // 0 } @VERDICTS), $grand{files} // 0, $grand{ok} // 0, $grand{notok} // 0;
print "   ", split_text(\%gsplit, \%ghid), "\n" if %cause;
print "   (no --causes: the not-ok split needs the blessed row baseline)\n" unless %cause;
print "$_\n" for sort @mismatch;
if ($oracle) {
  my %o; my $secs = 0;
  for my $r (@rows) { $o{ $r->[15] || 'none' }++; $secs += $r->[16] || 0 }
  printf "ORACLE: %d file(s) -- %d cached, %d measured (%.1f s of perl), %d timed out%s\n",
         scalar(@rows), $o{cached} // 0, $o{measured} // 0, $secs, $o{timeout} // 0,
         $o{none} ? ", $o{none} with no answer" : '';
} else {
  print "ORACLE: OFF (--no-oracle): a file PCL skips reads FAIL, SKIP cannot be decided\n";
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
           . "#   `*FILE*` row per t-file that produced no TAP rows at all, and one\n"
           . "#   `*PLAN*` row per t-file that ran rows but missed its plan or exited non-zero.\n"
           . "# Columns: dist, t-file, num, description, got, expected [, cause]\n"
           . "# Join key: (dist, t-file, description) — never the number (#1041).\n"
           . "# Written by tools/cpan-scoreboard.pl --rows, tree %s, %04d-%02d-%02d, perl %vd\n"
           . "# Command: %s\n",
           (length $sha ? $sha : 'unknown'), $t[5]+1900, $t[4]+1, $t[3], $^V,
           join(' ', $0, @argv_copy);
  my $nrows = 0;
  for my $r (sort { $a->[0] cmp $b->[0] || $a->[1] cmp $b->[1] } @rows) {
    for my $row (file_rows($r)) {
      print $fh join("\t", basename($r->[0]), $r->[1], @$row), "\n";
      $nrows++;
    }
  }
  close $fh;
  print "wrote $rows_file ($nrows row(s))\n";
}
exit 0;
