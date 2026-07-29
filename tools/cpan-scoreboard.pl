#!/usr/bin/env perl

# Copyright (c) 2025-2026
# This is free software; you can redistribute it and/or modify it
# under the same terms as the Perl 5 programming language system itself.

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
#
# --no-dist-lib applies to every dist AFTER the flag (see run-dist-t.pl's
# caveat: XS-stubbed dists like Scalar-List-Utils must NOT put their lib/ on
# pl2cl's @INC or the dist copy shadows PCL's shim).
#
# Classification per t-file (TAP counted by run-dist-t.pl --summary):
#   PASS    = at least one ok, zero not-ok, clean exit
#   PARTIAL = at least one ok, but not-ok rows or a non-zero exit (late crash)
#   FAIL    = zero ok (transpile failure, load crash, timeout, no TAP)

use strict;
use warnings;
use File::Basename qw(basename dirname);
use Cwd qw(abs_path);

my $root = abs_path(dirname(abs_path($0)) . "/..");

my $jobs = 8;
my $timeout = 120;
my @dists;           # [dir, no_dist_lib]
my $no_dist_lib = 0;
while (@ARGV) {
  my $a = shift @ARGV;
  if    ($a eq '--jobs')        { $jobs = shift @ARGV }
  elsif ($a eq '--timeout')     { $timeout = shift @ARGV }
  elsif ($a eq '--no-dist-lib') { $no_dist_lib = 1 }
  else                          { push @dists, [abs_path($a), $no_dist_lib] }
}
@dists or die "usage: $0 [--jobs N] [--timeout S] [--no-dist-lib] <dist-dir>...\n";

# Work list: one entry per t-file.
my @work;
for my $d (@dists) {
  my ($dir, $ndl) = @$d;
  -d "$dir/t" or die "no t/ in $dir\n";
  for my $t (sort glob "$dir/t/*.t") {
    push @work, { dist => $dir, ndl => $ndl, t => $t };
  }
}

# Fork pool: each child runs one t-file, writes one result line to a pipe.
my %kids;
my @queue = @work;
pipe(my $rd, my $wr) or die "pipe: $!";
my @rows;

sub spawn_one {
  my ($w) = @_;
  my $pid = fork() // die "fork: $!";
  if ($pid == 0) {
    close $rd;
    my @cmd = ("timeout", $timeout, "$root/tools/run-dist-t.pl", "--summary");
    push @cmd, "--no-dist-lib" if $w->{ndl};
    push @cmd, $w->{dist}, $w->{t};
    my $out = do {
      open(my $p, '-|', @cmd) or exit 9;
      local $/; my $o = <$p> // ''; close $p; $o;
    };
    my $rc = $? >> 8;
    my ($ok, $notok) = $out =~ /pass=(\d+) fail=(\d+)/ ? ($1, $2) : (0, 0);
    my $class = $ok == 0             ? 'FAIL'
              : ($notok || $rc != 0) ? 'PARTIAL'
              :                        'PASS';
    $class = 'FAIL' if $rc == 124;   # timeout
    print $wr join("\t", $w->{dist}, basename($w->{t}), $class, $ok, $notok, $rc), "\n";
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
  push @rows, [split /\t/, $line];
  my $done = wait();
  delete $kids{$done} if $done > 0;
  spawn_one(shift @queue) if @queue;
}
close $rd;
1 while wait() > 0;

# Report.
my %by_dist;
for my $r (sort { $a->[0] cmp $b->[0] || $a->[1] cmp $b->[1] } @rows) {
  my ($dist, $file, $class, $ok, $notok, $rc) = @$r;
  push @{ $by_dist{$dist} }, $r;
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
exit 0;
