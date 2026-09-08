#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# gate-profile.pl — WHERE THE GATE'S TIME GOES, per test file.
#
# The gate (`tools/prove-core`, or plain `prove -j8 Pl/t/`) is the throughput
# constraint on this project: a batch runs it six to nine times.  Before any
# lever is built, this says which files carry the cost and WHAT the cost is —
# so a lever is chosen against a measurement instead of a guess (s473u,
# task #1544).
#
# Two modes:
#
#   --log FILE      read a `prove --timer` log and print the per-file table:
#                   wall, the STATIC estimate of how many `pl2cl` transpiles
#                   and how many `sbcl` runs the file performs, what those
#                   cost at the measured unit prices, and the residue.
#
#   --measure F.t   run ONE file alone and print its EXACT numbers: wall and
#                   CPU from /usr/bin/time, and the exact process counts from
#                   `strace -f -e trace=execve` (every `pl2cl`, every `sbcl`).
#                   This is the accurate instrument; --log is the cheap survey
#                   over all 220-odd files.
#
# THE UNIT PRICES (measured s473u; re-measure with --measure if the machine
# changes).  Two numbers each, because a gate row is paid under `-j8` load and
# a bench row on a quiet box — the DEFAULTS below are the loaded ones, which is
# what the gate actually spends:
#   * one `pl2cl` spawn        0.13 s CPU quiet / ~0.19 under load — perl + PPI
#                              + every Pl::* module, paid per call, independent
#                              of the size of the snippet
#   * one `sbcl` run (CORE)    0.007 s quiet / ~0.03 under load — start is under
#                              5 ms; the rest is compile-loading the row's own
#                              program
#   * one `sbcl` run that      2.89 s CPU.  Four hundred times the core price,
#     `--load`s the runtime    and the reason `Pl/t/gate-cost-01.t` gates the
#     FROM SOURCE              shape: 15 gate files spelled their own sbcl
#                              command line and paid it on EVERY row — 974 of
#                              the gate's 2933 CPU-s for 8 % of its rows.
# The residue is everything else: module loads through ~/.pcl-cache, row
# programs that actually run for a while, the ir-conform corpus.
#
# THE STATIC ESTIMATE'S LIMIT, stated so it is never mistaken for a count: it
# counts CALL SITES, resolved one level through the file's own helper subs.  A
# helper called inside a loop counts once.  Use --measure on any file whose
# residue looks large before drawing a conclusion from it.
use strict;
use warnings;
use Getopt::Long;
use File::Basename qw(basename);

my ($log, $measure, $top, $tsv, $t_price, $s_price);
$top = 25; $t_price = 0.19; $s_price = 0.03;
GetOptions(
  'log=s'     => \$log,
  'measure=s' => \$measure,
  'top=i'     => \$top,
  'tsv'       => \$tsv,
  'transpile-price=f' => \$t_price,
  'sbcl-price=f'      => \$s_price,
) or die "usage: gate-profile.pl --log <prove --timer log> [--top N] [--tsv]\n"
       . "       gate-profile.pl --measure Pl/t/<file>.t\n";

# One body -> (pl2cl spawns, sbcl runs).  A `$pl2cl` inside a command string is
# one spawn; `sbcl` likewise.  PCLCore::transpile takes a command string that
# already contains $pl2cl, so it is not counted twice.
sub count_spawns {
  my ($src) = @_;
  my $t = () = $src =~ /\$pl2cl\b/g;
  my $s = () = $src =~ /(?:`|qq?\{|system\s*\()[^`;]*\bsbcl\b/g;
  return ($t, $s);
}

# For one .t: how many pl2cl spawns and how many sbcl runs does it perform?
# Helper subs are the unit — every file in Pl/t defines its own `run_cl`,
# `test_transpile`, … and calls them from the top level.  So: count the spawns
# in each sub body, resolve calls between subs to a fixpoint, then count the
# top-level invocations of each sub.
sub analyse_file {
  my ($path) = @_;
  my @lines = do { open my $fh, '<', $path or return (0, 0); <$fh> };
  # Split into sub bodies (a sub ends at the first line that starts with "}")
  # and the top-level remainder.  That is the house style in Pl/t; a sub whose
  # closing brace is indented is counted as top level, which over-counts its
  # body once — noted rather than papered over.
  my (%body, @top);
  my ($cur, @cur);
  for my $l (@lines) {
    if (!defined $cur && $l =~ /^sub\s+(\w+)/) { $cur = $1; @cur = (); next }
    if (defined $cur) {
      if ($l =~ /^\}/) { $body{$cur} = join('', @cur); $cur = undef; next }
      push @cur, $l; next;
    }
    push @top, $l;
  }
  $body{$cur} = join('', @cur) if defined $cur;
  my $top_src = join('', @top);

  my %direct;   # name -> [transpiles, sbcl runs]
  for my $n (keys %body) { $direct{$n} = [ count_spawns($body{$n}) ] }
  my @topd = count_spawns($top_src);

  # fixpoint over helper-to-helper calls
  my %tot = map { $_ => [ @{ $direct{$_} } ] } keys %direct;
  for (1 .. 10) {
    my $moved = 0;
    for my $n (keys %body) {
      my ($t, $s) = @{ $direct{$n} };
      for my $m (keys %body) {
        next if $m eq $n;
        my $c = () = $body{$n} =~ /\b\Q$m\E\s*\(/g;
        next unless $c;
        $t += $c * $tot{$m}[0]; $s += $c * $tot{$m}[1];
      }
      $moved = 1 if $t != $tot{$n}[0] || $s != $tot{$n}[1];
      $tot{$n} = [ $t, $s ];
    }
    last unless $moved;
  }

  my ($T, $S) = @topd;
  for my $m (keys %body) {
    my $c = () = $top_src =~ /\b\Q$m\E\s*\(/g;
    next unless $c;
    $T += $c * $tot{$m}[0]; $S += $c * $tot{$m}[1];
  }
  return ($T, $S);
}

# The exact instrument: one file, alone, timed and traced.
sub do_measure {
  my ($file) = @_;
  die "gate-profile.pl: no such file: $file\n" unless -f $file;
  my $tmp = "/tmp/gate-profile-$$";
  my $timed = qx{/usr/bin/time -f '%e %U %S' prove "$file" 2>&1 >/dev/null | tail -1};
  chomp $timed;
  my ($wall, $usr, $sys) = split ' ', $timed;
  ($wall, $usr, $sys) = (0, 0, 0) unless defined $sys && $sys =~ /^[\d.]+$/;
  system(qq{strace -f -e trace=execve -qq -o $tmp.strace prove "$file" >/dev/null 2>&1});
  my ($pl2cl, $sbcl, $perl) = (0, 0, 0);
  if (open my $s, '<', "$tmp.strace") {
    while (<$s>) {
      next unless /execve\(/;
      $pl2cl++ if m{"[^"]*/pl2cl"};
      $sbcl++  if m{execve\("[^"]*/sbcl"};
      $perl++  if m{execve\("[^"]*/perl[\d.]*"};
    }
    close $s;
  }
  unlink "$tmp.strace";
  my $cpu = $usr + $sys;
  my $attr = $pl2cl * $t_price + $sbcl * $s_price;
  printf "%s\n", $file;
  printf "  wall %.2f s   cpu %.2f s (%.2f usr + %.2f sys)\n", $wall, $cpu, $usr, $sys;
  printf "  execs: pl2cl %d   sbcl %d   perl %d\n", $pl2cl, $sbcl, $perl;
  printf "  at %.2f s/pl2cl and %.2f s/sbcl: %.1f s attributed, residue %.1f s\n",
         $t_price, $s_price, $attr, $cpu - $attr;
  return 0;
}

exit(do_measure($measure)) if defined $measure;
die "gate-profile.pl: give --log <file> or --measure <file.t>\n" unless defined $log;

open my $lh, '<', $log or die "gate-profile.pl: cannot read $log: $!\n";
my @rows;
while (my $l = <$lh>) {
  # [08:49:08] Pl/t/loop-labels-01.t ....... ok    82703 ms ( 0.25 usr … )
  next unless $l =~ m{(Pl/t/\S+\.t)\s.*?\b(\d+)\s*ms};
  my ($f, $ms) = ($1, $2);
  my ($T, $S) = analyse_file($f);
  push @rows, { file => $f, ms => $ms, t => $T, s => $S };
}
close $lh;
die "gate-profile.pl: no timed rows in $log (was it run with --timer?)\n" unless @rows;

for my $r (@rows) {
  $r->{attr} = $r->{t} * $t_price + $r->{s} * $s_price;
  $r->{res}  = $r->{ms} / 1000 - $r->{attr};
}
my @sorted = sort { $b->{ms} <=> $a->{ms} } @rows;
my ($wall, $T, $S) = (0, 0, 0);
for my $r (@rows) { $wall += $r->{ms} / 1000; $T += $r->{t}; $S += $r->{s} }

if ($tsv) {
  print "file\twall_s\ttranspiles\tsbcl_runs\tattributed_s\tresidue_s\n";
  printf "%s\t%.2f\t%d\t%d\t%.1f\t%.1f\n",
         $_->{file}, $_->{ms}/1000, $_->{t}, $_->{s}, $_->{attr}, $_->{res}
    for @sorted;
  exit 0;
}

printf "%-42s %8s %7s %7s %9s %9s\n",
       'file', 'wall_s', 'pl2cl', 'sbcl', 'attrib_s', 'residue_s';
my $last = $top - 1 < $#sorted ? $top - 1 : $#sorted;
printf "%-42s %8.2f %7d %7d %9.1f %9.1f\n",
       basename($_->{file}), $_->{ms}/1000, $_->{t}, $_->{s}, $_->{attr}, $_->{res}
  for @sorted[0 .. $last];
printf "\n%d files; sum of per-file wall %.0f s (parallel, so NOT the gate's wall);\n",
       scalar(@rows), $wall;
printf "estimated %d pl2cl spawns (%.0f s at %.2f s each) and %d sbcl runs (%.0f s at %.2f s each).\n",
       $T, $T * $t_price, $t_price, $S, $S * $s_price, $s_price;
