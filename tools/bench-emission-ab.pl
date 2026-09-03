#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# bench-emission-ab.pl — execution-speed A/B of TWO COMPILERS' EMISSIONS of
# the same program, on ONE saved core, interleaved.
#
#   perl tools/bench-emission-ab.pl --ref <git-ref> PROG.pl N_BIG
#   perl tools/bench-emission-ab.pl --tree /path/to/worktree PROG.pl N_BIG
#   K=9 perl tools/bench-emission-ab.pl --ref main prog.pl 600000
#
# WHY THIS EXISTS (s466bd, task #995).  `tools/bench-exec.pl`'s runtime A/B
# (`BENCH_RT_B`) shares ONE transpile between its two columns on purpose — it
# is built for a RUNTIME change, and for an EMISSION change its two columns
# are the same code.  The obvious alternative, running the whole tool once per
# worktree, carries a per-tree offset that a busy box makes large: at #995 the
# byte-identical `feread` row read -8 % across trees while the row under test
# read -26 %, i.e. a third of the "signal" was the instrument.
#
# So: transpile PROG.pl with BOTH compilers, then time the two .lisp files
# against the SAME core, interleaving the four series (A-big, B-big, A-small,
# B-small) round by round, best-of-K.  Everything except the emitted code is
# held fixed — one core, one machine state, one interleaving.
#
# PROG.pl must read its iteration count from $ENV{N} (`my $n = $ENV{N};`), as
# the rows in tools/bench-exec.pl do: exec = t(N_BIG) - t(0) then cancels
# process startup and the per-program compile for both sides equally.
#
# THE ROW'S OWN CONTROL IS PRINTED, not assumed: the two emissions are
# normalized (each tree embeds its absolute path in the preamble) and
# byte-compared, and the header says IDENTICAL or DIFFERENT.  Run a program
# whose emission is IDENTICAL to read the instrument's noise on this box, in
# the same conditions, before believing a number from one that is not.
#
# A SINGLE NUMBER FROM A LOADED BOX CAN BE WRONG, and this instrument's own
# failure mode was seen at #995: one control run of a byte-IDENTICAL pair read
# -31.2 % because a load spike sat on the B slots for all K rounds, while five
# other runs of the same pair in the same session read +0.7 / +1.3 / +1.8 /
# +3.2 / -2.4 / -3.1 %.  Best-of-K minima do not save a run whose noise is
# sustained.  So: run the control in the SAME window as the row under test,
# and repeat the pair until both are stable — a claim is the agreement of
# several runs, not the prettiest one.
use v5.30;
use strict;
use warnings;
use Time::HiRes qw(time);
use File::Basename qw(dirname);
use File::Temp qw(tempdir);
use Cwd qw(abs_path);

my $ROOT;
BEGIN { $ROOT = dirname(dirname(abs_path($0))) }
use lib "$ROOT/tools/lib";
use PCLSbcl qw(sbcl_prefix_str);

my $K = $ENV{K} // 7;
my ($ref, $tree);
while (@ARGV && $ARGV[0] =~ /^--/) {
  my $opt = shift @ARGV;
  if    ($opt eq '--ref')  { $ref  = shift @ARGV }
  elsif ($opt eq '--tree') { $tree = shift @ARGV }
  else { die "bench-emission-ab: unknown option $opt\n" }
}
my ($src, $big) = @ARGV;
die "usage: bench-emission-ab.pl [--ref REF | --tree DIR] PROG.pl N_BIG\n"
  unless defined $src && defined $big;
die "bench-emission-ab: give exactly one of --ref / --tree\n"
  if (defined $ref) == (defined $tree);

# Side B's compiler: a temp worktree of REF, or a tree that is already there.
my $tmp;
if (defined $ref) {
  $tmp = tempdir('pcl-emitab-XXXXXX', TMPDIR => 1, CLEANUP => 0);
  $tree = "$tmp/ref-tree";
  system("git -C '$ROOT' worktree add --detach -q '$tree' '$ref'") == 0
    or die "bench-emission-ab: git worktree add $ref failed\n";
}
END {
  if (defined $tmp && -d "$tmp/ref-tree") {
    system("git -C '$ROOT' worktree remove --force '$tmp/ref-tree' 2>/dev/null");
    system("rm -rf '$tmp'");
  }
}

# ONE core, from THIS tree's runtime: it times both sides, so a difference in
# the two trees' runtimes (the generation string, at least) cannot leak in.
my $sbcl = sbcl_prefix_str(runtime => "$ROOT/cl/pcl-runtime.lisp");

my %lisp = ('A(tree)' => "/tmp/bench-emitab-A.lisp",
            'B(ref)'  => "/tmp/bench-emitab-B.lisp");
my %from = ('A(tree)' => $ROOT, 'B(ref)' => $tree);
for my $k (sort keys %lisp) {
  system("$from{$k}/pl2cl '$src' > $lisp{$k} 2>/dev/null");
  die "bench-emission-ab: $k produced no emission (pl2cl exits 0 when a Pl "
    . "module fails to compile — check by hand)\n" if -z $lisp{$k};
}

# Normalize the preamble before comparing: each tree writes its own absolute
# path into @INC / *pcl-pl2cl-path*, and the gen stamp differs across a
# generation bump.  Without this every pair "differs".
my @norm = map {
  my $t = do { open my $h, '<', $lisp{$_} or die; local $/; <$h> };
  $t =~ s/\Q$ROOT\E/ROOT/g;
  $t =~ s/\Q$tree\E/ROOT/g;
  $t =~ s/^;;; pcl:.*\n//m;
  $t
} sort keys %lisp;
printf "emission: %s\n", $norm[0] eq $norm[1]
  ? 'IDENTICAL (this row cannot move — it is a CONTROL)' : 'DIFFERENT';

# Verify before timing (the #814 rule): a crashed run is fast, and two
# crashes subtract to zero.
my %out;
for my $k (sort keys %lisp) {
  local $ENV{N} = $big;
  $out{$k} = `$sbcl --load $lisp{$k} 2>/dev/null`;
}
die "bench-emission-ab: OUTPUTS DIFFER — not timed\n"
  . "  A: $out{'A(tree)'}\n  B: $out{'B(ref)'}\n"
  if $out{'A(tree)'} ne $out{'B(ref)'};
printf "output agrees: %s\n", ($out{'A(tree)'} =~ s/\s+/ /gr);

my @series = (['A(tree)', $big], ['B(ref)', $big], ['A(tree)', 0], ['B(ref)', 0]);
my @min = (undef) x @series;
for my $round (1 .. $K) {
  for my $i (0 .. $#series) {
    my ($k, $n) = @{ $series[$i] };
    local $ENV{N} = $n;
    my $t = time();
    system("$sbcl --load $lisp{$k} >/dev/null 2>&1");
    my $e = time() - $t;
    $min[$i] = $e if !defined $min[$i] || $e < $min[$i];
  }
}
my $a = $min[0] - $min[2];
my $b = $min[1] - $min[3];
$_ < 0 and $_ = 0 for $a, $b;
printf "A(tree) exec %.4f s   B(ref) exec %.4f s   A/B %s\n",
       $a, $b, $b > 0 ? sprintf('%+.1f%%', 100 * ($a / $b - 1)) : 'n/a';
print "A/B < 0 = the working tree's emission is FASTER.\n";
