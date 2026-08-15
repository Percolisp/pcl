#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# emission-ab.pl — parallel A/B compare of EMITTED CL over any file list.
#
# Byte-compares the normalized emission (and the exit status) of two
# compilers — or of ONE compiler under two environments — for every input
# file, in parallel.  It is tools/corpus-diff.pl generalized: any population
# (perl's t/, a CPAN dist's t/ + lib/, lib/ shims), two git refs, 8 jobs.
# Written for #153 FOLD chunk 3 (s398), where the bar was "byte-identical
# over corpus + perl t/ + 14-dist board + lib, main-vs-branch AND
# fold-on-vs-off"; the s375 chunk-1 A/B was the same recipe done by hand.
#
# Usage:
#   tools/emission-ab.pl --ref main --list files.txt          # main vs working tree
#   tools/emission-ab.pl --ref main --new HEAD --list files.txt  # two refs (worktrees)
#   tools/emission-ab.pl --env PCL_SOME_FLAG=1 --list files.txt  # same compiler,
#                                        # side A with the env var set, B without
#   tools/emission-ab.pl --ref main perl-tests/*.t
#
# Options:
#   --ref REF        side A compiler = a worktree of REF (default: none —
#                    side A is the working tree, which only makes sense with --env)
#   --new REF        side B compiler = a worktree of REF (default: working tree)
#   --env NAME=VAL   set NAME on side A's transpiles only (repeatable)
#   --list FILE      input paths, one per line (repeatable; args are files too)
#   --jobs N         parallel pairs (default 8)
#   --out DIR        keep both sides' outputs here (default: tempdir, removed)
#   --timeout S      per-transpile timeout (default 300)
#
# Exit 0 when every pair is SAME with equal exit status; 1 otherwise, listing
# the DIFF and RC files (inspect with --out).  Normalization: the pipeline
# marker line is dropped and both repo roots become ROOT (the worktree's
# absolute path is embedded in the preamble).
#
# GOTCHA this file exists to remember (s398): the worktree-removing END block
# must run in the PARENT only — the first forked child to exit otherwise
# deletes the ref compiler under its siblings, every side-A transpile returns
# rc 2 with EMPTY stderr, and a naive summary reads SAME on empty pairs.
# Hence the $$ == $parent guard and the non-empty check below.
use strict; use warnings;
use File::Temp qw(tempdir);
use File::Path qw(make_path);
use FindBin qw($RealBin);
use Cwd qw(abs_path);

my $root = abs_path("$RealBin/..");
my ($ref, $newref, $out, $jobs, $timeout) = (undef, undef, undef, 8, 300);
my (%env, @files);
while (@ARGV) {
  my $a = shift @ARGV;
  if    ($a eq '--ref')     { $ref     = shift @ARGV }
  elsif ($a eq '--new')     { $newref  = shift @ARGV }
  elsif ($a eq '--env')     { my ($k, $v) = split /=/, shift(@ARGV), 2; $env{$k} = $v // 1 }
  elsif ($a eq '--list')    { my $l = shift @ARGV; open my $f, '<', $l or die "$l: $!"; push @files, map { chomp; $_ } grep { /\S/ } <$f>; close $f }
  elsif ($a eq '--jobs')    { $jobs    = shift @ARGV }
  elsif ($a eq '--out')     { $out     = shift @ARGV }
  elsif ($a eq '--timeout') { $timeout = shift @ARGV }
  elsif ($a =~ /^--/)       { die "unknown option: $a\n" }
  else                      { push @files, $a }
}
@files = grep { -f $_ } map { abs_path($_) // $_ } @files;
die "no input files\n" if !@files;
die "nothing to compare: give --ref (two compilers) and/or --env (two environments)\n"
  if !defined $ref && !%env;

my $tmp = $out // tempdir("pcl-emission-ab-XXXXXX", TMPDIR => 1, CLEANUP => 1);
make_path("$tmp/A", "$tmp/B");
my ($wta, $wtb) = ();
sub worktree { my ($r, $dir) = @_; system("git -C \Q$root\E worktree add --quiet \Q$dir\E \Q$r\E") == 0 or die "worktree add $r failed\n"; $dir }
my $dir_a = defined $ref    ? ($wta = worktree($ref,    "$tmp/ref-tree")) : $root;
my $dir_b = defined $newref ? ($wtb = worktree($newref, "$tmp/new-tree")) : $root;
my $parent = $$;
END {
  local $?;
  return if $$ != $parent;            # forked children must NOT remove the worktrees
  for my $w ($wta, $wtb) {
    system("git -C \Q$root\E worktree remove --force \Q$w\E >/dev/null 2>&1") if defined $w && -d $w;
  }
}
my $env_a = join ' ', map { "$_=" . quotemeta($env{$_}) } sort keys %env;

my (%kids, %tag);
my $n = 0;
my @todo = @files;
while (@todo || %kids) {
  while (@todo && keys(%kids) < $jobs) {
    my $f = shift @todo; my $i = $n++;
    my $pid = fork(); die "fork: $!" if !defined $pid;
    if (!$pid) {
      my ($ta, $tb) = ("$tmp/A/$i.lisp", "$tmp/B/$i.lisp");
      system("cd \Q$dir_a\E && $env_a timeout $timeout ./pl2cl < \Q$f\E > \Q$ta\E 2>\Q$ta\E.err; echo \$? > \Q$ta\E.rc");
      system("cd \Q$dir_b\E && timeout $timeout ./pl2cl < \Q$f\E > \Q$tb\E 2>\Q$tb\E.err; echo \$? > \Q$tb\E.rc");
      exit 0;
    }
    $kids{$pid} = 1; $tag{$i} = $f;
  }
  my $d = wait(); delete $kids{$d} if $d > 0;
}

my $slurp = sub { my ($p) = @_; open my $fh, '<', $p or return ''; local $/; my $t = <$fh>; $t // '' };
my $norm = sub {
  my $t = $slurp->(shift);
  $t =~ s/^;;; pcl: pipeline=.*\n//m;
  $t =~ s/\Q$wta\E/ROOT/g if defined $wta;
  $t =~ s/\Q$wtb\E/ROOT/g if defined $wtb;
  $t =~ s/\Q$root\E/ROOT/g;
  return $t;
};
my ($same, $empty, @diff, @rcdiff) = (0, 0);
for my $i (0 .. $n - 1) {
  my ($ra, $rb) = map { my $r = $slurp->("$tmp/$_/$i.lisp.rc"); chomp $r; $r } qw(A B);
  push @rcdiff, "$tag{$i}\trc A=$ra B=$rb" if $ra ne $rb;
  my ($a, $b) = ($norm->("$tmp/A/$i.lisp"), $norm->("$tmp/B/$i.lisp"));
  $empty++ if $a eq '' && $b eq '';
  if ($a eq $b) { $same++ } else { push @diff, "$tag{$i}\t(pair $i)" }
}
printf "emission-ab: files=%d SAME=%d DIFF=%d RCDIFF=%d (both-empty pairs: %d)\n",
  $n, $same, scalar @diff, scalar @rcdiff, $empty;
print "DIFF\t$_\n" for @diff;
print "RC\t$_\n"   for @rcdiff;
print "outputs kept in $tmp\n" if $out;
warn "emission-ab: WARNING every pair is empty on both sides — did the compilers run at all?\n"
  if $n && $empty == $n;
exit(@diff || @rcdiff ? 1 : 0);
