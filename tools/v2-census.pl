#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# v2-census.pl — T0.2/T0.3 census driver (docs/v2-transfer-plan.md).
#
# Runs pl2cl over a corpus and aggregates, per file:
#   - which pipeline produced the output (the T0.1 marker line),
#   - the whole-file gate reason when v2 fell back (PCL_V2_VERBOSE stderr),
#   - the seam histograms (PCL_V2_SEAM_CENSUS stderr; retargeted s411/Phase A
#     when the second expression generator was deleted):
#       stmt   — statements lowered via _fallback_stmt (v1 statement seam —
#                the E5.3 port worklist, docs/plan-one-compiler-s411.md §4.1)
#       head   — first-token shape of every expression the ONE generator lowers
#     The raw-residue census (which SUBTREES still print as v1 text) is
#     PCL_E2_RAW_CENSUS, a separate knob.
#
# Usage:
#   perl tools/v2-census.pl [--jobs N] [FILES...]        # default: perl-tests/*.t
#   perl tools/v2-census.pl --jobs 8 lib/**/*.pm         # module corpus (T0.3)
#
# Output: a markdown report on stdout.

use v5.20;
use strict;
use warnings;
use Getopt::Long;
use File::Basename;
use Cwd qw(abs_path);

my $jobs = 8;
GetOptions('jobs=i' => \$jobs) or die "Usage: v2-census.pl [--jobs N] [files...]\n";

my $root = dirname(dirname(abs_path($0)));
my @files = @ARGV ? @ARGV : sort glob("$root/perl-tests/*.t");
die "no input files\n" unless @files;

# ---------------------------------------------------------------- run corpus
my (%gate, %pipeline, %file_seam, %tot);
my (%h_stmt, %h_head);

my @queue = @files;
my %kids;   # pid → file
my %result; # file → { pipeline, gate, seamlines }

sub spawn_one {
  my $f = shift @queue or return 0;
  my $pid = fork() // die "fork: $!";
  if (!$pid) {
    my $err = "/tmp/v2census.$$.err";
    my $out = "/tmp/v2census.$$.out";
    $ENV{PCL_V2_SEAM_CENSUS} = 1;
    $ENV{PCL_V2_VERBOSE}     = 1;
    system("perl -I$root $root/pl2cl --no-cache " . quotemeta($f) . " >$out 2>$err");
    # First line of stdout = pipeline marker; stderr = gate reason + seam TSV.
    my ($marker) = do { open my $fh, '<', $out or exit 9; scalar <$fh> };
    my $stderr   = do { local $/; open my $fh, '<', $err or exit 9; <$fh> // '' };
    unlink $out, $err;
    open my $r, '>', "/tmp/v2census.res.$$" or exit 9;
    print $r ($marker // ''), "\x00", $stderr;
    exit 0;
  }
  $kids{$pid} = $f;
  return 1;
}

spawn_one() for 1 .. $jobs;
while (%kids) {
  my $pid = wait();
  last if $pid < 0;
  my $f = delete $kids{$pid} or next;
  my $res = do { local $/; open my $fh, '<', "/tmp/v2census.res.$pid" or undef; $fh ? <$fh> : '' };
  unlink "/tmp/v2census.res.$pid";
  my ($marker, $stderr) = split /\x00/, ($res // ''), 2;
  $result{$f} = { marker => $marker // '', stderr => $stderr // '' };
  spawn_one();
}

# ------------------------------------------------------------------ aggregate
for my $f (@files) {
  my $r = $result{$f} or next;
  my $short = $f;
  $short =~ s{^\Q$root\E/}{};
  my $pl = $r->{marker} =~ /pipeline=(\w+)/ ? $1 : '(none)';
  $pipeline{$pl}++;
  if ($pl ne 'v2') {
    my ($why) = $r->{stderr} =~ /v2 fell back to v1: (.+)/;
    $why //= '(no reason captured)';
    $why =~ s/\s+$//;
    $why =~ s/^Parser2( TODO)?: //;
    # Collapse per-file details (names, contents) into the gate FAMILY.
    $why =~ s/'[^']*'/'…'/g;
    $why =~ s/: .*$/: …/ if $why =~ /^(unsupported declaration|non-scalar state)/;
    push @{ $gate{$why} }, $short;
    next;
  }
  # v2-native file: fold its seam histograms into the global ones.
  my ($ex, $ss) = (0, 0);
  for my $line (split /\n/, $r->{stderr}) {
    my @c = split /\t/, $line;
    next unless @c >= 3 && $c[0] eq 'pcl-seam';
    if ($c[1] eq 'totals') {
      ($ex) = $c[3] =~ /(\d+)/; ($ss) = $c[4] =~ /(\d+)/;
    }
    elsif ($c[1] eq 'stmt') { $h_stmt{$c[2]} += $c[3] }
    elsif ($c[1] eq 'head') { $h_head{$c[2]} += $c[3] }
  }
  $file_seam{$short} = [$ex, $ss];
  $tot{expr} += $ex; $tot{stmt} += $ss;
}

# --------------------------------------------------------------------- report
my $nfiles = scalar @files;
my $nv2    = $pipeline{v2} // 0;
say "# v2 census — " . scalar(localtime());
say "";
say "Corpus: $nfiles files.  Pipeline: **$nv2 v2-native**, " . ($pipeline{v1} // 0) . " gated to v1"
    . (join('', map { $_ eq 'v2' || $_ eq 'v1' ? '' : ", $pipeline{$_} $_" } sort keys %pipeline)) . ".";
say "";
say "## Whole-file gates (T-A worklist)";
say "";
say "| n | gate | files |";
say "|---|------|-------|";
for my $g (sort { @{$gate{$b}} <=> @{$gate{$a}} || $a cmp $b } keys %gate) {
  my @fl = map { basename($_) } @{ $gate{$g} };
  say "| " . scalar(@fl) . " | $g | @fl |";
}
say "";
say "## Expressions and the statement seam";
say "";
say "Totals: **$tot{expr} expressions** through the one generator, **$tot{stmt} seam statements** (v1's statement layer, whole).";
say "";
say "### Statement seam (the E5.3 port worklist)";
say "";
say "| n | statement kind |";
say "|---|----------------|";
say "| $h_stmt{$_} | `$_` |"
  for sort { $h_stmt{$b} <=> $h_stmt{$a} || $a cmp $b } keys %h_stmt;
say "";
say "### Expression heads (what the corpus is made of)";
say "";
say "| n | first token |";
say "|---|-------------|";
say "| $h_head{$_} | `$_` |"
  for (sort { $h_head{$b} <=> $h_head{$a} || $a cmp $b } keys %h_head)[0 .. 39];
say "";
say "### Per-file seam load (worst 25 by seam statements)";
say "";
say "| file | expressions | seam-stmt |";
say "|------|-------------|-----------|";
my @worst = sort { $file_seam{$b}[1] <=> $file_seam{$a}[1] } keys %file_seam;
say "| $_ | $file_seam{$_}[0] | $file_seam{$_}[1] |"
  for @worst[0 .. ($#worst > 24 ? 24 : $#worst)];
