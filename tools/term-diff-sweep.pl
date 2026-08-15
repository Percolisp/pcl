#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# term-diff-sweep.pl — run a PCL probe env var over MANY perl files in
# parallel and collect the probe lines it writes to stderr.
#
# Written for #153 / E5.0 (the `_term_extent` migration), whose probes were
# `PCL_TERM_DIFF=1` warnings from Pl/PExpr.pm — but the script is
# probe-agnostic: it just transpiles each file with the environment you give
# it and keeps every stderr line matching --match.
#
# NOTE: BOTH `PCL_TERM_DIFF` probes were DELETED in s361 once their sites
# took their answer from the walker (a probe that can only report equality
# is dead code).  Steps 4–5 add their own probe when they widen the walker;
# point --env/--match at that one.  The `PCL_TERM_DIFF=1` examples below are
# kept because they are the shape to copy, not because the var is still live.
#
# WHY IT EXISTS (the s361 lesson, docs/DECIDED.md §s361): the 111-file
# perl-tests corpus is NOT a sufficient population for a "measured then
# flipped" step.  s359's inventory over the corpus reported ZERO
# walker-vs-legacy disagreements; the SAME probes over perl's own
# t/*/*.t (604 files) produced three real shapes, two of them live
# silent-wrongs (a dropped-arguments prototype bug and `getc $$_[0]`).
# Measure BOTH populations before flipping a site.
#
# Usage:
#   # the two standard populations, one after the other
#   tools/term-diff-sweep.pl --env PCL_TERM_DIFF=1 perl-tests/*.t
#   tools/term-diff-sweep.pl --env PCL_TERM_DIFF=1 \
#       /path/to/perl-5.40.3/t/*/*.t
#
#   # or feed a file list on stdin
#   ls perl-tests/*.t | tools/term-diff-sweep.pl --env PCL_TERM_DIFF=1 -
#
# Options:
#   --env NAME=VALUE   set an env var for each transpile (repeatable)
#   --match REGEX      keep stderr lines matching this (default: ^PCL_)
#   --jobs N           parallel transpiles (default 8)
#   --timeout N        per-file timeout in seconds (default 120)
#   --out PATH         write the collected lines here (default: stdout)
#
# Output is one line per probe hit, prefixed with the source file and a TAB,
# so `sort | uniq -c` over the message collapses it to SHAPES — which is how
# you read it.  A file that times out or dies contributes nothing and is
# counted in the summary on stderr.
use strict;
use warnings;
use File::Temp qw(tempdir);

my (%env, @files);
my $match   = '^PCL_';
my $jobs    = 8;
my $timeout = 120;
my $out;

while (@ARGV) {
  my $a = shift @ARGV;
  if    ($a eq '--env')     { my $kv = shift @ARGV;
                              my ($k, $v) = split /=/, $kv, 2;
                              $env{$k} = defined $v ? $v : 1 }
  elsif ($a eq '--match')   { $match   = shift @ARGV }
  elsif ($a eq '--jobs')    { $jobs    = shift @ARGV }
  elsif ($a eq '--timeout') { $timeout = shift @ARGV }
  elsif ($a eq '--out')     { $out     = shift @ARGV }
  elsif ($a eq '-')         { push @files, map { chomp; $_ } <STDIN> }
  elsif ($a =~ /^--/)       { die "unknown option: $a\n" }
  else                      { push @files, $a }
}
@files or die "no input files (give paths, or `-` to read them from stdin)\n";
%env    or warn "term-diff-sweep: no --env given; probes are usually gated on one\n";

my $rx  = qr/$match/;
my $dir = tempdir(CLEANUP => 1);

# Fork up to $jobs transpiles at a time; each child writes its own part file
# so the parent never interleaves output.
my (%kids, @parts);
my $idx = 0;
while (@files || %kids) {
  while (@files && keys(%kids) < $jobs) {
    my $file = shift @files;
    my $part = "$dir/part.$idx"; $idx++;
    my $pid  = fork();
    die "fork: $!" if !defined $pid;
    if (!$pid) {
      local %ENV = (%ENV, %env);
      my $err = "$part.err";
      system("timeout $timeout ./pl2cl < '$file' > /dev/null 2> '$err'");
      open my $o, '>', $part or exit 1;
      if (open my $e, '<', $err) {
        while (my $l = <$e>) { print $o "$file\t$l" if $l =~ $rx }
        close $e;
      }
      close $o;
      exit 0;
    }
    $kids{$pid} = 1;
    push @parts, $part;
  }
  my $done = wait();
  delete $kids{$done} if $done > 0;
}

my $fh = \*STDOUT;
if (defined $out) { open $fh, '>', $out or die "$out: $!\n" }
my $n = 0;
for my $p (@parts) {
  next unless open my $i, '<', $p;
  while (<$i>) { print $fh $_; $n++ }
  close $i;
}
close $fh if defined $out;
printf STDERR "term-diff-sweep: %d files, %d probe line(s)%s\n",
  $idx, $n, (defined $out ? " -> $out" : "");
