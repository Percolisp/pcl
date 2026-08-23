#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# drop-harvest.pl — the TEXT behind the drop census.  tools/drop-census.pl
# counts `;; PARSE ERROR` drops per file; this transpiles every file the
# census names with PCL_DROP_ANNOUNCE=all and collects the announced
# statements, one row each:
#
#   <census-rel-path><TAB><line><TAB><statement text><TAB><compiler reason>
#
#   tools/drop-harvest.pl baselines/parse-error-drop-census-s399.tsv out.tsv [jobs]
#
# WHY: a count says how many statements were lost; only the text says WHAT
# family they are (a missing feature such as given/when, a term-grammar shape,
# a lexer mis-lex, a deliberate syntax-error test).  The s407 sizing of Option
# B phase 2 (docs/option-b-phase2-plan.md) was done from this output — and it
# showed the census is dominated by feature ABSENCES, not parser gaps.
# Path resolution mirrors drop-census.pl (perl-tests/ and lib/ under the repo,
# t/... under perl's own t/ via PCLPaths).
use strict;
use warnings;
use FindBin;
use lib "$FindBin::RealBin/lib";
use PCLPaths qw(perl_suite_t);
my ($census, $out, $jobs) = @ARGV;
die "usage: drop-harvest.pl CENSUS.tsv OUT.tsv [JOBS]\n" unless $census && $out;
$jobs ||= 8;
my $root = "$FindBin::RealBin/..";
my $tdir = perl_suite_t();
open my $c, '<', $census or die "$census: $!";
my @files;
while (<$c>) {
  next if /^#/; chomp;
  my ($rel) = split /\t/; next unless $rel;
  my $abs = $rel =~ m{^perl-tests/|^lib/} ? "$root/$rel"
          : do { (my $r = $rel) =~ s{^t/}{}; "$tdir/$r" };
  push @files, [$rel, $abs] if -f $abs;
}
close $c;
my (%kid, @rows); my $seq = 0;
sub reap {
  my $pid = waitpid(-1, 0);
  return unless $pid > 0 && $kid{$pid};
  my ($rel, $tmp) = @{ delete $kid{$pid} };
  if (open my $fh, '<', $tmp) {
    while (<$fh>) {
      chomp;
      next unless /^PCL: statement dropped at \S+ line (\d+): (.*) -- (.*)$/;
      push @rows, join("\t", $rel, $1, $2, $3);
    }
    close $fh;
  }
  unlink $tmp;
}
for my $f (@files) {
  reap() while keys %kid >= $jobs;
  my $tmp = "/tmp/drop-harvest.$$." . ($seq++) . ".err";
  my $pid = fork; die "fork: $!" unless defined $pid;
  if (!$pid) {
    $ENV{PCL_DROP_ANNOUNCE} = 'all';
    open STDOUT, '>', '/dev/null'; open STDERR, '>', $tmp;
    exec 'timeout', '120', "$root/pl2cl", '--no-cache', $f->[1];
    exit 1;
  }
  $kid{$pid} = [$f->[0], $tmp];
}
reap() while keys %kid;
open my $o, '>', $out or die "$out: $!";
print $o "$_\n" for sort @rows;
close $o;
printf "drop-harvest: %d files, %d dropped statements -> %s\n", scalar(@files), scalar(@rows), $out;
