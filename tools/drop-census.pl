#!/usr/bin/env perl
# drop-census.pl — count the #138 family: every file whose EMITTED CL contains
# a `(progn ;; PARSE ERROR: … nil)`, i.e. a statement the compiler could not
# lower, replaced by nil, with execution continuing.
#
#   tools/drop-census.pl <repo-root> <out.tsv> [jobs]
#
# Output: <rel-path><TAB><drops in the file><TAB><the compiler's own message(s)
# with a count each>.  The blessed census is
# docs/parse-error-drop-census-s399.tsv (72 files / 379 drops at c754abc);
# diff a fresh run against it to see whether a change added or removed drops.
#
# WHY IT EXISTS: a drop is silent at RUN time — the statement simply is not
# there — so nothing in the gate or the sweep notices one.  perl-tests/bless.t
# carries exactly one, and it is the test row
# `is ref $untied, "main", '…' or diag $@;`: an assertion that never runs and
# appears in no count, in a file the sweep reports as passing.  Task #343 has
# the analysis and the minimised trigger.
#
# ~3.5 minutes at 8 jobs (658 files: perl-tests + perl's t/ default dirs +
# lib/**.pm; op/cond.t excluded, as in every measurement).
#
# Companion: tools/gate-set-scan.pl, same populations, reads stderr instead.
use strict;
use warnings;
use FindBin;
use lib "$FindBin::RealBin/lib";
use PCLPaths qw(perl_suite_t);

my $root = shift or die "usage: drop-census.pl ROOT OUT [JOBS]\n";
my $out  = shift or die "usage: drop-census.pl ROOT OUT [JOBS]\n";
my $jobs = shift || 8;
# perl's own t/ — derived (PCL_PERL_SUITE_T, else the perlbrew build tree of
# the running perl), never hard-coded: task #278.
my $tdir = perl_suite_t();

my @files = sort glob("$root/perl-tests/*.t");
push @files, sort glob("$tdir/$_/*.t")
  for qw(base cmd comp opbasic op mro class run uni re io);
push @files, sort glob("$root/lib/*.pm"), sort glob("$root/lib/*/*.pm");
@files = grep { $_ !~ m{/op/cond\.t$} } @files;

my (%kid, @rows);
sub reap {
  my $pid = waitpid(-1, 0);
  return unless $pid > 0 && $kid{$pid};
  my ($file, $tmp) = @{ delete $kid{$pid} };
  open my $fh, '<', $tmp or die $!;
  my $cl = do { local $/; <$fh> };
  close $fh; unlink $tmp;
  $cl //= '';
  my @hits = $cl =~ /;; PARSE ERROR: *([^\n]*)/g;
  return unless @hits;
  my $rel = $file;
  $rel =~ s/\Q$root\E\///; $rel =~ s/\Q$tdir\E\//t\//;
  my %seen;
  for my $h (@hits) {
    $h =~ s/\s+$//;
    $h = substr($h, 0, 110);
    $seen{$h}++;
  }
  push @rows, sprintf("%s\t%d\t%s\n", $rel, scalar(@hits),
                      join(" | ", map { "$_ x$seen{$_}" } sort keys %seen));
}

for my $f (@files) {
  reap() while keys(%kid) >= $jobs;
  my $tmp = "/tmp/pcl-drop-census.$$." . int(rand 1e9);
  my $pid = fork();
  die "fork: $!" unless defined $pid;
  if (!$pid) {
    open STDOUT, '>', $tmp or die $!;
    open STDERR, '>', '/dev/null' or die $!;
    exec 'timeout', '90', "$root/pl2cl", $f;
    exit 127;
  }
  $kid{$pid} = [$f, $tmp];
}
reap() while keys %kid;

open my $o, '>', $out or die "$out: $!";
print $o sort @rows;
close $o;
my $total = 0; $total += (split /\t/, $_)[1] for @rows;
printf STDERR "drop-census: %d files carry a PARSE ERROR drop, %d drops total -> %s\n",
  scalar(@rows), $total, $out;
