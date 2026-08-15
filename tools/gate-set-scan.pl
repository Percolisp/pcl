#!/usr/bin/env perl
# gate-set-scan.pl — the s372 two-population GATE-SET measurement: transpile
# every file of BOTH populations and record what the compiler said (first
# stderr line, normalized), so a before/after diff shows exactly which files'
# verdicts moved and nothing else.
#
#   tools/gate-set-scan.pl <repo-root> <out.tsv> [jobs]
#
# The rule it implements (project memory, s372): when a fix widens what a
# CHECKER sees, diff the gate set FILE BY FILE over BOTH populations —
# detection can turn a silent-wrong into a DIE, and a one-population check
# cannot see that.  Run it once against a `git worktree` of the base commit
# and once against the working tree, then `diff` the two tsvs:
#
#   git worktree add /tmp/wt-base <sha>
#   tools/gate-set-scan.pl /tmp/wt-base /tmp/before.tsv
#   tools/gate-set-scan.pl "$PWD"     /tmp/after.tsv
#   diff /tmp/before.tsv /tmp/after.tsv
#
# NORMALIZATION matters: absolute paths, `at FILE line N` tails and long
# numbers vary run to run, so they are stripped — without that the diff is
# unreadable and a real move hides in the noise.
#
# ~2.5 minutes per population at 8 jobs (638 files).  op/cond.t is excluded by
# name: it is the known memory hog (20k-deep ternary, MemoryMax guard) and is a
# blessed non-participant in every measurement.
#
# Companion: tools/drop-census.pl, which reads the same populations but looks
# at the emitted CL instead of stderr.
use strict;
use warnings;
use File::Basename qw(basename);

my $root = shift or die "usage: gateset.pl ROOT OUT [JOBS]\n";
my $out  = shift or die "usage: gateset.pl ROOT OUT [JOBS]\n";
my $jobs = shift || 8;
my $tdir = "/home/bernt/perl5/perlbrew/build/perl-5.40.3/perl-5.40.3/t";

my @files = sort glob("$root/perl-tests/*.t");
push @files, sort glob("$tdir/$_/*.t")
  for qw(base cmd comp opbasic op mro class run uni re io);

# op/cond.t is the known memory hog (MemoryMax guard, 20k-deep ternary) — it is
# a blessed non-participant in every measurement, so skip it by name rather
# than letting it eat the box.
@files = grep { $_ !~ m{/op/cond\.t$} } @files;

my %kid;
my @rows;
sub reap {
  my $pid = waitpid(-1, 0);
  return unless $pid > 0 && $kid{$pid};
  my ($file, $tmp) = @{ delete $kid{$pid} };
  open my $fh, '<', $tmp or die $!;
  my $err = do { local $/; <$fh> };
  close $fh;
  unlink $tmp;
  $err //= '';
  # First non-empty stderr line, normalized: absolute paths, line numbers and
  # the compiler's own "at FILE line N" tails vary run to run and file to file.
  my ($first) = grep { /\S/ } split /\n/, $err;
  $first //= 'OK';
  $first =~ s/\Q$root\E\/?//g;
  $first =~ s/\Q$tdir\E\/?//g;
  $first =~ s/ at \S+ line \d+\.?//g;
  $first =~ s/line \d+/line N/g;
  $first =~ s/\d{3,}/NNN/g;
  $first = substr($first, 0, 160);
  my $rel = $file;
  $rel =~ s/\Q$root\E\///; $rel =~ s/\Q$tdir\E\//t\//;
  push @rows, "$rel\t$first\n";
}

for my $f (@files) {
  reap() while keys(%kid) >= $jobs;
  my $tmp = "/tmp/gateset.$$." . scalar(keys %kid) . "." . int(rand 1e6);
  my $pid = fork();
  die "fork: $!" unless defined $pid;
  if (!$pid) {
    open STDOUT, '>', '/dev/null' or die $!;
    open STDERR, '>', $tmp or die $!;
    exec 'timeout', '90', "$root/pl2cl", $f;
    exit 127;
  }
  $kid{$pid} = [$f, $tmp];
}
reap() while keys %kid;

open my $o, '>', $out or die "$out: $!";
print $o sort @rows;
close $o;
printf STDERR "gateset: %d files -> %s\n", scalar(@rows), $out;
