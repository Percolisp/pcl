#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# drop-census.pl — count the #138 family: every file whose EMITTED CL contains
# a `(progn ;; PARSE ERROR: … nil)`, i.e. a statement the compiler could not
# lower, replaced by nil, with execution continuing.
#
#   tools/drop-census.pl <repo-root> <out.tsv> [jobs] [--board] [--board-dir DIR]
#
# Output: <rel-path><TAB><drops in the file><TAB><the compiler's own message(s)
# with a count each>.  The blessed census is
# docs/parse-error-drop-census-s399.tsv; diff a fresh run against it to see
# whether a change added or removed drops.
#
# WHY IT EXISTS: a drop is silent at RUN time — the statement simply is not
# there — so nothing in the gate or the sweep notices one.  perl-tests/bless.t
# carried exactly one, and it was the test row
# `is ref $untied, "main", '…' or diag $@;`: an assertion that never runs and
# appears in no count, in a file the sweep reports as passing.  Task #343 has
# the analysis and the minimised trigger.
#
# SIX POPULATIONS (task #462, s434; the sixth added s438, task #473).  The
# census used to cover perl-tests +
# perl's t/ + the shipped lib/ shims, and NOTHING covered the two module
# populations — which is where the last FULLY silent members of the #138 family
# live, because `pl2cl --module` does not even announce a drop (ruled s403).
# Measured by hand in s431: 12 modules, 20 drops, none of them countable by any
# instrument, and eight of them minimised into four filed bugs (#457, #464,
# #465, #466) that had been sitting there unseen.  A price nobody can count is
# not a price, and the announce->DIE flip reaches these files too.
#
#   perl-tests   perl-tests/*.t                      program mode
#   perl-t       $PCL_PERL_SUITE_T/<default dirs>/*.t program mode
#   lib          lib/**.pm                            MODULE mode
#   cpan-tests   cpan-tests/modules/**/*.pm           MODULE mode
#   cpan-t       cpan-tests/modules/**/t/**/*.t       program mode
#   board        the 14-dist CPAN board's own lib/**  MODULE mode, --board only
#
# THE SIXTH, `cpan-t` (task #473, s438): a dist's own test suite.  They are
# PROGRAMS -- that is what a `.t` is -- so they are transpiled without
# `--module`, unlike the dist's `.pm` files beside them.  They were outside
# every population until the s436 four-population emission A/B (a DIFFERENT
# instrument: it byte-compares two compilers over a file LIST, so it sees
# every file a change touches whatever the census defines) reported 43 files
# / 92 drop sites in them -- 92 sites the flip's price sheet did not carry.
# `xt/` and `examples/` are deliberately NOT in it: they are author/release
# tests, not the dist's suite, and nothing here runs them.
#
# A `.pm` is transpiled with `--module`, because that is the emission the
# runtime caches and therefore the one that runs.  (The shipped lib/ shims
# moved from program mode to module mode in s434: measured zero drops either
# way, so the blessed rows are unaffected and the rule is now uniform.)
#
# --board reaches OUTSIDE the checkout (~/.cpan/build), so it is opt-in and its
# rows are only refreshed by a run that passes it — see the census header.  The
# 14 dists are read from docs/cpan-board14-s343.tsv (the board's own
# definition), never written down here; the build root is $PCL_CPAN_BUILD, else
# $HOME/.cpan/build, else --board-dir DIR.
#
# ~3.5 minutes at 8 jobs for the three original populations (658 files);
# op/cond.t excluded, as in every measurement.
#
# t/japh/ IS NOT A POPULATION AND WILL NOT BECOME ONE (ruled s437): the
# companion population is exactly tools/run-perl-suite.pl's @DEFAULT_DIRS,
# where `japh` is excluded as obfuscated -- nothing runs those files, so they
# are not a measurement population.  t/japh/abigail.t carries 2 drops of the
# family-1 `1 while -f ++$file` shape; that count is stated in the census
# header and is not re-measured here.
#
# Companion: tools/gate-set-scan.pl, same populations, reads stderr instead.
use strict;
use warnings;
use File::Find;
use FindBin;
use lib "$FindBin::RealBin/lib";
use PCLPaths qw(perl_suite_t);
use Cwd qw(abs_path);

my ($board, $board_dir, @pos);
while (@ARGV) {
  my $a = shift @ARGV;
  # --board takes NO argument: a `--board DIR` form would swallow the ROOT
  # positional the day someone writes the flag first.
  if    ($a eq '--board')     { $board = 1 }
  elsif ($a eq '--board-dir') { $board = 1; $board_dir = shift @ARGV }
  else { push @pos, $a }
}
my $root = shift @pos or die "usage: drop-census.pl ROOT OUT [JOBS] [--board] [--board-dir DIR]\n";
my $out  = shift @pos or die "usage: drop-census.pl ROOT OUT [JOBS] [--board] [--board-dir DIR]\n";
my $jobs = shift(@pos) || 8;
my $abs_root = abs_path($root) // $root;
# perl's own t/ — derived (PCL_PERL_SUITE_T, else the perlbrew build tree of
# the running perl), never hard-coded: task #278.
my $tdir = perl_suite_t();

# One work item = [ absolute path, population tag, rel-path in the census,
# module mode? ].
my @files;
push @files, [$_, 'perl-tests', rel_repo($_), 0] for sort glob("$root/perl-tests/*.t");
for my $d (qw(base cmd comp opbasic op mro class run uni re io)) {
  push @files, [$_, 'perl-t', rel_t($_), 0] for sort glob("$tdir/$d/*.t");
}
# RECURSIVE, not the old two-level glob: `lib/*.pm` + `lib/*/*.pm` missed
# lib/File/Spec/Functions.pm and lib/Math/BigInt/Calc.pm, which had therefore
# never been in any census at all (both measure zero drops — found s434 while
# adding the module populations).
push @files, [$_, 'lib', rel_repo($_), 1] for pm_under("$root/lib");
push @files, [$_, 'cpan-tests', rel_repo($_), 1] for pm_under("$root/cpan-tests/modules");
# PROGRAM mode, and only under a `t/` directory: a dist's own test suite.
push @files, [$_, 'cpan-t', rel_repo($_), 0] for dist_t_under("$root/cpan-tests/modules");
if ($board) {
  my $build = $board_dir // $ENV{PCL_CPAN_BUILD} // "$ENV{HOME}/.cpan/build";
  my @dists = board_dists("$root/docs/cpan-board14-s343.tsv");
  my $found = 0;
  for my $dist (@dists) {
    next unless -d "$build/$dist/lib";
    $found++;
    push @files, [$_, 'board', 'board/' . substr($_, length("$build/") ), 1]
      for pm_under("$build/$dist/lib");
  }
  die "drop-census: --board found none of the ", scalar(@dists),
      " board dists under $build\n"
    . "  (set PCL_CPAN_BUILD, or pass --board-dir DIR)\n" if !$found;
  warn sprintf("drop-census: --board: %d of %d dists present under %s\n",
               $found, scalar(@dists), $build) if $found < @dists;
}
@files = grep { $_->[0] !~ m{/op/cond\.t$} } @files;

sub rel_repo { my $p = shift; $p =~ s{^\Q$root\E/}{}; $p }
sub rel_t    { my $p = shift; $p =~ s{^\Q$tdir\E/}{t/}; $p }
sub pm_under {
  my ($dir) = @_;
  return () unless -d $dir;
  my @pm;
  find({ no_chdir => 1,
         wanted => sub { push @pm, $File::Find::name if -f $File::Find::name && /\.pm$/ } },
       $dir);
  return sort @pm;
}
# Every `.t` at any depth under a `t/` directory (Test-Simple's suite nests
# four levels).  `xt/` and `examples/` are excluded by the `/t/` test itself.
sub dist_t_under {
  my ($dir) = @_;
  return () unless -d $dir;
  my @t;
  find({ no_chdir => 1,
         wanted => sub {
           push @t, $File::Find::name
             if -f $File::Find::name && /\.t$/ && m{/t/};
         } },
       $dir);
  return sort @t;
}
# The board is 14 dists, and its definition lives in the board's own survey —
# deriving it here keeps ONE list (task #278's spirit: never write down what
# can be read).
sub board_dists {
  my ($tsv) = @_;
  open my $fh, '<', $tsv or die "drop-census: --board needs $tsv: $!\n";
  my %d;
  while (<$fh>) {
    next if /^#/ || !/\S/;
    my ($first) = split /\t/;
    $d{(split m{/}, $first)[0]} = 1 if defined $first && length $first;
  }
  close $fh;
  return sort keys %d;
}

my (%kid, @rows, %pop_files, %pop_hit, %pop_drops);
sub reap {
  my $pid = waitpid(-1, 0);
  return unless $pid > 0 && $kid{$pid};
  my ($file, $tmp, $pop, $rel) = @{ delete $kid{$pid} };
  open my $fh, '<', $tmp or die $!;
  my $cl = do { local $/; <$fh> };
  close $fh; unlink $tmp;
  $cl //= '';
  $pop_files{$pop}++;
  my @hits = $cl =~ /;; PARSE ERROR: *([^\n]*)/g;
  return unless @hits;
  $pop_hit{$pop}++;
  $pop_drops{$pop} += @hits;
  my %seen;
  for my $h (@hits) {
    $h =~ s/\s+$//;
    # A few compiler messages quote the FILE they were raised in (PExpr's
    # "unhandled postfix '->' term in F: ..."), so the row would otherwise
    # depend on how ROOT was spelled on the command line -- `.` and an
    # absolute path produced different bytes for the same drop, and the diff
    # against the blessed census would read as a change.  Strip both spellings.
    $h =~ s{\Q$abs_root\E/}{}g;
    $h =~ s{\Q$root\E/}{}g;
    $h = substr($h, 0, 110);
    $seen{$h}++;
  }
  push @rows, sprintf("%s\t%d\t%s\n", $rel, scalar(@hits),
                      join(" | ", map { "$_ x$seen{$_}" } sort keys %seen));
}

for my $item (@files) {
  my ($f, $pop, $rel, $module) = @$item;
  reap() while keys(%kid) >= $jobs;
  my $tmp = "/tmp/pcl-drop-census.$$." . int(rand 1e9);
  my $pid = fork();
  die "fork: $!" unless defined $pid;
  if (!$pid) {
    open STDOUT, '>', $tmp or die $!;
    open STDERR, '>', '/dev/null' or die $!;
    exec 'timeout', '90', "$root/pl2cl", ($module ? ('--module') : ()), $f;
    exit 127;
  }
  $kid{$pid} = [$f, $tmp, $pop, $rel];
}
reap() while keys %kid;

open my $o, '>', $out or die "$out: $!";
print $o sort @rows;
close $o;
my $total = 0; $total += (split /\t/, $_)[1] for @rows;
# Per POPULATION, because the module half is the new one and its rows must be
# readable on their own (task #462) — a single total hides exactly the number
# the flip needs.
printf STDERR "drop-census: %-11s %4d files  %3d with drops  %4d drops%s\n",
  $_, $pop_files{$_} // 0, $pop_hit{$_} // 0, $pop_drops{$_} // 0,
  ($_ eq 'board' ? '   (outside the checkout — only with --board)' : '')
  for grep { $pop_files{$_} } qw(perl-tests perl-t lib cpan-tests cpan-t board);
printf STDERR "drop-census: %d files carry a PARSE ERROR drop, %d drops total -> %s\n",
  scalar(@rows), $total, $out;
print STDERR "drop-census: --board NOT given: the board rows in the blessed census"
           . " were not re-measured by this run\n" if !$board;
