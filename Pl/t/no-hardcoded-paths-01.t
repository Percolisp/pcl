#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# no-hardcoded-paths-01.t — hand-written sources must carry no absolute
# home-directory path (task #278, the v0.1 release gate's cheapest half).
#
# PCL is developed in one checkout on one machine, so a path like
# /home/<user>/perl5/perlbrew/... works forever HERE and fails on the first
# machine that is not this one.  They accumulate silently: nothing runs on
# another machine, so nothing complains.  The known cases when this file
# landed were cl/pcl-test.lisp's which_perl (a literal perlbrew perl, task
# #207) and three tools defaulting to the author's perl build tree.
#
# THE RULE: a path outside the checkout is DERIVED at runtime (FindBin,
# %Config, an env override with a documented name — tools/lib/PCLPaths.pm is
# where that lives) or supplied at install time.  Never written down.
#
# COVERED SINCE s404 (task #349 closed #217): the checked-in TRANSPILED
# ARTIFACTS (cl/pcl-pack.lisp, cl/pcl-mro.lisp, cl/pcl-warnings.lisp) are
# scanned like any hand-written file.  They used to be EXCLUDED because their
# preamble embedded the build machine's @INC — which turned out to be the whole
# of #217's problem: they carried a PROGRAM preamble an extension never needed,
# and it reset the running program's @INC on load.  `pl2cl --extension` emits
# no preamble, so there is nothing left to excuse.  They are still identified
# BY THEIR gen STAMP (the discovery rule of Pl/t/artifact-staleness-01.t,
# normative in docs/ir-spec.md §9.2) and counted, so the day one of them
# regrows a machine path this test says which file and why.
#
# Files are read as BYTES and matched with a plain regex: `grep` prints
# nothing at all for a file it decides is binary, which is exactly how the
# s399 #323 census missed three of perl's regex test files (see
# baselines/perl-suite-run.tsv's s400 header note).  A guard must not have that
# blind spot.

use v5.30;
use strict;
use warnings;
use Test::More;
use FindBin qw($RealBin);
use File::Find;

my $root = "$RealBin/../..";

# An absolute path into somebody's home directory, on Linux or macOS.
my $HOME_PATH = qr{/(?:home|Users)/[A-Za-z0-9._-]+/};

# The scope of the invariant: the compiler, the runtime, the shipped module
# shims, the tools, and the runners at the repo root.
my @DIRS  = qw(Pl tools cl lib);
my @FILES = qw(pl2cl runpcl runt clt sweep-perl-tests.pl);

my (@offenders, @artifacts, $scanned);

sub scan_file {
  my ($path) = @_;
  open my $fh, '<:raw', $path or die "cannot read $path: $!";
  my $first = <$fh>;
  # A transpiled artifact announces itself in line 1 — same rule as
  # artifact-staleness-01.t, and the same promise: docs/ir-spec.md §9.2 fixes
  # the stamp's format.  It is COUNTED here, not skipped (task #349): a
  # generated file has no more licence to carry this machine's paths than a
  # hand-written one, now that `pl2cl --extension` gives it none.
  push @artifacts, rel($path)
    if defined $first && $first =~ /^;;;\s*pcl:\s*pipeline=\S+\s+gen=\S+/;
  $scanned++;
  my $line = 1;
  for my $chunk (defined $first ? ($first) : (), <$fh>) {
    push @offenders, sprintf('%s:%d: %s', rel($path), $line, trim($chunk))
      if $chunk =~ $HOME_PATH;
    $line++;
  }
  close $fh;
}

sub rel  { my $p = shift; $p =~ s{^\Q$root\E/}{}; $p }
sub trim { my $s = shift // ''; $s =~ s/\s+$//; $s =~ s/^\s+//;
           length($s) > 90 ? substr($s, 0, 90) . '...' : $s }

for my $dir (@DIRS) {
  find({ no_chdir => 1,
         wanted   => sub { scan_file($File::Find::name) if -f $File::Find::name } },
       "$root/$dir");
}
scan_file("$root/$_") for grep { -f "$root/$_" } @FILES;

ok($scanned > 100, "scanned the source tree ($scanned files)")
  or BAIL_OUT('found almost no files — this test cannot check anything');

is(scalar @offenders, 0, 'no hand-written source carries an absolute home path')
  or diag("hard-coded paths (derive them, or move them to an env override —\n"
        . "see tools/lib/PCLPaths.pm):\n  " . join("\n  ", @offenders));

# Nothing is excluded any more.  The count is pinned anyway: it says the three
# artifacts are still discovered by their stamp AND went through the scan above
# (a regression that re-introduced a preamble would fail the offenders row, not
# this one — this row is what tells you the scan saw them at all).
is(scalar @artifacts, 3,
   'the three transpiled artifacts are scanned like any other source (#349)')
  or diag("stamped files scanned: " . join(', ', sort @artifacts));

done_testing();
