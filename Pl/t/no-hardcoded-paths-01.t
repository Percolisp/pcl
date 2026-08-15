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
# NOT COVERED, deliberately: the checked-in TRANSPILED ARTIFACTS
# (cl/pcl-pack.lisp, cl/pcl-mro.lisp, cl/pcl-warnings.lisp).  Their preamble
# embeds the BUILD machine's @INC because pl2cl emits it for every file it
# compiles; making that preamble relocatable is task #217 and an emission
# change, not a hygiene edit.  They are excluded BY THEIR gen STAMP, the same
# discovery rule Pl/t/artifact-staleness-01.t uses — and counted, so the
# exclusion cannot silently widen to cover a hand-written file.
#
# Files are read as BYTES and matched with a plain regex: `grep` prints
# nothing at all for a file it decides is binary, which is exactly how the
# s399 #323 census missed three of perl's regex test files (see
# docs/perl-suite-run.tsv's s400 header note).  A guard must not have that
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
  # artifact-staleness-01.t, and the same promise: docs/ir-spec.md §9.2
  # fixes the stamp's format, which is what lets this guard tell a GENERATED
  # file (whose machine paths are the emitter's, task #217/#349) apart from a
  # hand-written source file (whose machine path is a bug).
  # Excluded, and counted (task #217 owns them).
  if (defined $first && $first =~ /^;;;\s*pcl:\s*pipeline=\S+\s+gen=\S+/) {
    close $fh;
    push @artifacts, rel($path);
    return;
  }
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

# The exclusion is the only way a real offender could hide, so it is pinned.
is(scalar @artifacts, 3,
   'exactly three files are excluded, all of them transpiled artifacts (#217)')
  or diag("excluded: " . join(', ', sort @artifacts));

done_testing();
