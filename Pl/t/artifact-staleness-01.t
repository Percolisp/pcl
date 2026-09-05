#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# artifact-staleness-01.t — the CHECKED-IN TRANSPILED ARTIFACTS must be built
# by the CURRENT compiler (task #331).
#
# `cl/pcl-pack.lisp` (from `cl/pack-impl.pl`), `cl/pcl-mro.lisp` (from
# `lib/mro.pm`) and `cl/pcl-warnings.lisp` (from `lib/warnings.pm`) are PCL
# output checked into the tree, and the runtime LOADS them — so
# `tools/corpus-diff.pl` cannot see them and nothing else notices
# when they fall behind the emitter that is supposed to have produced them.
# It has now happened twice: gen v2-30 against a v2-71 compiler (s316, 40
# generations), and gen v2-136 against v2-147 (s396, eleven).  Both times
# every pack.t / mro run in between was silently testing the OLD emitter.
#
# Each artifact stamps its generation in line 1
# (`;;; pcl: pipeline=v2 gen=v2-NNN`); the compiler's is
# `*pcl-cache-generation*` in cl/pcl-runtime.lisp.  That stamp is a PROMISE,
# not an incidental of today's emitter — `docs/ir-spec.md` §9.2 states its
# format normatively, and this row is one of the two consumers it names.
# Drift is a mismatch, so the session that bumps the generation without
# regenerating gets a red row instead of a silent debt.
#
# The stamp is only HALF the promise (task #1072).  It cannot see an artifact
# whose stamp is CURRENT while its body is not what today's compiler emits,
# and that is a measured event, not a hypothetical: on main `9bee19b`
# (s468bc) cl/pcl-pack.lisp carried the tree's own generation and this file
# was green, yet regenerating it produced a body ~134 lines different.  Every
# pack.t run in between was measuring an emitter the tree no longer had.  So
# the second half of this file REGENERATES each artifact into a temp file and
# compares the body.
#
# TO FIX A FAILING ROW HERE (do not edit this test).  The one-liners are also
# in %RECIPE below — that is the copy a failing row prints, and the copy the
# file itself RUNS; what is here is the extra context (the verification bar,
# the pitfalls) that does not fit on a diagnostic line:
#   cl/pcl-pack.lisp   tools/rebuild-pack
#                      then  perl sweep-perl-tests.pl --jobs 1 --timeout 380 \
#                              perl-tests/pack.t   &&  tools/sweep-diff.pl
#   cl/pcl-mro.lisp    ./pl2cl --extension lib/mro.pm > cl/pcl-mro.lisp
#                      (pl2cl exits 0 with EMPTY output on a compile error —
#                      check the size.  --extension = no program preamble,
#                      task #349; Pl/t/extension-preamble-01.t guards it)
#   cl/pcl-warnings.lisp
#                      ./pl2cl --extension lib/warnings.pm > cl/pcl-warnings.lisp
#   BOTH of the above then need  tools/tag-license FILE  — the transpiler does
#   not emit the license header the tree's own gate (Pl/t/license-tag-01.t)
#   requires, so a regeneration without it turns one red row into two (s414).

use v5.30;
use strict;
use warnings;
use Test::More;
use FindBin qw($RealBin);
use File::Temp qw(tempdir);

my $root = "$RealBin/../..";

# --- the compiler's generation -------------------------------------------
my $runtime = "$root/cl/pcl-runtime.lisp";
open my $rfh, '<', $runtime or die "cannot read $runtime: $!";
my $compiler_gen;
while (my $line = <$rfh>) {
  if ($line =~ /^\(defparameter\s+\*pcl-cache-generation\*\s+"([^"]+)"/) {
    $compiler_gen = $1;
    last;
  }
}
close $rfh;

ok(defined $compiler_gen && length $compiler_gen,
   'cl/pcl-runtime.lisp declares *pcl-cache-generation*')
  or BAIL_OUT('*pcl-cache-generation* not found — this test cannot check anything');

# --- every artifact that stamps one ---------------------------------------
# Discovered, not listed: a future third artifact is covered the day it lands.
my @artifacts;
for my $path (sort glob("$root/cl/*.lisp")) {
  open my $fh, '<', $path or die "cannot read $path: $!";
  my $first = <$fh>;
  close $fh;
  next unless defined $first;
  next unless $first =~ /^;;;\s*pcl:\s*pipeline=\S+\s+gen=(\S+)/;
  my $gen = $1;            # before the substitution below resets $1
  my $rel = $path;
  $rel =~ s{^\Q$root\E/}{};
  push @artifacts, { path => $rel, gen => $gen };
}

# A vacuous pass is the failure mode this file exists to prevent: if the
# header format ever changes, @artifacts goes empty and NO row runs.
is(scalar @artifacts, 3,
   'found the three checked-in transpiled artifacts by their gen stamp')
  or diag("found: " . join(', ', map { $_->{path} } @artifacts));

for my $a (@artifacts) {
  is($a->{gen}, $compiler_gen,
     "$a->{path} was built by the current compiler (gen $compiler_gen)")
    or diag("$a->{path} is stamped $a->{gen}; regenerate it — see the "
            . "header of this test file for the exact procedure and its "
            . "verification bar.");
}

# --- the BODY, not only the stamp (task #1072) ----------------------------
# Each artifact is rebuilt into a temp file BY THE DOCUMENTED TOOL — never by
# a copy of the recipe spelled out here, which would be free to drift from the
# recipe it is checking — and every line after line 1 must match.  Line 1 is
# the gen stamp, which the rows above own.
#
# NOTHING is normalised, because nothing legitimately varies.  `--extension`
# emits no program preamble (task #349), so no absolute path reaches an
# artifact in the first place — Pl/t/extension-preamble-01.t is the row that
# guards that, and it is why task #217's build-machine paths are gone.  What
# DOES depend on the caller is the source path AS SPELLED: a `die` in
# lib/mro.pm becomes the literal string "lib/mro.pm line 71" in the emitted
# CL, so the regeneration runs with cwd = the repo root and the same relative
# spelling the recipe uses.  With that fixed, two runs are byte-equal.
#
# Cost: one `pl2cl --extension` per artifact (~0.2 s each for mro/warnings,
# ~2.5 s for pack).  Everything it needs is inside the checkout plus perl,
# PPI, Moo and sbcl — the dependency set Pl/t/core-deps-01.t pins.

# Recipe per artifact.  An artifact the stamp loop discovers and this table
# does not classify DIES naming it (rule 12) — silently not checking a new
# artifact is exactly the hole this file exists to close.
my %RECIPE = (
  'cl/pcl-pack.lisp' => {
    build => sub { run_at_root('./tools/rebuild-pack', '-o', $_[0]) },
    fix   => 'tools/rebuild-pack',
  },
  'cl/pcl-mro.lisp' => {
    build => sub { build_extension('lib/mro.pm', $_[0]) },
    fix   => './pl2cl --extension lib/mro.pm > cl/pcl-mro.lisp'
             . ' && tools/tag-license cl/pcl-mro.lisp',
  },
  'cl/pcl-warnings.lisp' => {
    build => sub { build_extension('lib/warnings.pm', $_[0]) },
    fix   => './pl2cl --extension lib/warnings.pm > cl/pcl-warnings.lisp'
             . ' && tools/tag-license cl/pcl-warnings.lisp',
  },
);

my $tmpdir = tempdir(CLEANUP => 1);   # never in the tree
my $n = 0;
for my $a (@artifacts) {
  my $rel = $a->{path};
  my $recipe = $RECIPE{$rel}
    or die "artifact-staleness-01.t: no regeneration recipe for $rel — it "
           . "stamps a generation, so something builds it; add it to \%RECIPE "
           . "(and to CLAUDE.md's generated-artifacts paragraph) or this file "
           . "silently stops checking it.\n";

  my $fresh = "$tmpdir/fresh-" . ++$n . ".lisp";
  $recipe->{build}->($fresh);

  my $have = body_of("$root/$rel");        # what is checked in
  my $want = body_of($fresh);              # what today's compiler emits
  my $at   = first_difference($have, $want);

  ok(!defined $at, "$rel body matches what the current compiler emits")
    or diag("$rel is BODY-stale within its own generation stamp.\n"
            . "  first difference at line " . ($at + 2) . " of $rel\n"
            . "  regenerate with:  $recipe->{fix}\n"
            . diff_head($have, $want, $at, 40));
}

done_testing();

# --- helpers ---------------------------------------------------------------

# Run a command with cwd = the repo root and capture its stdout (which must
# never reach TAP's stdout: tag-license and rebuild-pack both print).  Dies on
# a non-zero exit — a regeneration that failed cannot answer the question.
sub run_at_root {
  my (@cmd) = @_;
  my $pid = open(my $ph, '-|');
  defined $pid or die "artifact-staleness-01.t: fork: $!\n";
  if (!$pid) {                                     # child
    chdir $root or die "artifact-staleness-01.t: chdir $root: $!\n";
    exec { $cmd[0] } @cmd
      or die "artifact-staleness-01.t: exec $cmd[0]: $!\n";
  }
  my $out = do { local $/; <$ph> };
  close $ph;
  die "artifact-staleness-01.t: @cmd failed (status $?)\n"
      . (defined $out && length $out ? "  output: $out" : '')
    if $?;
  return $out;
}

# The mro/warnings recipe: pl2cl --extension, then the license tag (the
# transpiler does not emit it; without it one red row becomes two, s414).
sub build_extension {
  my ($source, $dest) = @_;
  my $cl = run_at_root('./pl2cl', '--extension', $source);
  # pl2cl exits 0 with EMPTY output when a Pl module fails to compile.
  die "artifact-staleness-01.t: pl2cl --extension $source produced no output\n"
    unless defined $cl && length $cl;
  open my $fh, '>:raw', $dest or die "artifact-staleness-01.t: $dest: $!\n";
  print {$fh} $cl;
  close $fh;
  run_at_root('./tools/tag-license', $dest);
  return;
}

# Everything after line 1.  For cl/pcl-pack.lisp that includes the
# hand-written appendix, which rebuild-pack copies out of the checked-in file
# itself — so it compares equal by construction and only the generated part
# can move the row.
sub body_of {
  my ($path) = @_;
  open my $fh, '<:raw', $path or die "artifact-staleness-01.t: $path: $!\n";
  my @lines = <$fh>;
  close $fh;
  shift @lines;
  return \@lines;
}

sub first_difference {
  my ($have, $want) = @_;
  my $max = @$have > @$want ? @$have : @$want;
  for my $i (0 .. $max - 1) {
    my $h = $i < @$have ? $have->[$i] : undef;
    my $w = $i < @$want ? $want->[$i] : undef;
    return $i if !defined $h || !defined $w || $h ne $w;
  }
  return undef;
}

# A bounded head starting at the first difference: '-' is the checked-in file,
# '+' is what the compiler emits now, ' ' is a line the two agree on.  Lines
# are compared position by position (not an LCS diff), so an INSERTED line
# makes everything below it read as changed — which is honest for a pointer at
# the first divergence, and why the head stops three agreeing lines after the
# last one it printed.
sub diff_head {
  my ($have, $want, $at, $limit) = @_;
  my @out;
  my $from = $at > 2 ? $at - 2 : 0;
  push @out, "  " . rtrim($have->[$_]) for $from .. $at - 1;
  my $max = @$have > @$want ? @$have : @$want;
  my $i = $at;
  my $agreeing = 0;
  while ($i < $max && @out < $limit && $agreeing < 3) {
    my $h = $i < @$have ? $have->[$i] : undef;
    my $w = $i < @$want ? $want->[$i] : undef;
    if (defined $h && defined $w && $h eq $w) {
      push @out, "  " . rtrim($h);
      $agreeing++;
    }
    else {
      push @out, "- " . rtrim($h) if defined $h;
      push @out, "+ " . rtrim($w) if defined $w;
      $agreeing = 0;
    }
    $i++;
  }
  push @out, "  ... (head truncated at $limit lines; " . ($max - $i)
             . " lines not shown)"
    if @out >= $limit && $i < $max;
  return join("\n", "  --- checked in / +++ regenerated ---", @out);
}

sub rtrim { my ($s) = @_; $s //= ''; $s =~ s/\s+\z//; return $s }
