#!/usr/bin/env perl
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
# `*pcl-cache-generation*` in cl/pcl-runtime.lisp.  Drift is a mismatch, so
# this file is three greps and no SBCL spawn — the session that bumps the
# generation without regenerating gets a red row instead of a silent debt.
#
# TO FIX A FAILING ROW HERE (do not edit this test):
#   cl/pcl-pack.lisp   tools/rebuild-pack
#                      then  perl sweep-perl-tests.pl --jobs 1 --timeout 380 \
#                              perl-tests/pack.t   &&  tools/sweep-diff.pl
#   cl/pcl-mro.lisp    ./pl2cl lib/mro.pm > cl/pcl-mro.lisp   (pl2cl exits 0
#                      with EMPTY output on a compile error — check the size)
#   cl/pcl-warnings.lisp
#                      ./pl2cl lib/warnings.pm > cl/pcl-warnings.lisp

use v5.30;
use strict;
use warnings;
use Test::More;
use FindBin qw($RealBin);

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

done_testing();
