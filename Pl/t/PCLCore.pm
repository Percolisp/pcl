# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

package PCLCore;
# Optional saved-core acceleration for the test suite.
#
# `--load`ing the runtime SOURCE recompiles it (~1.2s) on EVERY sbcl spawn, and
# the big transpile-test files spawn sbcl once per test — so runtime compilation
# dominates the gate.  A saved core (an SBCL image with the runtime already
# compiled in) drops that to ~0.003s.
#
# This module is a pure CONSUMER: it never builds a core itself.  Two cores
# can serve it: PCL_TEST_CORE, built FRESH once per run by tools/prove-core
# (sbcl_prefix refuses one older than the runtime, so a hand-set stale core
# can never mask an edit), and -- since s439, the DEFAULT when PCL_TEST_CORE
# is unset -- the CACHED core tools/lib/PCLSbcl.pm builds on first use and
# names by a hash of the runtime source + SBCL version (content-keyed, so it
# cannot be stale either).  Plain `prove -j8 Pl/t/` runs at prove-core speed.
use strict;
use warnings;
use File::Basename qw(dirname);
use File::Temp qw(tempfile);
use lib dirname(__FILE__) . "/../../tools/lib";
use PCLSbcl ();

# sbcl args that go BETWEEN `sbcl` and the caller's `--load <cl_file>`:
#   core mode:   --core <core> --control-stack-size 512 --noinform --non-interactive
#   source mode: --control-stack-size 512 --noinform --non-interactive --load <runtime>
#
# The prefix itself is BUILT IN ONE PLACE for all five runners that spawn SBCL
# (tools/lib/PCLSbcl.pm, task #344) — the gate reaches it through here, so the
# ~40 Pl/t files calling sbcl_prefix() need no change.  What stays here is the
# gate's own contract: the core comes from PCL_TEST_CORE, freshness-checked.
sub sbcl_prefix {
    my ($runtime) = @_;
    return PCLSbcl::sbcl_prefix(runtime => $runtime, env_core => 1);
}

# transpile("$pl2cl [flags] $pl_file") -> the generated CL, and NOTHING else;
# transpile_raw(...) -> ($cl, $stderr, $exit) with no verdict at all (#355).
#
# The idiom this replaces was `my $cl = \`$pl2cl $file 2>&1\`` in ~40 gate
# files: transpile stderr went straight into the .lisp the test then loads.
# Nothing warns there today, so the gate is green — but the day a snippet
# DROPS a statement, the row dies with a Lisp READER error on the announcement
# line instead of saying "a statement was dropped", and a transpile error is
# just as opaque.  Silencing that noise is what the two blanket
# $SIG{__WARN__} handlers deleted in s402 were for.
#
# So: stderr is captured SEPARATELY, never into the CL, and it is JUDGED.
#   * `PCL: statement dropped …` (the fixed prefix task #339 gives every drop)
#     FAILS the row — a dropped statement inside a gate snippet is a compiler
#     bug, and the gate is the right place to catch it (rule 12's spirit: the
#     sin is the silence).
#   * a nonzero pl2cl exit FAILS the row, with stderr as the diagnostic.
#   * anything else on stderr is passed through as a diag and changes no
#     verdict — a warning is information, not a failure.
# A failure here also breaks the file's plan count, which is deliberate: two
# loud signals rather than a row that reads as a content mismatch.
#
# A caller that MEANS to read stderr — a row asserting that pl2cl REFUSES
# something perl-shaped — asks for transpile_raw() below, which judges nothing.
sub transpile_raw {
    my ($cmd) = @_;
    my ($efh, $errfile) = tempfile(SUFFIX => '.err', UNLINK => 1);
    close $efh;
    my $cl  = `$cmd 2>$errfile`;
    my $rc  = $?;
    my $err = do { local $/; my $fh; open($fh, '<', $errfile) ? <$fh> : '' };
    return ($cl, (defined $err ? $err : ''), $rc);
}

sub transpile {
    my ($cmd) = @_;
    my ($cl, $err, $rc) = transpile_raw($cmd);

    my @drops = grep { /^PCL: statement dropped/ } split /\n/, $err;
    if ($rc != 0) {
        Test::More::fail("transpile FAILED (exit " . ($rc >> 8) . "): $cmd");
        Test::More::diag($err) if length $err;
    }
    elsif (@drops) {
        Test::More::fail("transpile DROPPED a statement: $cmd");
        Test::More::diag($_) for @drops;
    }
    elsif (length $err) {
        Test::More::diag("transpile stderr: $err");
    }
    return $cl;
}

1;
