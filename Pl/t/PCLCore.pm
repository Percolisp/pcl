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
# This module is a pure CONSUMER: it never builds a core.  The core is built
# FRESH, once per run, by tools/prove-core, which points PCL_TEST_CORE at it.
# Building every run (rather than caching one across runs) is deliberate — a
# stale core would silently run tests against OLD runtime code.  As a second
# guard, sbcl_prefix refuses a core older than the runtime and falls back to
# source-load, so a hand-set PCL_TEST_CORE can never mask a runtime edit.
use strict;
use warnings;
use File::Basename qw(dirname);
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

1;
