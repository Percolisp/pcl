#!/usr/bin/env bash
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# install-and-verify.sh — the ONE recipe for a fresh-machine PCL install
# (task #876).  Run from the PCL repo root, as root, on a stock Ubuntu or
# Debian image.  Both consumers run exactly this file:
#
#   - .github/workflows/install-matrix.yml, inside a `container:` job over
#     ubuntu:22.04 / ubuntu:24.04 / debian:12 / debian:13
#   - tools/install-matrix/Dockerfile (the local harness)
#
# SINCE s470bz IT IS TWO HALVES (task #1304, rule 11): deps.sh (apt, PPI from
# CPAN, the pinned SBCL — no Lisp-library step since PCL vendors cl-ppcre,
# task #1597) and verify.sh (from
# tools/install-pcl on).  This file is the two in sequence and nothing else,
# so the workflow's step invocation is unchanged.  The split exists because
# the LOCAL container test (tools/t/install-container.t) bakes the dependency
# half into an image ONCE and re-runs the verify half in seconds — the
# difference between a push per attempt and a run per attempt.
#
# Each half sets its own environment (a child process does not inherit an
# exported PATH from its sibling), which is why verify.sh picks up
# $HOME/sbcl/bin itself when deps.sh put SBCL there.  The rationale for every
# dependency choice lives in deps.sh's header and in .github/workflows/ci.yml.

set -euo pipefail

here="$(cd "$(dirname "$0")" && pwd)"

"$here/deps.sh"
"$here/verify.sh"

echo "== install-and-verify: PASS =="
