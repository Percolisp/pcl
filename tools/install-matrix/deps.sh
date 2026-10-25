#!/usr/bin/env bash
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# deps.sh — the DEPENDENCY half of the fresh-machine recipe (tasks #876, #1304).
#
# Everything a stock Ubuntu/Debian image needs before PCL can be installed:
# apt packages (PPI itself is vendored in the tree -- vendor/PPI, s527a -- so
# only its five non-core dependencies are installed here: Clone, Params::Util,
# Task::Weaken, Safe::Isa and YAML::PP -- the last is upstream master's hard
# `use YAML::PP` in PPI::Document, found by the first red run of MAIN-READY-48)
# and the pinned
# SBCL binary from sbcl.org.  There is NO Lisp-library step: PCL vendors
# cl-ppcre (cl/vendor/cl-ppcre/, task #1597), so SBCL itself is the whole Lisp
# side and the Quicklisp install this file used to do is gone.
# Nothing here knows about
# PCL's own installer — that is verify.sh, and the two together are
# install-and-verify.sh, which is what .github/workflows/install-matrix.yml
# runs (unchanged).  The split exists so the LOCAL container test
# (tools/t/install-container.t) can bake this half into an image once and
# re-run the verify half in seconds, instead of re-downloading SBCL per
# attempt — one recipe, two consumers (rule 11).
#
# Run as root, from anywhere.  SBCL lands under $HOME/sbcl, so the caller
# decides WHOSE it is: the CI matrix runs as root and gets root's; the
# container image builds with HOME pointed at a world-readable directory so
# every user in the image can use it.
#
# Rationale for each choice lives in .github/workflows/ci.yml's header comment:
# PPI is vendored, so apt brings only its dependencies; SBCL is
# sbcl.org's pinned binary because the distro's is under the 2.5.2 floor;
# Data::Dump + Try::Tiny are gate-row FIXTURES, not deps.  SBCL_VERSION is
# caller-chosen because the 2.6.0 binary needs glibc >= 2.38 (first-run fact):
# older bases get 2.5.2, the validated floor (glibc 2.34).
#
# Container gotchas this file settles: locales (C.UTF-8 is built into both
# distros — no `locales` package needed), tzdata's interactive prompt
# (DEBIAN_FRONTEND), no /dev/tty (nothing here reads one), no perlbrew
# (tools/lib/PCLPaths.pm derives paths).

set -euo pipefail

export DEBIAN_FRONTEND=noninteractive
export LANG=C.UTF-8
export LC_ALL=C.UTF-8

SBCL_VERSION="${SBCL_VERSION:-2.6.0}"

echo "== distro =="
. /etc/os-release && echo "$PRETTY_NAME"

echo "== apt packages =="
apt-get update -q
apt-get install -qy --no-install-recommends \
    perl cpanminus make gcc curl ca-certificates bzip2 git \
    libclone-perl libparams-util-perl libtask-weaken-perl libsafe-isa-perl libyaml-pp-perl \
    libmoo-perl libdata-dump-perl libtry-tiny-perl

echo "== PPI is vendored (vendor/PPI); tools/install-pcl checks that it loads =="
perl -MMoo -e 'print "Moo $Moo::VERSION\n"'

echo "== SBCL $SBCL_VERSION (pinned binary from sbcl.org) =="
curl -fsSL -o /tmp/sbcl.tar.bz2 \
    "https://downloads.sourceforge.net/project/sbcl/sbcl/${SBCL_VERSION}/sbcl-${SBCL_VERSION}-x86-64-linux-binary.tar.bz2"
tar -xjf /tmp/sbcl.tar.bz2 -C /tmp
( cd "/tmp/sbcl-${SBCL_VERSION}-x86-64-linux" && INSTALL_ROOT="$HOME/sbcl" sh install.sh )
export PATH="$HOME/sbcl/bin:$PATH"
export SBCL_HOME="$HOME/sbcl/lib/sbcl"
sbcl --version

echo "== deps: PASS =="
