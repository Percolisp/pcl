#!/usr/bin/env bash
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# deps.sh — the DEPENDENCY half of the fresh-machine recipe (tasks #876, #1304).
#
# Everything a stock Ubuntu/Debian image needs before PCL can be installed:
# apt packages, PPI from CPAN (apt's is under the 1.291 floor), the pinned
# SBCL binary from sbcl.org, Quicklisp and cl-ppcre.  Nothing here knows about
# PCL's own installer — that is verify.sh, and the two together are
# install-and-verify.sh, which is what .github/workflows/install-matrix.yml
# runs (unchanged).  The split exists so the LOCAL container test
# (tools/t/install-container.t) can bake this half into an image once and
# re-run the verify half in seconds, instead of re-downloading SBCL per
# attempt — one recipe, two consumers (rule 11).
#
# Run as root, from anywhere.  SBCL lands under $HOME/sbcl and Quicklisp under
# $HOME/quicklisp, so the caller decides WHOSE they are: the CI matrix runs as
# root and gets root's; the container image builds with HOME pointed at a
# world-readable directory so every user in the image can use them.
#
# Rationale for each choice lives in .github/workflows/ci.yml's header comment:
# PPI comes from CPAN because apt's 1.277 is under the 1.291 floor; SBCL is
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
# EXPORTED because the floor check below is a child perl reading $ENV{PPI_MIN}:
# unexported, that comparison was against undef and could not fail (the CI
# workflow exports it as a job env, which is why it worked there).
export PPI_MIN="${PPI_MIN:-1.291}"

echo "== distro =="
. /etc/os-release && echo "$PRETTY_NAME"

echo "== apt packages =="
apt-get update -q
apt-get install -qy --no-install-recommends \
    perl cpanminus make gcc curl ca-certificates bzip2 git \
    libppi-perl libmoo-perl libdata-dump-perl libtry-tiny-perl

echo "== PPI >= $PPI_MIN from CPAN (apt's is under the floor) =="
# apt's PPI brings the dependency chain as packages; cpanm then puts the
# current PPI in /usr/local, which precedes /usr/share in @INC.
cpanm --notest --quiet PPI
perl -MPPI -e 'print "PPI $PPI::VERSION\n"; exit($PPI::VERSION >= $ENV{PPI_MIN} ? 0 : 1)'
perl -MMoo -e 'print "Moo $Moo::VERSION\n"'

echo "== SBCL $SBCL_VERSION (pinned binary from sbcl.org) =="
curl -fsSL -o /tmp/sbcl.tar.bz2 \
    "https://downloads.sourceforge.net/project/sbcl/sbcl/${SBCL_VERSION}/sbcl-${SBCL_VERSION}-x86-64-linux-binary.tar.bz2"
tar -xjf /tmp/sbcl.tar.bz2 -C /tmp
( cd "/tmp/sbcl-${SBCL_VERSION}-x86-64-linux" && INSTALL_ROOT="$HOME/sbcl" sh install.sh )
export PATH="$HOME/sbcl/bin:$PATH"
export SBCL_HOME="$HOME/sbcl/lib/sbcl"
sbcl --version

echo "== Quicklisp + cl-ppcre =="
curl -fsSL -o /tmp/quicklisp.lisp https://beta.quicklisp.org/quicklisp.lisp
sbcl --non-interactive --load /tmp/quicklisp.lisp \
     --eval '(quicklisp-quickstart:install)' \
     --eval '(ql-util:without-prompting (ql:add-to-init-file))'
sbcl --non-interactive --eval '(ql:quickload :cl-ppcre)'

echo "== deps: PASS =="
