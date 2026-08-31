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
#   - tools/install-matrix/Dockerfile (the local harness, via
#     tools/install-matrix.pl, the day a container runtime exists locally)
#
# The recipe is ci.yml's, minus `sudo` (a container runs as root) and plus
# the packages a bare image lacks that the hosted runner preinstalls
# (make/gcc for cpanm, curl, bzip2 for the SBCL tarball, git for checkout
# parity).  Rationale for each choice lives in .github/workflows/ci.yml's
# header comment: PPI comes from CPAN because apt's 1.277 is under the 1.291
# floor; SBCL is sbcl.org's pinned binary because the distro's is under the
# 2.5.2 floor; Data::Dump + Try::Tiny are gate-row FIXTURES, not deps.
# SBCL_VERSION is caller-chosen because the 2.6.0 binary needs glibc >= 2.38
# (first-run fact): older bases get 2.5.2, the validated floor (glibc 2.34).
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
PPI_MIN="${PPI_MIN:-1.291}"
PREFIX="${PREFIX:-$HOME/.local}"

[ -x tools/install-pcl ] || {
    echo "install-and-verify.sh: run me from the PCL repo root" >&2
    exit 2
}

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

echo "== tools/install-pcl --prefix $PREFIX (self-verifying) =="
tools/install-pcl --prefix "$PREFIX"

echo "== installed tools on PATH transpile and run a program =="
export PATH="$PREFIX/bin:$PATH"
printf 'my @w = map { uc } qw(pcl works); print "@w\n";\n' > /tmp/hello.pl
runpcl /tmp/hello.pl
runpcl /tmp/hello.pl | grep -qx 'PCL WORKS'

echo "== a utf8 source transpiles and runs (the locale gotcha, exercised) =="
cat > /tmp/utf8.pl <<'EOF'
use utf8;
my $str = "värde";
print $str, "\n";
EOF
runpcl /tmp/utf8.pl | grep -q 'rde'

echo "== installer end-to-end test =="
prove tools/t/install-pcl.t

echo "== install-and-verify: PASS =="
