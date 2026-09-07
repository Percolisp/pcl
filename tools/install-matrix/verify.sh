#!/usr/bin/env bash
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# verify.sh — the VERIFY half of the fresh-machine recipe (tasks #876, #1304).
#
# From tools/install-pcl on: install PCL, run a program with the installed
# tools on PATH, transpile and run a utf8 source (the locale gotcha), and run
# the installer's own end-to-end test.  Run from the PCL repo root, after
# deps.sh (or on any machine that already has the dependencies).
#
# The dependency half is deps.sh; install-and-verify.sh is the two in
# sequence, and that is what .github/workflows/install-matrix.yml runs.  The
# local container test (tools/t/install-container.t) bakes deps.sh into an
# image and runs THIS file as its root leg, so a matrix failure can be
# reproduced without a push — one recipe, two consumers (rule 11).

set -euo pipefail

export DEBIAN_FRONTEND=noninteractive
export LANG=C.UTF-8
export LC_ALL=C.UTF-8

PREFIX="${PREFIX:-$HOME/.local}"

[ -x tools/install-pcl ] || {
    echo "verify.sh: run me from the PCL repo root" >&2
    exit 2
}

# deps.sh installs SBCL under its own $HOME and exports PATH/SBCL_HOME — which
# a child process does not inherit, since the two halves are separate scripts.
# Pick it up when it is there; leave PATH alone when sbcl is already on it
# (a developer machine, or a container image with its own /usr/local/bin/sbcl).
if [ -x "$HOME/sbcl/bin/sbcl" ]; then
    export PATH="$HOME/sbcl/bin:$PATH"
    export SBCL_HOME="$HOME/sbcl/lib/sbcl"
fi

echo "== tools/install-pcl --prefix $PREFIX (self-verifying) =="
tools/install-pcl --prefix "$PREFIX"

echo "== installed tools on PATH transpile and run a program =="
export PATH="$PREFIX/bin:$PATH"
printf 'my @w = map { uc } qw(pcl works); print "@w\n";\n' > /tmp/hello.pl
runpcl /tmp/hello.pl
runpcl /tmp/hello.pl | grep -qx 'PCL WORKS'
pcl /tmp/hello.pl | grep -qx 'PCL WORKS'

echo "== a utf8 source transpiles and runs (the locale gotcha, exercised) =="
cat > /tmp/utf8.pl <<'EOF'
use utf8;
my $str = "värde";
print $str, "\n";
EOF
runpcl /tmp/utf8.pl | grep -q 'rde'

echo "== installer end-to-end test =="
prove tools/t/install-pcl.t

echo "== verify: PASS =="
