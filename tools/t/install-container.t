#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# install-container.t — the installer, in a container (task #1304).
#
# NOT part of the Pl/t gate, and not part of tools/t's quick set either: it is
# minutes.  Run it before a tag, and after any change to tools/install-pcl or
# tools/install-matrix/*:
#
#     prove tools/t/install-container.t
#
# WHY IT EXISTS.  tools/t/install-pcl.t installs into a temp prefix on THIS
# machine, as THIS user, with the developer's perl, SBCL and $HOME.  Three
# things it therefore cannot see, and all three are shapes a first user meets:
#
#   * a stock distro image, with none of a dev box's accumulated conveniences
#     — which is what .github/workflows/install-matrix.yml checks, on PUSH
#     only, as root only.  A push per attempt is the wrong loop for installer
#     work, so leg (a) runs the matrix's own recipe here.
#   * a NON-ROOT user installing into $HOME/.local (leg b).
#   * a SHARED install: root installs once into /opt/pcl and other users run
#     it, each with their own compiled-module cache (legs c and d).  That is
#     the #1303 shape — the cache directory has to be a fact of the RUNNING
#     process, not of whoever built the core — and no single-user rehearsal
#     reproduces it.
#
# HOW.  The dependency half of the shared recipe (tools/install-matrix/deps.sh)
# is baked into an image ONCE, tagged by a hash of that script plus the SBCL
# version plus this file's Containerfile, so an edit to any of them makes a new
# image rather than reusing a stale one.  Later runs cost the legs only.
#
# The image runs deps.sh with HOME pointed at a world-readable /opt/pcldeps
# instead of root's home: the recipe installs SBCL and Quicklisp under $HOME,
# and the non-root legs need both.  Same script, one variable different.
#
# Its /etc/sbclrc loads Quicklisp only `(unless (find-package "QUICKLISP-CLIENT"))`,
# and that guard is load-bearing, not tidiness.  A PCL core already CONTAINS
# Quicklisp and cl-ppcre (it was loaded to build the core), so re-entering ASDF
# at startup is pure waste — and worse than waste here, because a saved core
# memoises `asdf:*user-cache*` as the BUILDER's ~/.cache and ASDF then tries to
# compile into it whoever is running.  Leg (c) found that on its first run:
# root's core, pcluser's process, "Can't create directory /root/.cache".  It is
# a PCL bug, filed as task #1327 with a container-free reproducer; the guard is
# what an /etc/sbclrc should say anyway, and it keeps this file measuring the
# installation rather than that.
#
# The repo enters as a `git archive HEAD` extraction bind-mounted READ-ONLY at
# /src — never the live checkout, so nothing a leg does can touch the working
# tree, and what is tested is exactly what is committed.  Uncommitted work is
# NOT in it, and the file says so in a diag rather than letting you assume.

use strict;
use warnings;
use Test::More;
use Digest::SHA qw(sha1_hex);
use File::Path qw(make_path);
use File::Temp qw(tempdir);
use FindBin qw($RealBin);
use Cwd qw(abs_path);

my $root = abs_path("$RealBin/../..");
my $SBCL_VERSION = $ENV{SBCL_VERSION} // '2.6.0';
my $BASE_IMAGE   = $ENV{PCL_CONTAINER_BASE} // 'ubuntu:24.04';

plan skip_all => "PCL_INSTALL_CONTAINER_SKIP=1" if $ENV{PCL_INSTALL_CONTAINER_SKIP};

# podman first (rootless and daemonless — no `docker` group, which is
# root-equivalent membership on a dev box), docker accepted.  `info` rather
# than `--version`: a CI runner can have the client with no socket behind it,
# and that must skip, not fail.
my $RT;
for my $c (qw(podman docker)) {
    next unless `command -v $c 2>/dev/null`;
    system("$c info >/dev/null 2>&1");
    $RT = $c, last if $? == 0;
}
plan skip_all => "no container runtime: probed `podman info` then `docker info`, "
               . "neither answered" unless $RT;

plan tests => 17;

diag("container runtime: $RT");

# ---------------------------------------------------------------- the image
#
# The Containerfile is HERE, not in tools/install-matrix/: that directory holds
# the recipe CI shares, and this is this test's own scaffolding.  Its text is
# part of the image key, so editing it below rebuilds rather than reusing.
my $CONTAINERFILE = <<"DOCKERFILE";
FROM $BASE_IMAGE
ENV SBCL_VERSION=$SBCL_VERSION
COPY deps.sh /deps.sh
RUN mkdir -p /opt/pcldeps \\
 && HOME=/opt/pcldeps /deps.sh \\
 && chmod -R a+rX /opt/pcldeps \\
 && ln -sf /opt/pcldeps/sbcl/bin/sbcl /usr/local/bin/sbcl \\
 && printf '%s\\n' '(unless (find-package "QUICKLISP-CLIENT") (load "/opt/pcldeps/quicklisp/setup.lisp"))' > /etc/sbclrc \\
 && useradd -m -s /bin/bash pcluser
ENV SBCL_HOME=/opt/pcldeps/sbcl/lib/sbcl
DOCKERFILE

my $deps_path = "$root/tools/install-matrix/deps.sh";
ok(-f $deps_path, 'the dependency half of the shared recipe exists')
    or BAIL_OUT("no $deps_path — the recipe split (task #1304) is not in this tree");
my $deps_text = do { open my $fh, '<', $deps_path or die $!; local $/; <$fh> };

my $key = substr(sha1_hex(join("\0", $deps_text, $CONTAINERFILE, $SBCL_VERSION)), 0, 12);
my $tag = "pcl-install-base:$key";

my $built_now = 0;
system("$RT image inspect $tag >/dev/null 2>&1");
if ($? != 0) {
    my $ctx = tempdir(CLEANUP => 1);
    open my $d, '>', "$ctx/deps.sh" or die $!; print $d $deps_text; close $d;
    chmod 0755, "$ctx/deps.sh";
    open my $c, '>', "$ctx/Containerfile" or die $!; print $c $CONTAINERFILE; close $c;
    diag("building $tag from $BASE_IMAGE (first run: apt, cpanm PPI, SBCL $SBCL_VERSION, Quicklisp)");
    my $t0 = time;
    my $log = `$RT build -t $tag -f $ctx/Containerfile $ctx 2>&1`;
    my $rc = $?;
    $built_now = time - $t0;
    unless ($rc == 0) {
        diag(tail($log, 40));
        BAIL_OUT("the base image build FAILED — that is the shared recipe's dependency half failing on a stock $BASE_IMAGE");
    }
    diag("image built in ${built_now}s");
}
else { diag("reusing image $tag") }
pass("base image $tag is available" . ($built_now ? " (built in ${built_now}s)" : " (reused)"));

# ----------------------------------------------------------------- the repo
#
# `git archive HEAD`: what is COMMITTED, never the working tree.  World
# readable because rootless podman maps this user to container root and every
# other container user to a subuid that cannot read a 0700 tempdir.
my $dirty = `git -C $root status --porcelain --untracked-files=no 2>/dev/null`;
diag("NOTE: the checkout has uncommitted changes — this test runs `git archive HEAD`, "
   . "so they are NOT in what it tests") if defined $dirty && length $dirty;

my $src = tempdir(CLEANUP => 1);
system("git -C $root archive HEAD | tar -x -C $src") == 0
    or BAIL_OUT("could not extract `git archive HEAD` into $src");
system("chmod -R a+rX $src");
ok(-x "$src/tools/install-pcl", 'the archive extraction is a PCL tree');

my $legs = tempdir(CLEANUP => 1);
system("chmod -R a+rX $legs");

# Run one leg script inside a fresh container.  Returns (output, exit code);
# every leg's whole output becomes a diag when its rows fail, because a
# container failure is only diagnosable from what the container said.
sub leg {
    my ($name, $script, %opt) = @_;
    open my $fh, '>', "$legs/$name.sh" or die $!;
    print $fh $script;
    close $fh;
    chmod 0755, "$legs/$name.sh";
    my $t0 = time;
    my $out = `$RT run --rm -v $src:/src:ro -v $legs:/legs:ro -w /src $tag bash /legs/$name.sh 2>&1`;
    my $rc = $?;
    diag(sprintf("leg %s: %d s, exit %d", $name, time - $t0, $rc >> 8));
    return ($out, $rc);
}

sub tail { my ($s, $n) = @_; my @l = split /\n/, $s; return join "\n", @l[-$n .. -1] if @l > $n; return $s }

# ============================================================= (a) the matrix
# The verify half as root on a stock image — exactly what
# .github/workflows/install-matrix.yml runs, minus the dependency half the
# image already has.  One local run instead of a push per attempt.
{
    my ($out, $rc) = leg('lega', <<'SH');
set -euo pipefail
cd /src
bash tools/install-matrix/verify.sh
SH
    is($rc, 0, '(a) the shared recipe\'s verify half passes as root on a stock image')
        or diag(tail($out, 40));
    like($out, qr/verify: PASS/, '(a) and says so');
    like($out, qr/All tests successful|Result: PASS/,
         '(a) including the installer\'s own end-to-end test, run inside the container')
        or diag(tail($out, 25));
}

# ========================================================== (b) a normal user
# The shape a person actually has: not root, installing into $HOME/.local.
{
    my ($out, $rc) = leg('legb', <<'SH');
set -euo pipefail
su - pcluser -c '
  set -e
  cd /src
  tools/install-pcl --prefix "$HOME/.local"
  echo "--- installed, now running as $(id -un) ---"
  "$HOME/.local/bin/pcl" -e "print 40 + 2, qq{\n}"
  echo "print 1;" | "$HOME/.local/bin/pl2cl" > /dev/null && echo "pl2cl ok"
'
SH
    is($rc, 0, '(b) a NON-ROOT user can install into $HOME/.local')
        or diag(tail($out, 40));
    like($out, qr{export PATH="/home/pcluser/\.local/bin:\$PATH"},
         '(b) and is told the exact line that puts it on PATH') or diag(tail($out, 20));
    like($out, qr/^42$/m, '(b) and the installed pcl runs from that user\'s shell');
}

# ================================================ (c) + (d) a SHARED install
# root installs once into /opt/pcl; a different user runs it with an EMPTY
# home.  The module cache must appear in the USER's home — the core was
# compiled by root, and before #1303 the cache directory was baked in when the
# core was SAVED, so every user's modules would have gone to /root/.pcl-cache
# (which they cannot even write).  Both legs share one container, and one core
# build, because they are two questions about the same installation.
{
    my ($out, $rc) = leg('legc', <<'SH');
set -euo pipefail
cd /src
tools/install-pcl --prefix /opt/pcl --quiet
rm -rf /home/pcluser/.pcl-cache
ls -A /root | sort > /tmp/root-before
echo "--- the user's run ---"
su - pcluser -c '/opt/pcl/bin/pcl -e "use List::Util qw(sum); print sum(1..3), qq{\n}"'
ls -A /root | sort > /tmp/root-after
if diff -q /tmp/root-before /tmp/root-after > /dev/null; then echo "ROOT-HOME-UNCHANGED"; else
  echo "ROOT-HOME-CHANGED:"; diff /tmp/root-before /tmp/root-after || true
fi
echo "--- the user's cache ---"
su - pcluser -c 'ls -A ~/.pcl-cache' | sed 's/^/cache-entry: /'
echo "--- cache-info ---"
su - pcluser -c '/opt/pcl/bin/pcl --cache-info'
SH
    is($rc, 0, '(c) a shared /opt/pcl install serves a different user')
        or diag(tail($out, 40));
    like($out, qr/^6$/m, '(c) `use List::Util; sum(1..3)` prints 6 for that user')
        or diag(tail($out, 30));
    like($out, qr/^cache-entry: modules$/m,
         '(c) and the compiled-module cache appears under the USER\'s home (#1303)')
        or diag(tail($out, 30));
    like($out, qr/ROOT-HOME-UNCHANGED/,
         '(c) while nothing new appears under /root') or diag(tail($out, 30));

    like($out, qr{Cache directory: /home/pcluser/\.pcl-cache},
         '(d) `pcl --cache-info` reports THAT user\'s cache directory')
        or diag(tail($out, 20));
    like($out, qr/PCL_COMPILE_DIRS: unset/,
         '(d) and the compile policy in force, with its source');
    like($out, qr{Core for this run: /opt/pcl/lib/pcl/pcl\.core},
         '(d) and the installed core, not a per-user cached one');
}

# --------------------------------------------------------------- the skip path
# Exercised once here rather than trusted: a runner without a container socket
# must skip cleanly, and this file is the only thing that can say it does.
{
    my $out = `PCL_INSTALL_CONTAINER_SKIP=1 $^X $0 2>&1`;
    like($out, qr/^1\.\.0 # SKIP PCL_INSTALL_CONTAINER_SKIP=1/m,
         'PCL_INSTALL_CONTAINER_SKIP=1 skips the whole file, cleanly') or diag($out);
}
