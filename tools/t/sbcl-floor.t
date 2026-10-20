#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# sbcl-floor.t — THE SUPPORTED SBCL FLOOR, on this box (task #3528).
#
# CLAUDE.md's floor is SBCL 2.5.2; the install matrix's ubuntu:22.04 and
# debian:12 legs are the only other place it runs, weekly.  #3528 was a
# 2.5.2-only COMPILER error (`sb-vm::move-double-float-arg is not the name of
# a defined VOP') in a shape every `pack' call compiles, and it sat unnoticed
# because nothing on the dev box runs 2.5.2.  This file runs a set of gate
# files -- the pack-using ones, int()'s ranges, the TAP layer -- under a SECOND
# SBCL named by PCL_SBCL_FLOOR, and skips without it.  NOT in the gate.
#
# GETTING A 2.5.2 BESIDE 2.6.0 (what tools/install-matrix/deps.sh does, with
# another INSTALL_ROOT):
#     curl -fsSL -o /tmp/sbcl.tar.bz2 \
#       https://downloads.sourceforge.net/project/sbcl/sbcl/2.5.2/sbcl-2.5.2-x86-64-linux-binary.tar.bz2
#     tar -xjf /tmp/sbcl.tar.bz2 -C /tmp
#     (cd /tmp/sbcl-2.5.2-x86-64-linux && INSTALL_ROOT=$HOME/sbcl-2.5.2 sh install.sh)
#     PCL_SBCL_FLOOR=$HOME/sbcl-2.5.2/bin/sbcl prove -v tools/t/sbcl-floor.t
# or copy it out of the install-matrix base image
# (`podman create --name x pcl-install-base:<tag>; podman cp x:/opt/pcldeps/sbcl DIR`).
# SBCL_HOME is taken from <floor>/../lib/sbcl.  The runtime core is keyed by
# `sbcl --version`, so the floor builds and keeps its own core.

use strict;
use warnings;
use Test::More;
use File::Basename qw(dirname);
use File::Temp qw(tempdir);
use FindBin qw($RealBin);

my $floor = $ENV{PCL_SBCL_FLOOR};
plan skip_all => 'set PCL_SBCL_FLOOR=/path/to/a-second/sbcl (see the header)'
    unless defined $floor && length $floor;
-x $floor or BAIL_OUT("PCL_SBCL_FLOOR=$floor is not an executable");

my $root = "$RealBin/../..";
my $bin  = dirname($floor);
my $home = dirname($bin) . "/lib/sbcl";
local $ENV{SBCL_HOME} = $home if -d $home;
local $ENV{PATH} = "$bin:$ENV{PATH}";

my $ver = `sbcl --version 2>&1`;
chomp $ver;
like($ver, qr/^SBCL \d/, "the floor answers on PATH: $ver");

# The #3528 program itself: the first pack call compiles the pack artifact.
my $tmp = tempdir(CLEANUP => 1);
my $prog = "$tmp/packmin.pl";
open my $p, '>', $prog or die;
print $p 'print pack("C", 65), " ", join(",", unpack("C4 V C C", pack("C4 V C C", 1..7))), "\n";';
close $p;
my $out = `$root/runpcl $prog 2>&1`;
like($out, qr/^A 1,2,3,4,5,6,7$/m, "pack/unpack run under $ver (the pack artifact compiles)");

# Gate files, each run whole under the floor.
my @files = qw(int-wide-01.t tap-exit-01.t storable-shim-01.t digest-shim-01.t
               extension-preamble-01.t list-arg-context-01.t);
for my $f (@files) {
    my $res = `prove $root/Pl/t/$f 2>&1`;
    my ($verdict) = $res =~ /^Result: (\w+)/m;
    is($verdict // 'none', 'PASS', "Pl/t/$f passes under $ver")
        or diag($res);
}

done_testing;
