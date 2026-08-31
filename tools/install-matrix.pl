#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# install-matrix.pl — run the multi-distro install matrix LOCALLY (task
# #876), the same four images the GitHub workflow covers, by building
# tools/install-matrix/Dockerfile once per base image.  The recipe script
# is self-verifying, so a successful image build IS a pass.
#
# Needs docker or podman.  As of s455f NO container runtime exists on the
# dev box (installing one is a system install — ask the user first); until
# then the GitHub workflow (.github/workflows/install-matrix.yml) is the
# only consumer that actually runs, and this driver is the day-one harness
# for when that changes.
#
# Usage:  tools/install-matrix.pl [IMAGE ...]     # default: all four

use strict;
use warnings;

my @images = @ARGV ? @ARGV
    : qw(ubuntu:22.04 ubuntu:24.04 debian:12 debian:13);

-x 'tools/install-pcl'
    or die "install-matrix.pl: run me from the PCL repo root\n";

my ($engine) = grep { system("command -v \Q$_\E >/dev/null 2>&1") == 0 }
    qw(docker podman);
if (!defined $engine) {
    die "install-matrix.pl: no container runtime found (docker or podman).\n"
      . "Installing one is a system install -- ask the user first.\n"
      . "The GitHub workflow install-matrix.yml runs the same matrix.\n";
}

# Same per-image SBCL pin as install-matrix.yml: sbcl.org's 2.6.0 binary
# needs glibc >= 2.38 (first-run fact, 2026-08-31), so older bases get the
# validated floor 2.5.2, whose binary needs only 2.34.
my %sbcl_for = (
    'ubuntu:22.04' => '2.5.2',
    'debian:12'    => '2.5.2',
);

my %verdict;
for my $image (@images) {
    (my $tag = lc "pcl-install-matrix:$image") =~ tr/:.\//---/;
    my $sbcl = $sbcl_for{$image} // '2.6.0';
    print "\n=== $image (via $engine, sbcl $sbcl) ===\n";
    my $status = system($engine, 'build',
        '--file', 'tools/install-matrix/Dockerfile',
        '--build-arg', "BASE_IMAGE=$image",
        '--build-arg', "SBCL_VERSION=$sbcl",
        '--tag', $tag, '.');
    $verdict{$image} = $status == 0 ? 'PASS' : 'FAIL';
}

print "\n=== install matrix ===\n";
printf "%-14s %s\n", $_, $verdict{$_} for @images;
my @failed = grep { $verdict{$_} eq 'FAIL' } @images;
exit(@failed ? 1 : 0);
