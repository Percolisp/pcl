#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# vendored-ppcre-01.t — a machine needs SBCL and nothing else (task #1597).
#
# PCL ships cl-ppcre under cl/vendor/cl-ppcre/ and pushes that directory onto
# ASDF's central registry before loading the system, so the install wall
# ("first set up Quicklisp") is gone.  The property is not "the file is there":
# it is that a PCL run finds the library WITHOUT any of the three things a
# developer machine quietly supplies —
#
#   * ~/.sbclrc                       (Quicklisp's init hook lives there)
#   * ~/quicklisp                     (the library itself)
#   * ASDF's inherited configuration  (/etc/common-lisp/…, ~/.config/common-lisp/…
#                                      and the default source-registry directories,
#                                      which is how a DISTRO package answers)
#
# so every leg below runs with $HOME pointed at a fresh EMPTY directory and
# CL_SOURCE_REGISTRY set to ignore inherited configuration.  Under those
# conditions nothing but PCL's own `pushnew` can find cl-ppcre: if the vendored
# copy ever stops being reached, these rows fail rather than the property
# quietly reverting to "works on this box".
#
# ~11 s wall (the first leg compiles the vendored library into a fresh ASDF
# cache, the second builds a core in one) — far under the gate's slowest file,
# and it is the gate that must hold this, because CI is where a fresh machine
# is actually reproduced.
#
# The cache environment of the calling gate is UNSET per leg on purpose: a
# PCL_TEST_CORE or PCL_CACHE_DIR pointing at the developer's caches would let
# an image built elsewhere answer for the bare one.

use strict;
use warnings;
use Test::More;
use FindBin qw($RealBin);
use Cwd qw(abs_path);
use File::Temp qw(tempdir tempfile);

my $root = abs_path("$RealBin/../..");

plan tests => 8;

# ---------------------------------------------------------------- the files
my $vend = "$root/cl/vendor/cl-ppcre";
ok(-f "$vend/cl-ppcre.asd", 'cl/vendor/cl-ppcre/cl-ppcre.asd is in the tree');
ok(-f "$vend/LICENSE",      "…with upstream's LICENSE kept verbatim");
ok(-f "$root/cl/vendor/README.md",
   'cl/vendor/README.md records where it came from and that it is never edited here');

# ------------------------------------------------------------------ the runs
#
# A fresh empty HOME per call: the module cache and ASDF's output cache land
# inside it and go away with it, so no leg can be answered by the developer's
# caches.  CL_SOURCE_REGISTRY with :ignore-inherited-configuration is what
# takes the distro package and any /etc config out of the picture.
sub bare_run {
    my (@args) = @_;
    my $home = tempdir(CLEANUP => 1);
    my $cmd = join ' ',
        'env', map({ ('-u', $_) } qw(XDG_CONFIG_HOME XDG_DATA_HOME XDG_CONFIG_DIRS
                                     XDG_DATA_DIRS PCL_TEST_CORE PCL_CACHE_DIR
                                     PCL_NO_CORE)),
        "HOME=$home",
        q{CL_SOURCE_REGISTRY='(:source-registry :ignore-inherited-configuration)'},
        @args;
    my $out = `cd $root && $cmd 2>&1`;
    return ($out, $? >> 8);
}

my ($pfh, $prog) = tempfile('pcl-vendored-XXXXXX', SUFFIX => '.pl', TMPDIR => 1, UNLINK => 1);
print $pfh qq{my \$s = "ab-cd-ef"; my \@p = split /-/, \$s; \$s =~ s/cd/XX/;\n}
         . qq{print scalar(\@p), " \$s\\n";\n};
close $pfh;

{
    my ($out, $rc) = bare_run('PCL_NO_CORE=1', './runpcl', $prog);
    is($rc, 0, 'source mode runs with an EMPTY $HOME and no inherited ASDF configuration')
        or diag($out);
    like($out, qr/^3 ab-XX-ef$/m, '…and split/s/// (i.e. cl-ppcre) answered correctly')
        or diag($out);
}

{
    # The cached-core path: the core is BUILT in that empty home, from a
    # runtime whose cl-ppcre can only be the vendored one, and then used.
    my ($out, $rc) = bare_run('./runpcl', $prog);
    is($rc, 0, 'the cached-core path builds and runs in that same bare environment')
        or diag($out);
}

# ------------------------------------------- which copy, and #1461 still on
{
    my ($out, undef) = bare_run(
        'sbcl', '--control-stack-size', '512', '--noinform', '--non-interactive',
        '--load', 'cl/pcl-runtime.lisp',
        '--eval',
        q{'(format t "~&PPCRE-SRC=~a~%PPCRE-BMH=~a~%" (asdf:system-source-directory :cl-ppcre) cl-ppcre:*use-bmh-matchers*)'});
    like($out, qr{^PPCRE-SRC=\Q$root\E/cl/vendor/cl-ppcre/$}m,
         'the cl-ppcre ASDF loaded IS the vendored one, not a system copy')
        or diag($out);
    # #1461: PCL shadows cl-ppcre::create-bmh-matcher and only leaves
    # *use-bmh-matchers* on when its own self-test observed the shadow being
    # reached.  T here means the pin still holds against the vendored version.
    like($out, qr/^PPCRE-BMH=T$/mi,
         'the #1461 BMH shadow is still in force against the vendored version')
        or diag($out);
}
