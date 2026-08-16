#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# End-to-end test for tools/install-pcl (task #277).
#
# NOT part of the Pl/t gate: that gate measures the transpiler, this installs
# one.  Run it directly:  prove tools/t/install-pcl.t
#
# It installs into a temp prefix with --no-core (the core build is a minute of
# SBCL and adds nothing this test can check that the smoke test does not), and
# then asks the questions an installation has to answer:
#   * is the tree there, in the RELATIVE shape the lookups depend on?
#   * do the wrappers run the installed tools, not the checkout's?
#   * does a program transpiled AND run by the installed tools print what perl
#     prints?
#   * does the installed tree carry no development material?
use strict;
use warnings;
use Test::More;
use File::Temp qw(tempdir);
use FindBin qw($RealBin);

my $root = "$RealBin/../..";
my $inst = "$root/tools/install-pcl";

plan skip_all => "install-pcl not executable" unless -x $inst;
plan skip_all => "sbcl not found" unless `which sbcl 2>/dev/null`;
plan tests => 11;

my $prefix = tempdir(CLEANUP => 1);

# --- the dry run changes nothing -------------------------------------------
my $dry = `$inst --prefix $prefix --dry-run 2>&1`;
is($?, 0, 'dry run exits 0');
like($dry, qr/checking dependencies/, 'dry run reports the dependency check');
ok(!-e "$prefix/bin", 'dry run created nothing');

# --- the real install -------------------------------------------------------
my $out = `$inst --prefix $prefix --no-core 2>&1`;
is($?, 0, 'install exits 0') or diag($out);

ok(-x "$prefix/bin/pl2cl" && -x "$prefix/bin/runpcl", 'both wrappers are executable');
ok(-f "$prefix/lib/pcl/cl/pcl-runtime.lisp" && -f "$prefix/lib/pcl/Pl/Parser2.pm"
   && -f "$prefix/lib/pcl/tools/lib/PCLSbcl.pm" && -d "$prefix/lib/pcl/lib",
   'the tree is installed in its repo-relative shape');
ok(!-e "$prefix/lib/pcl/Pl/t", 'the gate is NOT part of an installation');

# --- the installed tools actually work --------------------------------------
my $src = "$prefix/hello.pl";
open my $fh, '>', $src or die $!;
print $fh qq{my \@w = map { ucfirst } qw(installed pcl);\nprint "\@w\\n";\n};
close $fh;

my $perl_out = `perl $src 2>&1`;
is($perl_out, "Installed Pcl\n", 'the fixture is what we think it is');

my $cl = `$prefix/bin/pl2cl $src 2>&1`;
is($?, 0, 'the installed pl2cl transpiles') or diag($cl);
like($cl, qr/\(p-print/, 'and emits CL');

my $run = `$prefix/bin/runpcl $src 2>&1`;
is($run, $perl_out, 'the installed runpcl prints exactly what perl prints')
    or diag($run);
