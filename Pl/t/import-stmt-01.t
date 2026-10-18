#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# import-stmt-01.t - `import PACKAGE LIST;` is PACKAGE->import(LIST) when
# `import` is not callable here (task #1506), and a run-time
# `constant->import(...)` installs constants through the lib/constant.pm shim.
# Programs = Fable's s522 merge probe b5-import-stmt.pl, one row each; expected
# lines are perl 5.40.3's output.  Inverse: on the aff112d7 base rows 1-4, 7
# and 8 fail ("Undefined subroutine &main::constant" / a lowercase package
# read as a call); rows 5 and 6 are the must-stay shapes (a file's own `sub
# import`, an import of a package the file declares).

use v5.30;
use strict;
use warnings;
use Test::More;
use File::Temp qw(tempfile);
use FindBin qw($RealBin);
use lib $RealBin;
use PCLCore;

my $project_root = "$RealBin/../..";
my $pl2cl        = "$project_root/pl2cl";
my $runtime      = "$project_root/cl/pcl-runtime.lisp";
my @sbcl_rt = PCLCore::sbcl_prefix($runtime);

plan skip_all => "pl2cl not found" unless -x $pl2cl;
plan skip_all => "sbcl not found"  unless `which sbcl 2>/dev/null`;

plan tests => 8;

sub run_cl {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;
    my $cl_code = `$pl2cl $pl_file 2>/dev/null`;
    my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cl_fh $cl_code;
    close $cl_fh;
    my $output = `sbcl @sbcl_rt --load $cl_file 2>/dev/null`;
    $output =~ s/^;.*\n//gm;
    $output =~ s/^PCL Runtime loaded\n//gm;
    $output =~ s/^\s*\n//gm;
    return $output;
}

sub test_cl {
    my ($name, $code, $expected) = @_;
    is(run_cl($code), $expected, $name);
}

test_cl('import constant NAME => VALUE in a BEGIN',
    'BEGIN { require constant; import constant HAVE_THREADS => 0; } print "r1 ", HAVE_THREADS, "\n";',
    "r1 0\n");
test_cl('constant->import(NAME => VALUE)',
    'BEGIN { require constant; constant->import(TWO => 2) } print "r2 ", TWO, "\n";',
    "r2 2\n");
test_cl('constant->import({ HASH }) installs every name',
    'BEGIN { require constant; constant->import({ H1 => 3, H2 => 4 }) } print "r3 ", H1() + H2(), "\n";',
    "r3 7\n");
test_cl('a LIST constant in list and scalar-count context',
    'BEGIN { require constant; constant->import(LST => (5, 6, 7)) } my @l = LST; my $c = () = LST; print "r4 @l $c\n";',
    "r4 5 6 7 3\n");
test_cl('a file\'s own sub import keeps `import "foo", 1` its call',
    'package Mine; sub import { print "r5 own-import(@_)\n" } sub go { import "foo", 1 } package main; Mine::go();',
    "r5 own-import(foo 1)\n");
test_cl('import My::Mod LIST with My::Mod declared in the file',
    'package My::Mod; sub import { my $c = shift; print "r6 imported: @_\n" } package main; BEGIN { $INC{"My/Mod.pm"} = 1; } BEGIN { import My::Mod A => 1; }',
    "r6 imported: A 1\n");
test_cl('a constant installed into a non-main caller',
    'package Q; BEGIN { require constant; constant->import(K => 7) } package main; print "r7 ", Q::K(), "\n";',
    "r7 7\n");
test_cl('import of an unloadable package is a quiet no-op, as in perl',
    'print "r8 ", (eval { import threads::nope::zz 1; 1 } ? "ok" : "died"), "\n";',
    "r8 ok\n");
