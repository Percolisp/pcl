#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# After print / printf / say a BAREWORD followed by a parenthesised list is
# a FILEHANDLE unless a sub of that name is already known (task #2193, folds
# #3183): `print STDERR ("x")` prints to STDERR, `sub foo {...} print foo
# ("x")` calls foo.  Every row compares against perl (5.38-safe: core only).

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

sub run_perl {
    my ($code) = @_;
    my ($fh, $file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;
    return `perl $file 2>&1`;
}

# Transpile and run CL, return output
sub run_cl {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;

    my $cl_code = PCLCore::transpile(qq{$pl2cl $pl_file});

    my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cl_fh $cl_code;
    close $cl_fh;

    my $output = `sbcl @sbcl_rt --load $cl_file 2>&1`;
    $output =~ s/^;.*\n//gm;
    $output =~ s/^caught .*\n//gm;
    $output =~ s/^compilation unit.*\n//gm;
    $output =~ s/^\s*Undefined.*\n//gm;
    $output =~ s/^-->.*\n//gm;
    $output =~ s/^==>.*\n//gm;
    $output =~ s/^PCL Runtime loaded\n//gm;
    $output =~ s/^WARNING:.*\n//gm;
    $output =~ s/^\s*\n//gm;
    return $output;
}

sub test_ph {
    my ($name, $code) = @_;
    my $perl_out = run_perl($code);
    my $cl_out   = run_cl($code);
    is($cl_out, $perl_out, $name) or diag("Perl: $perl_out\nCL:   $cl_out");
}

# 1. #2193: handle + parenthesised list (print / printf / say, a modifier,
#    inside a block and an argument), and the CALL readings beside them.
test_ph(q{print/printf/say HANDLE (LIST)}, <<'PERL');
printf STDOUT ("A %s\n", "x");
print STDOUT ("B\n") if 1;
my @l = ("C", "c", "\n"); print STDOUT (@l);
print STDOUT (1 ? "D\n" : "d\n");
use feature 'say'; say STDOUT ("E");
sub foo { "foo(@_)" } print foo ("F"), "\n";
print uc ("g"), "\n";
open(FH, ">&", \*STDOUT) or die; print FH ("H\n"); close FH;
print STDOUT ("I") . "i\n"; print "|\n";
my $r = join ",", map { print STDOUT ("J$_\n") } 1, 2;
PERL

# 2. the breaking cases: a sub declared BELOW is still a handle, a qualified
#    or already-declared sub is a call, `print STDOUT (1+2)*3`, `print STDOUT;`.
test_ph(q{print WORD (LIST): handle unless a sub is already known}, <<'PERL');
no warnings;
print STDOUT (1+2)*3; print "\n";
$_ = "dflt\n"; print STDOUT;
sub id { @_ } my @r = id(print STDOUT ("K\n"));
print BAR ("L-to-unopened\n"); print "after-L\n";
sub BAR { "bar(@_)" }
package Foo; sub bar { "Foo::bar(@_)" } package main;
print Foo::bar ("M"), "\n";
sub QUUX { "quux(@_)" } print QUUX ("N"), "\n";
print STDOUT ("O", "o"), "\n";
PERL

done_testing();
