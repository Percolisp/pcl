#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# capture-sigil-01.t -- task #3061: `$e`, `@e` and `%e` are THREE variables,
# and a named sub capturing more than one of them must promote each on its own.
#
# The promoter (`_promote_captured`) counted the "sole hard declaration" of a
# SCALAR canon over ALL sigils of the bare name, so `my $e; my @e;` read as two
# declarations of one name: `$e` was refused promotion, the gate
# (`_check_sub_captures`) then saw a captured, unpromoted file lexical and
# refused the WHOLE FILE ("Parser2 TODO: file lexical 'e' captured by sub rf").
# Text-CSV's t/util.pl (`my $ebcdic` + `my @ebcdic`) is that shape and is
# `require`d by 15 of the dist's test files.  Every rewrite the promotion does
# already keys on the canonical symbol, so the count is sigil-exact now.
#
# Expected outputs are perl 5.40.3's, probed (scratch/s522a/pr in the s522a
# worktree); none depends on the perl version.  Every row except the `our @e`
# and the block-scoped anon-sub rows (shapes that must STAY right) FAILS on the
# aff112d7 base with the refusal.

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

plan tests => 10;

sub write_pl {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;
    return $pl_file;
}

sub run_cl {
    my ($code) = @_;
    my $cl_code = PCLCore::transpile("$pl2cl " . write_pl($code));
    my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cl_fh $cl_code;
    close $cl_fh;
    my $output = `sbcl @sbcl_rt --load $cl_file 2>&1`;
    $output =~ s/^;.*\n//gm;
    $output =~ s/^PCL Runtime loaded\n//gm;
    $output =~ s/^\s*\n//gm;
    return $output;
}

sub agree {
    my ($code, $expected, $desc) = @_;
    is(run_cl($code), $expected, $desc);
}

agree(<<'PL', "6\n", "the reproducer: \$e + \$e[1] in one sub");
my $e = 1;
my @e = (5, 6);
sub rf { return $e ? $e[1] : 0 }
print rf(), "\n";
PL

agree(<<'PL', "7\n", "\$e + \$e{k}");
my $e = 1; my %e = (k => 7);
sub rf { return $e ? $e{k} : 0 }
print rf(), "\n";
PL

agree(<<'PL', "16\n", "\$e and \$e[1] read in two different subs");
my $e = 1; my @e = (5, 6);
sub a1 { $e } sub a2 { $e[1] }
print a1(), a2(), "\n";
PL

agree(<<'PL', "6\n", "\$e + an our \@e");
my $e = 1; our @e = (5, 6);
sub rf { return $e ? $e[1] : 0 }
print rf(), "\n";
PL

agree(<<'PL', "6\n", "block-scoped pair captured by an anon sub");
my $f;
{ my $e = 1; my @e = (5, 6); $f = sub { $e ? $e[1] : 0 } }
print $f->(), "\n";
PL

agree(<<'PL', "15 9 19 9 19 9 6\n", "the sub WRITES \$e and \$e[0]: both cells promoted independently");
my $e = 1; my @e = (5, 6);
sub rf { $e = $e[0] + 10; $e[0] = 9; return "$e $e[0]" }
print rf(), " ", rf(), " $e @e\n";
PL

agree(<<'PL', "323\n", "a later file-level my \@e beside \$e");
my $e = 3;
sub rf { return defined $e ? $e : 'u' }
my @e = (5, 6);
sub rg { return scalar(@e) . $e }
print rf(), rg(), "\n";
PL

agree(<<'PL', "3\n", "\$e + \$#e");
my $e = 1; my @e = (5, 6, 7);
sub rf { return $e + $#e }
print rf(), "\n";
PL

agree(<<'PL', "1-6-v-2-k\n", "\$e + \@e + %e in one sub, interpolated too");
my $e = 1; my @e = (5, 6); my %e = (k => 'v');
sub rf { return "$e-$e[1]-$e{k}-" . scalar(@e) . "-" . join(",", keys %e) }
print rf(), "\n";
PL

agree(<<'PL', "1 6 5 6 5 6\n", "\$e, \$e[1], \@e and an \@e slice interpolated in the sub");
my $e = 1; my @e = (5, 6);
sub rf { return "$e $e[1] @e @e[0,1]" }
print rf(), "\n";
PL
