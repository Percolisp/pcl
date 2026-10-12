#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# print-block-fh-01.t -- task #3244 (s516a): `print({$fh} LIST)`, the
# PARENTHESISED block-filehandle form (and say / printf), is one expression in
# perl.  PPI 1.291 splits the parens' content into a compound statement plus a
# second statement, or makes the block a hash Constructor (ppi-upstream-bugs.md
# §37), and PCL DROPPED the statement ("Fell through").  The repair
# (Pl::Parser::_repair_paren_block_filehandle) hands the shape to the existing
# paren-form filehandle path.  Every row compares with perl itself; the
# finders were HTTP-Tiny t/Util.pm (`print({$fh} @_) || die`) and Path-Tiny's
# spew (`print( {$fh} map {...} @data)`).

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
    print $fh "use feature 'say';\nopen(my \$fh, '>', \\my \$buf) or die;\n$code";
    close $fh;
    return $pl_file;
}

# Transpile (PCLCore::transpile FAILS the row on a dropped statement) and run.
sub run_cl {
    my ($code) = @_;
    my $cl = PCLCore::transpile("$pl2cl " . write_pl($code));
    my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cl_fh $cl;
    close $cl_fh;
    my $output = `sbcl @sbcl_rt --load $cl_file 2>&1`;
    $output =~ s/^;.*\n//gm;
    $output =~ s/^PCL Runtime loaded\n//gm;
    return $output;
}

sub both_agree {
    my ($code, $desc) = @_;
    my $perl = `perl @{[ write_pl($code) ]} 2>&1`;
    my $pcl  = run_cl($code);
    s/0x[0-9a-f]+/0xADDR/g for $perl, $pcl;
    is($pcl, $perl, "$desc (perl: " . ($perl =~ s/\n/\\n/gr) . ")");
}

both_agree('print({$fh} "a", "b"); print "[$buf]\n";',
           'print({$fh} LIST) writes LIST to $fh');
both_agree('my @l = ("c", "d"); print({$fh} @l) || die "x"; print "[$buf]\n";',
           'print({$fh} @l) || die -- the || is outside the call');
both_agree('say({$fh} "e"); print "[$buf]";',
           'say({$fh} LIST)');
both_agree('printf({$fh} "%s-%s", "f", "g"); print "[$buf]\n";',
           'printf({$fh} FMT, LIST) (PPI: a Constructor)');
both_agree('printf( {$fh} "%s", "h"); print "[$buf]\n";',
           'printf( {$fh} ...) with a space after the paren');
both_agree('my @data = ("i", "j"); print( {$fh} map { "<$_>" } @data); print "[$buf]\n";',
           'print( {$fh} map {...} @data) -- the Path-Tiny spew shape');
both_agree('open(my $g, ">", \my $gb) or die; for my $ok (1, 0) { print({ $ok ? $fh : $g } "k$ok") } print "[$buf|$gb]\n";',
           'a computed block: print({ $ok ? $fh : $g } ...)');
both_agree('my $r = print({$fh} "l"); print "r=$r [$buf]\n";',
           'the value of print({$fh} ...) is its success');
# ---- negatives: a hash constructor stays a hash ----------------------------
both_agree('print({a => 1}); print "\n";',
           'NEGATIVE: print({a => 1}) prints the HASH ref');
both_agree('print({a => 1}, "z"); print "\n";',
           'NEGATIVE: print({a => 1}, "z") -- a comma follows: an argument');
