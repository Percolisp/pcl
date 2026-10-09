#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# misc-fixes-03.t -- small fixes with their guard rows (s513f fillers), each
# compared against perl's STDOUT.  No 5.40 syntax: CI's perl is 5.38.

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

plan tests => 2;

sub write_pl {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh "no warnings;\n$code";
    close $fh;
    return $pl_file;
}

# Transpile (PCLCore::transpile FAILS the row on a dropped statement) and run.
sub run_cl {
    my ($code) = @_;
    my $cl_code = PCLCore::transpile("$pl2cl " . write_pl($code));
    my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cl_fh $cl_code;
    close $cl_fh;
    my $output = `sbcl @sbcl_rt --load $cl_file 2>/dev/null`;
    $output =~ s/^;.*\n//gm;
    $output =~ s/^PCL Runtime loaded\n//gm;
    $output =~ s/^\s*\n//gm;
    return $output;
}

sub both_agree {
    my ($code, $desc) = @_;
    my $file = write_pl($code);
    my $perl = scalar `perl $file 2>/dev/null`;
    my $pcl  = run_cl($code);
    is($pcl, $perl, "$desc (perl: " . ($perl =~ s/\n/\\n/gr) . ")");
}

# ---- #2982: `^` under /m does not match after a newline that ENDS the string

both_agree(q{for my $s ("", "\n", "x\n", "x\ny", "x\ny\n", "\n\n") {
  my @m = ($s =~ /^/mg); my $u = $s; $u =~ s/^/> /mg; my @f = split /^/m, $s; my @g = split /^/, $s;
  (my $v = $s) =~ s/^(.?)/[$1]/mg; my @p; while ($s =~ /^/mg) { push @p, pos($s) }
  print scalar(@m), " ", ($u =~ s/\n/N/gr), " ", scalar(@f), "/", scalar(@g), " ", ($v =~ s/\n/N/gr), " p=@p\n";
}},
           '#2982 /m `^`: counts, s///mg, split, pos over six strings');

both_agree(q{print "a\n" =~ /a\n^/m ? "m1\n" : "m0\n"; print "x\n" =~ /^\z/m ? "z1\n" : "z0\n";
print "x\ny" =~ /(?m:^y)/ ? "g1\n" : "g0\n"; print "x\n" =~ /x\n(?m)^/ ? "f1\n" : "f0\n"; print "a\nb" =~ /\n^b/ ? "n1\n" : "n0\n";},
           '#2982 /m `^` at the end, in a (?m:) group, after (?m), and without /m');
