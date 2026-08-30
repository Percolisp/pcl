#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# Task #755: a perl filename is a LITERAL string.  CL's namestring parser
# reads `*' `?' `[' as WILD components and `\' as an escape, so any path
# operation on such a name used to die ("Can't find the truename of wild
# pathname") or touch the WRONG file.  The runtime now routes every
# user-filename -> pathname conversion through the one seam %p-literal-path
# (sb-ext:parse-native-namestring).  glob() keeps its own wildcard parsing
# by design (guarded by Pl/t/glob-01.t).
# Also guards the p-unlink/p-rename upgrades that rode along: unlink(2)
# removes a dangling symlink and answers EISDIR on a directory instead of
# crashing; rename(2) replaces an existing target.
#
# The oracle is REAL PERL run on the same program at test time.

use strict;
use warnings;
use Test::More tests => 11;
use File::Temp qw(tempfile);
use FindBin qw($RealBin);
use lib $RealBin;
use PCLCore;

my $pl2cl = './pl2cl';
my $runtime = 'cl/pcl-runtime.lisp';
my @sbcl_rt = PCLCore::sbcl_prefix($runtime);

my $prog = <<'PROG';
my $d = "/tmp/pcl_wildt_$$";
mkdir $d or die "mkdir: $!";
chdir $d or die "chdir: $!";
for my $n ("q_?_n", "b_[x]_n", "t_~_n", "s_\\_n", "*") {
    open my $fh, ">", $n or die "open($n): $!";
    print $fh "V:$n";
    close $fh;
    print "e($n)=", (-e $n ? 1 : 0), " s=", (-s $n), "\n";
    open my $in, "<", $n or die "reopen($n): $!";
    my $l = <$in>;
    close $in;
    print "r($n)=$l\n";
}
symlink("q_?_n", "*ln") or die "symlink: $!";
print "l=", (-l "*ln" ? 1 : 0), "\n";
rename("b_[x]_n", "out_*") or die "ren: $!";
print "ren=", (-e "out_*" ? 1 : 0), (-e "b_[x]_n" ? 1 : 0), "\n";
mkdir "d_*_ir" or die "mkdir wild: $!";
open my $sf, ">", "d_*_ir/in.txt" or die "open inner: $!";
close $sf;
open my $wf, ">", "d_*_ir/en*[t]ry?" or die "open wild inner: $!";
close $wf;
opendir(my $dh, "d_*_ir") or die "opendir: $!";
my @e = sort readdir($dh);
closedir $dh;
print "rd=[@e]\n";
symlink("/nope_$$", "dang") or die "symlink dang: $!";
print "dang=", unlink("dang"), "\n";
my $u = unlink("d_*_ir");
print "udir=$u\n";
unlink "d_*_ir/in.txt", "d_*_ir/en*[t]ry?";
rmdir "d_*_ir" or die "rmdir wild: $!";
my $cnt = 0;
$cnt += unlink($_) for ("q_?_n", "t_~_n", "s_\\_n", "*", "*ln", "out_*");
print "cnt=$cnt\n";
chdir "/";
rmdir $d or die "rmdir: $!";
print "done\n";
PROG

my ($fh, $pl_file) = tempfile(SUFFIX => '.pl');
print $fh $prog;
close $fh;

# Oracle: real perl on the same program.
my $perl_out = `perl $pl_file 2>&1`;

# PCL side.
my $cl_code = PCLCore::transpile(qq{$pl2cl --no-cache $pl_file});
my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp');
print $cl_fh $cl_code;
close $cl_fh;
my $pcl_out = `sbcl @sbcl_rt --load $cl_file 2>&1`;
$pcl_out =~ s/^;.*\n//gm;
$pcl_out =~ s/PCL Runtime loaded\n?//g;
$pcl_out =~ s/STYLE-WARNING.*\n//g;
unlink $pl_file, $cl_file;

# The oracle must itself have run to completion, or every keyed compare
# below would pass vacuously on undef == undef.
like($perl_out, qr/^done$/m, 'perl oracle ran the battery to completion');

# Targeted rows first, so a failure names the broken area.
my %perl = map { /^([^=]+)=(.*)$/ ? ($1 => $2) : () } split /\n/, $perl_out;
my %pcl  = map { /^([^=]+)=(.*)$/ ? ($1 => $2) : () } split /\n/, $pcl_out;

is($pcl{'e(*)'},  $perl{'e(*)'},  'a file named only "*" is created and -e finds it');
is($pcl{'r(*)'},  $perl{'r(*)'},  'reading the "*" file returns its own content');
is($pcl{'r(s_\\_n)'}, $perl{'r(s_\\_n)'}, 'a literal backslash in a name is a character, not an escape');
is($pcl{'l'},     $perl{'l'},     '-l sees a symlink whose name contains a wildcard char');
is($pcl{'ren'},   $perl{'ren'},   'rename with wildcard chars in both names');
is($pcl{'rd'},    $perl{'rd'},    'readdir: wild-named dir listed, wild-named entries UNESCAPED (#755 output half)');
is($pcl{'dang'},  $perl{'dang'},  'unlink removes a DANGLING symlink (unlink(2), not probe-file)');
is($pcl{'udir'},  $perl{'udir'},  'unlink of a directory answers 0/EISDIR instead of crashing');
is($pcl{'cnt'},   $perl{'cnt'},   'unlink count over all wildcard-char names');

is($pcl_out, $perl_out, 'full wildcard-filename battery matches perl byte for byte');
