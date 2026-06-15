#!/usr/bin/env perl

# Copyright (c) 2025-2026
# This is free software; you can redistribute it and/or modify it
# under the same terms as the Perl 5 programming language system itself.

# run-dist-t.pl — transpile and run a CPAN dist's own t/*.t file through PCL.
#
# Usage:
#   tools/run-dist-t.pl <dist-dir> <t-file>            # run one t-file
#   tools/run-dist-t.pl --no-dist-lib <dist-dir> <t>   # don't add <dist>/lib to @INC
#   tools/run-dist-t.pl --summary <dist-dir> <t-file>  # print only the TAP summary
#
# <t-file> may be relative to <dist-dir> (e.g. t/basic.t) or an absolute path.
#
# `use Test::More` resolves to PCL's TAP layer (cl/pcl-test.lisp), loaded on
# demand by the runtime — so a dist's author tests run as TAP.
#
# @INC for the transpile: the dist's t/lib and (by default) lib are added via
# -I so the module-under-test and its test helpers resolve.
#
# CAVEAT (do NOT remove): for XS-stubbed dists (Scalar-List-Utils etc.) adding
# <dist>/lib pollutes pl2cl's OWN @INC — pl2cl `use`s Moo -> Scalar::Util, and a
# dist copy of Scalar/Util.pm shadows PCL's shim, causing a false TRANSPILE-FAIL.
# PCL uses its lib/ shims for those anyway, so pass --no-dist-lib for them.

use strict;
use warnings;
use File::Basename qw(dirname);
use Cwd qw(abs_path);

my $root = abs_path(dirname(abs_path($0)) . "/..");

my ($no_dist_lib, $summary_only) = (0, 0);
my @args;
for (@ARGV) {
  if    ($_ eq '--no-dist-lib') { $no_dist_lib = 1 }
  elsif ($_ eq '--summary')     { $summary_only = 1 }
  else                          { push @args, $_ }
}

@args == 2 or die "usage: $0 [--no-dist-lib] [--summary] <dist-dir> <t-file>\n";
my ($dist, $tfile) = @args;
$dist = abs_path($dist) or die "dist dir not found: $args[0]\n";
$tfile = "$dist/$tfile" unless $tfile =~ m{^/};
-f $tfile or die "t-file not found: $tfile\n";

# @INC additions for the transpile (-I dirs are baked into the generated @INC).
my @inc = ("$dist/t/lib");
push @inc, "$dist/lib" unless $no_dist_lib;
@inc = grep { -d } @inc;
my $iflags = join " ", map { "-I$_" } @inc;

my $lisp = "/tmp/run-dist-t_$$.lisp";
my $err  = "/tmp/run-dist-t_$$.err";

my $rc = system(
  "perl -I$root $iflags $root/pl2cl --no-cache --lenient-ppi $tfile >$lisp 2>$err");
if ($rc != 0) {
  my $e = do { local $/; open(my $f, '<', $err) or die; <$f> };
  print STDERR "TRANSPILE-FAIL ($tfile):\n$e";
  unlink $lisp, $err;
  exit 2;
}

my $raw = `sbcl --control-stack-size 512 --noinform --non-interactive --load $root/cl/pcl-runtime.lisp --eval "(setf pcl::*pcl-skip-cache* t)" --load $lisp 2>&1`;
unlink $lisp, $err;

# Filter SBCL noise.
$raw =~ s/^;[^\n]*\n//gm;
$raw =~ s/^PCL (?:Runtime|Test)[^\n]*\n//gm;
$raw =~ s/^STYLE-WARNING[^\n]*\n//gm;

if ($summary_only) {
  # Count TAP ok/not ok lines.
  my $ok    = () = $raw =~ /^ok \d+/mg;
  my $notok = () = $raw =~ /^not ok \d+/mg;
  print "pass=$ok fail=$notok  ($tfile)\n";
  print STDERR $raw if $notok;   # show detail on failures
} else {
  print $raw;
}
