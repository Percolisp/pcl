#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# config-extensions-01.t -- %Config lists the extensions PCL PROVIDES (#1705).
#
# Tests guard on `$Config{extensions} =~ /\bEncode\b/` (Pod-Simple's
# corpus.t / enc-chars.t / JustPod_corpus.t, perl's io/socket.t) and
# `$Config{d_getpbyname}`; an empty list made them skip files PCL can run.
# The list is the ONE list of modules PCL ships as XS replacements -- the
# `# pcl-shim: must-win` marker in lib/ -- and lib/Config.pm holds it as one
# line.  Row 1 keeps that line equal to the marker (and prints the line to
# paste when it drifts); row 2 reads it through PCL (FAILS on ade297e7, where
# the list was empty and d_getpbyname absent).

use v5.30;
use strict;
use warnings;
use Test::More;
use File::Find;
use File::Temp qw(tempdir);
use FindBin qw($RealBin);
use lib $RealBin;
use PCLCore;

my $project_root = "$RealBin/../..";
my $lib = "$project_root/lib";

my @shims;
find(sub {
  return unless /\.pm\z/;
  open my $fh, '<', $_ or return;
  while (my $line = <$fh>) {
    last if $. > 60;
    if ($line =~ /^# pcl-shim: must-win\b/) {
      (my $m = $File::Find::name) =~ s{\A\Q$lib\E/}{};
      $m =~ s/\.pm\z//;
      push @shims, $m;
      last;
    }
  }
}, $lib);
my $want = join ' ', sort @shims;

my $have = `$^X -e 'do "$lib/Config.pm" or die \$\@; print \$Config::Config{dynamic_ext}, "|", \$Config::Config{extensions}'`;
is($have, "$want|$want",
   'lib/Config.pm lists exactly the must-win shims as dynamic_ext and extensions')
  or diag "paste into lib/Config.pm:\nmy \$PCL_SHIM_EXTENSIONS = '$want';";

my $pl2cl   = "$project_root/pl2cl";
my $runtime = "$project_root/cl/pcl-runtime.lisp";
my @sbcl_rt = PCLCore::sbcl_prefix($runtime);
SKIP: {
  skip "pl2cl/sbcl not available", 1 unless -x $pl2cl && `which sbcl 2>/dev/null`;
  my $dir = tempdir(CLEANUP => 1);
  open my $s, '>', "$dir/c.pl" or die;
  print {$s} 'use Config; print join(" ", map { $Config{extensions} =~ /\b\Q$_\E\b/ ? "$_=yes" : "$_=no" } qw(Encode Socket List/Util)), " d_getpbyname=", ($Config{d_getpbyname} // "undef"), "\n";';
  close $s;
  my $cl = `$pl2cl $dir/c.pl 2>/dev/null`;
  open my $c, '>', "$dir/c.lisp" or die; print {$c} $cl; close $c;
  my $out = `sbcl @sbcl_rt --load $dir/c.lisp 2>&1`;
  $out =~ s/^;.*\n//gm; $out =~ s/^PCL Runtime loaded\n//gm; $out =~ s/^\s*\n//gm;
  is($out, "Encode=yes Socket=yes List/Util=yes d_getpbyname=define\n",
     'a program sees the shipped extensions and d_getpbyname through %Config');
}

done_testing;
