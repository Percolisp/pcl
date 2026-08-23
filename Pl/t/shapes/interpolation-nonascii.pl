# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later
#
# SHAPES corpus (task #496) â interpolation and NON-ASCII identifiers under
# `use utf8`: a fullwidth name and its ASCII twin are distinct (#418: globals,
# lexicals, subs, packages, labels, bareword handles), a repaired non-ASCII
# symbol with a space before its subscript (#422.2), a non-ASCII name as the
# SUBSCRIPT of an interpolated element and inside an embedded block (#435),
# and the ASCII inverse of each.  Lifted from Pl/t/utf8-source-01.t.  The file
# is UTF-8; valid perl: run it.
use utf8;
binmode STDOUT, ":utf8";
{ package S01; our %Ｘ = (a=>1); our %X = (a=>2); our $Ｘ = 5; our $X = 6; our @Ｘ = (7); our @X = (8);
  print $Ｘ{a}, $X{a}, $Ｘ, $X, $Ｘ[0], $X[0], "\n"; }
{ package S02; my %Ｘ = (a=>1); my %X = (a=>2); my $Ｘ = 5; my $X = 6; my @Ｘ = (7); my @X = (8);
  print $Ｘ{a}, $X{a}, $Ｘ, $X, $Ｘ[0], $X[0], "\n"; }
{ package S03; sub Ｆ {1} sub F {2} print Ｆ(), F(), "\n"; }
{ package S04; { package ＦＯＯ; our $z = 5; sub bar { 1 } }
  print exists $ＦＯＯ::{bar} ? 1 : 0, $ＦＯＯ::z, ＦＯＯ::bar(), "\n"; }
{ package S05; my $out = "";
  ＬＯＯＰ: for my $i (1..3) { LOOP: for my $j (1..3) { next ＬＯＯＰ if $j == 2; $out .= "$i$j,"; } }
  print "$out\n"; }
{ package S06; my $tmp = "/tmp/pcl-shapes-utf8-$$.txt";
  open(ＦＨ, ">", $tmp) or die; print ＦＨ "ok\n"; close(ＦＨ);
  open(ＦＨ, "<", $tmp) or die; my $l = <ＦＨ>; close ＦＨ; print $l; unlink $tmp; }
{ package S07; my %Ｘ = (a => 1, b => 2); my @Ｖ = (5, 6, 7);
  print $Ｘ {a}, $Ｖ [1], "\n";
  my @sl = @Ｘ {qw(a b)}; print "@sl\n";
  if ($Ｖ [0]) { print "blk\n" }
  my @ms = map { $_ } @Ｖ; print scalar(@ms), "\n"; }
{ package S08; our @Ｘ = (1,2,3); our %Ｈ = (k => 8); our $Ｖ = 1; my $ｉ = 1; my $ｋ = "k";
  print "$Ｘ[$ｉ]", " ", "$Ｈ{$ｋ}", "\n";
  print "$Ｘ[$ｉ+1]", " ", "@{[ $Ｘ[0] + $Ｖ ]}", "\n"; }
{ package S09; our @X = (1,2,3); our %H = (k => 8); my $i = 1; my $k = "k";
  print "$X[$i] $H{$k} $X[$i+1] @{[ $X[0] + 1 ]}\n"; }
{ package S10; my %ｈ = (ｋ => "v", k => "w"); print "$ｈ{ｋ}$ｈ{k}|", $ｈ{ｋ}, "\n"; }
{ package S11; our @Ｘ = (1,2,3); print $#Ｘ, " ", $#{Ｘ}, " ", "$#Ｘ $#{Ｘ}\n"; }
