# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later
#
# SHAPES corpus (task #496) — sub heads: signature parameters vs same-named
# file lexicals (#454), the feature region's own line (#455), the one-line
# spelling where PPI hands over a Token::Prototype, `qw()`/bundle spellings
# (s439 review fix).  Lifted from Pl/t/sig-param-shadow-01.t.
{ package S01; use feature "signatures"; sub f ($x) { "f($x)" } my $x = f 5; print "$x\n"; }
{ package S02; use feature "signatures"; sub f ($x = 1) { "f($x)" } my $x = f 5; print "$x\n"; }
{ package S03; use feature "signatures"; sub f ($x, @r) { "f($x)[@r]" } my $x = f 5; print "$x\n"; }
{ package S04; use feature "signatures"; sub m1 ($x) { my $inner = sub { "in($x)" }; $inner->() } my $x = 4; print m1(2), " x=$x\n"; }
{ package S05; use feature "signatures"; sub f ($x) { "f($x)" } my $x = f 5; print "$x\n"; }
{ package S06; use feature "signatures";
  my $x = 9;
  sub k ($x) { "k($x)" }
  print k(1), " x=$x\n"; }
{ package S07; use feature "signatures"; sub f ($x, @r) { "[@r]" }
  print f(0), "\n"; }
{ package S08; use feature qw(signatures say); sub f ($x, @r) { scalar(@r) . "-" . ($r[0] // "u") }
  print f(1), "|", f(1,2,3), "\n"; }
{ package S09; use v5.36; sub f ($x, @r) { "v536[$x|@r]" } print f(1, 2), "\n"; }
