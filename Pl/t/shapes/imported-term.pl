# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later
#
# SHAPES corpus (task #496) — an imported `()`-prototype sub is a TERM (#365):
# the prototype crosses the `use` on its own shape, with an @EXPORT built from
# a variable (lib/T438/Konst.pm).  `use lib` is a literal path relative to the
# COMPILER's root, which is how tools/corpus-diff.pl and tools/emission-ab.pl
# run pl2cl (cd <tree> && ./pl2cl < FILE) on both sides.  Lifted from
# Pl/t/imported-term-01.t.
use lib "Pl/t/shapes/lib";
use T438::Konst;
print kpi, "\n";
my $w = 2 * kpi; print "$w\n";
my $z = kpi + 1; print "$z\n";
my @l = (kpi, 1); print "@l\n";
my $y = kpi; print "$y\n";
print kname(), "\n";
print "x=", nosuchword, "\n";
