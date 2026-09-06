# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 144-local -- harvested from s470/fable/bk/rt2.pl

use strict;
my @x; eval { @{local $x[0][0]} = 1; }; print "A: ", ($@ ? "died" : "ok"), " def=", (defined $x[0][0] ? 1 : 0), " x0=", (defined $x[0] ? "viv" : "undef"), "\n";
my @y; eval { local $y[0][0] = 5; print "B-in: $y[0][0]\n" }; print "B: ", ($@ ? "died" : "ok"), " after=", (defined $y[0][0] ? "def" : "undef"), " y0=", (defined $y[0] ? "viv" : "undef"), "\n";
my @z; eval { @{$z[0][0]} = 1; }; print "C: ", ($@ ? "died" : "ok"), " def=", (defined $z[0][0] ? 1 : 0), "\n";
my @w; eval { my $q = local $w[0][0]; }; print "D: ", ($@ ? "died" : "ok"), " w0=", (defined $w[0] ? "viv" : "undef"), "\n";
