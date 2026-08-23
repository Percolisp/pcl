# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later
#
# Fixture for Pl/t/shapes/imported-term.pl: a module whose @EXPORT is built
# from a VARIABLE (Math::Complex's spelling), exporting a `()`-prototype sub.
package T438::Konst;
use strict; use warnings;
require Exporter;
our @ISA = qw(Exporter);
my @consts = qw( kpi khalf );
our @EXPORT = (qw( kname ), @consts);
sub kpi   () { 3.25 }
sub khalf () { 0.5 }
sub kname { "T438" }
1;
