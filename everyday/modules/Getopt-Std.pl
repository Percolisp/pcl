# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# origin: s492 modwork / mw-03-Getopt-Std.pl
use strict; use warnings; use Getopt::Std;
local @ARGV = qw(-a -b val -c file); my %o; getopts("ab:c", \%o); print join(",", map { "$_=$o{$_}" } sort keys %o), " rest=@ARGV\n";
