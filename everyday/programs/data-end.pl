# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# origin: s492 battery2 / b2-09-data-end.pl
use strict; use warnings;
my @rows; while (my $l = <DATA>) { chomp $l; next if $l =~ /^#/ || $l !~ /\S/; push @rows, [split /\s*,\s*/, $l] }
print scalar(@rows), " rows; ", join(" ", map { "$_->[0]=$_->[1]" } @rows), "\n";
__DATA__
# comment
alpha, 1
beta , 2

gamma,3
