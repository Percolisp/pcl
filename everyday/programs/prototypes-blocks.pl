# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# origin: s492 battery2 / b2-12-prototypes-blocks.pl
use strict; use warnings;
sub apply(&@) { my ($code, @l) = @_; map { $code->($_) } @l }
sub try_it(&;$) { my ($c, $default) = @_; my @r = eval { $c->() }; $@ ? $default : $r[0] }
sub mymax(\@) { my $r = shift; my $m = $r->[0]; $_ > $m and $m = $_ for @$r; $m }
sub opt($;$) { defined $_[1] ? "$_[0]+$_[1]" : "$_[0]" }
my @a = (3, 9, 4);
print join(",", apply { $_[0] * 2 } 1, 2, 3), " ", try_it { die "x" } "dflt"; print " ", try_it { 7 }; print " ", mymax(@a), " ", opt(1), " ", opt(1, 2), "\n";
