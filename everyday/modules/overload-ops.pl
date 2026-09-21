# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# origin: s492 modwork / mw-27-overload-ops.pl
use strict; use warnings;
package Vec2; use overload "+" => \&add, "-" => sub { Vec2->new($_[2] ? ($_[1] - $_[0]{x}) : ($_[0]{x} - (ref $_[1] ? $_[1]{x} : $_[1])), 0) }, "*" => \&mul, "==" => sub { $_[0]{x} == $_[1]{x} && $_[0]{y} == $_[1]{y} }, '""' => sub { "($_[0]{x}, $_[0]{y})" }, "bool" => sub { $_[0]{x} || $_[0]{y} }, "<=>" => sub { my ($a, $b, $sw) = @_; my $r = $a->len <=> (ref $b ? $b->len : $b); $sw ? -$r : $r }, "neg" => sub { Vec2->new(-$_[0]{x}, -$_[0]{y}) };
sub new { my ($c, $x, $y) = @_; bless { x => $x, y => $y }, $c } sub add { Vec2->new($_[0]{x} + $_[1]{x}, $_[0]{y} + $_[1]{y}) } sub mul { ref $_[1] ? $_[0]{x} * $_[1]{x} + $_[0]{y} * $_[1]{y} : Vec2->new($_[0]{x} * $_[1], $_[0]{y} * $_[1]) } sub len { sqrt($_[0]{x} ** 2 + $_[0]{y} ** 2) }
package main; my ($p, $q) = (Vec2->new(1, 2), Vec2->new(3, 4)); my $c = $p + $q; $c += $p; my @s = sort { $a <=> $b } (Vec2->new(3, 4), Vec2->new(1, 0), Vec2->new(0, 2));
print "$c ", $p * $q, " ", $p * 3, " ", -$p, " ", ($p == Vec2->new(1, 2) ? "eq" : "ne"), " ", (Vec2->new(0, 0) ? "true" : "false"), " @s ", "str:" . $p, " ", ($q > $p ? "gt" : "le"), "\n";
