# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# origin: s492 modwork / mw-26-constant-parent-base-fields.pl
use strict; use warnings;
package Shape; sub new { my ($c, %a) = @_; my $s = bless { %a }, $c; $s->init; $s } sub init { } sub area { 0 } sub describe { my $s = shift; sprintf "%s area=%.2f", ref($s), $s->area }
package Circle; use parent -norequire, "Shape"; use constant PI => 4 * atan2(1, 1); use constant { UNIT => "cm", DEBUG => 0 }; sub area { PI * $_[0]{r} ** 2 }
package Square; use base "Shape"; sub init { $_[0]{side} //= 1 } sub area { $_[0]{side} ** 2 }
package main; print join(" | ", map { $_->describe } Circle->new(r => 1), Square->new(side => 3), Square->new), " ", Circle::UNIT, " ", (Circle->can("PI") ? "const-sub" : "no-sub"), " ", Circle->isa("Shape") ? "isa" : "nota", " ", __PACKAGE__->can("nope") ? "can" : "cannot", "\n";
