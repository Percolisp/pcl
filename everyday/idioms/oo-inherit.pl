# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# origin: s491 idioms / idiom-17-oo-inherit.pl
no warnings;
package A; sub new { my ($c,%a) = @_; bless {%a}, $c } sub hi { "A:" . $_[0]->name } sub name { $_[0]{n} } package B; our @ISA = ("A"); sub hi { my $s = shift; "B>" . $s->SUPER::hi() } package main; my $o = B->new(n => "x"); print $o->hi, " ", $o->can("name") ? 1 : 0, " ", B->isa("A") ? 1 : 0, " ", ref $o, "\n";
