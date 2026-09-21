# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# origin: s491 idioms / idiom-07-die-object.pl
no warnings;
package E; sub new { bless {msg => $_[1]}, $_[0] } sub msg { $_[0]{msg} } package main; eval { die E->new("boom") }; print ref($@), " ", $@->msg, " ", ($@->isa("E") ? 1 : 0), "\n";
