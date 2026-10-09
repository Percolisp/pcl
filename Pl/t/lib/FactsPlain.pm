# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# Fixture for Pl/t/facts-overlay-01.t (task #2878): a module with NO overlay.
package FactsPlain;
require Exporter;
our @ISA = ('Exporter');
our @EXPORT = qw(twice);
sub twice (&) { my $c = shift; $c->() + $c->() }
sub PI () { 3 }
1;
