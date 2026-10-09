# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# Overlay fixture (task #2878): disagrees with FactsConflict.pm on purpose.
package FactsConflict;
# CONST is `()` in the source; this says `($)` -- the transpile must die.
sub CONST ($);
1;
