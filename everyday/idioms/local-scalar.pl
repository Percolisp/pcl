# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# origin: s491 idioms / idiom-01-local-scalar.pl
no warnings;
our $g = 1; sub show { $g } sub t { local $g = 2; show() } print t(), show(), "\n";
