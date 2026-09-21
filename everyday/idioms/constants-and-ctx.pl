# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# origin: s491 idioms / idiom-35-constants-and-ctx.pl
no warnings;
use constant { PI => 3.14159, NAMES => ["a", "b"] }; use constant DEBUG => 0; my @l = (PI, NAMES->[1], DEBUG ? "dbg" : "nodbg", __PACKAGE__, __LINE__ > 0 ? "line" : "x"); print "@l\n";
