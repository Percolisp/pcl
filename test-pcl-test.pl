# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# Simple test to verify PCL test infrastructure

plan(5);

ok(1, "true is ok");
ok(1 == 1, "1 == 1");

is(2 + 2, 4, "addition works");
isnt(2 + 2, 5, "2 + 2 is not 5");

cmp_ok(10, '>', 5, "10 > 5");
