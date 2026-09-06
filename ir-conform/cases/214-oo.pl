# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 214-oo -- harvested from s470/bo-agent-a4964c8ec026d7882/s470bo/m8.pl

package Diamond_A; sub hello { "A" }
package Diamond_B; use base ('Diamond_A');
package Diamond_C; use base ('Diamond_A');
package Diamond_D; use base ('Diamond_B', 'Diamond_C');
package main;
print "ISA_D=[@Diamond_D::ISA]\n";
print "hello=", Diamond_D->hello, "\n";
print "isa=", (Diamond_D->isa('Diamond_A') ? 1 : 0), "\n";
