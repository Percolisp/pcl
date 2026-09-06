# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 216-oo -- harvested from s470/bo-agent-a4964c8ec026d7882/s470bo/pr2.pl

package Foo; sub hi { "hi" }
package Baz; sub hi2 { "h2" }
package Bar; use parent -norequire, 'Foo', 'Baz';
package main;
print "ISA=[@Bar::ISA]\n";
