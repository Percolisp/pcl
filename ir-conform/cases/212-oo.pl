# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 212-oo -- harvested from s470/bm-agent-ac55b94993add67d0/s470bm/f13.pl

package Foo; sub hi { "hi" }
package Bar; use parent qw( -norequire Foo );
package main;
print "ISA=[@Bar::ISA]\n";
print "isa_foo=", (Bar->isa("Foo")?1:0), "\n";
print "isa_nore=", (Bar->isa("-norequire")?1:0), "\n";
