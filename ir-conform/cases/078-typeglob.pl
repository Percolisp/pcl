# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 078-typeglob -- harvested from s470/bi-agent-a4b9fedc2175a6513/s470bi/p1020/undef_.pl

sub f { undef *_; return defined *_{ARRAY} ? "def" : "undef" }
print "in-sub *_{ARRAY} after undef: ", f(1,2,3), "\n";
