# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# origin: s493 perlfunc index walk / pf-local-misc.pl
our @a = (1,2,3); our %h = (k => 1); sub show { print "@a $h{k}\n" } sub t { local @a = (9); local $h{k} = 2; show() } t(); show(); my @s = (1..5); { local $#s; } print scalar(@s), "\n"; { local $, = "-"; local $\ = "!\n"; print 1,2,3 } print "x\n";
