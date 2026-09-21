# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# origin: s493 perlfunc index walk / pf-goto-sub.pl
sub a { goto &b } sub b { return "b got @_" } print a(1,2), "\n"; my $i = 0; L: $i++; goto L if $i < 3; print "i=$i\n"; sub c { my $n = shift; goto DONE if $n; print "not\n"; DONE: print "done\n" } c(1);
