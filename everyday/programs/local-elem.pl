# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# origin: s492 battery2 / b2-01-local-elem.pl
use strict; use warnings;
our %cfg = (mode => "a", depth => 1); our @stack = (1, 2, 3);
sub show { "$cfg{mode}/$cfg{depth}/@stack" }
sub t { local $cfg{mode} = "b"; local $stack[1] = 9; local $cfg{new} = 1; return show() . "/" . (exists $cfg{new} ? "new" : "nonew") }
print t(), "\n", show(), "/", (exists $cfg{new} ? "new" : "nonew"), "\n";
