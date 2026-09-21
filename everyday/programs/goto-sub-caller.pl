# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# origin: s492 battery2 / b2-13-goto-sub-caller.pl
use strict; use warnings;
sub inner { my @c = caller(0); my @c1 = caller(1); return "$c[3] from " . ($c1[3] // "main") . " args=@_" }
sub wrapper { unshift @_, "w"; goto &inner }
sub outer { wrapper("x", "y") }
sub whoami { (caller(0))[3] } sub line { (caller)[2] > 0 ? "line" : "noline" }
print outer(), "\n", whoami(), " ", line(), " ", __PACKAGE__, " ", (defined &inner ? "defined" : "undef"), " ", (exists &nosuch ? "exists" : "nosuch"), "\n";
