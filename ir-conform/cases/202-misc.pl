# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 202-misc -- harvested from s470/bo-agent-a4964c8ec026d7882/s470bo/sig.pl

use feature 'signatures';
no warnings;
sub f ($a = 222) { return $a }
print f(), "\n";
print f(7), "\n";
