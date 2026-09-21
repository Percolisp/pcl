# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# origin: s493 perlfunc index walk / pf-priority.pl
my $p = getpriority(0, 0); print defined $p ? "prio-ok\n" : "undef\n"; print setpriority(0, 0, $p) ? "set\n" : "noset\n"; print getppid() > 0 ? "ppid\n" : "no\n"; print getpgrp() > 0 ? "pgrp\n" : "no\n";
