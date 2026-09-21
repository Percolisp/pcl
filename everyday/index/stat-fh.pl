# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# origin: s493 perlfunc index walk / pf-stat-fh.pl
open my $h, "<", "/etc/passwd" or die; my @s = stat($h); print scalar(@s), "\n"; my $sz = (stat $h)[7]; print $sz > 0 ? "ok\n" : "zero\n"; print -s $h ? "s\n" : "nos\n"; use File::stat; my $st = File::stat::stat("/etc/passwd"); print ref($st), " ", ($st->size > 0 ? 1 : 0), "\n";
