# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 245-regex -- harvested from s470/bj-agent-a7cf766b7d18923d1/s470bj/probe/ro.pl

my @a=(1,2);
Internals::SvREADONLY(@a,1);
my $ok = eval { push @a, 3; 1 };
print "ok=", ($ok?1:0), " err=", ($@ =~ /read-only/ ? "ro" : "other:$@"), " n=", scalar(@a), "\n";
