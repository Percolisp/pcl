# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 233-refs -- harvested from s470/bj-agent-a7cf766b7d18923d1/s470bj/probe/hd.pl

my @a;
my $s = <<"END";
x@{[ push @a, 99 ]}y
END
push @a, 1;
print "$s@a\n";
