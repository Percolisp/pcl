# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 003-array -- harvested from s469/bh-agent-ab83d94ab96620fab/s469bh/p1056/pr16a.pl

my @a = (10, 20, 30, 40, 50);
for (my $i = @a; -- $i;) {
  my $j = ($i * 7) % $i;
  @a[$i, $j] = @a[$j, $i];
}
print "P16a @a\n";
