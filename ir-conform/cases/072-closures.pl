# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 072-closures -- harvested from s469/bg-agent-af8ba53a7b37a7a58/s469bg/t2.pl

my @out;
{
  my $a = 7;
  my $b = 8;
  push @out, $a + $b;
}
sub named { my $x = 1; my $c = sub { $x + 1 }; return $c->() }
push @out, named();
print join(",", @out), "\n";
