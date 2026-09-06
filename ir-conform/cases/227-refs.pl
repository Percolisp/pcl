# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 227-refs -- harvested from s469/bg-agent-af8ba53a7b37a7a58/s469bg/t7.pl

my $x = do { my $a = 1; $a + 2 };
sub f { 9 } my $ref = \&f;
my $z = do &$ref;
print $x + $z;
