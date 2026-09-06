# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 200-misc -- harvested from s470/bi-agent-a4b9fedc2175a6513/s470bi/p1118/a.pl

my $first_captured_cell_name = "abc";
my $second_captured_cell_name = "def";
sub f { my $t = $first_captured_cell_name . $second_captured_cell_name; return "$t!" }
print f(), "\n";
