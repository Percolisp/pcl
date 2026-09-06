# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 322-string -- harvested from s470/br-agent-ae6b0c4523801002e/s470br/ue.pl

my $s = "\x{2080}x";
utf8::encode($s);
print length($s), "\n";
my $t = $s; utf8::decode($t); print length($t), "\n";
