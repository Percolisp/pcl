# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 117-io -- harvested from s470/br-agent-ae6b0c4523801002e/s470br/p1115/probe6.pl

binmode(STDOUT, ":utf8");
print "A\x{2019}\n";
print STDERR "MARK1\n";
binmode(STDOUT);
print "B\x{2019}\n";
print STDERR "MARK2\n";
