# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 318-string -- harvested from s470/br-agent-ae6b0c4523801002e/s470br/p-pfd.pl

no strict 'refs';
our ($name, $name_utf8);
$name_utf8 = $name = chr 9787;
utf8::encode $name_utf8;
print "len name=", length($name), " len utf8=", length($name_utf8), "\n";
$name->$* = "Face";
print "via unicode=", (defined($name->$*) ? $name->$* : 'undef'), "\n";
print "via octets=", (defined($name_utf8->$*) ? $name_utf8->$* : 'undef'), "\n";
