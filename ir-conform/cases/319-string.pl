# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 319-string -- harvested from s470/br-agent-ae6b0c4523801002e/s470br/probe/chop-high.pl

my $utf = chr(0x80000001) . chr(0x80000000);
my $result = chop($utf);
print "remnant-ok: ", ($utf eq chr(0x80000001) ? 1 : 0), "\n";
print "result-ok: ",  ($result eq chr(0x80000000) ? 1 : 0), "\n";
no warnings;
$utf = chr(0x7fffffffffffffff) . chr(0x7ffffffffffffffe);
$result = chop($utf);
print "hi-remnant-ok: ", ($utf eq chr(0x7fffffffffffffff) ? 1 : 0), "\n";
print "hi-result-ok: ",  ($result eq chr(0x7ffffffffffffffe) ? 1 : 0), "\n";
