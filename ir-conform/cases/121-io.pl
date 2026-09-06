# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 121-io -- harvested from s470/br-agent-ae6b0c4523801002e/s470br/probe/print-overlong.pl

use strict;
use warnings;

no warnings 'utf8';

# These form overlong "oops"
open my $fh, "<:utf8", \"\xC1\xAF\xC1\xAF\xC1\xB0\xC1\xB3"
    or die "Could not open\n";
read($fh, my $s, 10) or die "Could not read\n";
print $s;
