# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 334-string-eval -- harvested from s470/br-agent-ae6b0c4523801002e/s470br/ev.pl

my $x = 5; my $s = q{$x+1}; my $r = eval $s; print "$r\n";
