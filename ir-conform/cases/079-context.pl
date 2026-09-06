# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 079-context -- harvested from s469/bg-agent-af8ba53a7b37a7a58/s469bg/t8.pl

sub fib { my ($n) = @_; my $a = 0; my $b = 1; for my $i (2..$n) { my $c = $a + $b; $a = $b; $b = $c; } return $b; }
print fib(30), "\n";
