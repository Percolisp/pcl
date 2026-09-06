# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 247-regex -- harvested from s470/bm-agent-ac55b94993add67d0/s470bm/verify3.pl

my @c = ("abc" =~ /(z)(y)/);
print scalar(@c), "\n";
my $q = qr/ab/;
my $x = "ab"; my $re = "a" . "b"; my $m = ($x =~ /$re/);
print "$q $m\n";
