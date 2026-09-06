# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 240-refs -- harvested from s470/s470bw-agent-a3fb82677892740ba/lu-guard.pl

use strict;
use warnings;
use List::Util qw(first any all none notall);
my @n; my $v;
$v = first  { push @n, scalar(@_); 1 } 7;
$v = any    { push @n, scalar(@_); 1 } 7;
$v = all    { push @n, scalar(@_); 1 } 7;
$v = none   { push @n, scalar(@_); 0 } 7;
$v = notall { push @n, scalar(@_); 0 } 7;
sub cb { my $n = shift; return defined($n) ? "ARG" : "none" }
print join(",", @n), "|", first(\&cb, 5), "\n";
