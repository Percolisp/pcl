# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 083-context -- harvested from s470/s470bw-agent-a3fb82677892740ba/lu-args.pl

use strict;
use warnings;
use List::Util qw(first any all none notall);
my ($n, $v);
$v = first  { $n = scalar(@_); 1 } 7;  print "first  nargs=$n\n";
$v = any    { $n = scalar(@_); 1 } 7;  print "any    nargs=$n\n";
$v = all    { $n = scalar(@_); 1 } 7;  print "all    nargs=$n\n";
$v = none   { $n = scalar(@_); 0 } 7;  print "none   nargs=$n\n";
$v = notall { $n = scalar(@_); 0 } 7;  print "notall nargs=$n\n";
