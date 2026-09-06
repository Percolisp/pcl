# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 275-sort -- harvested from s470/a5-agent-a9d4cd4f997886d1b/s470a5/probe/doblk.pl

my @d = (3,1,2);
$_++ for do { sort { $a <=> $b } @d };
print "do-block: @d\n";
my @e = (3,1,2);
$_++ for eval { sort { $a <=> $b } @e };
print "eval-block: @e\n";
sub s1 { return sort { $a <=> $b } @main::f }
our @f = (3,1,2);
$_++ for s1();
print "sub-tail: @f\n";
