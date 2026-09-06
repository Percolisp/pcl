# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 223-overload -- harvested from s470/bk-agent-ad0b34477151560d8/s470bk/bench-fe.pl

package P;
use overload q(+)=>sub{P->new($_[0]{v}+(ref($_[1])?$_[1]{v}:$_[1]))}, q(0+)=>sub{$_[0]{v}}, q("")=>sub{"P(".$_[0]{v}.")"}, fallback=>1;
sub new { bless {v=>$_[1]}, "P" }
package main;
my @l = (1, P->new(2), 3, P->new(4));
my $t = 0;
for my $k (1 .. 200000) { for my $e (@l) { $t += $e } $t = 0 }
print "done\n";
