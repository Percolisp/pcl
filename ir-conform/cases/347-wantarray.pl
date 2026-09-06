# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 347-wantarray -- harvested from s470/bi-agent-a4b9fedc2175a6513/s469bi/p1045/c.pl

our @a = (7,8,9);
sub ra    { return @a }
sub rgoto { my $t = 0; goto &ra }
my @x = rgoto();
print "arr: @x\n";
my @y = ( rgoto() );
print "lst: @y\n";
print "anon: @{[ rgoto() ]}\n";
my $s = rgoto();
print "sca: $s\n";
sub wa { return wantarray ? "L" : (defined wantarray ? "S" : "V") }
sub gw { my $t = 0; goto &wa }
my @w = gw(); my $w2 = gw();
print "ctx: @w $w2\n";
print "ctx2: ", join(",", gw()), " ", scalar(gw()), "\n";
