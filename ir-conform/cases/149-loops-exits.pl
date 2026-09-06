# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 149-loops-exits -- harvested from s470/bi-agent-a4b9fedc2175a6513/s469bi/p1045/a.pl

our @a = (7,8,9);
sub ra    { return @a }
sub rgoto { my $t = 0; goto &ra }
sub rgotoref { my $t = 0; my $c = \&ra; goto &$c }
sub rgotoblk { my $t = 0; goto &{ \&ra } }
print "n:  @{[ rgoto() ]} / ", scalar(rgoto()), "\n";
print "r:  @{[ rgotoref() ]} / ", scalar(rgotoref()), "\n";
print "b:  @{[ rgotoblk() ]} / ", scalar(rgotoblk()), "\n";
