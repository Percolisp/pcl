# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 238-refs -- harvested from s470/bn-agent-a28fa0a7a0844c434/s470bn/p2b.pl

no strict 'refs';
our $g = 2; our @ga = (1,2,3); our %gh = (a=>1);
sub foo { 42 }
print "1:", ${'main::g'}, "\n";
print "2:", ${'g'}, "\n";
print "3:", scalar(@{'main::ga'}), "\n";
print "4:", ${'main::ga'}[1], "\n";
print "5:", ${'main::gh'}{'a'}, "\n";
print "6:", $#{'main::ga'}, "\n";
print "7:", scalar(keys %{'main::gh'}), "\n";
print "8:", &{'main::foo'}(), "\n";
