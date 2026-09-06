# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 326-string-eval -- harvested from s470/bi-agent-a4b9fedc2175a6513/s470bi/p1083/nest.pl

sub probe { my ($n,$m) = @_; my @s = ($m->(), $m->()); print "$n: ", ($s[0]==$s[1]?"same":"different"), "\n" }
probe('outer-of-inner-eval', sub { sub { my $q = sub { eval "1" }; 2 } });
probe('inner-eval-itself',   sub { my $o = sub { sub { eval "1" } }; $o->() });
probe('ee-in-nested',        sub { sub { my $q = sub { s/1/1/ee }; 3 } });
probe('sq-pattern',          sub { sub { my $x; s'$x'1' } });
