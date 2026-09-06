# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 325-string-eval -- harvested from s470/bi-agent-a4b9fedc2175a6513/s470bi/p1083/g2.pl

sub probe { my ($n,$m) = @_; my @s = ($m->(), $m->());
            print "$n: ", ($s[0] == $s[1] ? "same" : "different"), "\n" }
probe("str-eval",    sub { sub { eval "1" } });
probe("ee",          sub { sub { s/1/1/ee } });
probe("re-eval+pat", sub { sub { use re "eval"; my $x; s/$x/1/ } });
