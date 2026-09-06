# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 091-hash -- harvested from s470/bi-agent-a4b9fedc2175a6513/s469bi/p1020/a.pl

our $s = 5; our @a = (9); our %h = (k=>1);
sub c { "code" }
undef *s; undef *a; undef *h; undef *c;
print "scalar: ", (defined $s ? "STILL $s" : "cleared"), "\n";
print "array:  ", (@a ? "STILL @a" : "cleared"), "\n";
print "hash:   ", (%h ? "STILL ".join(",",%h) : "cleared"), "\n";
print "code:   ", (defined &c ? "STILL ".c() : "cleared"), "\n";
