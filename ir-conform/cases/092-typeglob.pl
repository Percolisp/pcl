# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 092-typeglob -- harvested from s470/bi-agent-a4b9fedc2175a6513/s469bi/p1020/b.pl

# what perl does to the slot INTROSPECTION and to later use
our @a = (9); our %h = (k=>1); our $s = 5;
undef *a; undef *h; undef *s;
print "arr slot: ", (defined *a{ARRAY} ? "present" : "undef"), "\n";
print "hsh slot: ", (defined *h{HASH}  ? "present" : "undef"), "\n";
print "sca slot: ", (defined *s{SCALAR} ? "present" : "undef"), "\n";
push @a, 42; $h{z} = 1; $s = 7;
print "after use: @a / ", join(",", %h), " / $s\n";
sub c { 1 }
undef *c;
print "code: ", (defined &c ? "STILL" : "cleared"), "\n";
print "code slot: ", (defined *c{CODE} ? "present" : "undef"), "\n";
