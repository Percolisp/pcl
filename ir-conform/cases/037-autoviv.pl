# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 037-autoviv -- harvested from s470/bi-agent-a4b9fedc2175a6513/s470bi/p1020/slots.pl

our $s = 5; our @a = (9); our %h = (k=>1);
sub c { "code" }
sub d;                       # forward declaration only
print "pre:  defined&c=", (defined &c ?1:0), " exists&c=", (exists &c ?1:0),
      " defined&d=", (defined &d ?1:0), " exists&d=", (exists &d ?1:0), "\n";
undef *s; undef *a; undef *h; undef *c; undef *d;
print "scalar: ", (defined $s ? "STILL $s" : "cleared"), "\n";
print "array:  ", (@a ? "STILL @a" : "cleared"), "\n";
print "hash:   ", (%h ? "STILL ".join(",",%h) : "cleared"), "\n";
print "code:   ", (defined &c ? "STILL ".c() : "cleared"), "\n";
print "exists&c=", (exists &c ?1:0), " exists&d=", (exists &d ?1:0), "\n";
print "slots: SCALAR=", (defined *s{SCALAR} ? "def" : "undef"),
      " ARRAY=", (defined *a{ARRAY} ? "def" : "undef"),
      " HASH=", (defined *h{HASH} ? "def" : "undef"),
      " CODE=", (defined *c{CODE} ? "def" : "undef"), "\n";
push @a, 42; print "after push: @a\n";
