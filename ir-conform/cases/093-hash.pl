# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 093-hash -- harvested from s470/bi-agent-a4b9fedc2175a6513/s470bi/p1020/copy.pl

our $x = 5; our @y = (1,2); our %z = (k=>1); sub s1 { "S1" }
our $b2 = 7; our @b2 = (9); sub b2 { "B2" }
*x = *b2;
print "copy: x=", (defined $x ? $x : "undef"),
      " y-after-copy=", (@y ? "@y" : "empty"),
      " s1=", (defined &s1 ? s1() : "gone"), "\n";
*s1 = *b2;
print "codecopy: s1=", (defined &s1 ? s1() : "gone"), "\n";
*neverdefinedglob2 = *b2;
*z = *neverdefinedglob;
print "clearcopy: z=", (%z ? "STILL" : "cleared"),
      " created=", (defined $main::neverdefinedglob ? "yes" : "no"), "\n";
undef *nosuchthing;
print "undef-of-nothing: created=", (defined $main::nosuchthing ? "yes" : "no"),
      " arr=", (defined *nosuchthing{ARRAY} ? "yes" : "no"), "\n";
