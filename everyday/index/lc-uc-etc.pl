# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# origin: s493 perlfunc index walk / pf-lc-uc-etc.pl
print ucfirst(lc("HELLO wORLD")), " ", lcfirst("ABC"), " ", uc("straße"), "\n"; print join(",", map { sprintf "%s", $_ } (abs(-3), int(-3.7), sqrt(16), 2**10, 10 % 3, -10 % 3, atan2(1,1)*4, exp(0), log(exp(1)), hex("ff"), oct("0x1f"), oct("755"), oct("0b101"))), "\n"; print join(",", ord("A"), chr(66), lc("ÀB"), length("abc"), index("hello","l"), rindex("hello","l"), substr("hello",1,3), reverse("abc"), ucfirst("élan")), "\n"; print "a" x 3, "-", join(",", (1,2) x 2), "\n"; print join(",", sort { $a <=> $b } (10, 9, 100, 1)), " ", join(",", reverse 1..4), "\n"; print join("|", split(//, "abc")), " ", join("|", split(/,/, "a,b,,c,,")), " ", join("|", split(" ", "  a  b ")), " ", join("|", split(/(,)/, "a,b")), " ", scalar(my @x = split(/,/, "a,b,c", 2)), "\n"; print lc(sprintf("%s", 1e15)), " ", 1e16, " ", 0.1+0.2, " ", 1/3, " ", 1e100, " ", -17/4, " ", 2**0.5, " ", 9**20, " ", int(7.999999999999999), "\n";
