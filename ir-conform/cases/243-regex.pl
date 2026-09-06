# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 243-regex -- harvested from s470/bi-agent-a4b9fedc2175a6513/s469bi/p1084/u.pl

my $s = "\x{e2}\x{80}\x{99}";
print "before: flag=", (utf8::is_utf8($s)?1:0), " len=", length($s), "\n";
utf8::upgrade($s);
print "upgr:   flag=", (utf8::is_utf8($s)?1:0), " len=", length($s), "\n";
my $c = $s;
my $r = utf8::decode($c);
print "decode: r=", ($r?1:0), " len=", length($c), " ord0=", ord($c), "\n";
my $d = "\x{e9}";
utf8::upgrade($d);
my $r2 = utf8::decode($d);
print "bad:    r=", ($r2?1:0), " len=", length($d), " ord0=", ord($d), "\n";
my $w = "\x{2019}";
print "wide:   has>255=", ($w =~ /[^\x00-\xFF]/ ? 1 : 0), "\n";
print "narrow: has>255=", ($s =~ /[^\x00-\xFF]/ ? 1 : 0), "\n";
