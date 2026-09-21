# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# origin: s492 battery2 / b2-16-string-number.pl
use strict; use warnings; no warnings 'numeric';
print join(" ", "10" + "20", "3.5abc" * 2, "abc" + 1, "1e3" + 0, "0x10" + 0, "010" + 0, ".5" + 0, "5." + 0, " 12 " + 0, "-" . 5, "5" . "6", "2" ** "3", 7 <=> "7.0", "abc" lt "abd" ? "lt" : "ge", "10" == 10.0 ? "numeq" : "ne", "1.0" eq "1" ? "streq" : "strne"), "\n";
print join(" ", 1/4, 1e21, 1e-7, 0.1 + 0.7, 1_000 * 1_000, 2**53 + 1, -17 / 4, int(-17 / 4), -17 % 4, 17 % -4, 2**0.5 * 2**0.5 == 2 ? "exact" : "inexact", 10 % 3.7, 255 == 0xff ? "hex" : "nohex", 1 <=> 2, "a" x 2.7, "9" + "1" . "0"), "\n";
my $i = "9"; $i++; my $s = "Zz"; $s++; my $e = ""; $e++; my $u; { no warnings; $u++; } my $neg = "-3"; $neg++; print "$i $s $e $u $neg ", "12abc" == 12 ? "pref" : "nopref", " ", "abc" == 0 ? "zero" : "nonzero", "\n";
