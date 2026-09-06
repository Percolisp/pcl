# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 225-overload -- harvested from s470/bk-agent-ad0b34477151560d8/s470bk/pov/m1005c.pl

package AB;
use overload q(<) => sub { my ($s,$o,$sw)=@_; $sw ? 1 : 0 },
             q(-) => sub { "AB-MINUS" }, q("") => sub { "AB" };
sub new { bless {}, shift }
package BL;
use overload q(bool) => sub { 0 }, q("") => sub { "BL" };
sub new { bless {}, shift }
package main;
print abs(AB->new), "\n";
print((!BL->new ? "T" : "F"), "\n");
print abs(-7), " ", abs(7), " ", abs(-7.5), "\n";
printf "%.4f %.4f %.4f\n", sqrt(9), cos(0), sin(0);
printf "%.4f %.4f %.4f\n", exp(1), log(1), atan2(1, 1);
print((!0 ? "T" : "F"), (!1 ? "T" : "F"), (!"" ? "T" : "F"), "\n");
print((not 0), "|", (not 1), "|\n");
