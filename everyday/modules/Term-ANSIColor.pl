# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# origin: s492 modwork / mw-13-Term-ANSIColor.pl
use strict; use warnings; use Term::ANSIColor qw(color colored colorstrip);
my $s = colored("warn", "bold red") . color("reset"); print join(" ", length($s), colorstrip($s), (color("green") eq "\e[32m" ? "green-ok" : "green-bad"), colorstrip(colored(["blue on_white"], "x"))), "\n";
