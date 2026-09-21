# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# origin: s491 idioms / idiom-32-list-util.pl
# s495e repair: `scalar(pairs(1..4))` printed a _Pair object, i.e. its ADDRESS,
# so perl's own answer changed between runs and the program could never be an
# oracle.  `ref(scalar(...))` asks the same question (what does pairs return in
# scalar context?) and is a fact of the program.
no warnings;
use List::Util qw(sum max min first reduce shuffle uniq any all none pairs); print join(" ", sum(1..4), max(3, 9, 2), min(3, 9, 2), first { $_ > 2 } (1..5)), " ", reduce { $a * $b } 1..5; print " ", join(",", uniq(1, 1, 2, 3, 3)), " ", (any { $_ == 2 } 1, 2, 3) ? "any" : "none", " ", ref(scalar(pairs(1..4))), "\n";
