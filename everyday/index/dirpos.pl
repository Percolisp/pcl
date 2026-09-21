# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# origin: s493 perlfunc index walk / pf-dirpos.pl
opendir my $d, "/etc" or die; my $e = readdir $d; my $pos = telldir $d; my $n1 = readdir $d; seekdir $d, $pos; my $n2 = readdir $d; print $n1 eq $n2 ? "same\n" : "differ\n"; rewinddir $d; my $f = readdir $d; print $f eq $e ? "rewound\n" : "no\n"; closedir $d;
