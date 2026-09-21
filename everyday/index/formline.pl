# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# origin: s493 perlfunc index walk / pf-formline.pl
formline("@<<< @>>>\n", "ab", "cd"); print $^A; $^A = ""; my $s = swrite("@<<<|@>>>|", 1, 2); print "$s\n"; sub swrite { my $f = shift; $^A = ""; formline($f, @_); return $^A }
