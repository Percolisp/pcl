# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# origin: s491 idioms / idiom-06-string-eval.pl
no warnings;
my $v = 3; my $r = eval q{ my $w = $v * 2; $w + 1 }; print "$r ", (eval { die "x\n"; 1 } ? "lived" : "died:$@");
