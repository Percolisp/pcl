# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# origin: s491 idioms / idiom-37-system-backtick.pl
no warnings;
my $o = `echo hi`; my @l = `printf "a\nb\n"`; my $rc = system("true"); my $rc2 = system("sh", "-c", "exit 3") >> 8; open(my $p, "-|", "echo", "piped") or die; my $pl = <$p>; close $p; open(my $w, "|-", "cat") or die; print $w "to-cat\n"; close $w; print "$o", scalar(@l), " $rc $rc2 $pl";
