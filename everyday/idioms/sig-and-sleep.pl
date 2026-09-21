# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# origin: s491 idioms / idiom-36-sig-and-sleep.pl
no warnings;
my $got = 0; local $SIG{ALRM} = sub { $got = 1 }; local $SIG{__WARN__} = sub { print "warned: $_[0]" }; warn "w1\n"; alarm 1; my $t0 = time; sleep 3; print " $got ", (time - $t0 <= 2 ? "interrupted" : "slept-through"), "\n";
