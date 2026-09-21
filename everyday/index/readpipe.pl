# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# origin: s493 perlfunc index walk / pf-readpipe.pl
my $x = readpipe("echo hi"); print $x; my @l = readpipe("printf \"a\\nb\\n\""); print scalar(@l), "\n"; print `echo bt`; my $rc = system("true"); print "rc=$rc\n"; print qx{echo $$} == $$ ? "pid-interp\n" : "no\n";
