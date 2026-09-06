# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 123-io -- harvested from s470/bs-agent-abb21d9ade6a1c1c3/s470bs/p26.pl

my $d = -d 't';
print "d=", ($d?1:0), " errno-after-filetest=", ($!+0), "\n";
open F, '.' and binmode F and sysread F, $_, 1;
my $err = $! + 0;
print "err=$err\n";
close F;
