# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 122-io -- harvested from s470/bs-agent-abb21d9ade6a1c1c3/s470bs/p25.pl

my $o = open(F, '.') ? 1 : 0;
my $b = $o ? (binmode(F) ? 1 : 0) : -1;
my $s = ($o && $b>0) ? (defined(sysread(F, $_, 1)) ? 1 : 0) : -1;
my $err = $! + 0;
print "open=$o binmode=$b sysread=$s err=$err\n";
close F;
$! = 0;
my $o2 = open(F, '.') ? 1 : 0;
my $l = $o2 ? <F> : undef;
print "open2=$o2 err2=", ($!+0), " defined=", (defined($l)?1:0), "\n";
close F;
