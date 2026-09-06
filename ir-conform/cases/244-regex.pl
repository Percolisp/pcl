# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 244-regex -- harvested from s470/bi-agent-a4b9fedc2175a6513/s470bi/p1083/ee.pl

my $s = "3";
(my $t = $s) =~ s/(\d)/'$1+1'/ee;
print "ee: $t\n";
my $u = "3";
$u =~ s/(\d)/'$1+1'/e;
print "e:  $u\n";
my $v = "ab";
my $code = '"X" . uc($1)';
$v =~ s/(a)/$code/ee;
print "ee2: $v\n";
