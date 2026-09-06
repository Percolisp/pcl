# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 255-regex -- harvested from s470/bq-agent-a972ef7a99864b78a/s470bq/c2.pl

my $x = "abc"; my $r = { k => "z" }; my $ar = [1,2];
my $m  = $x =~ /a.c/;
my $q  = qr/\d+/i;
my $mi = $x =~ /$x/;
my $mc = $x =~ /pre${x}post/i;
my $md = $x =~ /$r->{k}/;
my $me = $x =~ /$ar->[0]/;
$x =~ s/a/b/;
$x =~ tr/a/b/;
print $m;
