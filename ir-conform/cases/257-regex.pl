# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 257-regex -- harvested from s470/bq-agent-a972ef7a99864b78a/s470bq/t1.pl

my $n = 3;
print "fib: $n\n";
my $s = "a\tb";
if ($s =~ /(\w+)\s+/) { print "m=$1\n" }
$s =~ s/a/X/g;
$s =~ tr/b/Y/;
my $re = qr/ab+c/i;
print "$s|$re\n";
my $p = "dyn"; print "yes\n" if "dyn" =~ /$p/;
print <<EOT;
heredoc line1
line2
EOT
