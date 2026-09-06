# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 140-local -- harvested from s470/bq-agent-a972ef7a99864b78a/s470bq/t2.pl

sub f { my $x = shift; return $x+1 }
sub g { print "hi\n"; local $/ = ":"; die "x" if $x; }
sub h { my $s = "a"; $s =~ /a*+/ ? 1 : 2 }
print f(1), "\n";
