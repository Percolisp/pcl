# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 112-io -- harvested from s470/bo-agent-a4964c8ec026d7882/s470bo/m1.pl

use strict; use warnings;
my $ok1 = open my $fh1, "+>", undef;
print "plain=", ($ok1 ? 1 : 0), "\n";
print "paren=", ((open my $fh2, "+>", undef) ? 1 : 0), "\n";
sub take { my ($v, $d) = @_; print "take[$d]=", ($v ? 1 : 0), "\n" }
take((open my $fh3, "+>", undef), "call-arg");
