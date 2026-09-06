# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 116-io -- harvested from s470/br-agent-ae6b0c4523801002e/s470br/br/warnre.pl

my $n = 0;
$SIG{__WARN__} = sub { $n++; warn "nested: $_[0]" if $n < 10; };
warn "outer\n";
print "handler calls: $n\n";

# and: does the nested warn reach stderr (the default action)?
my $m = 0;
$SIG{__WARN__} = sub { $m++; warn "INNER-REACHED-STDERR\n" };
warn "second\n";
print "second handler calls: $m\n";
