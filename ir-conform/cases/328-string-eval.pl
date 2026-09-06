# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 328-string-eval -- harvested from s470/bi-agent-a4b9fedc2175a6513/s470bi/p1084/e1.pl

my $src = <<'SRC';
# a curly quote ’ in a comment
$main::v = 42;
$main::len = length("’");
1;
SRC
eval $src or die "eval: $@";
print "eval: v=$main::v len=$main::len\n";
