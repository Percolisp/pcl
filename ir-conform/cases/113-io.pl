# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 113-io -- harvested from s470/bo-agent-a4964c8ec026d7882/s470bo/m5.pl

use strict; use warnings;
for my $m ('+>', '+<', '>', '<', '>>') {
  my $ok = open(my $fh, $m, undef);
  if ($ok) {
    my $w = eval { print $fh "hello\n"; 1 } || 0;
    my $r = 0; my $line = '';
    if ($w) { eval { seek($fh, 0, 0); $line = <$fh> // ''; $r = 1; 1 } }
    chomp $line;
    print "$m ok=1 wrote=$w read=[$line]\n";
    close $fh;
  } else {
    print "$m ok=0 err=$!\n";
  }
}
