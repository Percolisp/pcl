# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 138-local -- harvested from s470/bo-agent-a4964c8ec026d7882/s470bo/m7.pl

use strict; use warnings;
for my $m ('+>', '+<', '>', '<', '>>') {
  my @w;
  local $SIG{__WARN__} = sub { push @w, $_[0] };
  my $ok = open(my $fh, $m, undef);
  my $pr = print($fh "hello\n") ? 1 : 0;
  my $sk = seek($fh, 0, 0) ? 1 : 0;
  my $line = <$fh>;
  my $def = defined $line ? 1 : 0;
  $line //= ''; chomp $line;
  my $ws = join('|', map { my $x = $_; $x =~ s/ at .*//s; $x } @w);
  print "$m ok=", ($ok ? 1 : 0), " print=$pr seek=$sk defined=$def read=[$line] warns=[$ws]\n";
  close $fh;
}
