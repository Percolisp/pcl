# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 344-string-eval -- harvested from s470/fable/bi/p1083.pl

my (@plain, @ev, @ee, @re, @nest);
for (1..2) {
  push @plain, sub { 1 };
  push @ev, sub { eval "1" };
  push @ee, sub { my $s = "a"; $s =~ s/a/'b'/ee; $s };
  { use re 'eval'; my $p = "(?{1})"; push @re, sub { "x" =~ /$p/ } }
  push @nest, sub { sub { eval "2" } };
}
print "plain:", ($plain[0] == $plain[1] ? "same" : "diff"), " ev:", ($ev[0] == $ev[1] ? "same" : "diff"),
      " ee:", ($ee[0] == $ee[1] ? "same" : "diff"), " re:", ($re[0] == $re[1] ? "same" : "diff"),
      " nest:", ($nest[0] == $nest[1] ? "same" : "diff"), "\n";
