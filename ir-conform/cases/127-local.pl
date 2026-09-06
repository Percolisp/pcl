# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 127-local -- harvested from s469/bg-agent-af8ba53a7b37a7a58/s469bg/inverse2.pl

use strict; use warnings;
for my $dir (qw(emit-base emit-new)) {
  my ($plet, $psub, $praw) = (0, 0, 0);
  for my $f (glob "scratch/s469bg/$dir/*.lisp") {
    open my $fh, '<:raw', $f or next; local $/; my $s = <$fh>;
    $plet += () = $s =~ /\(p-let[\s(]/g;
    $psub += () = $s =~ /\(p-sub[\s(]/g;
    $praw += () = $s =~ /\(p-raw-params[\s(]/g;
  }
  printf "%-9s p-let=%-5d p-sub=%-4d p-raw-params=%d\n", $dir, $plet, $psub, $praw;
}
