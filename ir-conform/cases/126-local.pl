# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 126-local -- harvested from s469/bg-agent-af8ba53a7b37a7a58/s469bg/inverse.pl

use strict; use warnings;
for my $dir (qw(emit-base emit-new)) {
  my ($facts, $plist, $classed, $plet) = (0, 0, 0, 0);
  for my $f (glob "scratch/s469bg/$dir/*.lisp") {
    open my $fh, '<:raw', $f or next; local $/; my $s = <$fh>;
    $facts   += () = $s =~ /:perl "|:why :|:captured t/g;
    $plist   += () = $s =~ /\(p-sub \S+\s+\(&rest %_args\)\s+\(/g;
    $classed += () = $s =~ /\(p-raw-params \(\(/g;
    $plet    += () = $s =~ /\(p-let /g;
  }
  printf "%-9s fact-tokens=%-5d p-sub-with-plist=%-5d p-raw-params-classed=%-4d p-let=%d\n",
    $dir, $facts, $plist, $classed, $plet;
}
