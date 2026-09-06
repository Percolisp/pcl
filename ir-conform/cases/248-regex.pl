# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 248-regex -- harvested from s470/bn-agent-a28fa0a7a0844c434/s470bn/macro-text.pl

use strict; use warnings;
my $n = $ENV{N};
# ~200 kB of lines, built once; the loop is the regex/substr/.=/split/join work.
# Text::Wrap was the plan's named example and does NOT run under PCL (#1186,
# "This shouldn't happen" at Wrap.pm:78), so the row does its own wrapping.
my @src = map { "line $_: the quick brown fox jumps over the lazy dog, number $_, tag=t" . ($_ % 17) } 1 .. 2000;
my $blob = join("\n", @src);
my $s = 0;
for (1 .. $n) {
  my $out = '';
  for my $l (split /\n/, $blob) {
    next unless $l =~ /tag=t(\d+)/;
    my $tag = $1;
    my $up = uc(substr($l, 0, 20));
    $out .= "$up|$tag;";
    $s++ while $l =~ /o/g;
  }
  $s += length($out);
  my $para = join(' ', @src[0 .. 19]);
  my ($col, $wrapped) = (0, '');
  for my $w (split /\s+/, $para) {
    if ($col + length($w) > 40) { $wrapped .= "\n  "; $col = 2 }
    $wrapped .= "$w "; $col += length($w) + 1;
  }
  $s += length($wrapped);
}
print "$s\n";
