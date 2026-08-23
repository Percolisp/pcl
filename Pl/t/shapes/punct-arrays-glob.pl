# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later
#
# SHAPES corpus (task #496) — punctuation arrays and glob/readline: `@?` and
# its siblings (#415, #451), `<~>` and the glob word model (#450), readline vs
# glob disambiguation.  Lifted from Pl/t/punct-array-glob-01.t's oracle
# programs (the `<<'PL'` heredocs), one block-scoped package each.  Valid
# perl: run it (some snippets print their own diagnostics).
{ package S01;
  no warnings; no strict;
  print "empty:", (! @? ? 1 : 0), ":", scalar(@?), "\n";
  @? = (1, 2, 3);
  @! = (4); @. = (5); @/ = (6); @~ = (7); @^ = (8);
  @& = (9); @% = (10); @= = (11); @< = (12); @> = (13);
  print "n:", scalar(@?), " e:", $?[1], "\n";
  print "all:", scalar(@!), scalar(@.), scalar(@/), scalar(@~), scalar(@^),
                scalar(@&), scalar(@%), scalar(@=), scalar(@<), scalar(@>), "\n";
  my @c = @?;
  print "copy:", scalar(@c), "\n";
}
{ package S02;
  no warnings; no strict;
  print "A\n";
  print "e:", $?[1], "\n";
  print "B\n";
}
{ package S03;
  no warnings; no strict;
  my @a = (1, 2, 3);
  my $r = \@a;
  print scalar(@$r), scalar(@{$r}), join("", @{[ 4, 5 ]}), "\n";
  print( (@a ? "y" : "n"), (@$r ? "y" : "n"), 7 % 3, "\n");
}
{ package S04;
  print "A\n";
  my @g = <~>;
  print "n:", (scalar(@g) > 0 ? 1 : 0), "\n";
  print "B\n";
}
{ package S05;
  for my $p ('~', '~/.bashrc', '~root') {
      my @g = glob($p);
      print "$p=", scalar(@g), ":", join(",", @g), "\n";
  }
}
{ package S07;
  my @g = </etc/hostname>;
  print scalar(@g), ":@g\n";
}
{ package S08;
  sub ok { print "ok:$_[1]\n" if $_[0] }
  ok <~>, '~ works';
}
{ package S09;
  use strict; use warnings;
  my $a = 5; my $b = 3;
  print "1:", ($a < ~$b ? "y" : "n"), "\n";
  print "2:", (~0 & 0xff), "\n";
  sub f { 7 }
  print "3:", (f() < 9 ? "lt" : "ge"), "\n";
  print "4:", ($a <=> $b), "\n";
}
{ package S10;
  for my $p ('/nope-xyz', '/home/', 'x~y', '~nosuchuser42', 'aa bb') {
      my @g = glob($p);
      print scalar(@g), ":", join("|", @g), "\n";
  }
}
{ package S11;
  my @c = glob("/etc/host*");
  print((grep { $_ eq "/etc/hostname" } @c) ? "found\n" : "missing\n");
  print scalar(glob("")), ":", scalar(my @e = glob("   ")), "\n";
}
# the __DATA__ snippet is LAST and UNWRAPPED: the marker ends the program, and
# the handle it opens belongs to the package in scope there (main::DATA).
use strict; use warnings;
my $tmp = "/tmp/pcl-punct-array-glob-01-$$.txt";
open(my $out, '>', $tmp) or die; print $out "l1\nl2\nl3\n"; close $out;
open(my $fh, '<', $tmp) or die;
my $first = <$fh>; chomp $first;
my @rest = <$fh>;
close $fh;
open(FH, '<', $tmp) or die; my $bare = <FH>; chomp $bare; close FH;
open(my $g, '<', $tmp) or die;
my $n = 0; while (my $line = <$g>) { $n++ } close $g;
my @d = <DATA>;
unlink $tmp;
print "$first/", scalar(@rest), "/$bare/$n/", scalar(@d), "\n";
__DATA__
d1
d2
