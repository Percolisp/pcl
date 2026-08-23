# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later
#
# SHAPES corpus (task #496) — bareword filehandles: package-QUALIFIED names in
# `<>`, `readline()`, the print `:fh` slot and `open()` (#452; the three
# spellings #452 left are #491 -- when they are fixed their rows come here
# too), a lexical handle (passed through), an unqualified bareword (quoted),
# and the punctuation-array INTERPOLATION family (#451) beside them.
# Lifted from Pl/t/punct-array-glob-01.t's emission rows.  Valid perl.
{ package S01;
  open(main::FH5, ">", "/dev/null") or die;
  open(main::FH2, "<", "/etc/hostname") or die;
  my $l = <main::FH2>; print main::FH5 "x";
  my $r = readline(main::FH2);
  close main::FH2; close main::FH5;
}
{ package S02;
  open(my $fh, "<", "/etc/hostname") or die; my $a = <$fh>; close $fh;
  open(FH6, "<", "/etc/hostname") or die; my $b = <FH6>; close FH6;
  print "ok\n";
}
{ package S03;
  no warnings; no strict;
  @? = (11,22,33); @. = (1,2,3); @/ = (4,5,6);
  print "A:$?[1] B:$.[2] C:$/[0]\n";
  "abc" =~ /b/; print "D:$-[0]:$+[0]\n";
  @x = (); print "E:[$?[9]]\n";
}
