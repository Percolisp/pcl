# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later
#
# SHAPES corpus (task #496) — bareword filehandles: package-QUALIFIED names in
# `<>`, `readline()`, the print `:fh` slot and `open()` (#452), the three
# spellings #452 left (#491: `print main::STDOUT`, a handle in a package that
# is not declared, and the registry keyed on the SPELLING), a lexical handle
# (passed through), an unqualified bareword (quoted), the `(*)` prototype slot
# of a USER sub and the builtin one beside it (#495), and the punctuation-array
# INTERPOLATION family (#451).
# Lifted from Pl/t/punct-array-glob-01.t's and Pl/t/user-unary-01.t's emission
# rows.  Valid perl.
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
{ package S04;                           # #491: the three spellings #452 left
  print main::STDOUT "";
  printf main::STDOUT "%s", "";
  my $f = "/dev/null";
  open(Zz9::H1, ">", $f) or die;         # no `package Zz9` anywhere
  print Zz9::H1 "x"; close(Zz9::H1);
  open(main::FH7, "<", "/etc/hostname") or die;
  my $l = <FH7>; my $m = readline(main::FH7); close FH7;
  opendir(main::D1, "/etc") or die; my @e = readdir(main::D1); closedir(D1);
  my $r = print Foo9::STDOUT "";         # NOT STDOUT: a different glob
  print STDOUT "";
}
{ package S05;                           # #495: the `*` and strictly-single slots
  sub fh (*) { return "fh($_[0])" }
  sub star (*&) { return &{$_[1]} }
  sub NAMED () { 42 }
  my $a = fh FOO;  my $b = fh(FOO);      # a bareword `*` slot is its NAME
  my $c = star BAR, sub { 1 };
  my $d = star(BAR, sub { 1 });
  my $e = tell NAMED;                    # a BUILTIN `*` slot: the handle, not 42
  open(G9, "<", "/etc/hostname") or die;
  my $g = close G9 ? "a" : "b";          # the operand ENDS at `?`
  open(H9, "<", "/etc/hostname") or die;
  my $h = eof H9 ? "y" : "n";
  print "";
}
{ package S03;
  no warnings; no strict;
  @? = (11,22,33); @. = (1,2,3); @/ = (4,5,6);
  print "A:$?[1] B:$.[2] C:$/[0]\n";
  "abc" =~ /b/; print "D:$-[0]:$+[0]\n";
  @x = (); print "E:[$?[9]]\n";
}
