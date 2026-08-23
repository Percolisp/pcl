# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later
#
# SHAPES corpus (task #496) — operand grammar: a USER sub whose prototype makes
# it a named unary operator (#453).  Lifted from Pl/t/user-unary-01.t; the
# rows there assert perl's answer, this file only has to EXIST so an A/B sees
# the shape.  One block-scoped package per snippet.  Valid perl: run it.
{ package S01; sub f ($) { "f($_[0])" } print f "a" . "b", "\n"; }
{ package S02; sub o (;$) { "o($_[0])" } print o "a" . "b", "\n"; }
{ package S03; sub s1 (*) { "s1($_[0])" } print s1 "a" . "b", "\n"; }
{ package S04; sub u (_) { "u($_[0])" } print u "a" . "b", "\n"; }
{ package S05; sub f ($) { "f($_[0])" } my $x = 5; print f $x + 1, "\n"; }
{ package S06; sub f ($) { "f($_[0])" } my $x = 5; print f $x * 2, "\n"; }
{ package S07; sub g (*) { "g($_[0])" } print "R=", g + 1, "\n"; }
{ package S08; sub f ($) { "f($_[0])" } print f -1, "\n"; }
{ package S09; sub opt ($;$) { "opt(" . join(",", @_) . ")" } my @r = (3, 4); my $s = opt @r[0,1]; print "$s\n"; }
{ package S10; sub lst (@) { "lst(" . join(",", @_) . ")" } my $s = lst 3, 4; print "$s\n"; }
{ package S11; sub trail ($;) { "trail($_[0])" } my $s = trail "a" . "b"; print "$s\n"; }
{ package S12; open(F, "<", "/etc/hostname") or die; my $ok = close F; print "ok=$ok\n"; print length "ab" . "cd", "\n"; }
