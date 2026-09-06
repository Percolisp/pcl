# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 288-sort -- harvested from s470/fable/bi/p1022b.pl

sub f { last }  sub g { next }  sub h { redo }
my @seen; for my $i (1..3) { push @seen, $i; f() } print "last-for: @seen\n";
my @n; for my $i (1..3) { g(); push @n, $i } print "next-for: @n\n";
my $r = 0; my @rd; for my $i (1..2) { push @rd, $i; if ($r++ < 1) { h() } } print "redo-for: @rd\n";
my $w = 0; while ($w < 5) { $w++; f() } print "last-while: $w\n";
my $c = 0; for (my $i = 0; $i < 5; $i++) { $c++; g(); $c += 100 } print "next-cfor: $c\n";
sub inner { f() } sub outer { inner() } my $k = 0; for (1..3) { $k++; outer() } print "chain: $k\n";
my $code = \&f; my $cr = 0; eval { for (1..3) { $cr++; $code->() } }; print "coderef: $cr ", ($@ ? "died" : "ok"), "\n";
{ package O; sub m { last } } my $mc = 0; eval { for (1..3) { $mc++; O->m() } }; print "method: $mc ", ($@ ? "died" : "ok"), "\n";
eval { f() }; print "noloop: ", ($@ =~ /Can't "last" outside a loop block/ ? "perl-text" : "other:$@"), "\n";
my $d = 0; eval { do { $d++; f() } while ($d < 3) }; print "dowhile: $d ", ($@ ? "died" : "ok"), "\n";
my $b = 0; { $b++; f(); $b = 99 } print "bare: $b\n";
my @s = sort { f(); 0 } (2,1); print "sortblk: ", scalar(@s), "\n";
