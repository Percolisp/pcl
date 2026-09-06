# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 262-regex -- harvested from s470/fable/bi/p1022.pl

sub f { last }
my @seen;
for my $i (1..3) { push @seen, $i; eval { f() }; print "err: ", ($@ ? "die" : "none"), "\n"; last if $@ }
print "seen: @seen\n";
sub g { for my $x (1..3) { last if $x == 2; print "g$x\n" } print "g-done\n" }  g();
sub h { print "h$_\n" for 1..2; last if 0; }  # statement-modifier for = a loop
sub k { my $n = 0; while ($n < 2) { $n++; next if $n == 1; print "k$n\n" } }  k();
sub m { for (my $i = 0; $i < 2; $i++) { print "m$i\n"; next } }  m();
sub w { my $i = 0; $i++ while $i < 3; print "w$i\n" }  w();
eval { sub z { redo } z() }; print "z: ", ($@ =~ /redo/ ? "die" : "none:$@"), "\n";
