# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 143-local -- harvested from s470/fable/bj/facts.pl

my @p; push @p, $_*2 for 1..5; print "p: @p ", scalar(@p), "\n";
my @r; push @r, 1; my $rr = \@r; push @$rr, 2; push @r, 3; print "r: @r\n";
sub w { $_[0] = "W" } my @s = (1,2); w(@s); push @s, 3; print "s: @s\n";
my @c; my $cl = sub { push @c, "c" }; $cl->(); push @c, "d"; print "c: @c\n";
my @f = (1,2); for (@f) { $_ *= 10 } push @f, 3; print "f: @f\n";
my @fa = (1,2,3); for my $x (@fa) { $fa[0] = 99; print "fa: $x\n"; last }
my @fs = (2,1); for my $x (sort { $a <=> $b } @fs) { $fs[1] = 99; print "fs: $x\n"; last }
my @l; my @m = (1,2); push @l, @m; push @l, (3,4); push @l, %{{k=>1}}; print "l: ", scalar(@l), "\n";
my @n; my $len = push @n, "x"; my $h = {a=>1}; push @n, $h; $h->{a} = 2; print "n: $len $n[1]{a}\n";
my $v = 5; push @n, $v; $v = 6; print "n2: $n[2]\n";
our @g = (1); sub lg { local @g = (9); push @g, 2; "@g" } print "g: ", lg(), " @g\n";
my @t = (2,1); for my $x (reverse @t) { $x .= "!" } print "t: @t\n";
my %hv = (k=>1); for my $x (values %hv) { $x++ } print "hv: $hv{k}\n";
my @mp = (1,2); my @out = map { $_++ } @mp; push @mp, 9; print "mp: @mp\n";
my @wi = (1,2); for my $x (@wi) { push @wi, 3 if $x == 1; print "wi: $x\n" }
my @sh = (1,2,3); for my $x (@sh) { shift @sh; print "sh: $x\n" }
