# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 235-refs -- harvested from s470/bj-agent-a7cf766b7d18923d1/s470bj/probe/pos.pl

# POSITIVE cases: each must take %p-push1.
my @p1; push @p1, 1;
my @p2; my $i = 5; push @p2, $i * 2;
my @p3; push @p3, "a" . "b";
my @p4; push @p4, [1,2];
my @p5; my %h; push @p5, $h{k};
my @p6; my $s; push @p6, \$s;
my @p7; my @src=(1); push @p7, $src[0];
my @p8; print scalar(@p8), "@p8\n"; push @p8, 8;
my @p9; push @p9, "x$i";
my @pa; push @pa, -$i;
my @pb; my @pbx=(1); push @pb, $pbx[0] + 1;
# LIST arguments must keep p-push even on a clean array.
my @q1; my @o=(1,2); push @q1, @o;
my @q2; my %g=(a=>1); push @q2, %g;
my @q3; push @q3, keys %g;
my @q4; push @q4, (1,2);
my @q5; push @q5, 1, 2;
my @q6; push @q6, qq1();
sub qq1 { return (1,2) }
print "@p1 @p2 @p3 @p5 @p7 @p8 @p9 @pa @pb @q1 @q3 @q4 @q5\n";
