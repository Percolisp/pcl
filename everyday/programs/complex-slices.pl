# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# origin: s492 battery2 / b2-31-complex-slices.pl
use strict; use warnings;
my %h = (a => 1, b => 2, c => 3, d => 4); my @a = (10..20); my $hr = \%h; my $ar = \@a;
my %sub; @sub{qw(a c)} = @h{qw(a c)}; my %kv = %h{qw(b d)}; my @ix = %a[0, 1]; my ($first, @others) = @$ar[0, 2, 4]; my @hs = @{$hr}{qw(a b)}; my @hs2 = @$hr{qw(c d)}; my @pf = $hr->@{qw(a d)}; my @af = $ar->@[1, 2]; my $last = $ar->$#*;
print join(",", map { "$_=$sub{$_}" } sort keys %sub), " ", join(",", map { "$_=$kv{$_}" } sort keys %kv), " @ix $first @others @hs @hs2 @pf @af $last\n";
my @aoh = ({ n => 1, t => [qw(x y)] }, { n => 2, t => [qw(z)] }); my @tags = map { @{ $_->{t} } } @aoh; my %idx = map { $_->{n} => $_ } @aoh; my @ns = map { $_->{n} } grep { @{ $_->{t} } > 1 } @aoh;
my ($x, $y) = @{ $aoh[0] }{qw(n t)}; delete @h{qw(a b)}; my @e = (exists $h{a} ? 1 : 0, exists $h{c} ? 1 : 0); my @nested = ([1, 2], [3, 4]); my @col = map { $_->[1] } @nested; my @flat = map { @$_ } @nested; $nested[2][1] = "v";
print "@tags $idx{2}{t}[0] @ns $x @$y @e @col @flat ", scalar(@nested), " ", (defined $nested[2][0] ? "def" : "undef"), " ", join(",", map { scalar @$_ } @nested), " ", wantarray() // "undef-ctx", "\n";
