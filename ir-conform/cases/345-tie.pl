# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 345-tie -- harvested from s469/bh-agent-ab83d94ab96620fab/s469bh/p1056/pr08.pl

# P8  $r->{$k} where $r is a TIED hash: FETCH/STORE must still see the key
package TH;
sub TIEHASH { bless { d => {}, log => [] }, shift }
sub STORE   { my ($s,$k,$v) = @_; push @{$s->{log}}, "S:$k"; $s->{d}{$k} = $v }
sub FETCH   { my ($s,$k) = @_; push @{$s->{log}}, "F:$k"; $s->{d}{$k} }
sub FIRSTKEY { my $s = shift; my @k = sort keys %{$s->{d}}; $s->{i}=1; $k[0] }
sub NEXTKEY  { my $s = shift; my @k = sort keys %{$s->{d}}; $k[$s->{i}++] }
sub EXISTS  { exists $_[0]{d}{$_[1]} }
package main;
my %t;
my $obj = tie %t, 'TH';
my $r = \%t;
my $s = 0;
for my $n (1..3) { my $k = "k" . $n; $r->{$k} = $n; }
for my $n (1..3) { my $k = "k" . $n; $s += $r->{$k}; }
print "P8 $s ", join(",", @{$obj->{log}}), "\n";
