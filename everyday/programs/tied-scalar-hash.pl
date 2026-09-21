# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# origin: s492 battery2 / b2-20-tied-scalar-hash.pl
use strict; use warnings;
package UpperScalar; sub TIESCALAR { my ($c, $v) = @_; bless { v => $v, n => 0 }, $c } sub FETCH { $_[0]{n}++; uc $_[0]{v} } sub STORE { $_[0]{v} = $_[1] }
package CountHash; require Tie::Hash; our @ISA = ("Tie::StdHash"); my $stores = 0; sub STORE { $stores++; $_[0]->SUPER::STORE($_[1], $_[2]) } sub stores { $stores }
package main;
tie my $s, "UpperScalar", "abc"; my $v1 = $s; $s = "xyz"; my $v2 = "$s!"; my $obj = tied $s; print "$v1 $v2 $obj->{n}\n"; undef $obj; untie $s;
tie my %h, "CountHash"; $h{a} = 1; $h{b} = 2; $h{a}++; print join(",", map { "$_=$h{$_}" } sort keys %h), " ", CountHash::stores(), " ", (exists $h{a} ? "ex" : "nex"), " ", scalar(keys %h), "\n";
