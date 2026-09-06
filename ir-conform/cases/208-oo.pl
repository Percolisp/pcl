# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 208-oo -- harvested from s469/bh-agent-ab83d94ab96620fab/s469bh/p1046/methret.pl

my $n = $ENV{N}; package C; sub new { bless { v => $_[1] }, $_[0] } sub bump { my $self = shift; return $self->{v} + 1 } package main; my $o = C->new(1); my $s=0; for my $i (1..$n) { $s += $o->bump } print "$s\n";
