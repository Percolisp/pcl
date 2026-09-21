# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# origin: s493 perlfunc index walk / pf-each-delete.pl
my %h = (a=>1,b=>2,c=>3); my $n = 0; while (my ($k,$v) = each %h) { $n += $v } print "$n\n"; my @a = (5,6,7); while (my ($i,$v) = each @a) { print "$i=$v;" } print "\n"; print join(",", map { "$_=$h{$_}" } sort keys %h), "\n"; my @d = delete @h{qw(a b)}; print "@d ", scalar(keys %h), "\n"; print exists $h{c} ? "e" : "ne", defined $h{zz} ? "d" : "nd", exists $h{zz} ? "viv" : "noviv", "\n"; my @arr = (1,2,3); delete $arr[1]; print defined $arr[1] ? "def" : "undef", " ", scalar(@arr), "\n"; print "wantarray-keys: ", scalar(%h) ? "true" : "false", "\n"; my ($first) = %h; print "$first\n";
