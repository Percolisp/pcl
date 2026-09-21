# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# origin: s492 battery2 / b2-33-local-in-recursion.pl
use strict; use warnings;
our $depth = 0; my @trace;
sub walk { my ($t, $cb) = @_; local $depth = $depth + 1; $cb->($t->{v}, $depth); walk($_, $cb) for @{ $t->{kids} || [] } }
my $tree = { v => "root", kids => [ { v => "a", kids => [ { v => "a1" } ] }, { v => "b" } ] };
walk($tree, sub { push @trace, ("  " x ($_[1] - 1)) . $_[0] }); print join("\n", @trace), "\ndepth after: $depth\n";
sub fact { my $n = shift; $n <= 1 ? 1 : $n * fact($n - 1) } sub fib { my $n = shift; $n < 2 ? $n : fib($n - 1) + fib($n - 2) } my %memo; sub mfib { my $n = shift; $memo{$n} //= $n < 2 ? $n : mfib($n - 1) + mfib($n - 2) }
my $count; $count = sub { my $n = shift; $n ? 1 + $count->($n - 1) : 0 }; print fact(10), " ", fib(15), " ", mfib(50), " ", $count->(100), " ", fact(20), "\n";
