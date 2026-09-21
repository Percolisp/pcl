# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# origin: s492 battery2 / b2-21-carp-attribution.pl
use strict; use warnings;
package Lib; use Carp qw(croak carp confess cluck); sub check { croak "bad value '$_[0]'" if $_[0] < 0; $_[0] } sub deep { Lib::check(@_) } sub w { carp "careful" }
package main;
$SIG{__WARN__} = sub { my $m = shift; $m =~ s/ at \S+ line (\d+)\.?\n//; print "WARN[$m] line-ok=", ($1 > 0 ? 1 : 0), "\n" };
eval { Lib::check(-1) }; my $e = $@; my ($line) = $e =~ / at \S+ line (\d+)/; print(($e =~ /^bad value '-1' at / ? "msg" : "nomsg:$e"), " ", ($line == __LINE__ ? "caller-line" : "other-line($line vs " . __LINE__ . ")"), "\n");
eval { Lib::deep(-2) }; print(($@ =~ /^bad value '-2' at \S+ line \d+\.$/ ? "one-line" : "multi:$@"), "\n"); Lib::w();
eval { Carp::confess("trace") }; my @tl = split /\n/, $@; print((@tl >= 2 ? "has-trace" : "no-trace"), " ", ($tl[1] =~ /eval \{\.\.\.\} called at/ ? "eval-frame" : "frame:$tl[1]"), "\n");
