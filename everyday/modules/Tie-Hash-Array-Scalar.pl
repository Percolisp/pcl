# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# origin: s492 modwork / mw-23-Tie-Hash-Array-Scalar.pl
use strict; use warnings;
package RevScalar; require Tie::Scalar; our @ISA = ("Tie::StdScalar"); sub FETCH { scalar reverse ${$_[0]} }
package LogArray; require Tie::Array; our @ISA = ("Tie::StdArray"); my @log; sub STORE { push @log, "S$_[1]"; $_[0]->SUPER::STORE($_[1], $_[2]) } sub log { @log }
package main; tie my $s, "RevScalar"; $s = "abc"; tie my @a, "LogArray"; @a = (1, 2); push @a, 3; $a[0] = 9; print "$s @a ", scalar(@a), " ", join(",", LogArray::log()), " ", (tied(@a) ? "tied" : "untied"), "\n";
