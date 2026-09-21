# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# origin: s492 battery2 / b2-30-heredoc-formats.pl
use strict; use warnings;
my %u = (name => "Ann", items => [qw(a b)], n => 3);
print <<"A", <<'B', <<~C;
Dear $u{name}, you have $u{n} items: @{$u{items}} (@{[ scalar @{$u{items}} ]})
  total: ${\ sprintf("%.2f", $u{n} * 1.5)}
A
raw $u{name} \n stays
B
    indented $u{name}
      keeps relative
    C
my $txt = lc(<<END) . "tail\n";
SHOUT $u{name}
END
print $txt; print "a\tb\\n|\x41\x{263a}|\101|\cA|\e[0m|\0|" =~ s/[^\x20-\x7e]/?/gr, "\n"; print 'single $u{name} \n', "\n", q{q-braces {nested}}, " ", qq{qq $u{n}}, " ", "email\@example.com", "\n";
