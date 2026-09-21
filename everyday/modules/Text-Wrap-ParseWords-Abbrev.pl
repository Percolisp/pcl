# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# origin: s492 modwork / mw-12-Text-Wrap-ParseWords-Abbrev.pl
use strict; use warnings; use Text::Wrap qw(wrap fill); use Text::ParseWords qw(shellwords quotewords parse_line); use Text::Abbrev; use Text::Tabs;
$Text::Wrap::columns = 30; print wrap("> ", "  ", "The quick brown fox jumps over the lazy dog and keeps running far away"), "\n";
print join("|", shellwords(q{one "two three" four\ five 'six seven'})), " ", join("|", quotewords(",", 0, q{a,"b,c",d})), " ", scalar(keys %{{ abbrev(qw(list load quit)) }}), " ", join("|", expand("a\tb")), "\n";
