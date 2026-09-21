# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# origin: s491 idioms / idiom-42-sprintf-pos-g.pl
no warnings;
my $s = "aXbXc"; my @p; while ($s =~ /X/g) { push @p, pos($s) } $s =~ /b/g; my $rest = substr($s, pos($s)); my $c = () = $s =~ /X/g; print "@p $rest $c\n";
