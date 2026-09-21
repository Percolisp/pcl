# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# origin: s491 idioms / idiom-20-local-special.pl
no warnings;
my @r; { local $, = "-"; local $\ = "!\n"; print "a", "b"; } { local $/ = undef; } { local $_ = "topic"; push @r, $_ } my @w = do { local @ARGV = ("x"); @ARGV }; print "@r @w\n";
