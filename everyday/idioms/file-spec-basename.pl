# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# origin: s491 idioms / idiom-31-file-spec-basename.pl
no warnings;
use File::Basename; use File::Spec; my ($n, $d, $s) = fileparse("/a/b/c.tar.gz", qr/\.[^.]*/); print basename("/x/y.pl"), " ", dirname("/x/y.pl"), " $n $s ", File::Spec->catfile("a", "b", "c.txt"), " ", File::Spec->rel2abs("x") =~ m{^/} ? "abs" : "rel", "\n";
