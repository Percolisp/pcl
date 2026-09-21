# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# origin: s492 modwork / mw-06-File-Basename.pl
use strict; use warnings; use File::Basename qw(basename dirname fileparse);
my ($n, $d, $s) = fileparse("/a/b/file.tar.gz", qr/\.[^.]*/); print join("|", basename("/x/y/z.txt"), basename("/x/y/z.txt", ".txt"), dirname("/x/y/z.txt"), dirname("file"), dirname("/"), $n, $d, $s, basename("/trailing/"), (fileparse("noext"))[0]), "\n";
