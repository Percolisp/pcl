# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# origin: s491 idioms / idiom-28-data-dumper.pl
no warnings;
use Data::Dumper; $Data::Dumper::Sortkeys = 1; $Data::Dumper::Indent = 0; print Dumper({ a => [1, "two"], b => { c => undef } }), "\n";
