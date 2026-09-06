# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 077-coercion -- harvested from s470/bi-agent-a4b9fedc2175a6513/s470bi/p1020/subrow.pl

sub xsub { print "@_\n" }
undef *_;
eval { &utf8::encode };
print "val=", (defined *_{ARRAY} ? *_{ARRAY} : "undef"), "\n";
