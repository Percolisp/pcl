# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 213-oo -- harvested from s470/bn-agent-a28fa0a7a0844c434/s470bn/p7-mix.pl

no strict 'refs';
package P; our $n = "Ps"; our @n = ("Pa"); our %n = (k => "Ph");
package main; our $n = "Ms"; our @n = ("Ma"); our %n = (k => "Mh");
print ${'P::n'}, ${'main::n'}, ${'n'};
print $n[0] eq "Ma" ? "" : "X", "@{'P::n'}", "@{'main::n'}";
print ${'P::n'} eq 'Ps' ? "" : "X";
print $n{k} eq 'Mh' ? "" : "X", ${'main::n'}, "\n";
PERL
