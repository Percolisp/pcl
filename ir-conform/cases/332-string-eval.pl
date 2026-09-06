# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 332-string-eval -- harvested from s470/bn-agent-a28fa0a7a0844c434/s470bn/p5-pkg.pl

no strict 'refs';
package A; our $v = "A"; our @l = ('a1'); sub get { return ${"v"} } sub geta { return "@{'l'}" }
package B; our $v = "B"; our @l = ('b1'); sub get { return ${"v"} } sub geta { return "@{'l'}" }
package main;
our $v = "M";
print A::get(), B::get(), A::geta(), B::geta(), " ";
# the SAME source text run in two packages through string eval (the one way
# *pcl-current-package* can differ at one site)
sub e { my $p = shift; return eval "package $p; no strict 'refs'; \${'v'}" }
print e('A'), e('B'), e('main'), e('A'), " ";
# leading-:: root spelling and an explicitly main-qualified one
print ${'::v'}, ${'main::v'}, " ";
# a NON-constant operand keeps the generic path
my $pk = 'A';
print ${"${pk}::v"}, "\n";
