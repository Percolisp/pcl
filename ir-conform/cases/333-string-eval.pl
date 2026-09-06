# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 333-string-eval -- harvested from s470/bn-agent-a28fa0a7a0844c434/s470bn/p6-run.pl

no strict 'refs';
package A; our $v = "A"; sub get { return ${"v"} }
package B; our $v = "B"; sub get { return ${"v"} }
package main;
our $v = "M";
our $g = 1; our @ga = (1,2,3); our %gh = (a => 1);
sub sg { return ${'main::g'} }
print sg();                                   # 1
{ local $main::g = 7; print sg() }            # 7   (a): a fresh box in the cell
print sg();                                   # 1
print A::get(), B::get();                     # AB  (b): two packages, two sites
sub e { my $p = shift; return eval "package $p; no strict 'refs'; \${'v'}" }
print e('A'), e('B'), e('main'), e('A');      # ABMA (b): ONE site, four packages
sub rd { my $x = ${'Nope::Later::x'}; return defined($x) ? $x : "u" }
print rd(), rd();                             # uu  (c): a miss stays a miss
${'Nope::Later::x'} = 5;
print rd(), rd();                             # 55
print scalar(@{'main::ga'}), scalar(keys %{'main::gh'});
{ local @main::ga = (4,5); print scalar(@{'main::ga'}) }
print scalar(@{'main::ga'}), "\n";
