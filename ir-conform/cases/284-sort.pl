# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 284-sort -- harvested from s470/bn-agent-a28fa0a7a0844c434/s470bn/p4-viv.pl

no strict 'refs';
# A name whose PACKAGE does not exist at the first access: the read must NOT
# cache a miss, and the later write must vivify.
sub r { my $v = ${'Nope::Later::x'}; print defined($v) ? "[$v]" : "[u]"; }
r(); r();
${'Nope::Later::x'} = 5;
r(); r();
# same for an array and a hash
sub ra { print "(", join(',', @{'Nope2::y'}), ")"; }
ra();
push @{'Nope2::y'}, 3, 4;
ra(); ra();
sub rh { print "{", join(',', map { "$_" } sort keys %{'Nope3::z'}), "}"; }
rh();
${'Nope3::z'}{'k'} = 1;
$Nope3::z{'j'} = 2;
rh(); rh();
print "\n";
