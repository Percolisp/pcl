# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# origin: s492 battery2 / b2-15-ref-types.pl
use strict; use warnings; use Scalar::Util qw(blessed reftype looks_like_number weaken refaddr dualvar);
my @r = (\1, [1], {a=>1}, sub {1}, \\1, qr/x/, \*STDOUT, bless([], "Foo"), bless({}, "Bar"));
print join(" ", map { ref } @r), "\n", join(" ", map { reftype($_) } @r), "\n", join(" ", map { blessed($_) // "-" } @r), "\n";
print join(" ", map { looks_like_number($_) ? 1 : 0 } (1, "1.5", "1e5", "abc", "", " 1", "0x10", "Inf", undef // "u")), "\n";
my $cr = sub { "called:@_" }; print "" . ($cr =~ /^CODE\(0x[0-9a-f]+\)$/ ? "codestr" : "bad:$cr"), " ", $cr->(1), " ", &$cr(2), " ", &{$cr}(3), "\n";
my $aref = [1,2]; my $copy = $aref; print((refaddr($aref) == refaddr($copy) ? "same" : "diff"), " ", ($aref == $copy ? "numeq" : "numne"), " ", ("$aref" eq "$copy" ? "streq" : "strne"), "\n");
