# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 343-string-eval -- harvested from s470/bs-agent-abb21d9ade6a1c1c3/s470bs/p20.pl

sub c { my $v = shift; return "EMPTY" if $v eq ''; return $v }
my $q = eval "1/3";
print "A\n";
my $r = c($q);
print "r=[$r]\n";
printf "no-width [%s]\n", c($q);
print "C\n";
printf "width [%-8s]\n", c($q);
print "B\n";
