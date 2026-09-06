# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 291-sprintf -- harvested from s470/bo-agent-a4964c8ec026d7882/s470bo/d6.pl

sub have { 1 }
my @a = (1, 2);
my $ar = [1, 2];
printf "sub yes   => %s def=%d\n", (exists &have), (defined(exists &have) ? 1 : 0);
printf "sub no    => [%s] def=%d\n", (exists &nope), (defined(exists &nope) ? 1 : 0);
printf "ary in    => %s def=%d\n", (exists $a[0]), (defined(exists $a[0]) ? 1 : 0);
printf "ary out   => [%s] def=%d\n", (exists $a[9]), (defined(exists $a[9]) ? 1 : 0);
printf "aryref out=> [%s] def=%d\n", (exists $ar->[9]), (defined(exists $ar->[9]) ? 1 : 0);
my $cr = \&have;
printf "coderef   => %s def=%d\n", (exists &$cr), (defined(exists &$cr) ? 1 : 0);
printf "env no    => [%s] def=%d\n", (exists $ENV{NO_SUCH_VAR_XYZ}), (defined(exists $ENV{NO_SUCH_VAR_XYZ}) ? 1 : 0);
printf "env yes   => %s def=%d\n", (exists $ENV{PATH}), (defined(exists $ENV{PATH}) ? 1 : 0);
