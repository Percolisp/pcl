# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 231-refs -- harvested from s469/bh-agent-ab83d94ab96620fab/s469bh/p1056/pr09.pl

# P9  a NUMERIC subscript through ->[] whose index slot holds a string "3abc"
#     (perl numifies it to 3; the freeze must numify the same way)
my $ar = [10, 20, 30, 40];
my @a  = (10, 20, 30, 40);
my $s = "";
for my $n (2..3) {
  my $i = $n . "abc";
  $s .= "[$ar->[$i]/$a[$i]]";
}
print "P9 $s\n";
