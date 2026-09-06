# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 254-regex -- harvested from s470/bo-agent-a4964c8ec026d7882/s470bo/d5.pl

no warnings;
my @c = (
  ['5 % 0.5',   sub { 5 % 0.5 }],
  ['5 % 0.9',   sub { 5 % 0.9 }],
  ['5 / 0.5',   sub { 5 / 0.5 }],
  ['1 / -0.0',  sub { my $z = -0.0; 1 / $z }],
  ['1 % -0.0',  sub { my $z = -0.0; 1 % $z }],
  ['1 / "abc"', sub { my $s = "abc"; 1 / $s }],
  ['1 / undef', sub { my $u; 1 / $u }],
  ['1 % undef', sub { my $u; 1 % $u }],
  ['1 / ""',    sub { my $s = ""; 1 / $s }],
);
for my $c (@c) {
  my ($n, $f) = @$c;
  my $v = eval { $f->() };
  my $e = $@ || ''; $e =~ s/ at .*//s;
  printf "%-12s => %s err=[%s]\n", $n, (defined $v ? "[$v]" : 'undef'), $e;
}
