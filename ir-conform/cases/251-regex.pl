# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 251-regex -- harvested from s470/bo-agent-a4964c8ec026d7882/s470bo/d1.pl

use strict; use warnings;
no warnings 'numeric';
my $inf = 9**9**9;
my $nan = $inf - $inf;
my @cases = (
  ['5 % 0',        sub { 5 % 0 }],
  ['5 % 0.0',      sub { 5 % 0.0 }],
  ['0 % 0',        sub { 0 % 0 }],
  ['%= 0',         sub { my $x = 7; $x %= 0; $x }],
  ['1 / 0',        sub { 1 / 0 }],
  ['1 / 0.0',      sub { 1 / 0.0 }],
  ['0 / 0',        sub { 0 / 0 }],
  ['/= 0',         sub { my $x = 7; $x /= 0; $x }],
  ['5 % inf',      sub { 5 % $inf }],
  ['inf % 5',      sub { $inf % 5 }],
  ['5 % nan',      sub { 5 % $nan }],
  ['nan % 5',      sub { $nan % 5 }],
  ['5 % -3',       sub { 5 % -3 }],
  ['-5 % 3',       sub { -5 % 3 }],
  ['5 % "abc"',    sub { my $s = "abc"; 5 % $s }],
);
for my $c (@cases) {
  my ($name, $code) = @$c;
  my $v = eval { $code->() };
  my $e = $@ || '';
  $e =~ s/ at .*//s;
  printf "%-12s => %s err=[%s]\n", $name,
         (defined $v ? "[$v]" : 'undef'), $e;
}
