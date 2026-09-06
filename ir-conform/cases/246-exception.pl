# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 246-exception -- harvested from s470/bm-agent-ac55b94993add67d0/s470bm/p-contract.pl

my @t = (
  ['5 % 0',            sub { 5 % 0 }],
  ['substr oob',       sub { substr("abc", 10, 2) }],
  ['sprintf bad conv', sub { sprintf("%q", 1) }],
  ['aref on string',   sub { my $s = "x"; $s->[0] }],
  ['gethash on str',   sub { my $s = "x"; $s->{k} }],
  ['delete on str',    sub { my $s = "x"; delete $s->{k} }],
  ['exists on str',    sub { my $s = "x"; exists $s->{k} }],
  ['oct bad',          sub { oct("zz") }],
  ['chr -1',           sub { chr(-1) }],
  ['1/0',              sub { 1 / 0 }],
  ['2**0.5',           sub { 2 ** 0.5 }],
  ['-"foo"',           sub { -"foo" }],
);
for my $r (@t) {
  my $v = eval { $r->[1]->() };
  my $e = $@;
  $e = '' if !defined $e;
  $e =~ s/\n.*//s;
  printf "%-18s => %s | err=%s\n", $r->[0], (defined $v ? "[$v]" : "undef"),
         (length($e) ? $e : "-");
}
