#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# vendor-ppi-01.t -- PCL's VENDORED PPI is what its stamp says, and it is
# the PPI PCL loads (s527a; docs/ppi-fix-at-source-s527.md §5).
#
# vendor/PPI/lib is the lib/ of the branch `pcl` of github.com/Percolisp/PPI
# (upstream master + PCL's fix commits), written by tools/vendor-ppi together
# with the stamp vendor/PPI/UPSTREAM.  This file checks:
#   * the lib's git TREE id is the stamp's `lib-tree` -- an edit made in
#     vendor/PPI/lib instead of on the branch fails here;
#   * Pl::VendorPPI loads PPI from vendor/ (and PCL_SYSTEM_PPI=1 does not);
#   * a `use PPI` that runs BEFORE Pl::VendorPPI is a die, not a silent mix;
#   * every fix the stamp names has its registry row marked fixed_in 'pcl',
#     and the canary passes on the vendored lib.
# Light: no transpile, no SBCL.
use strict;
use warnings;
use Test::More;
use FindBin qw($RealBin);
use Cwd qw(abs_path);
use lib "$RealBin/../..";
use lib "$RealBin/../../tools/lib";
use PCLVendor;

my $root = abs_path("$RealBin/../..");
my $vend = "$root/vendor/PPI";
my $perl = $^X;

# ── the copy is the branch's lib/ ───────────────────────────────────────────
my $s = PCLVendor::read_stamp("$vend/UPSTREAM");
for my $k (qw(upstream-sha branch branch-sha lib-tree)) {
  like($s->{$k} // '', qr/\S/, "the stamp names $k");
}
is(PCLVendor::tree_sha("$vend/lib"), $s->{'lib-tree'},
   'vendor/PPI/lib is the tree the stamp names (refresh it with tools/vendor-ppi, never by hand)');
ok(-f "$vend/LICENSE", 'PPI\'s LICENSE travels with it');

# ── Pl::VendorPPI is the loader ─────────────────────────────────────────────
sub child {
  my ($code, %env) = @_;
  local @ENV{ keys %env } = values %env;
  my $out = qx{"$perl" -I"$root" -e '$code' 2>&1};
  return ($? >> 8, $out);
}
{
  my ($rc, $out) = child('use Pl::VendorPPI; print $INC{"PPI.pm"}, " ", Pl::VendorPPI::is_vendored()');
  is($rc, 0, 'Pl::VendorPPI loads') or diag $out;
  like($out, qr{^\Q$vend\E/lib/PPI\.pm 1$}, 'PPI comes from vendor/PPI/lib');
}
{
  my ($rc, $out) = child('use Pl::Parser; print $INC{"PPI.pm"}');
  like($out, qr{^\Q$vend\E/lib/PPI\.pm$}, 'the compiler (Pl::Parser) parses with the vendored PPI')
    or diag $out;
}
SKIP: {
  my ($has) = child('require PPI; print 1', PCL_SYSTEM_PPI => 1);
  skip 'no system PPI installed', 1 if $has != 0;
  my ($rc, $out) = child('use Pl::VendorPPI; print Pl::VendorPPI::is_vendored()', PCL_SYSTEM_PPI => 1);
  is($out, '0', 'PCL_SYSTEM_PPI=1 loads the installed PPI instead');
}
{
  my ($rc, $out) = child('BEGIN { unshift @INC, sub { return if $_[1] ne "PPI.pm"; my $s = "package PPI; our \$VERSION = 0; 1;"; open my $fh, "<", \$s; return $fh } } use PPI (); use Pl::VendorPPI;');
  isnt($rc, 0, 'a PPI loaded before Pl::VendorPPI is a die');
  like($out, qr/PPI was loaded from .* before Pl::VendorPPI/, '... that names what happened');
}

# ── the fixes the stamp carries are the registry's fixed_in 'pcl' rows ───────
{
  require Pl::PPIBugs;
  my %pcl = map { $_->{id} => 1 } grep { ($_->{ppi}{fixed_in} // '') eq 'pcl' } Pl::PPIBugs::bugs();
  my %stamp = map { $_ => 1 } map { /§(\d+)/g } @{ $s->{fix} || [] };
  is_deeply([ sort { $a <=> $b } keys %pcl ], [ sort { $a <=> $b } keys %stamp ],
            'the registry rows marked fixed_in pcl are exactly the sections the stamp\'s fixes name');
  my $out = qx{"$perl" "$root/Pl/t/ppi-bugs-registry-01.t" 2>&1};
  is($? >> 8, 0, 'the registry canary passes on the vendored PPI')
    or diag join "\n", grep { /^not ok/ } split /\n/, $out;
}

done_testing();
