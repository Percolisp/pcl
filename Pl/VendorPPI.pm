# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

package Pl::VendorPPI;

# Pl::VendorPPI -- THE ONE PLACE PCL loads PPI from (s527a,
# docs/ppi-fix-at-source-s527.md §5).
#
# PCL parses with its OWN PPI: vendor/PPI/lib, the lib/ of the branch `pcl`
# of github.com/Percolisp/PPI = upstream PPI master + PCL's fix commits
# (vendor/PPI/UPSTREAM names them).  Every module and test that needs PPI
# says `use Pl::VendorPPI;` instead of `use PPI;`: this puts the vendored lib
# at the FRONT of @INC and loads PPI from it.  The root is found from this
# file's own location, so it holds for pl2cl, the tools, the Pl/t files and
# an installed tree alike.
#
#   PCL_SYSTEM_PPI=1   do not put the vendored lib on @INC: load whatever PPI
#                      perl finds (the canary's "did upstream catch up" run,
#                      and A/Bs against the installed PPI).
#
# A PPI already loaded from elsewhere is a DIE, not a silent mix: some code
# said `use PPI` before this module ran, and would have parsed with the
# wrong tokenizer.

use strict;
use warnings;
use File::Basename ();
use Cwd ();

our $LIB;
BEGIN {
  my $root = File::Basename::dirname(File::Basename::dirname(Cwd::abs_path(__FILE__)));
  $LIB = "$root/vendor/PPI/lib";
  if (!$ENV{PCL_SYSTEM_PPI}) {
    die "PCL: the vendored PPI is missing: no $LIB/PPI.pm\n" if !-f "$LIB/PPI.pm";
    if ($INC{'PPI.pm'} && index($INC{'PPI.pm'}, "$LIB/") != 0) {
      die "PCL: PPI was loaded from $INC{'PPI.pm'} before Pl::VendorPPI could put "
        . "$LIB first (say `use Pl::VendorPPI;`, not `use PPI;`; PCL_SYSTEM_PPI=1 "
        . "means the system PPI on purpose)\n";
    }
    unshift @INC, $LIB if !grep { $_ eq $LIB } @INC;
  }
}
use PPI ();

# The vendored lib directory, and whether the PPI in use came from it.
sub lib { $LIB }
sub is_vendored { return index($INC{'PPI.pm'} // '', "$LIB/") == 0 ? 1 : 0 }

1;
