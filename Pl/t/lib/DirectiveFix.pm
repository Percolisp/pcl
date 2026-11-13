# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# Fixture for Pl/t/facts-overlay-01.t's import-directive rows (task #4881): a
# sub whose CALL imports from the module its first argument names -- plain
# Perl, the shape of Test::More's use_ok.  Its overlay
# Pl/t/lib/PCL/Facts/DirectiveFix.pm declares the fact for PCL's parse.
package DirectiveFix;
use strict;
use warnings;
require Exporter;
our @ISA       = ('Exporter');
our @EXPORT_OK = qw(import_from);

# import_from(MODULE, LIST): load MODULE and install LIST (default: its
# @EXPORT) into the caller's package.
sub import_from {
  my ($module, @names) = @_;
  (my $file = "$module.pm") =~ s{::}{/}g;
  require $file;
  my $into = caller;
  no strict 'refs';
  @names = @{"${module}::EXPORT"} if !@names;
  *{"${into}::$_"} = \&{"${module}::$_"} for @names;
  return 1;
}

1;
