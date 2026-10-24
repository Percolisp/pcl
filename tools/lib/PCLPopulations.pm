# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

package PCLPopulations;

# PCLPopulations -- the four NAMED POPULATIONS the parser-structure checkers
# walk (s525q: extracted from tools/ppi-expect-check.pl so that it and
# tools/binder-check.pl read ONE list -- no third population walker).
#
#   files($name, $root)   the sorted file list of a population:
#     perl-tests  $root/perl-tests/*.t
#     perl-t      perl's own t/ (tools/run-perl-suite.pl's --all dirs)
#     everyday    $root/everyday/*/*.pl
#     cpan        $root/cpan-tests/modules/*/{lib,t}/**/*.{pm,t}
#     all         the four, in that order
#   A `.pm` is a MODULE (the caller transpiles it with --module).

use strict;
use warnings;
use File::Find;

our @NAMES = qw(perl-tests perl-t everyday cpan);

sub files {
  my ($name, $root) = @_;
  return map { files($_, $root) } @NAMES if $name eq 'all';
  if ($name eq 'perl-tests') { return sort glob "$root/perl-tests/*.t" }
  if ($name eq 'everyday')   { return sort glob "$root/everyday/*/*.pl" }
  if ($name eq 'perl-t') {
    require PCLPaths;
    my $t = PCLPaths::perl_suite_t();
    # tools/run-perl-suite.pl's @DEFAULT_DIRS, the --all scan
    return map { sort glob "$t/$_/*.t" } qw(base cmd comp opbasic op mro class run uni re io);
  }
  if ($name eq 'cpan') {
    my @f;
    for my $dist (sort grep { -d } glob "$root/cpan-tests/modules/*") {
      for my $sub (qw(lib t)) {
        next unless -d "$dist/$sub";
        find({ no_chdir => 1, wanted => sub { push @f, $_ if -f && /\.(?:pm|t)$/ } }, "$dist/$sub");
      }
    }
    return sort @f;
  }
  die "unknown population '$name' (perl-tests perl-t everyday cpan all)\n";
}

1;
