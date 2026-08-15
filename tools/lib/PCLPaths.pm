package PCLPaths;
# Paths PCL's tools need that live OUTSIDE the checkout — derived, never
# hard-coded (task #278: a grep for an absolute home-directory path over Pl/
# tools/ cl/ lib/ must come back empty, so the repo runs on a machine that is
# not the author's — the guard is Pl/t/no-hardcoded-paths-01.t).
#
# Today that is one path: perl's own t/ tree, the companion suite's corpus.
# It is not an installed thing — it exists only in a perl BUILD tree — so it
# cannot come from %Config alone.  Three sources, in order, and a die naming
# the override when none of them resolves; a WRONG guess must never look like
# an empty corpus.
use strict;
use warnings;
use Config;
use File::Basename qw(dirname);
use Exporter 'import';
our @EXPORT_OK = qw(perl_suite_t);

# The t/ directory of the perl BUILD tree matching the running perl.
#   1. $PCL_PERL_SUITE_T                      — explicit, always wins
#   2. $PERLBREW_ROOT/build/perl-V/perl-V/t   — perlbrew's layout
#   3. <prefix>/../../build/perl-V/perl-V/t   — the same, derived from
#      %Config{prefix} for a shell that never exported PERLBREW_ROOT (cron)
sub perl_suite_t {
    my $v = $Config{version};
    my @cand;
    push @cand, $ENV{PCL_PERL_SUITE_T} if defined $ENV{PCL_PERL_SUITE_T} && length $ENV{PCL_PERL_SUITE_T};
    push @cand, "$ENV{PERLBREW_ROOT}/build/perl-$v/perl-$v/t" if defined $ENV{PERLBREW_ROOT};
    push @cand, dirname(dirname($Config{prefix})) . "/build/perl-$v/perl-$v/t";
    for my $c (@cand) { return $c if -d $c }
    die "PCLPaths: cannot find the t/ tree of perl $v (tried: @cand).\n"
      . "Set PCL_PERL_SUITE_T to the t/ directory of a perl-$v build tree.\n";
}

1;
