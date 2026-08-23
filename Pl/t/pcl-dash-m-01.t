#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# Test the `pcl` runner's -M handling, matching perl(1) semantics:
#   -MModule            → use Module;
#   -MModule=a,b        → use Module qw(a b);   (import list)
#   -M-Module           → no Module;
# Regression: `pcl -MData::Dump=dump -E '...'` used to ignore the `=dump`
# import list, so `dump` was never imported and the run died with
# "function pl-dump is undefined".

use v5.30;
use strict;
use warnings;
use Test::More;
use FindBin qw($RealBin);

my $root = "$RealBin/../..";
my $pcl  = "$root/pcl";

plan skip_all => "pcl not found"  unless -x $pcl;
plan skip_all => "sbcl not found" unless `which sbcl 2>/dev/null`;

# Strip the runtime banner line that pcl prints on stderr/stdout.
sub run_pcl {
    my (@args) = @_;
    my $cmd = join(' ', $pcl, @args) . ' 2>&1';
    my $out = `$cmd`;
    $out =~ s/^PCL Runtime loaded\n//m;
    return $out;
}

# -MModule=imports → the named imports are available unqualified.
{
    my $out = run_pcl(q{-MList::Util=sum,max -E 'say sum(1,2,3); say max(4,5,6)'});
    is($out, "6\n6\n", '-MList::Util=sum,max imports sum and max');
}

# -MModule (no imports) → module loaded, fully-qualified call works.
{
    my $out = run_pcl(q{-MPOSIX -E 'say POSIX::floor(3.9)'});
    is($out, "3\n", '-MPOSIX with no import list still loads the module');
}

# The original report: Data::Dump=dump.  The SUBJECT is the real CPAN module
# (transpiled from @INC) -- a fixture, not a PCL dependency; the row SKIPS
# where it is not installed (a stock perl has only PPI and Moo).
SKIP: {
    skip "Data::Dump not installed (the CPAN module is this row's fixture)", 1
        if !eval { require Data::Dump; 1 };
    my $out = run_pcl(q{-MData::Dump=dump -E 'dump({a=>1,b=>[2,3]})'});
    like($out, qr/\{ a => 1, b => \[2, 3\] \}/,
         '-MData::Dump=dump imports dump()');
}

done_testing();
