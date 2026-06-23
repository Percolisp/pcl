#!/usr/bin/env perl
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

# The original report: Data::Dump=dump.
{
    my $out = run_pcl(q{-MData::Dump=dump -E 'dump({a=>1,b=>[2,3]})'});
    like($out, qr/\{ a => 1, b => \[2, 3\] \}/,
         '-MData::Dump=dump imports dump()');
}

done_testing();
