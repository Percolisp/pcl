#!/usr/bin/env perl
# Transpile tests part 8.  NEW TESTS GO HERE — -06 is at the ~50-test cap and
# -07 is past it, and the BIGGEST test file bounds the parallel suite's wall
# time (one SBCL spawn per test), so start a new file rather than grow one.

use v5.30;
use strict;
use warnings;
use Test::More;
use File::Temp qw(tempfile);
use FindBin qw($RealBin);
use lib $RealBin;
use PCLCore;

# Path to pl2cl and runtime
my $project_root = "$RealBin/../..";
my $pl2cl = "$project_root/pl2cl";
my $runtime = "$project_root/cl/pcl-runtime.lisp";
# Optional saved-core fast path (PCL_TEST_CORE=1); source-load otherwise.
my @sbcl_rt = PCLCore::sbcl_prefix($runtime);

# Check dependencies
plan skip_all => "pl2cl not found" unless -x $pl2cl;
plan skip_all => "sbcl not found" unless `which sbcl 2>/dev/null`;

# Run a Perl snippet and return output
sub run_perl {
    my ($code) = @_;
    my $full_code = 'use feature "state"; use Cwd; ' . $code;
    # Shell-escape embedded single quotes ('…' -> '\''), or any tick in the
    # snippet truncates the -e arg.
    (my $sh_code = $full_code) =~ s/'/'\\''/g;
    my $output = `perl -e '$sh_code' 2>&1`;
    return $output;
}

# Transpile and run CL, return output
sub run_cl {
    my ($code) = @_;

    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;

    my $cl_code = `$pl2cl $pl_file 2>&1`;

    my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cl_fh $cl_code;
    close $cl_fh;

    my $output = `sbcl @sbcl_rt --load $cl_file 2>&1`;

    $output =~ s/^;.*\n//gm;
    $output =~ s/^caught .*\n//gm;
    $output =~ s/^compilation unit.*\n//gm;
    $output =~ s/^\s*Undefined.*\n//gm;
    $output =~ s/^-->.*\n//gm;
    $output =~ s/^==>.*\n//gm;
    $output =~ s/^PCL Runtime loaded\n//gm;
    $output =~ s/^\s*\n//gm;

    return $output;
}

# Test helper: compare Perl and CL output
sub test_transpile {
    my ($name, $code) = @_;
    my $perl_out = run_perl($code);
    my $cl_out = run_cl($code);
    is($cl_out, $perl_out, $name) or diag("Perl: $perl_out\nCL: $cl_out");
}

# ---------------------------------------------------------------------------
# List assignment in LIST context returns the assigned targets as LVALUES, so
# `$_++ foreach (LIST) = (VALUES)` increments the real variables (op/hashassign.t
# t304/t307-309).  Two things used to break that, both silently — the write went
# into a temporary and the variable kept its old value:
#   * a hash's ODD trailing key stored a bare *p-undef* instead of an entry box,
#     so its padded value was not writable;
#   * a scalar target starved by a greedy array/hash collected *p-undef* rather
#     than its own box, so `$z` in ($x,$y,%h,$z) = (0) never saw the ++.
# The simple cases (duplicate keys, leading scalar) already worked, which is why
# this needs the odd/starved shapes specifically.
# ---------------------------------------------------------------------------
test_transpile('aassign list-ctx lvalues: odd hash tail and starved trailing scalar', '
sub sh { my %h = @_; join(",", map { "$_=>" . (defined $h{$_} ? $h{$_} : "u") } sort keys %h) }
my (%h, $x, $y, $z);
$_++ foreach %h = (1,2,1,4);          print "dup     : ", sh(%h), "\n";
%h = (); $_++ foreach ($x,%h) = (0,1,2,3,4); print "lead-sc : x=$x ", sh(%h), "\n";
%h = (); $_++ foreach %h = (1,2,3);   print "odd     : ", sh(%h), "\n";
%h = (); $_++ foreach ($x,$y,%h,$z) = (0);     print "starved1: $x,$y,", sh(%h), ",$z\n";
%h = (); $_++ foreach ($x,$y,%h,$z) = (0,1);   print "starved2: $x,$y,", sh(%h), ",$z\n";
%h = (); $_++ foreach ($x,$y,%h,$z) = (0,1,2); print "starved3: $x,$y,", sh(%h), ",$z\n";
%h = (); my @a; $_++ foreach ($x,@a,$z) = (7,8,9); print "ary-eat : $x,@a,$z\n";
');

# The returned values must NOT be aliased to the RHS (op/hashassign.t t305/t306):
# assigning from $x and then incrementing the returned list must leave $x alone.
test_transpile('aassign list-ctx returns are not aliased to the RHS', '
my %h; my $x = 0;
$_++ foreach %h = ($x,$x);
print "rhs-unaliased=$x\n";
my %g; my $y = 0;
$_++ foreach sub :lvalue { %g = ($y,$y) }->();
print "lvalue-sub-rhs-unaliased=$y\n";
');

# A hash written with an odd element list still reads back an undef value, and
# exists() must see the padded key — the entry box must not change that.
test_transpile('odd hash tail: padded key exists and reads undef', '
my %h = (1,2,3);
print "keys=", join(",", sort keys %h), "\n";
print "exists3=", (exists $h{3} ? 1 : 0), " defined3=", (defined $h{3} ? 1 : 0), "\n";
print "count=", scalar(keys %h), "\n";
$h{3} = 9; print "after-write=$h{3}\n";
');

done_testing();
