#!/usr/bin/env perl
# Transpile tests part 7.  NEW TESTS GO HERE (or a future -08) — the
# BIGGEST test file bounds the parallel suite's wall time (one SBCL spawn
# per test), so start a new file instead of growing the current largest.

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

# #25 suite family: high capture groups ($10+) and $^N.  $^N is the
# participating group with the rightmost CLOSING paren — the nested case
# is where it differs from $+ (highest-numbered opener).
test_transpile("capture groups past \$9", '
"abcdefghij" =~ /(a)(b)(c)(d)(e)(f)(g)(h)(i)(j)/;
print "ten:$10:$+\n";
"xy" =~ /(x)(y)/;
print "stale:", (defined $10 ? "still" : "cleared"), "\n";
');

test_transpile("\$^N rightmost-closer rule incl. nested groups", '
"ab" =~ /(a(b))/;      print "nested:", $^N, "\n";
"ab" =~ /(a)(b)/;      print "flat:", $^N, "\n";
"b"  =~ /(?:(a)|(b))/; print "alt:$^N\n";
');

# Task #114 (range.t 162, RT #130841): a range whose byte size wraps
# size_t must croak in perl's "panic: memory wrap|Out of memory" family,
# not a PCL-specific overflow message.
test_transpile("huge range croak speaks perl's memory-wrap family", '
my $max_iv = (~0 >> 1);
eval { my @range = 1..($max_iv - 1); };
print "err:", (($@ =~ /panic: memory wrap|Out of memory/) ? "wrap-family" : "other:$@"), "\n";
');

done_testing();
