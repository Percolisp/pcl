#!/usr/bin/env perl
# sprintf-invalid-01.t — invalid sprintf conversions.
#
# Perl leaves an unrecognised conversion (e.g. %C, %I, %Z, %vc) verbatim in the
# output and warns "Invalid conversion in sprintf"; it must NOT consume an
# argument, and a malformed spec suppresses the trailing "Redundant argument"
# warning.  (sprintf.t INVALID-marker cluster.)

use v5.30;
use strict;
use warnings;
use Test::More;
use File::Temp qw(tempfile);
use FindBin qw($RealBin);

my $project_root = "$RealBin/../..";
my $pl2cl        = "$project_root/pl2cl";
my $runtime      = "$project_root/cl/pcl-runtime.lisp";

plan skip_all => "pl2cl not found" unless -x $pl2cl;
plan skip_all => "sbcl not found"  unless `which sbcl 2>/dev/null`;

plan tests => 13;

sub run_cl {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;
    my $cl_code = `$pl2cl $pl_file 2>/dev/null`;
    my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cl_fh $cl_code;
    close $cl_fh;
    my $output = `sbcl --noinform --non-interactive --load $runtime --load $cl_file 2>&1`;
    $output =~ s/^;.*\n//gm;
    $output =~ s/^PCL Runtime loaded\n//gm;
    $output =~ s/^\s*\n//gm;
    return $output;
}

sub test_cl {
    my ($name, $code, $expected) = @_;
    is(run_cl($code), $expected, $name);
}

# The classic harness shape: collect warning markers and the verbatim output.
my $harness = <<'PL';
my $w;
local $SIG{__WARN__} = sub {
    if    ($_[0] =~ /^Invalid conversion/) { $w .= ' INVALID' }
    elsif ($_[0] =~ /^Redundant argument/) { $w .= ' REDUNDANT' }
    elsif ($_[0] =~ /^Missing argument/)   { $w .= ' MISSING' }
};
sub fmt { $w = undef; my $x = sprintf(shift, @_); $x .= $w if $w; $x }
PL

# Invalid uppercase letters are left verbatim + warn INVALID, no REDUNDANT.
test_cl('invalid %C verbatim + INVALID',
    $harness . q{print fmt('%C', ''), "\n";}, "%C INVALID\n");
test_cl('invalid %I verbatim + INVALID',
    $harness . q{print fmt('%I', ''), "\n";}, "%I INVALID\n");
test_cl('invalid %Z verbatim + INVALID',
    $harness . q{print fmt('%Z', ''), "\n";}, "%Z INVALID\n");
test_cl('invalid %S verbatim + INVALID',
    $harness . q{print fmt('%S', ''), "\n";}, "%S INVALID\n");

# Vector flag: only integer conversions valid; %vc is invalid and the arg
# pointer is restored so the following %d picks up the first arg.
test_cl('invalid %vc then %d (arg pointer restored)',
    $harness . q{print fmt('%vc,%d', 63, 64, 65), "\n";}, "%vc,63 INVALID\n");

# Malformed spec with embedded space stays verbatim.
test_cl('malformed "%6. 6s" verbatim + INVALID',
    $harness . q{print fmt('%6. 6s', ''), "\n";}, "%6. 6s INVALID\n");

# Valid conversions still behave: D/U/O synonyms, %X upper, normal %d.
test_cl('valid %D synonym for %ld', q{printf "%D\n", 0x7fffffff;}, "2147483647\n");
test_cl('valid %X upper hex',       q{printf "%X\n", 255;},        "FF\n");

# Normal Redundant warning still fires when there is NO invalid conversion.
test_cl('normal REDUNDANT still fires',
    $harness . q{print fmt('%d', 1, 2), "\n";}, "1 REDUNDANT\n");

# Integer overflow: a width/precision exceeding a C int (2**31-1) dies
# "Integer overflow in format string ..." instead of leaking an SBCL type
# error.  Covers literal precision, *-arg width, *-arg precision (even huge
# negative, which overflows before the "negative = omitted" rule).
sub like_cl {
    my ($name, $code, $re) = @_;
    like(run_cl($code), $re, $name);
}
like_cl('literal precision overflow dies Integer overflow',
    q{my $r = eval { sprintf "%.18446744073709551615a", 1.1 }; print $@;},
    qr/^Integer overflow in format string for sprintf /);
like_cl('*-arg width overflow dies Integer overflow',
    q{my $r = eval { sprintf "%*s", ~0, "abc" }; print $@;},
    qr/Integer overflow/);
like_cl('*-arg precision (huge negative) overflow dies Integer overflow',
    q{my $r = eval { sprintf "%.*s", -1-(~0>>1), "abc" }; print $@;},
    qr/Integer overflow/);
# A normal small width/precision must NOT trip the overflow guard.
test_cl('normal width/precision unaffected by overflow guard',
    q{printf "%5.2f|%*d|%.*f\n", 3.14159, 4, 7, 2, 3.14159;}, " 3.14|   7|3.14\n");
