#!/usr/bin/env perl
# reverse-01.t - reverse scalar context and deleted-element preservation

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

plan tests => 12;

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
    my $got = run_cl($code);
    is($got, $expected, $name);
}

# ── Scalar context: reverse reverses characters ──────────────────────────────

test_cl('reverse("abc") in scalar context gives "cba"',
    'my $r = reverse("abc"); print $r, "\n";',
    "cba\n");

test_cl('scalar(reverse("abc")) as function argument gives "cba"',
    'sub check_eq { print $_[0] eq $_[1] ? "ok" : "fail: got $_[0]", "\n" } check_eq(scalar(reverse("abc")), "cba");',
    "ok\n");

test_cl('reverse() uses $_ in scalar context',
    '$_ = "foobar"; my $r = reverse(); print $r, "\n";',
    "raboof\n");

test_cl('scalar reverse($a) reverses Unicode string',
    'my $a = "\x{263A}\x{263A}x\x{263A}y\x{263A}"; my $b = scalar reverse($a); my $c = scalar reverse($b); print $a eq $c ? "ok" : "fail", "\n";',
    "ok\n");

# ── List context: reverse reverses element order ──────────────────────────────

test_cl('reverse @array in list context',
    'my @a = (1, 2, 3, 4); my @b = reverse @a; print "@b\n";',
    "4 3 2 1\n");

test_cl('reverse preserves length',
    'my @a = ("foo", "bar"); my @b = reverse @a; print scalar(@b), "\n";',
    "2\n");

# ── Deleted element positions preserved after reverse ────────────────────────

test_cl('delete then reverse preserves nil slot',
    'my @a = (1, 2, 3, 4); @a = reverse @a;  # (4,3,2,1)
delete $a[1];              # (4,_,2,1)
@a = reverse @a;           # (1,2,_,4)
print exists($a[2]) ? "exists" : "deleted", "\n";',
    "deleted\n");

test_cl('values ok after delete+reverse',
    'my @a = (1, 2, 3, 4); @a = reverse @a;
delete $a[1];
@a = reverse @a;
print $a[0], $a[1], $a[3], "\n";',
    "124\n");

test_cl('two deletes then reverse preserves two nil slots',
    'my @a = (5, 6, 7, 8, 9); @a = reverse @a;  # (9,8,7,6,5)
delete $a[3];              # (9,8,7,_,5)
@a = reverse @a;           # (5,_,7,8,9)
delete $a[2];              # (5,_,_,8,9)
@a = reverse @a;           # (9,8,_,_,5)
print !exists($a[2]) && !exists($a[3]) ? "ok" : "fail", "\n";',
    "ok\n");

test_cl('reverse of empty array',
    'my @a; @a = reverse @a; print scalar(@a), "\n";',
    "0\n");

# ── Postfix for with split list: reverse/length context fix ──────────────────
# Bug: split in postfix-for list got SCALAR_CTX (wrapped in `length`), and
# reverse used as arg to length inherited list context from push → gave ARRAY(0x...)

test_cl('postfix for over split: length reverse uses $_ correctly',
    'my @x; push @x, length reverse for split "-", "abc--def"; print join(",",@x), "\n";',
    "3,0,3\n");

test_cl('postfix for over split: empty string gives length 0',
    'my @x; push @x, length reverse for split "-", "\x{100}--0"; print $x[1], "\n";',
    "0\n");
