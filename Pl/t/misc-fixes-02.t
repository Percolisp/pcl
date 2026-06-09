#!/usr/bin/env perl
# misc-fixes-02.t - Continuation of misc-fixes-01.t (which became the largest .t).
# Operator-semantics bugs surfaced by the differential fuzzer (tools/difftest-ops.pl).

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

plan tests => 4;

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

# ── difftest-ops fuzzer finds (session 241): operator bugs perl-tests/ missed ──

# [10] Chained string comparison crashed: the chained-comparison expander mapped
# `eq`/`ne`/`lt`/... to a non-existent p-eq (only the numeric p-== family exists);
# string ops live in the p-str-* family.  `'2' eq '10' eq '3'` → both compares
# false → "" (perl prints empty).  Was: UNDEFINED-FUNCTION PCL::P-EQ at load.
test_cl('chained string comparison (eq/lt) does not crash',
    'my $r = "2" eq "10" eq "3"; $r = "F" unless $r; print "[$r]\n";'
    . 'my $s = "a" lt "b" lt "c"; print "[$s]\n";',
    "[F]\n[1]\n");

# [9] Numeric bitwise & | ^ treat operands as UNSIGNED 64-bit in Perl, so a
# negative operand wraps to its two's-complement value.  `(2-3) | 4` = -1|4 =
# 2**64-1.  PCL was doing signed CL logior → -1.
test_cl('numeric bitwise | ^ treat operands as unsigned 64-bit',
    'my $a = (2 - 3) | 4; my $b = (2 - 3) ^ 4; print "$a $b\n";',
    "18446744073709551615 18446744073709551611\n");

# [2] Relational < > <= >= bind TIGHTER than equality == != eq ne, so
# `2 != 3 > 4` is `2 != (3 > 4)` = `2 != "" ` = 1.  The chained-comparison
# left-scan was crossing precedence tiers and chaining `(2!=3) && (3>4)` = "".
test_cl('relational binds tighter than equality (no cross-tier chaining)',
    'my $a = 2 != 3 > 4; my $b = 2 != 3 >= 4; print "[$a][$b]\n";',
    "[1][1]\n");

# Braced block-deref + subscript: PPI tags the `[...]` after a `${BLOCK}`/`@{BLOCK}`
# as a Constructor (anon array) not a Subscript, so `${$ar}[1]` / `@{$ar}[0,2]`
# fell through to the "Missing case" die and silently became undef.  The arrow
# (`$ar->[1]`), $$ (`$$ar[1]`), and hash (`${$hr}{a}`) forms always worked.
# Found by tools/difftest-ops.pl deref axis (session 241).
test_cl('braced array block-deref with index/slice subscript',
    'my @a=(10,20,30); my $ar=\@a;'
    . ' my $x = ${$ar}[1]; my @s = @{$ar}[0,2];'
    . ' print "[$x][@s]\n";',
    "[20][10 30]\n");
