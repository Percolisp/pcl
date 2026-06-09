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

plan tests => 9;

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

# Float stringification must match Perl's %.15g (15 significant digits, then
# strip trailing zeros), NOT SBCL's shortest-round-trip form.  `0.1+0.2` is the
# canonical case: perl prints 0.3, PCL was printing 0.30000000000000004.
# Found by tools/difftest-ops.pl numeric axis (session 242).  Covers the
# fixed-notation branch (0.1+0.2, 1/3, 2**0.5) and the exponential branch
# (the 1e+16 rounding-bump) of stringify-value.
test_cl('float stringification matches Perl %.15g',
    'print "[", 0.1+0.2, "][", 1/3, "][", 2**0.5, "][",'
    . ' 9.999999999999999e15, "]\n";',
    "[0.3][0.333333333333333][1.4142135623731][1e+16]\n");

# Compound /= and **= leaked a CL ratio: `$x /= 2` gave "7/2" and `$x **= -1`
# gave "1/2" instead of Perl's 3.5 / 0.5.  The macros divided/exponentiated
# raw (CL int/int -> ratio) while plain p-/ and p-** coerce ratio -> float.
# Fixed by delegating p-/= -> p-/ and p-**= -> p-**.  Found by the difftest-ops
# compound-assignment axis (session 242).
test_cl('compound /= and **= coerce ratio to float like Perl',
    'my $a=7; $a /= 2; my $b=2; $b **= -1;'
    . ' my $c=10; $c /= 4; print "[$a][$b][$c]\n";',
    "[3.5][0.5][2.5]\n");

# Array/hash SLICE inside string interpolation must join its elements with $"
# (list context), regardless of the context the string is used in.  Bug: a
# single-slice string "@a[1..2]" bypassed the string_concat/join wrapper, and
# even wrapped slices inherited the outer scalar context and reduced to the
# LAST element (`my $s = "@a[1..2]"` gave "3" not "2 3"; mid-string gave "x3y").
# Whole-array "@a" interpolation was always fine.  Found by the difftest-ops
# slice axis (session 242).
test_cl('array/hash slice in string interpolation joins with $" (list ctx)',
    'my @a=(1,2,3,4,5); my %h=(a=>1,b=>2,c=>3);'
    . ' my $s = "@a[1..2]"; my $t = "x@a[-2,-1]y"; my $u = "@h{qw(a c)}";'
    . ' print "[$s][$t][$u]\n";',
    "[2 3][x4 5y][1 3]\n");

# Anonymous array constructor [ ... ] evaluates its contents in LIST context and
# is never the enclosing sub's tail call.  Bug: a wantarray-sensitive builtin
# inside [...] (e.g. reverse) skipped its (let ((*wantarray* t)) ...) wrapper in
# tail position, so a scalar context leaked in — `do { ...; "@{[reverse @a]}" }`
# reversed the joined string ("123" -> "321") instead of the list (3,2,1).
# Found by the difftest-ops babycart axis (session 242).  sort/map (always-list)
# were unaffected; reverse exposed it.
test_cl('anon arrayref [ ] forces list context on contents (reverse in do-tail)',
    'my $r = do { my @a=(1,2,3); "@{[reverse @a]}" };'
    . ' my $d = "@{[reverse 1,2,3]}";'
    . ' sub f { return [ reverse @_ ] } my $t = "@{f(7,8,9)}";'
    . ' print "[$r][$d][$t]\n";',
    "[3 2 1][3 2 1][9 8 7]\n");

# Postfix conditional whose condition starts with a parenthesised group followed
# by an operator — `return X if (A) || (B)` — made PPI mislabel the leading `(A)`
# as a Structure::Condition (its `if (...)` bracket type) instead of a
# Structure::List, which PExpr's parse() did not handle ("unknown type
# PPI::Structure::Condition" -> the whole sub failed to transpile).  Now a
# Structure::Condition in expression position is parsed as a parenthesised expr,
# like Structure::List.  Found in Math::BigInt via the CPAN test-suite survey.
test_cl('postfix if with leading parenthesised condition (A) || (B)',
    'sub f { my ($a,$b)=@_; return "yes" if ($a == 1) || ($b == 2); return "no" }'
    . ' print "[", f(1,9), "][", f(9,2), "][", f(9,9), "]\n";',
    "[yes][yes][no]\n");
