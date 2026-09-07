#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# numeric-repr-01.t — THE NUMERIC REPRESENTATION family (s473d).
#
# One file for the questions that are all about how a Perl number is REPRESENTED
# in a p-box and how it comes back out as text:
#
#   A  #1230/#1245 — "is this a dualvar" is a FACT of the representation
#      (the :DUAL marker on p-box's NV-OK), never an inference from comparing
#      the two caches.  The inference false-positived on every cache-warm
#      float, because perl's %.15g rendering of a double does not round-trip
#      to the same double; %p-dualvar-copy then wrote the box's RAW value into
#      the STRING cache, and `printf "%-8s"` died inside sprintf-apply-width
#      with an SBCL type-error that killed the whole program.
#
#   B  #1248(a)/#1191 — `%` with an INFINITE RIGHT operand is the ordinary
#      mathematical modulo (sign of the right operand), not NaN.
#
#   C  #1248(b) — `**` returns an NV in perl, so 2**63 prints through %.15g.
#
#   D  #1012 — the %.15g switch to exponential happens AT 1e15, and deriving
#      the decimal exponent from a LOG got it wrong by one at exactly that
#      value (log(1e15, 10.0d0) is 14.999999999999998).
#
#   E  #1248(c) — Internals::SvREADONLY(@a,1) must not change what scalar(@a)
#      answers.
#
# EVERY expected string below was probed against perl 5.40.3 first: run the
# same program with `perl` and with `./runpcl` and the two are byte-equal.
# The rows are grouped into whole programs so the file costs a handful of SBCL
# launches rather than one per assertion.

use v5.30;
use strict;
use warnings;
use Test::More;
use File::Temp qw(tempfile);
use FindBin qw($RealBin);
use lib $RealBin;
use PCLCore;

my $project_root = "$RealBin/../..";
my $pl2cl        = "$project_root/pl2cl";
my $runtime      = "$project_root/cl/pcl-runtime.lisp";
my @sbcl_rt = PCLCore::sbcl_prefix($runtime);

plan skip_all => "pl2cl not found" unless -x $pl2cl;
plan skip_all => "sbcl not found"  unless `which sbcl 2>/dev/null`;

plan tests => 11;

sub run_cl {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;
    my $cl_code = `$pl2cl $pl_file 2>/dev/null`;
    my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cl_fh $cl_code;
    close $cl_fh;
    my $output = `sbcl @sbcl_rt --load $cl_file 2>&1`;
    $output =~ s/^;.*\n//gm;
    $output =~ s/^PCL Runtime loaded\n//gm;
    $output =~ s/^\s*\n//gm;
    return $output;
}

sub test_cl {
    my ($name, $code, $expected) = @_;
    is(run_cl($code), $expected, $name);
}

# ─────────────────────────────────────────────────────────────────────────────
# A — #1230/#1245.  A cache-warm float is NOT a dualvar, and printing one with
# a WIDTH must not reach a host type-error.  The crash needed all three of: a
# value from a STRING eval, returned through a sub whose body compares it with
# `eq ""` (that is what warms the string cache), and a conversion carrying a
# width.  `done` on the last line is the point: the whole program used to die.
# ─────────────────────────────────────────────────────────────────────────────
test_cl('#1245 a warm float survives printf with a width (the whole program runs)',
        <<'PL', <<'OUT');
sub cmp_sub  { my $v = shift; return "E" if $v eq ""; return $v }
sub bare_sub { my $v = shift; return $v }
my $fl = eval "1/3";
my $in = eval "42";
my $st = eval '"hello"';
printf "1 |%-8s| |%10s| |%s|\n", cmp_sub($fl), cmp_sub($fl), cmp_sub($fl);
printf "2 |%-8s|\n", bare_sub($fl);
printf "3 |%-8s| |%-8s|\n", cmp_sub($in), cmp_sub($st);
printf "4 %5.2f %d\n", cmp_sub($fl), cmp_sub($in);
my @a = (cmp_sub($fl)); my %h = (k => cmp_sub($fl));
printf "5 |%-8s| |%-8s|\n", $a[0], $h{k};
print "done\n";
PL
1 |0.333333333333333| |0.333333333333333| |0.333333333333333|
2 |0.333333333333333|
3 |42      | |hello   |
4  0.33 42
5 |0.333333333333333| |0.333333333333333|
done
OUT

# ─────────────────────────────────────────────────────────────────────────────
# A2 — the INVERSE guard.  Making "dualvar" a representation fact must not lose
# a GENUINE dualvar: $! and Scalar::Util::dualvar keep both halves through a
# sub frame, an array, a hash and an assignment, and isdual answers for the two
# shapes the old cache-comparison could not see (a dualvar whose halves happen
# to agree numerically).
# ─────────────────────────────────────────────────────────────────────────────
test_cl('#1230 inverse: a genuine dualvar keeps both halves and answers isdual',
        <<'PL', <<'OUT');
use Scalar::Util qw(dualvar isdual);
sub cmp_sub { my $v = shift; return "E" if $v eq ""; return $v }
my $dv = dualvar(42, "forty-two");
my $t = cmp_sub($dv);
printf "1 n=%d s=%s\n", $t+0, "$t";
my @a = ($dv); my %h = (k => $dv); my $c = $dv;
printf "2 %d %s %d %s %d %s\n", $a[0]+0, "$a[0]", $h{k}+0, "$h{k}", $c+0, "$c";
printf "3 %d %d %d\n", (isdual($dv)?1:0), (isdual(dualvar(0,"abc"))?1:0),
                       (isdual(dualvar(5,"5abc"))?1:0);
printf "4 %d %d\n", (isdual(1/3)?1:0), (isdual("x")?1:0);
open(my $fh, '<', '/nope-xyz-numeric-repr') or 1;
my $e = $!;  my $e2 = cmp_sub($e);
printf "5 %d %d %d\n", $e2+0, (length("$e2")>2 ? 1 : 0), (isdual($e2)?1:0);
PL
1 n=42 s=forty-two
2 42 forty-two 42 forty-two 42 forty-two
3 1 1 1
4 0 0
5 2 1 1
OUT

# ─────────────────────────────────────────────────────────────────────────────
# A3 — the emission is not what decides it: the same program under the general
# form compiler (PCL_OPT=none) prints the same thing.  The crash reproduced
# under -none too, which is how it was known to be the box model and not a
# raw-slot verdict.
# ─────────────────────────────────────────────────────────────────────────────
{
    local $ENV{PCL_OPT} = 'none';
    test_cl('#1245 identical under PCL_OPT=none', <<'PL', <<'OUT');
sub cmp_sub { my $v = shift; return "E" if $v eq ""; return $v }
my $fl = eval "1/3";
printf "1 |%-8s|\n", cmp_sub($fl);
print "done\n";
PL
1 |0.333333333333333|
done
OUT
}

# ─────────────────────────────────────────────────────────────────────────────
# B — #1248(a) / #1191.  pp_modulo's regime is chosen by the RIGHT operand.
# The whole 9x7 sign/magnitude matrix was probed against perl 5.40.3 and is
# byte-equal; these rows are the ones that were WRONG before (every infinite
# operand answered NaN, and a finite right operand at or above 2**64 answered
# an exact integer where perl answers an NV).
# ─────────────────────────────────────────────────────────────────────────────
test_cl('#1191 % with an infinite or out-of-UV-range right operand',
        <<'PL', <<'OUT');
no warnings;
my $inf = 9**9**9;
print "1 ", join(" ", 5 % $inf, -5 % $inf, 5 % -$inf, -5 % -$inf), "\n";
print "2 ", join(" ", 0 % $inf, 0 % -$inf, 5.7 % $inf, -5.7 % -$inf), "\n";
print "3 ", join(" ", $inf % 5, -$inf % 5, $inf % $inf, ($inf-$inf) % 5), "\n";
print "4 ", join(" ", 5 % ($inf-$inf), 5 % 1e30, -5 % 1e30, 5 % -1e30), "\n";
print "5 ", join(" ", -5 % 1e20, 1e30 % 1e20, -1e30 % 1e20, 1e30 % 3), "\n";
print "6 ", join(" ", 5 % 18446744073709551616, -5 % 18446744073709551616), "\n";
my $x = -5; $x %= $inf; my $y = 5; $y %= -$inf;
print "7 $x $y\n";
PL
1 5 Inf -Inf -5
2 0 0 5.7 -5.7
3 NaN NaN NaN NaN
4 NaN 5 1e+30 -1e+30
5 1e+20 19884624838656 9.99999801153752e+19 1
6 5 1.84467440737096e+19
7 Inf -Inf
OUT

# B2 — the shapes that must NOT change, plus pp_modulo's round-to-nearest
# quirk.  A right operand that still fits a UV keeps the integer regime (so
# `5 % 0.5` DIES on the TRUNCATED right), and when only the LEFT is out of
# range perl rounds BOTH magnitudes to nearest before the fmod: `1e30 % 3.7`
# is 0 because the modulus becomes 4, and `1e30 % 0.25` dies because 0.25
# rounds to 0.  An exact integer right and a float right are NOT the same
# operand here (`1e30 % 3` is 1, `1e30 % 3.7` is 0).
test_cl('#1191 inverse: the UV regime and pp_modulo round-to-nearest',
        <<'PL', <<'OUT');
no warnings;
my $inf = 9**9**9;
my @c = (['5%3',sub{5%3}], ['-5%3',sub{-5%3}], ['5%-3',sub{5%-3}],
         ['-5%-3',sub{-5%-3}], ['5%3.7',sub{5%3.7}], ['5.9%3',sub{5.9%3}],
         ['5%0',sub{5%0}], ['5%0.5',sub{5%0.5}], ['"7abc"%3',sub{"7abc"%3}],
         ['1e30%3.7',sub{1e30%3.7}], ['1e30%3',sub{1e30%3}],
         ['1e30%0.5',sub{1e30%0.5}], ['1e30%0.25',sub{1e30%0.25}],
         ['inf%0.5',sub{$inf%0.5}], ['1e19%3.7',sub{1e19%3.7}],
         ['5%18446744073709551615',sub{5%18446744073709551615}]);
for my $e (@c) {
  my ($n,$f) = @$e; my $v = eval { $f->() };
  if (!defined $v) { my $x = $@; $x =~ s/ at .* line \d+\.?\s*$//s; $v = "DIE:$x" }
  print "$n=$v\n";
}
PL
5%3=2
-5%3=1
5%-3=-1
-5%-3=-2
5%3.7=2
5.9%3=2
5%0=DIE:Illegal modulus zero
5%0.5=DIE:Illegal modulus zero
"7abc"%3=1
1e30%3.7=0
1e30%3=1
1e30%0.5=0
1e30%0.25=DIE:Illegal modulus zero
inf%0.5=NaN
1e19%3.7=1
5%18446744073709551615=5
OUT

# ─────────────────────────────────────────────────────────────────────────────
# C — #1248(b).  pp_pow uses integer arithmetic only where it is SURE, and
# WHICH branch fired is visible in the answer's spelling.  A POWER-OF-2 base is
# computed by repeated squaring in DOUBLES (perl's own choice), so `2**52`
# prints as 4.5035996273705e+15 and `2**63` as 9.22337203685478e+18; any other
# base uses integers only while `power * bitlength(base) <= 64`, which is why
# `7**19` is the exact 11398895185373143 and `3**40` is not.  PCL used to
# answer an exact bignum for every non-negative integer pair.
# ─────────────────────────────────────────────────────────────────────────────
test_cl('#1248(b) ** returns an NV wherever perl is not sure of an integer',
        <<'PL', <<'OUT');
no warnings;
for my $e ('2**10','2**31','2**32','2**52','2**53','2**62','2**63','2**64',
           '(-2)**63','3**40','10**15','10**16','10**20','7**19','(-7)**19',
           '(-7)**20','16**16','6**24','5**27','2**300','(-2)**301','4**31',
           '2**0.5','2**-1','0**0','2**1024','9**9**9','1.5**2') {
  print "$e = ", eval($e), "\n";
}
my $b = 2**63;
print "int ", int($b), " ; cmp ", ($b == 9223372036854775808 ? 1 : 0), "\n";
my %h = ( (2**63) => 1 ); print "key ", join(",", keys %h), "\n";
my $p = 2; my $q = 63; print "rt ", $p**$q, "\n";
PL
2**10 = 1024
2**31 = 2147483648
2**32 = 4294967296
2**52 = 4.5035996273705e+15
2**53 = 9.00719925474099e+15
2**62 = 4.61168601842739e+18
2**63 = 9.22337203685478e+18
2**64 = 1.84467440737096e+19
(-2)**63 = -9.22337203685478e+18
3**40 = 1.21576654590569e+19
10**15 = 1000000000000000
10**16 = 10000000000000000
10**20 = 1e+20
7**19 = 11398895185373143
(-7)**19 = -11398895185373143
(-7)**20 = 79792266297612001
16**16 = 1.84467440737096e+19
6**24 = 4.73838133832162e+18
5**27 = 7.45058059692383e+18
2**300 = 2.03703597633449e+90
(-2)**301 = -4.07407195266897e+90
4**31 = 4.61168601842739e+18
2**0.5 = 1.4142135623731
2**-1 = 0.5
0**0 = 1
2**1024 = Inf
9**9**9 = Inf
1.5**2 = 2.25
int 9223372036854775808 ; cmp 1
key 9.22337203685478e+18
rt 9.22337203685478e+18
OUT

# C2 — the pow() path's IEEE edges, which are C's and not CL's: a finite
# negative base with a finite non-integer exponent is NaN (CL's EXPT answers a
# COMPLEX number, which is not a Perl value at all), and a zero base with a
# negative exponent is +Inf (SBCL SIGNALS there, because :divide-by-zero is the
# one trap the runtime leaves armed).  An INFINITE base keeps C's answers.
test_cl('#1248(b) inverse: pow()`s IEEE edges are C`s, not CL`s',
        <<'PL', <<'OUT');
no warnings;
my $inf = 9**9**9; my $nan = $inf - $inf; my $ninf = -$inf;
my @c = (['(-8)**(1/3)',sub{(-8)**(1/3)}], ['(-2)**0.5',sub{(-2)**0.5}],
         ['(-2.5)**3',sub{(-2.5)**3}],     ['0**-1',sub{0**-1}],
         ['0**-2.5',sub{0**-2.5}],         ['0**$ninf',sub{0**$ninf}],
         ['0**$inf',sub{0**$inf}],         ['1**$nan',sub{1**$nan}],
         ['$nan**0',sub{$nan**0}],         ['2**$nan',sub{2**$nan}],
         ['2**$inf',sub{2**$inf}],         ['2**$ninf',sub{2**$ninf}],
         ['(-1)**$inf',sub{(-1)**$inf}],   ['$inf**-2',sub{$inf**-2}],
         ['(-$inf)**3',sub{(-$inf)**3}],   ['(-$inf)**2.5',sub{(-$inf)**2.5}],
         ['(-2)**-3',sub{(-2)**-3}],       ['0**5',sub{0**5}]);
for my $e (@c) {
  my ($n,$f) = @$e; my $v = eval { $f->() };
  if (!defined $v) { my $x = $@; $x =~ s/ at .* line \d+\.?\s*$//s; $v = "DIE:$x" }
  print "$n=$v\n";
}
PL
(-8)**(1/3)=NaN
(-2)**0.5=NaN
(-2.5)**3=-15.625
0**-1=Inf
0**-2.5=Inf
0**$ninf=Inf
0**$inf=0
1**$nan=1
$nan**0=1
2**$nan=NaN
2**$inf=Inf
2**$ninf=0
(-1)**$inf=1
$inf**-2=0
(-$inf)**3=-Inf
(-$inf)**2.5=Inf
(-2)**-3=-0.125
0**5=0
OUT

# ─────────────────────────────────────────────────────────────────────────────
# D — #1012.  perl's %.15g switches to exponential when the decimal exponent
# of the value ROUNDED TO 15 SIGNIFICANT DIGITS reaches 15 (or falls below
# -4).  PCL derived that exponent from a LOG, and `log(1e15, 10.0d0)` is
# 14.999999999999998, so 1e15 printed 1000000000000000; it also never applied
# the rounding, so 999999999999999.9 (which rounds up to 1e15) and
# 9.999999999999999e-5 (which rounds up to 1e-4) took the wrong branch.  The
# exponential branch had a second latent bug the fix exposed: SBCL's `~,14E`
# does NOT renormalise a mantissa that rounds up to 10, so the first value
# printed `10e+14`.
#
# The last row is the SIBLING: sprintf's own `%g` picked its style the same
# way and had the same missing carry, so `%g` of 999999.9 printed 1000000
# where perl prints 1e+06.  ONE reading of the rule now serves both.
# ─────────────────────────────────────────────────────────────────────────────
test_cl('#1012 the %.15g style switch is the ROUNDED exponent, computed exactly',
        <<'PL', <<'OUT');
no warnings;
my @v = (
 1e15, 1e14, 1e16, 1e13, 1e21, 1e-5, 1e-4, 1e-3, 0.1+0.2, 1/3,
 999999999999999.9, 999999999999999.0, 1000000000000001.0,
 123456789012345, 1234567890123456, 12345678901234567,
 9.999999999999999e14, 9.99999999999999e14, 9.9999999999999e14,
 1.0000000000000001e15, 1e15 + 0.5,
 9.999999999999999e-5, 9.99999999999999e-5, 9.9999999999999e-5,
 1.0000000000000001e-4, 0.0001, 0.00009999,
 1e300, 1e-300, 5e-324, 1.7976931348623157e308,
 0.5, 2.5, 1024, 4503599627370496, 9007199254740992,
 1e17, 1e20, 3.14159265358979, 2.718281828459045,
 1e15/7, 1e-15, 1e-16, 100000000000000.5, 99999999999999.98,
 1e100, 1.5e-10, 1e-323, 2.2250738585072014e-308, sqrt(1e30),
);
for my $x (@v) { print "[$x]\n" }
printf "pf %s %s %s\n", 1e15, 1e14, 1e16;
my %h = (1e15 => 'a'); print "key ", join(",", keys %h), "\n";
print "cat ", 1e15 . "", "\n";
printf "g %g %G %g %g %.3g %.15g\n", 1e15, 999999.9, 999999.4, 1e-5, 999999.9, 1e15;
PL
[1e+15]
[100000000000000]
[1e+16]
[10000000000000]
[1e+21]
[1e-05]
[0.0001]
[0.001]
[0.3]
[0.333333333333333]
[1e+15]
[999999999999999]
[1e+15]
[123456789012345]
[1234567890123456]
[12345678901234567]
[1e+15]
[999999999999999]
[999999999999990]
[1e+15]
[1e+15]
[0.0001]
[9.99999999999999e-05]
[9.9999999999999e-05]
[0.0001]
[0.0001]
[9.999e-05]
[1e+300]
[1e-300]
[4.94065645841247e-324]
[1.79769313486232e+308]
[0.5]
[2.5]
[1024]
[4503599627370496]
[9007199254740992]
[1e+17]
[1e+20]
[3.14159265358979]
[2.71828182845905]
[142857142857143]
[1e-15]
[1e-16]
[100000000000000]
[100000000000000]
[1e+100]
[1.5e-10]
[9.88131291682493e-324]
[2.2250738585072e-308]
[1e+15]
pf 1e+15 100000000000000 1e+16
key 1e+15
cat 1e+15
g 1e+15 1E+06 999999 1e-05 1e+06 1e+15
OUT

# D2 — the RESIDUE, asserted so it is countable rather than a surprise.  perl
# holds an integral arithmetic result as an IV and prints its digits, while
# the SAME value written as a float literal is an NV and prints through
# %.15g; PCL has one representation for both, so `1e14 * 10` and `1e15` are
# the same double here and print alike.  That divergence is the "Integers are
# unbounded / no IV-NV distinction" family (#1369), it is UNIFORM after this
# fix (it already applied above 1e16 — `1e15 * 10` is 1e+16 here and
# 10000000000000000 in perl) and PCL's pure-INTEGER arithmetic still agrees,
# which is what keeps it narrow.
test_cl('#1012 residue: an IV-valued arithmetic result has no NV/IV flag here',
        <<'PL', <<'OUT');
no warnings;
printf "lit-nv    %s\n", 1e15;                 # perl 1e+15
printf "nv*iv     %s\n", 1e14 * 10;            # perl 1000000000000000 (IV)
printf "iv*nv     %s\n", -1 * 1e15;            # perl -1000000000000000 (IV)
printf "nv+iv     %s\n", 1e15 + 1;             # perl 1000000000000001 (IV)
printf "lit-dot   %s\n", 1000000000000000.0;   # perl 1e+15
printf "lit-int   %s\n", 1000000000000000;     # perl 1000000000000000
printf "iv*iv     %s\n", 1000000 * 1000000000; # perl 1000000000000000
printf "nv*iv-e16 %s\n", 1e15 * 10;            # perl 10000000000000000 (IV)
PL
lit-nv    1e+15
nv*iv     1e+15
iv*nv     -1e+15
nv+iv     1e+15
lit-dot   1e+15
lit-int   1000000000000000
iv*iv     1000000000000000
nv*iv-e16 1e+16
OUT

# ─────────────────────────────────────────────────────────────────────────────
# E — #1248(c).  A read-only array is still an ARRAY.  Its storage used to be a
# plain SIMPLE vector, and sixteen sites across the runtime ask
# ADJUSTABLE-ARRAY-P to mean "is this value an array variable's storage" (as
# opposed to a string, a hash, a box holding an array REFERENCE, or one of the
# simple vectors that carry LIST temporaries — a `p-..` range, a `(vector …)`
# argument run).  A simple vector answered NO to that question as well as to
# "is it writable", so `scalar(@a)`, `0+@a`, `my $n = @a` and `scalar(@$r)`
# stopped collapsing to the element count and yielded the vector itself, which
# printed as ARRAY(0x1) or as its own elements.  The storage is now an
# ADJUSTABLE vector with NO FILL POINTER: adjustable answers the first
# question, the missing fill pointer answers the second and is what makes every
# size change fail.
# ─────────────────────────────────────────────────────────────────────────────
test_cl('#1248(c) a read-only array answers every scalar-context question as an array',
        <<'PL', <<'OUT');
no warnings;
sub ro { my @a = @_; Internals::SvREADONLY(@a,1); return \@a }
my @a=(1,2,3); Internals::SvREADONLY(@a,1);
print "scalar   ", scalar(@a), "\n";
print "numify   ", 0+@a, "\n";
my $n = @a;      print "assign   $n\n";
my $m; $m = @a;  print "assign2  $m\n";
print "lastidx  ", $#a, "\n";
print "bool     ", (@a ? "true" : "false"), "\n";
print "interp   @a\n";
print "join     ", join(",",@a), "\n";
print "elem     ", $a[1], "\n";
print "cmpnum   ", (@a == 3 ? "y" : "n"), "\n";
print "sprintf  ", sprintf("%d/%s", scalar(@a), scalar(@a)), "\n";
my $r = \@a;
print "ref-sc   ", scalar(@$r), "\n";
print "ref-num  ", 0+@$r, "\n";
print "ref-last ", $#$r, "\n";
print "sub-sc   ", sub { @a }->(), "\n";
print "fresh    ", scalar(@{ ro(5,6,7,8) }), "\n";
my @c = @a;      print "copy     ", scalar(@c), "\n";
print "grep     ", scalar(grep { $_ > 1 } @a), "\n";
print "sort     ", join("",sort { $b <=> $a } @a), "\n";
print "slice    ", join(",", @a[0,2]), "\n";
print "keys     ", join(",", keys @a), "\n";
PL
scalar   3
numify   3
assign   3
assign2  3
lastidx  2
bool     true
interp   1 2 3
join     1,2,3
elem     2
cmpnum   y
sprintf  3/3
ref-sc   3
ref-num  3
ref-last 2
sub-sc   123
fresh    4
copy     3
grep     2
sort     321
slice    1,3
keys     0,1,2
OUT

# E2 — the INVERSE: making the array an array again must not make it writable.
# Every size change still dies with perl's own text, an in-bounds ELEMENT write
# still lands (perl freezes the AV, not its elements), the flag getter still
# answers 1/"", and clearing the flag gives back a growable array.
test_cl('#1248(c) inverse: read-only still means fixed SIZE, and only the size',
        <<'PL', <<'OUT');
no warnings;
sub d { my ($n,$c) = @_; my $r = eval { $c->(); 1 };
        my $e = $@; $e =~ s/ at .*//s; $e =~ s/\n.*//s;
        print "$n ", ($r ? "lived" : $e), "\n" }
my @a=(1,2,3);
print "before   ", Internals::SvREADONLY(@a), " ", scalar(@a), "\n";
Internals::SvREADONLY(@a,1);
print "flag     ", Internals::SvREADONLY(@a), "\n";
d("push    ", sub { push @a, 4 });
d("pop     ", sub { pop @a });
d("shift   ", sub { shift @a });
d("unshift ", sub { unshift @a, 0 });
d("splice  ", sub { splice(@a,0,1) });
d("assign  ", sub { @a = (7,8) });
d("clear   ", sub { @a = () });
d("undef   ", sub { undef @a });
d("elemwr  ", sub { $a[1] = 9 });
print "after    ", join(",",@a), " n=", scalar(@a), "\n";
Internals::SvREADONLY(@a,0);
print "flagoff  ", Internals::SvREADONLY(@a), "\n";
push @a, 4;
print "grown    ", join(",",@a), " n=", scalar(@a), "\n";
PL
before    3
flag     1
push     Modification of a read-only value attempted
pop      Modification of a read-only value attempted
shift    Modification of a read-only value attempted
unshift  Modification of a read-only value attempted
splice   Modification of a read-only value attempted
assign   Modification of a read-only value attempted
clear    Modification of a read-only value attempted
undef    Modification of a read-only value attempted
elemwr   lived
after    1,9,3 n=3
flagoff  
grown    1,9,3,4 n=4
OUT
