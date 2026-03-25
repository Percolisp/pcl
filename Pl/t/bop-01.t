#!/usr/bin/env perl
# bop-01.t - Tests for old-style prototype ($) argument limiting at call sites
#
# Root cause: sub _and($) { ... } has prototype ($) = 1 scalar arg.
#   is _and 0, '0', 'str'  must parse as  is(_and(0), '0', 'str')
# PCL was parsing it as  is(_and(0, '0', 'str'))  — pl-is got 1 arg → SBCL crash.
#
# Fix: handle_subcalls in PExpr.pm now calls _proto_max_args to limit $end_pars
# when the function has an old-style prototype with no @ or % params.

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

plan tests => 25;

# ── Helpers ─────────────────────────────────────────────────────────────────

sub transpile {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;
    return scalar `$pl2cl $pl_file 2>/dev/null`;
}

sub run_cl {
    my ($code) = @_;
    my $cl_code = transpile($code);
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

# ── Codegen tests ────────────────────────────────────────────────────────────

# Test 1: ($) prototype limits to 1 arg — bop.t crash case
{
    my $cl = transpile('sub _and($) { $_[0] & $_[1] }  is _and 0, "0", "str";');
    like($cl, qr/\(pl-is \(pl-_and 0\)/,
         '($) proto: is _and 0, "0", "str" -> is(_and(0), ...)');
}

# Test 2: without fix, _and would have eaten all 3 args; verify it did not
{
    my $cl = transpile('sub _and($) { $_[0] & $_[1] }  is _and 0, "0", "str";');
    unlike($cl, qr/pl-_and 0 "0" "str"/,
           '($) proto: _and must NOT consume all 3 args');
}

# Test 3: ($$) prototype limits to 2 args
{
    my $cl = transpile('sub add($$) { $_[0] + $_[1] }  print add 3, 4, "\n";');
    like($cl, qr/pl-add 3 4/,
         '($$) proto: add 3, 4, "\n" -> add(3,4) + rest for print');
    unlike($cl, qr/pl-add 3 4 "\\\\n"/,
           '($$) proto: add must NOT consume the trailing \n arg');
}

# Test 4: no prototype — sub without prototype still eats all args
{
    my $cl = transpile('sub add { $_[0] + $_[1] }  print add 3, 4;');
    like($cl, qr/pl-add 3 4/,
         'no proto: add 3, 4 passes both args (no limiting)');
}

# ── Runtime tests ────────────────────────────────────────────────────────────

# Test 5: ($) proto limits at runtime — prototype sub gets 1 arg; rest go to push
{
    test_cl('($) proto: push gets remaining args after proto-limited call',
        'sub double_first($) { return $_[0] * 2 }
my @a;
push @a, double_first 3, 7, 9;
print scalar @a, "\n";
print $a[0], "\n";
print $a[1], "\n";
print $a[2], "\n";',
        "3\n6\n7\n9\n");
}

# Test 6: ($$) proto: sub gets exactly 2 args
{
    test_cl('($$) proto: sub gets 2 args; rest go to outer list',
        'sub sum2($$) { return $_[0] + $_[1] }
my @a;
push @a, sum2 10, 20, 30;
print scalar @a, "\n";
print $a[0], "\n";
print $a[1], "\n";',
        "2\n30\n30\n");
}

# ── Indirect object syntax: "new ClassName ARGS" ─────────────────────────────

# Test 8: "new version ~$_" — the bop.t test 454 crash case
# Before fix: right-to-left loop turned "version ~$_" into funcall(version,~$_),
# then "new funcall(...)" into funcall(new,funcall(version,...)) → MAIN::PL-VERSION crash.
{
    my $cl = transpile('new version ~$_');
    like($cl, qr/p-method-call.*version.*new/,
         'new version ~$_: generates p-method-call with version and new');
    unlike($cl, qr/pl-version/,
           'new version ~$_: must NOT call pl-version (indirect object, not funcall)');
}

# Test 10: runtime — "new Dog" works as ClassName->new()
{
    test_cl('new Dog: indirect object syntax works at runtime',
        'package Dog;
sub new { my $class = shift; bless { name => "Fido" }, $class }
sub name { $_[0]->{name} }
package main;
my $d = new Dog;
print ref($d), "\n";
print $d->name(), "\n";',
        "Dog\nFido\n");
}

# ── String bitwise ops ────────────────────────────────────────────────────────

# Test 11: & truncates to shorter length
{
    test_cl('string & truncates to shorter',
        'my $Aaz = chr(ord("A") & ord("z"));
print "AAAAA" & "zzzzz", "\n";',
        chr(ord("A") & ord("z")) x 5 . "\n");
}

# Test 12: | extends to longer length, short side padded with NUL
{
    test_cl('string | extends to longer',
        'my $foo = "A" x 5; my $bar = "z" x 3;
print $foo | $bar, "\n";',
        chr(ord("A") | ord("z")) x 3 . chr(ord("A") | 0) x 2 . "\n");
}

# Test 13: ^ extends to longer length (use ord() to avoid ';' in output getting stripped)
{
    test_cl('string ^ extends to longer',
        'my $foo = "A" x 5; my $bar = "z" x 3; my $r = $foo ^ $bar;
print length($r), " ", ord(substr($r,0,1)), " ", ord(substr($r,3,1)), "\n";',
        "5 " . (ord("A") ^ ord("z")) . " " . (ord("A") ^ 0) . "\n");
}

# Test 14: numeric & still works
{
    test_cl('numeric & still works',
        'print 0xdead & 0xbeef, "\n";',
        "40621\n");  # 0x9ead = 40621
}

# Test 15: numeric |= (COW) still works — bop.t tests 37-38
{
    test_cl('$cow |= number (numeric)',
        'my %h = (150 => 1); my $i = (keys %h)[0]; $i |= 105; print $i, "\n";',
        "255\n");
}

# Test 16: numeric &= (COW) still works
{
    test_cl('$cow &= number (numeric)',
        'my %h = (150 => 1); my $i = (keys %h)[0]; $i &= 105; print $i, "\n";',
        "0\n");
}

# Test 17: string &= assigns string result
{
    test_cl('string &= works',
        'my $s = "zzzzz"; $s &= "AAAAA"; print $s, "\n";',
        chr(ord("z") & ord("A")) x 5 . "\n");
}

# Test 18: string |= works
{
    test_cl('string |= works',
        'my $s = "AAA"; $s |= "zzzzz"; print $s, "\n";',
        chr(ord("A") | ord("z")) x 3 . chr(0 | ord("z")) x 2 . "\n");
}

# ── use integer bitwise (tests 19-25) ────────────────────────────────────────

# Test 19: use integer; ~0 == -1
{
    test_cl('use integer: ~0 is -1 (signed)',
        'print ~0 > 0 ? "unsigned" : "fail", "\n";
print do { use integer; ~0 } == -1 ? "ok" : "fail", "\n";',
        "unsigned\nok\n");
}

# Test 20: use integer; $cusp & -1 is negative
{
    test_cl('use integer: $cusp & -1 is negative (signed)',
        'my $bits = 0; for (my $i = ~0; $i; $i >>= 1) { ++$bits; }
my $cusp = 1 << ($bits - 1);
print +($cusp & -1) > 0 ? "unsigned-ok" : "fail", "\n";
print do { use integer; $cusp & -1 } < 0 ? "signed-ok" : "fail", "\n";',
        "unsigned-ok\nsigned-ok\n");
}

# Test 21: use integer; $cusp | 1 is negative
{
    test_cl('use integer: $cusp | 1 is negative',
        'my $bits = 0; for (my $i = ~0; $i; $i >>= 1) { ++$bits; }
my $cusp = 1 << ($bits - 1);
print do { use integer; $cusp | 1 } < 0 ? "ok" : "fail", "\n";',
        "ok\n");
}

# Test 22: use integer; 1 << ($bits-1) == -$cusp
{
    test_cl('use integer: 1 << 63 is -cusp',
        'my $bits = 0; for (my $i = ~0; $i; $i >>= 1) { ++$bits; }
my $cusp = 1 << ($bits - 1);
print do { use integer; 1 << ($bits - 1) } == -$cusp ? "ok" : "fail", "\n";',
        "ok\n");
}

# Test 23: use integer; large right-shift of negative = -1
{
    test_cl('use integer: -9 >> huge_amount is -1',
        'print do { use integer; -9 >> 18446744073709551616 } == -1 ? "ok" : "fail", "\n";',
        "ok\n");
}

# Test 24: use integer; negative left-shift amount (= right shift) of negative = -1
{
    test_cl('use integer: -4 << -2147483648 is -1',
        'print do { use integer; -4 << -2147483648 } == -1 ? "ok" : "fail", "\n";',
        "ok\n");
}

# Test 25: use integer; abs($cusp >> 1) == $cusp/2 (regression: must not break passing test)
{
    test_cl('use integer: abs(cusp >> 1) == cusp/2',
        'my $bits = 0; for (my $i = ~0; $i; $i >>= 1) { ++$bits; }
my $cusp = 1 << ($bits - 1);
print +($cusp >> 1) == ($cusp / 2) ? "first-ok" : "fail", "\n";
print do { use integer; abs($cusp >> 1) } == ($cusp / 2) ? "second-ok" : "fail", "\n";',
        "first-ok\nsecond-ok\n");
}
