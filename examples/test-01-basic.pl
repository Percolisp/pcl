#!/usr/bin/env perl
# Simple test file for PCL runtime comparison
# Avoids string interpolation, uses explicit concatenation

use v5.32;
use strict;
use warnings;

# ============================================================
# Arithmetic operations
# ============================================================
say "=== Arithmetic ===";

my $a = 10;
my $b = 3;

say "a=" . $a . ", b=" . $b;
say "a+b=" . ($a + $b);
say "a-b=" . ($a - $b);
say "a*b=" . ($a * $b);
say "a%b=" . ($a % $b);
say "a**b=" . ($a ** $b);

# ============================================================
# Increment/decrement
# ============================================================
say "";
say "=== Increment ===";

my $x = 5;
say "x=" . $x;
say "x++=" . $x++;
say "now x=" . $x;
say "++x=" . ++$x;
say "now x=" . $x;

# ============================================================
# Compound assignment
# ============================================================
say "";
say "=== Compound ===";

my $n = 100;
say "n=" . $n;
$n += 10;
say "n+10=" . $n;
$n -= 5;
say "n-5=" . $n;
$n *= 2;
say "n*2=" . $n;

# ============================================================
# String operations
# ============================================================
say "";
say "=== Strings ===";

my $greeting = "Hello";
my $name = "World";
my $msg = $greeting . ", " . $name . "!";
say "msg=" . $msg;
say "len=" . length($msg);

my $str = "test";
$str .= "_suffix";
say "concat=" . $str;

my $repeated = "ab" x 4;
say "repeat=" . $repeated;

# ============================================================
# Comparisons
# ============================================================
say "";
say "=== Compare ===";

my $p = 5;
my $q = 10;
my $r = 5;

say "p==r: " . ($p == $r ? "T" : "F");
say "p==q: " . ($p == $q ? "T" : "F");
say "p<q: " . ($p < $q ? "T" : "F");
say "p<=r: " . ($p <= $r ? "T" : "F");

my $s1 = "apple";
my $s2 = "banana";
say "lt: " . ($s1 lt $s2 ? "T" : "F");
say "eq: " . ($s1 eq $s1 ? "T" : "F");

# ============================================================
# While loop
# ============================================================
say "";
say "=== While ===";

my $i = 0;
my $sum = 0;
while ($i < 5) {
    $sum += $i;
    say "i=" . $i . " sum=" . $sum;
    $i++;
}
say "final=" . $sum;

# ============================================================
# For loop (C-style)
# ============================================================
say "";
say "=== For ===";

my $product = 1;
for (my $j = 1; $j <= 5; $j++) {
    $product *= $j;
    say "j=" . $j . " prod=" . $product;
}
say "5!=" . $product;

# ============================================================
# Conditionals
# ============================================================
say "";
say "=== Cond ===";

my $val = 42;
if ($val > 50) {
    say "gt50";
} elsif ($val > 40) {
    say "gt40";
} else {
    say "le40";
}

my $result = $val > 0 ? "pos" : "neg";
say "sign=" . $result;

# ============================================================
# Logical operators
# ============================================================
say "";
say "=== Logic ===";

my $t = 1;
my $f = 0;

say "t&&t=" . ($t && $t);
say "t&&f=" . ($t && $f);
say "f||t=" . ($f || $t);
say "f||f=" . ($f || $f);

my $default = 0;
my $value = $default || 42;
say "or42=" . $value;

say "";
say "=== Done ===";
