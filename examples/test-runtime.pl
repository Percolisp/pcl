#!/usr/bin/env perl
# Test file for PCL runtime comparison
# Run in Perl and compare output with translated CL version

use v5.32;
use strict;
use warnings;

# ============================================================
# Arithmetic operations
# ============================================================
say "=== Arithmetic ===";

my $a = 10;
my $b = 3;

say "a = $a, b = $b";
say "a + b = " . ($a + $b);
say "a - b = " . ($a - $b);
say "a * b = " . ($a * $b);
say "a / b = " . ($a / $b);
say "a % b = " . ($a % $b);
say "a ** b = " . ($a ** $b);

# ============================================================
# Increment/decrement
# ============================================================
say "";
say "=== Increment/Decrement ===";

my $x = 5;
say "x = $x";
say "x++ = " . $x++;
say "now x = $x";
say "++x = " . ++$x;
say "now x = $x";
say "x-- = " . $x--;
say "now x = $x";
say "--x = " . --$x;
say "now x = $x";

# ============================================================
# Compound assignment
# ============================================================
say "";
say "=== Compound Assignment ===";

my $n = 100;
say "n = $n";
$n += 10;
say "n += 10: $n";
$n -= 5;
say "n -= 5: $n";
$n *= 2;
say "n *= 2: $n";
$n /= 3;
say "n /= 3: $n";

# ============================================================
# String operations
# ============================================================
say "";
say "=== String Operations ===";

my $greeting = "Hello";
my $name = "World";
my $msg = $greeting . ", " . $name . "!";
say "msg = $msg";
say "length = " . length($msg);

my $str = "test";
$str .= "_suffix";
say "str .= '_suffix': $str";

my $repeated = "ab" x 4;
say "'ab' x 4 = $repeated";

# ============================================================
# Comparisons
# ============================================================
say "";
say "=== Comparisons ===";

my $p = 5;
my $q = 10;
my $r = 5;

say "p=$p, q=$q, r=$r";
say "p == r: " . ($p == $r ? "true" : "false");
say "p == q: " . ($p == $q ? "true" : "false");
say "p < q: " . ($p < $q ? "true" : "false");
say "q > p: " . ($q > $p ? "true" : "false");
say "p <= r: " . ($p <= $r ? "true" : "false");

# String comparisons
my $s1 = "apple";
my $s2 = "banana";
say "s1='$s1', s2='$s2'";
say "s1 lt s2: " . ($s1 lt $s2 ? "true" : "false");
say "s1 eq s1: " . ($s1 eq $s1 ? "true" : "false");

# ============================================================
# Loops - while
# ============================================================
say "";
say "=== While Loop ===";

my $i = 0;
my $sum = 0;
while ($i < 5) {
    $sum += $i;
    say "i=$i, sum=$sum";
    $i++;
}
say "Final sum: $sum";

# ============================================================
# Loops - for (C-style)
# ============================================================
say "";
say "=== For Loop (C-style) ===";

my $product = 1;
for (my $j = 1; $j <= 5; $j++) {
    $product *= $j;
    say "j=$j, product=$product";
}
say "5! = $product";

# ============================================================
# Loops - foreach
# ============================================================
say "";
say "=== Foreach Loop ===";

my @numbers = (10, 20, 30, 40, 50);
my $total = 0;
foreach my $num (@numbers) {
    $total += $num;
    say "num=$num, total=$total";
}
say "Total: $total";

# ============================================================
# Conditionals
# ============================================================
say "";
say "=== Conditionals ===";

my $val = 42;
if ($val > 50) {
    say "val > 50";
} elsif ($val > 40) {
    say "val > 40 but <= 50";
} else {
    say "val <= 40";
}

# Ternary
my $result = $val > 0 ? "positive" : "non-positive";
say "val is $result";

# ============================================================
# Arrays
# ============================================================
say "";
say "=== Arrays ===";

my @arr = (1, 2, 3, 4, 5);
say "arr[0] = $arr[0]";
say "arr[2] = $arr[2]";
say "arr[-1] = $arr[-1]";

$arr[1] = 20;
say "After arr[1]=20: arr[1] = $arr[1]";

# ============================================================
# Hashes
# ============================================================
say "";
say "=== Hashes ===";

my %data = (name => "Alice", age => 30, city => "Paris");
say "name = $data{name}";
say "age = $data{age}";

$data{age} = 31;
say "After age=31: age = $data{age}";

$data{country} = "France";
say "Added country: $data{country}";

# ============================================================
# Logical operators
# ============================================================
say "";
say "=== Logical Operators ===";

my $t = 1;
my $f = 0;

say "t && t: " . ($t && $t);
say "t && f: " . ($t && $f);
say "f || t: " . ($f || $t);
say "f || f: " . ($f || $f);

# Short-circuit
my $default = 0;
my $value = $default || 42;
say "0 || 42 = $value";

my $undef_val;
my $with_default = $undef_val // "default";
say "undef // 'default' = $with_default";

say "";
say "=== Done ===";
