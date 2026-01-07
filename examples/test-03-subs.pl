#!/usr/bin/env perl
# Test 3: Subroutines (simplified - signature style only)
# Tests: sub definition, signatures, defaults, recursion

use v5.32;
use strict;
use warnings;
use feature 'signatures';
no warnings 'experimental::signatures';

say "=== Test 03: Subroutines ===";
say "";

# ============================================================
# Simple sub with no parameters
# ============================================================
say "--- no params ---";

sub greet() {
    say "Hello!";
}

greet();

sub get_magic() {
    return 42;
}

my $magic = get_magic();
say "magic=" . $magic;

# ============================================================
# Sub with signature
# ============================================================
say "";
say "--- signature ---";

sub add($a, $b) {
    return $a + $b;
}

my $sum = add(10, 20);
say "10+20=" . $sum;

sub multiply($x, $y) {
    return $x * $y;
}

my $prod = multiply(6, 7);
say "6*7=" . $prod;

sub subtract($a, $b) {
    return $a - $b;
}

say "100-37=" . subtract(100, 37);

# ============================================================
# Default parameter values
# ============================================================
say "";
say "--- defaults ---";

sub greet_default($name = "World") {
    say "Hi, " . $name . "!";
}

greet_default("Charlie");
greet_default();

sub power($base, $exp = 2) {
    my $result = 1;
    for (my $i = 0; $i < $exp; $i++) {
        $result *= $base;
    }
    return $result;
}

say "3^2=" . power(3);
say "2^4=" . power(2, 4);

# ============================================================
# Implicit return (last expression)
# ============================================================
say "";
say "--- implicit return ---";

sub double($n) {
    $n * 2;
}

say "double(21)=" . double(21);

sub max($a, $b) {
    $a > $b ? $a : $b;
}

say "max(15,22)=" . max(15, 22);

# ============================================================
# Recursion
# ============================================================
say "";
say "--- recursion ---";

sub factorial($n) {
    if ($n <= 1) {
        return 1;
    }
    return $n * factorial($n - 1);
}

say "5!=" . factorial(5);
say "6!=" . factorial(6);

sub fib($n) {
    if ($n <= 1) {
        return $n;
    }
    return fib($n - 1) + fib($n - 2);
}

say "fib(7)=" . fib(7);
say "fib(10)=" . fib(10);

# ============================================================
# Sub calling sub
# ============================================================
say "";
say "--- sub calls sub ---";

sub square($n) {
    return $n * $n;
}

sub sum_of_squares($a, $b) {
    return square($a) + square($b);
}

say "3^2+4^2=" . sum_of_squares(3, 4);

# ============================================================
# Sub with many parameters
# ============================================================
say "";
say "--- many params ---";

sub sum5($a, $b, $c, $d, $e) {
    return $a + $b + $c + $d + $e;
}

say "1+2+3+4+5=" . sum5(1, 2, 3, 4, 5);

say "";
say "=== Done ===";
