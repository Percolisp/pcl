#!/usr/bin/env perl
# Test 2: Control Flow
# Tests: if/elsif/else, unless, while, until, for, foreach, ternary

use v5.32;
use strict;
use warnings;

say "=== Test 02: Control Flow ===";
say "";

# ============================================================
# if / elsif / else
# ============================================================
say "--- if/elsif/else ---";

my $x = 25;

if ($x > 50) {
    say "x>50";
} elsif ($x > 20) {
    say "x>20";
} elsif ($x > 10) {
    say "x>10";
} else {
    say "x<=10";
}

my $y = 5;
if ($y > 10) {
    say "y>10";
} else {
    say "y<=10";
}

my $z = 100;
if ($z > 50) {
    say "z>50";
}

# ============================================================
# unless
# ============================================================
say "";
say "--- unless ---";

my $flag = 0;
unless ($flag) {
    say "flag is false";
}

my $active = 1;
unless ($active) {
    say "not active";
}
say "after unless";

# ============================================================
# Ternary operator
# ============================================================
say "";
say "--- ternary ---";

my $a = 10;
my $b = 20;

my $max = $a > $b ? $a : $b;
say "max=" . $max;

my $min = $a < $b ? $a : $b;
say "min=" . $min;

# Nested ternary
my $val = 15;
my $size = $val > 20 ? "large" : $val > 10 ? "medium" : "small";
say "size=" . $size;

# ============================================================
# while loop
# ============================================================
say "";
say "--- while ---";

my $count = 0;
while ($count < 3) {
    say "count=" . $count;
    $count++;
}
say "done, count=" . $count;

# ============================================================
# until loop
# ============================================================
say "";
say "--- until ---";

my $n = 0;
until ($n >= 3) {
    say "n=" . $n;
    $n++;
}
say "done, n=" . $n;

# ============================================================
# C-style for loop
# ============================================================
say "";
say "--- for ---";

my $sum = 0;
for (my $i = 1; $i <= 5; $i++) {
    $sum += $i;
    say "i=" . $i . " sum=" . $sum;
}
say "total=" . $sum;

# Countdown
say "countdown:";
for (my $j = 3; $j >= 1; $j--) {
    say "  " . $j;
}
say "  go!";

# ============================================================
# Nested loops
# ============================================================
say "";
say "--- nested ---";

for (my $row = 1; $row <= 2; $row++) {
    for (my $col = 1; $col <= 3; $col++) {
        say "r" . $row . "c" . $col;
    }
}

# ============================================================
# Loop with compound conditions
# ============================================================
say "";
say "--- compound ---";

my $p = 0;
my $q = 10;
while ($p < 5 && $q > 5) {
    say "p=" . $p . " q=" . $q;
    $p++;
    $q--;
}

say "";
say "=== Done ===";
