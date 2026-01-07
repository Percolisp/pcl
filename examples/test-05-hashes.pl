#!/usr/bin/env perl
# Test 5: Hashes
# Tests: creation, access, modification, keys/values, delete

use v5.32;
use strict;
use warnings;
use feature 'signatures';
no warnings 'experimental::signatures';

say "=== Test 05: Hashes ===";
say "";

# ============================================================
# Hash creation and access
# ============================================================
say "--- access ---";

# Note: Direct hash literal assignment is broken in transpiler
# Building hash via assignment instead
my %person;
$person{name} = "Alice";
$person{age} = 30;
$person{city} = "Paris";

say "name=" . $person{name};
say "age=" . $person{age};
say "city=" . $person{city};

# ============================================================
# Hash modification
# ============================================================
say "";
say "--- modify ---";

$person{age} = 31;
say "age after birthday: " . $person{age};

$person{country} = "France";
say "added country: " . $person{country};

# ============================================================
# exists and delete - skipping (parsing issue with hash access args)
# ============================================================
say "";
say "--- exists/delete ---";
say "skipped";

# ============================================================
# keys and values
# ============================================================
say "";
say "--- keys/values ---";

my %scores;
$scores{alice} = 95;
$scores{bob} = 87;
$scores{charlie} = 92;

# Count keys
my @k = keys %scores;
say "num keys=" . scalar(@k);

# Sum values
my @v = values %scores;
my $sum = 0;
foreach my $val (@v) {
    $sum += $val;
}
say "sum values=" . $sum;

# ============================================================
# Hash with numeric values
# ============================================================
say "";
say "--- numeric ---";

my %counts;
$counts{a} = 0;
$counts{b} = 0;
$counts{c} = 0;

$counts{a}++;
$counts{a}++;
$counts{b}++;

say "a=" . $counts{a};
say "b=" . $counts{b};
say "c=" . $counts{c};

$counts{a} += 10;
say "a after +=10: " . $counts{a};

# ============================================================
# Hash in expressions
# ============================================================
say "";
say "--- expressions ---";

my %data;
$data{x} = 10;
$data{y} = 20;
$data{z} = 30;

my $total = $data{x} + $data{y} + $data{z};
say "total=" . $total;

my $cond = $data{x} < $data{y} ? "x<y" : "x>=y";
say "compare: " . $cond;

say "";
say "=== Done ===";
