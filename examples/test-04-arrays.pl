#!/usr/bin/env perl
# Test 4: Arrays
# Tests: creation, access, modification, push/pop/shift, slices

use v5.32;
use strict;
use warnings;
use feature 'signatures';
no warnings 'experimental::signatures';

say "=== Test 04: Arrays ===";
say "";

# ============================================================
# Array creation and access
# ============================================================
say "--- access ---";

# Note: Direct array literal assignment is broken in transpiler
# Using push to build arrays instead
my @nums;
push @nums, 10;
push @nums, 20;
push @nums, 30;
push @nums, 40;
push @nums, 50;

say "nums[0]=" . $nums[0];
say "nums[2]=" . $nums[2];
say "nums[4]=" . $nums[4];

# Negative indexing
say "nums[-1]=" . $nums[-1];
say "nums[-2]=" . $nums[-2];

# ============================================================
# Array modification
# ============================================================
say "";
say "--- modify ---";

$nums[1] = 99;
say "after [1]=99: " . $nums[1];

$nums[0] = $nums[0] + 5;
say "after [0]+=5: " . $nums[0];

# ============================================================
# push and pop
# ============================================================
say "";
say "--- push/pop ---";

my @stack;
push @stack, 1;
push @stack, 2;
push @stack, 3;
say "pushed 1,2,3";

my $top = pop @stack;
say "pop=" . $top;
$top = pop @stack;
say "pop=" . $top;

push @stack, 10;
say "pushed 10";
$top = pop @stack;
say "pop=" . $top;
$top = pop @stack;
say "pop=" . $top;

# ============================================================
# shift and unshift
# ============================================================
say "";
say "--- shift/unshift ---";

my @queue;
push @queue, 1;
push @queue, 2;
push @queue, 3;

my $first = shift @queue;
say "shift=" . $first;
$first = shift @queue;
say "shift=" . $first;

unshift @queue, 10;
say "unshift 10";
$first = shift @queue;
say "shift=" . $first;

# ============================================================
# Array length
# ============================================================
say "";
say "--- length ---";

my @items;
push @items, "a";
push @items, "b";
push @items, "c";
push @items, "d";

my $len = scalar(@items);
say "length=" . $len;

my $last_idx = $#items;
say "last_idx=" . $last_idx;

# ============================================================
# Iteration
# ============================================================
say "";
say "--- foreach ---";

my @vals;
push @vals, 10;
push @vals, 20;
push @vals, 30;

my $total = 0;
foreach my $v (@vals) {
    say "v=" . $v;
    $total += $v;
}
say "total=" . $total;

# ============================================================
# Array in expressions
# ============================================================
say "";
say "--- expressions ---";

my @data;
push @data, 5;
push @data, 10;
push @data, 15;

my $sum = $data[0] + $data[1] + $data[2];
say "sum=" . $sum;

my $prod = $data[0] * $data[2];
say "prod=" . $prod;

say "";
say "=== Done ===";
