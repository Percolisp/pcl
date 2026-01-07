#!/usr/bin/env perl
# Combined Test for PCL Transpiler - Phase 1 & 2
# Run with: perl examples/test-combined.pl
# Or transpile: ./pl2cl examples/test-combined.pl > examples/test-combined.lisp
# Then: sbcl --load cl/pcl-runtime.lisp --load examples/test-combined.lisp

use v5.32;
use strict;
use warnings;
use feature 'signatures';
no warnings 'experimental::signatures';

say "=== PCL Combined Test ===";
say "";

# ============================================================
# Phase 1: Basic Expressions and Operators
# ============================================================
say "--- arithmetic ---";
my $a = 10;
my $b = 3;
say "a=$a b=$b";
say "a+b=" . ($a + $b);
say "a-b=" . ($a - $b);
say "a*b=" . ($a * $b);
say "a/b=" . ($a / $b);
say "a%b=" . ($a % $b);
say "a**b=" . ($a ** $b);

say "";
say "--- comparison ---";
say "10>5=" . (10 > 5 ? 1 : 0);
say "10<5=" . (10 < 5 ? 1 : 0);
say "10==10=" . (10 == 10 ? 1 : 0);
say "10!=5=" . (10 != 5 ? 1 : 0);

say "";
say "--- logical ---";
say "1&&1=" . (1 && 1);
say "1&&0=" . (1 && 0);
say "0||1=" . (0 || 1);
say "!0=" . (!0 ? 1 : 0);

say "";
say "--- string ops ---";
my $s1 = "hello";
my $s2 = "world";
say "concat=" . $s1 . " " . $s2;
say "length=" . length($s1);
say "uc=" . uc($s1);
say "lc=" . uc($s1);
say "substr=" . substr($s1, 1, 3);

say "";
say "--- increment/decrement ---";
my $n = 5;
say "n=$n";
$n++;
say "n++=$n";
$n--;
say "n--=$n";
$n += 10;
say "n+=10=$n";
$n -= 3;
say "n-=3=$n";

# ============================================================
# Phase 1: Control Flow
# ============================================================
say "";
say "--- if/else ---";
my $x = 10;
if ($x > 5) {
    say "x>5";
}
if ($x > 20) {
    say "x>20";
} else {
    say "x<=20";
}
if ($x < 5) {
    say "x<5";
} elsif ($x < 15) {
    say "5<=x<15";
} else {
    say "x>=15";
}

say "";
say "--- while ---";
my $i = 0;
while ($i < 3) {
    say "i=$i";
    $i++;
}

say "";
say "--- for ---";
for (my $j = 0; $j < 3; $j++) {
    say "j=$j";
}

say "";
say "--- foreach ---";
my @items;
push @items, 10;
push @items, 20;
push @items, 30;
foreach my $item (@items) {
    say "item=$item";
}

# ============================================================
# Phase 2: Subroutines
# ============================================================
say "";
say "--- subs ---";

sub add($x, $y) {
    return $x + $y;
}

sub greet($name) {
    return "Hello, $name!";
}

say "add(3,4)=" . add(3, 4);
say "greet=" . greet("World");

sub factorial($n) {
    if ($n <= 1) {
        return 1;
    }
    return $n * factorial($n - 1);
}

say "5!=" . factorial(5);

# ============================================================
# Phase 2: Arrays
# ============================================================
say "";
say "--- arrays ---";
my @arr;
push @arr, 1;
push @arr, 2;
push @arr, 3;
push @arr, 4;
push @arr, 5;
say "arr[0]=" . $arr[0];
say "arr[2]=" . $arr[2];
say "arr[-1]=" . $arr[-1];
say "length=" . scalar(@arr);

push @arr, 6;
say "after push, last=" . $arr[-1];

my $popped = pop @arr;
say "popped=$popped";

my $shifted = shift @arr;
say "shifted=$shifted";

unshift @arr, 0;
say "after unshift, first=" . $arr[0];

# ============================================================
# Phase 2: Hashes
# ============================================================
say "";
say "--- hashes ---";
my %hash;
$hash{name} = "Alice";
$hash{age} = 30;
say "name=" . $hash{name};
say "age=" . $hash{age};

$hash{city} = "NYC";
say "city=" . $hash{city};

say "keys=" . scalar(keys %hash);

# ============================================================
# Phase 2: References
# ============================================================
say "";
say "--- refs ---";
my $val = 42;
my $ref = \$val;
say "val=$val";
say "deref=" . $$ref;

$$ref = 100;
say "after mod, val=$val";

my $aref = [1, 2, 3];
say "aref->[0]=" . $aref->[0];

my $href = {x => 10, y => 20};
say "href->{x}=" . $href->{x};

# ============================================================
# Phase 2: Objects
# ============================================================
say "";
say "--- objects ---";

package Counter {
    sub new {
        my $class = shift;
        my $start = shift;
        $start = 0 unless defined $start;
        my $self = {count => $start};
        bless $self, $class;
        return $self;
    }

    sub incr {
        my $self = shift;
        $self->{count}++;
    }

    sub get {
        my $self = shift;
        return $self->{count};
    }
}

package main;

my $counter = Counter->new(0);
say "counter=" . $counter->get();
$counter->incr();
$counter->incr();
say "after 2 incr=" . $counter->get();
say "ref=" . ref($counter);

say "";
say "=== All Tests Complete ===";
