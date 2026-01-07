#!/usr/bin/env perl
# Test 6: References
# Tests: scalar refs, array refs, hash refs, dereferencing, modify via ref

use v5.32;
use strict;
use warnings;
use feature 'signatures';
no warnings 'experimental::signatures';

say "=== Test 06: References ===";
say "";

# ============================================================
# Scalar references
# ============================================================
say "--- scalar refs ---";

my $x = 42;
say "x=" . $x;

my $ref = \$x;
say "ref points to x";

# Dereference
my $val = $$ref;
say "deref=" . $val;

# Modify via reference
$$ref = 100;
say "after $$ref=100, x=" . $x;

$$ref += 5;
say "after $$ref+=5, x=" . $x;

# ============================================================
# Pass by reference
# ============================================================
say "";
say "--- pass by ref ---";

sub increment_ref {
    my $r = shift;
    $$r = $$r + 1;
}

my $counter = 0;
say "counter=" . $counter;
increment_ref(\$counter);
say "after increment: " . $counter;
increment_ref(\$counter);
say "after increment: " . $counter;

sub double_ref {
    my $r = shift;
    $$r = $$r * 2;
}

my $num = 5;
double_ref(\$num);
say "5 doubled=" . $num;

# ============================================================
# Array references (arrow syntax)
# ============================================================
say "";
say "--- array refs ---";

my @arr;
push @arr, 10;
push @arr, 20;
push @arr, 30;

my $aref = \@arr;
say "aref->[0]=" . $aref->[0];
say "aref->[2]=" . $aref->[2];

$aref->[1] = 99;
say "after aref->[1]=99, arr[1]=" . $arr[1];

# ============================================================
# Hash references (arrow syntax)
# ============================================================
say "";
say "--- hash refs ---";

my %hash;
$hash{name} = "Bob";
$hash{age} = 25;

my $href = \%hash;
say "href->{name}=" . $href->{name};
say "href->{age}=" . $href->{age};

$href->{age} = 26;
say "after href->{age}=26, hash{age}=" . $hash{age};

$href->{city} = "NYC";
say "added via ref: " . $hash{city};

# ============================================================
# Anonymous array ref
# ============================================================
say "";
say "--- anon array ---";

my $anon_arr = [1, 2, 3, 4, 5];
say "anon[0]=" . $anon_arr->[0];
say "anon[4]=" . $anon_arr->[4];

$anon_arr->[2] = 30;
say "after [2]=30: " . $anon_arr->[2];

# ============================================================
# Anonymous hash ref
# ============================================================
say "";
say "--- anon hash ---";

my $anon_hash = {x => 10, y => 20, z => 30};
say "x=" . $anon_hash->{x};
say "y=" . $anon_hash->{y};

$anon_hash->{z} = 300;
say "after z=300: " . $anon_hash->{z};

# ============================================================
# Nested structures
# ============================================================
say "";
say "--- nested ---";

my $data = {
    name => "Test",
    values => [100, 200, 300]
};

say "name=" . $data->{name};
say "values[0]=" . $data->{values}->[0];
say "values[2]=" . $data->{values}->[2];

$data->{values}->[1] = 999;
say "after [1]=999: " . $data->{values}->[1];

# ============================================================
# ref() function
# ============================================================
say "";
say "--- ref() ---";

my $scalar = 42;
my $s_ref = \$scalar;
my $a_ref = [1, 2, 3];
my $h_ref = {a => 1};

say "ref(scalar)=" . (ref($scalar) || "not a ref");
say "ref(s_ref)=" . ref($s_ref);
say "ref(a_ref)=" . ref($a_ref);
say "ref(h_ref)=" . ref($h_ref);

say "";
say "=== Done ===";
