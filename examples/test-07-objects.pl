#!/usr/bin/env perl
# Test 7: Objects
# Tests: bless, method calls, constructors, ref()

use v5.32;
use strict;
use warnings;
use feature 'signatures';
no warnings 'experimental::signatures';

say "=== Test 07: Objects ===";
say "";

# ============================================================
# Basic bless
# ============================================================
say "--- bless ---";

my $obj = {name => "Widget", value => 42};
bless $obj, "Thing";

say "ref=" . ref($obj);
say "name=" . $obj->{name};
say "value=" . $obj->{value};

# ============================================================
# Package with methods
# ============================================================
say "";
say "--- Counter class ---";

package Counter {
    sub new {
        my $class = shift;
        my $start = shift;
        $start = 0 unless defined $start;
        my $self = {count => $start};
        bless $self, $class;
        return $self;
    }

    sub increment {
        my $self = shift;
        $self->{count} = $self->{count} + 1;
    }

    sub decrement {
        my $self = shift;
        $self->{count} = $self->{count} - 1;
    }

    sub get {
        my $self = shift;
        return $self->{count};
    }

    sub add {
        my $self = shift;
        my $n = shift;
        $self->{count} = $self->{count} + $n;
    }
}

package main;

my $c = Counter->new(0);
say "initial=" . $c->get();

$c->increment();
say "after incr=" . $c->get();

$c->increment();
$c->increment();
say "after 2 more=" . $c->get();

$c->decrement();
say "after decr=" . $c->get();

$c->add(10);
say "after add(10)=" . $c->get();

say "ref(c)=" . ref($c);

# ============================================================
# Another class: Point
# ============================================================
say "";
say "--- Point class ---";

package Point {
    sub new {
        my $class = shift;
        my $x = shift;
        my $y = shift;
        $x = 0 unless defined $x;
        $y = 0 unless defined $y;
        my $self = {x => $x, y => $y};
        bless $self, $class;
        return $self;
    }

    sub x {
        my $self = shift;
        return $self->{x};
    }

    sub y {
        my $self = shift;
        return $self->{y};
    }

    sub move {
        my $self = shift;
        my $dx = shift;
        my $dy = shift;
        $self->{x} = $self->{x} + $dx;
        $self->{y} = $self->{y} + $dy;
    }

    sub to_string {
        my $self = shift;
        return "(" . $self->{x} . "," . $self->{y} . ")";
    }
}

package main;

my $p = Point->new(10, 20);
say "point=" . $p->to_string();
say "x=" . $p->x();
say "y=" . $p->y();

$p->move(5, -3);
say "after move(5,-3)=" . $p->to_string();

say "ref(p)=" . ref($p);

# ============================================================
# Multiple instances
# ============================================================
say "";
say "--- multiple instances ---";

my $p1 = Point->new(0, 0);
my $p2 = Point->new(100, 100);

say "p1=" . $p1->to_string();
say "p2=" . $p2->to_string();

$p1->move(10, 10);
say "p1 after move=" . $p1->to_string();
say "p2 unchanged=" . $p2->to_string();

say "";
say "=== Done ===";
