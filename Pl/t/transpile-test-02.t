#!/usr/bin/env perl
# Transpile tests part 2: string operations, arrays, hashes, refs, objects

use v5.30;
use strict;
use warnings;
use Test::More;
use File::Temp qw(tempfile);
use FindBin qw($RealBin);

# Path to pl2cl and runtime
my $project_root = "$RealBin/../..";
my $pl2cl = "$project_root/pl2cl";
my $runtime = "$project_root/cl/pcl-runtime.lisp";

# Check dependencies
plan skip_all => "pl2cl not found" unless -x $pl2cl;
plan skip_all => "sbcl not found" unless `which sbcl 2>/dev/null`;

# Run a Perl snippet and return output
sub run_perl {
    my ($code) = @_;
    # Use -E to enable say and other modern features
    my $output = `perl -E '$code' 2>&1`;
    return $output;
}

# Transpile and run CL, return output
sub run_cl {
    my ($code) = @_;

    # Write Perl code to temp file
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;

    # Transpile
    my $cl_code = `$pl2cl $pl_file 2>&1`;

    # Write CL to temp file
    my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cl_fh $cl_code;
    close $cl_fh;

    # Run with sbcl
    my $output = `sbcl --noinform --non-interactive --load $runtime --load $cl_file 2>&1`;

    # Filter out warnings and "PCL Runtime loaded"
    $output =~ s/^;.*\n//gm;
    $output =~ s/^caught .*\n//gm;
    $output =~ s/^compilation unit.*\n//gm;
    $output =~ s/^\s*Undefined.*\n//gm;
    $output =~ s/^-->.*\n//gm;
    $output =~ s/^==>.*\n//gm;
    $output =~ s/^PCL Runtime loaded\n//gm;
    $output =~ s/^\s*\n//gm;

    return $output;
}

# Test helper: compare Perl and CL output
sub test_transpile {
    my ($name, $code) = @_;
    my $perl_out = run_perl($code);
    my $cl_out = run_cl($code);
    is($cl_out, $perl_out, $name) or diag("Perl: $perl_out\nCL: $cl_out");
}

# ============ TESTS ============

# chomp and chop
test_transpile("chomp: basic", 'my $s = "hello\n"; chomp($s); say "[$s]";');
test_transpile("chomp: no newline", 'my $s = "hello"; chomp($s); say "[$s]";');
test_transpile("chomp: return value", 'my $s = "hi\n"; my $n = chomp($s); say $n;');

test_transpile("chop: basic", 'my $s = "hello"; chop($s); say "[$s]";');
test_transpile("chop: return value", 'my $s = "world"; my $c = chop($s); say "removed: $c";');

# Arrays - use variable workaround for print $a[n] limitation
test_transpile("array: create and access", 'my @a; push @a, 1; push @a, 2; push @a, 3; my $x = $a[1]; say $x;');
test_transpile("array: negative index", 'my @a; push @a, 10; push @a, 20; push @a, 30; my $x = $a[-1]; say $x;');
test_transpile("array: push/pop", 'my @a; push @a, 1; push @a, 2; my $x = pop @a; say $x;');
test_transpile("array: shift/unshift", 'my @a; push @a, 2; push @a, 3; unshift @a, 1; my $x = shift @a; say $x;');
test_transpile("array: scalar context (length)", 'my @a; push @a, 1; push @a, 2; push @a, 3; my $len = scalar(@a); say $len;');

# Hashes - use variable workaround for print $h{k} limitation
test_transpile("hash: create and access", 'my %h; $h{name} = "Alice"; my $x = $h{name}; say $x;');
test_transpile("hash: keys count", 'my %h; $h{a} = 1; $h{b} = 2; my @k = keys %h; my $n = scalar(@k); say $n;');

# References
test_transpile("ref: scalar ref", 'my $x = 42; my $ref = \\$x; my $v = $$ref; say $v;');
test_transpile("ref: modify through ref", 'my $x = 10; my $ref = \\$x; $$ref = 20; say $x;');
test_transpile("ref: array ref", 'my $aref = [1, 2, 3]; my $v = $aref->[1]; say $v;');
test_transpile("ref: hash ref", 'my $href = {a => 1, b => 2}; my $v = $href->{b}; say $v;');

# Objects
test_transpile("object: bless and ref", '
package Counter;
sub new { my $class = shift; my $self = {count => 0}; bless $self, $class; return $self; }
package main;
my $c = Counter->new();
say ref($c);
');

test_transpile("object: method call", '
package Counter;
sub new { my $class = shift; my $self = {count => 0}; bless $self, $class; return $self; }
sub get { my $self = shift; return $self->{count}; }
sub incr { my $self = shift; $self->{count}++; }
package main;
my $c = Counter->new();
$c->incr();
$c->incr();
my $v = $c->get();
say $v;
');

# wantarray
test_transpile("wantarray: scalar context", '
sub ctx { if (wantarray()) { return (1, 2, 3); } else { return "scalar"; } }
my $s = ctx();
say $s;
');

test_transpile("wantarray: list context", '
sub ctx { if (wantarray()) { return (1, 2, 3); } else { return "scalar"; } }
my @a = ctx();
say "@a";
');

# ============ MULTIPLE PACKAGES ============

test_transpile("packages: switch between two", '
package Foo;
sub greet { return "Hello from Foo"; }

package Bar;
sub greet { return "Hello from Bar"; }

package main;
my $f = Foo::greet();
my $b = Bar::greet();
say $f;
say $b;
');

test_transpile("packages: three packages", '
package Alpha;
sub value { return 1; }

package Beta;
sub value { return 2; }

package Gamma;
sub value { return 3; }

package main;
my $sum = Alpha::value() + Beta::value() + Gamma::value();
say $sum;
');

test_transpile("packages: call across packages", '
package Math;
sub double { my $n = shift; return $n * 2; }
sub triple { my $n = shift; return $n * 3; }

package Compute;
sub calc { my $n = shift; return Math::double($n) + Math::triple($n); }

package main;
my $r = Compute::calc(5);
say $r;
');

# ============ MULTIPLE CLASSES ============

test_transpile("classes: two classes", '
package Dog;
sub new { my $class = shift; bless { name => shift }, $class; }
sub speak { my $self = shift; return $self->{name} . " says woof"; }

package Cat;
sub new { my $class = shift; bless { name => shift }, $class; }
sub speak { my $self = shift; return $self->{name} . " says meow"; }

package main;
my $d = Dog->new("Rex");
my $c = Cat->new("Whiskers");
say $d->speak();
say $c->speak();
');

test_transpile("classes: class with multiple methods", '
package Point;
sub new {
    my $class = shift;
    my $self = { x => shift, y => shift };
    bless $self, $class;
    return $self;
}
sub x { my $self = shift; return $self->{x}; }
sub y { my $self = shift; return $self->{y}; }
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

package main;
my $p = Point->new(3, 4);
say $p->to_string();
$p->move(2, -1);
say $p->to_string();
');

test_transpile("classes: objects in array", '
package Item;
sub new { my $class = shift; bless { val => shift }, $class; }
sub val { my $self = shift; return $self->{val}; }

package main;
my @items;
push @items, Item->new(10);
push @items, Item->new(20);
push @items, Item->new(30);
my $sum = 0;
foreach my $i (@items) {
    $sum = $sum + $i->val();
}
say $sum;
');

test_transpile("classes: objects in hash", '
package Person;
sub new { my $class = shift; bless { name => shift }, $class; }
sub name { my $self = shift; return $self->{name}; }

package main;
my %people;
$people{alice} = Person->new("Alice");
$people{bob} = Person->new("Bob");
say $people{alice}->name();
say $people{bob}->name();
');

# ============ COMPLEX CONTROL FLOW ============

test_transpile("complex: nested loops with last/next", '
my $count = 0;
foreach my $i (1, 2, 3) {
    foreach my $j (1, 2, 3, 4) {
        if ($j == 2) { next; }
        if ($j == 4) { last; }
        $count++;
    }
}
say $count;
');

test_transpile("complex: while with multiple conditions", '
my $x = 0;
my $y = 10;
while ($x < 5 && $y > 5) {
    $x++;
    $y--;
}
say "$x $y";
');

test_transpile("complex: chained method calls", '
package Builder;
sub new { my $class = shift; bless { parts => [] }, $class; }
sub add {
    my $self = shift;
    my $p = shift;
    push @{$self->{parts}}, $p;
    return $self;
}
sub build {
    my $self = shift;
    return join("-", @{$self->{parts}});
}

package main;
my $b = Builder->new();
$b->add("A");
$b->add("B");
$b->add("C");
say $b->build();
');

test_transpile("complex: recursive function", '
sub fib {
    my $n = shift;
    if ($n <= 1) { return $n; }
    return fib($n - 1) + fib($n - 2);
}
say fib(10);
');

test_transpile("complex: mutual recursion", '
sub is_even {
    my $n = shift;
    if ($n == 0) { return 1; }
    return is_odd($n - 1);
}
sub is_odd {
    my $n = shift;
    if ($n == 0) { return 0; }
    return is_even($n - 1);
}
say is_even(10);
say is_odd(7);
');

# ============ DATA STRUCTURES ============

test_transpile("data: array of arrays", '
my @matrix;
push @matrix, [1, 2, 3];
push @matrix, [4, 5, 6];
my $v = $matrix[1]->[2];
say $v;
');

test_transpile("data: hash of hashes", '
my %data;
$data{user1} = { name => "Alice", age => 30 };
$data{user2} = { name => "Bob", age => 25 };
say $data{user1}->{name};
say $data{user2}->{age};
');

test_transpile("data: mixed nested structure", '
my $data = {
    users => [
        { name => "Alice" },
        { name => "Bob" }
    ],
    count => 2
};
say $data->{users}->[0]->{name};
say $data->{count};
');

# ============ STRING OPERATIONS ============

test_transpile("string: uc and lc", '
my $s = "Hello World";
say uc($s);
say lc($s);
');

test_transpile("string: substr", '
my $s = "Hello World";
say substr($s, 0, 5);
say substr($s, 6);
');

test_transpile("string: index", '
my $s = "Hello World";
say index($s, "o");
say index($s, "o", 5);
');

test_transpile("string: concatenation chain", '
my $a = "Hello";
my $b = " ";
my $c = "World";
my $d = "!";
my $result = $a . $b . $c . $d;
say $result;
');

test_transpile("string: repetition", '
my $s = "ab";
my $r = $s x 4;
say $r;
');

# ============ NUMERIC OPERATIONS ============

test_transpile("numeric: int and abs", '
say int(3.7);
say int(-3.7);
say abs(-42);
');

test_transpile("numeric: modulo", '
say 17 % 5;
say -17 % 5;
');

test_transpile("numeric: compound assignment", '
my $x = 10;
$x += 5;
say $x;
$x -= 3;
say $x;
$x *= 2;
say $x;
');

done_testing();
