#!/usr/bin/env perl
# closure-01.t: Phase 2 closure tests — lexical my-var renaming inside subs

use v5.30;
use strict;
use warnings;
use Test::More;
use File::Temp qw(tempfile);
use FindBin qw($RealBin);

my $project_root = "$RealBin/../..";
my $pl2cl        = "$project_root/pl2cl";
my $runtime      = "$project_root/cl/pcl-runtime.lisp";

plan skip_all => "pl2cl not found" unless -x $pl2cl;
plan skip_all => "sbcl not found"  unless `which sbcl 2>/dev/null`;

sub run_cl {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;

    my $cl_code = `$pl2cl $pl_file 2>&1`;

    my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cl_fh $cl_code;
    close $cl_fh;

    my $out = `sbcl --noinform --non-interactive --load $runtime --load $cl_file 2>&1`;
    $out =~ s/^;.*\n//gm;
    $out =~ s/^caught .*\n//gm;
    $out =~ s/^compilation unit.*\n//gm;
    $out =~ s/^\s*Undefined.*\n//gm;
    $out =~ s/^-->.*\n//gm;
    $out =~ s/^==>.*\n//gm;
    $out =~ s/^PCL Runtime loaded\n//gm;
    $out =~ s/^\s*\n//gm;
    return $out;
}

sub transpile_cl {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;
    return `$pl2cl $pl_file 2>&1`;
}

sub test_io {
    my ($name, $code, $expected) = @_;
    my $out = run_cl($code);
    is($out, $expected, $name) or diag("Got: $out");
}

# ============ TEST 1: independent counters from same factory ============

test_io("make_counter: two independent counters", '
use feature "state";
sub make_counter {
    my $n = 0;
    return sub { $n++ };
}
my $c1 = make_counter();
my $c2 = make_counter();
print $c1->(), "\n";
print $c1->(), "\n";
print $c2->(), "\n";
', "0\n1\n0\n");

# ============ TEST 2: per-call capture (read-only) ============

test_io("bar closure: per-call capture", '
sub bar {
    my $i = shift;
    return sub { $i };
}
my $f4 = bar(4);
my $f5 = bar(5);
print $f4->(), "\n";
print $f5->(), "\n";
', "4\n5\n");

# ============ TEST 3: per-call capture (mutable) ============

test_io("bar closure: mutable per-call capture", '
sub bar {
    my $i = shift;
    return sub {
        if (@_) { $i = shift }
        $i
    };
}
my $f = bar(10);
print $f->(), "\n";
$f->(99);
print $f->(), "\n";
', "10\n99\n");

# ============ TEST 4: non-captured vars not renamed ============

{
    my $cl = transpile_cl('
sub count_it {
    my $total = 0;
    for (my $i = 0; $i < 3; $i++) {
        $total += $i;
    }
    return $total;
}
print count_it(), "\n";
');
    # $i is not captured by any closure — should not appear renamed
    ok($cl !~ /\$i__lex__/, "loop counter \$i not renamed when not captured");
    # $total IS captured... wait, no — $total is not in any sub{} block. Not renamed either.
    ok($cl !~ /\$total__lex__/, "non-captured \$total not renamed either");
}

# ============ TEST 5: list declaration ============

test_io("list decl: both vars captured", '
sub make_adder {
    my ($x, $y) = @_;
    return sub { $x + $y };
}
my $add = make_adder(3, 7);
print $add->(), "\n";
', "10\n");

# ============ TEST 6: package-level my vs sub-level my ============

test_io("package \$i independent of sub's \$i", '
my $i = 99;
sub bar {
    my $i = shift;
    return sub { $i };
}
my $f = bar(42);
print $f->(), "\n";
print $i, "\n";
', "42\n99\n");

# ============ TEST 7: my $i = $i + 1 (RHS sees outer $i) ============

test_io("my \$i = \$i + 1 shadows outer \$i correctly", '
my $i = 5;
sub bar {
    my $i = $i + 1;
    return sub { $i };
}
print bar()->(), "\n";
', "6\n");

# ============ TEST 9: two closures share the same captured var ============

test_io("shared getter/setter: mutation visible via both closures", '
sub make_pair {
    my $n = shift;
    my $get = sub { $n };
    my $set = sub { $n = shift };
    return ($get, $set);
}
my ($g, $s) = make_pair(10);
print $g->(), "\n";
$s->(99);
print $g->(), "\n";
', "10\n99\n");

# ============ TEST 10: doubly-nested closure ============

test_io("doubly-nested closure: sub { sub { \$i } } captures correctly", '
sub outer {
    my $i = shift;
    return sub { sub { $i } };
}
print outer(7)->()->(), "\n";
', "7\n");

# ============ TEST 11: captured array ============

test_io("captured array: push/pop via closures", '
sub make_stack {
    my @items;
    my $push_fn = sub { push @items, shift };
    my $pop_fn  = sub { pop @items };
    return ($push_fn, $pop_fn);
}
my ($push, $pop) = make_stack();
$push->(10); $push->(20); $push->(30);
print $pop->(), "\n";
print $pop->(), "\n";
', "30\n20\n");

# ============ TEST 12: bare declaration then assign then capture ============

test_io("bare my decl then assign: closure captures the assigned value", '
sub foo {
    my $x;
    $x = shift;
    return sub { $x };
}
print foo(42)->(), "\n";
', "42\n");

# ============ TEST 13: package-level foreach closure capture ============
# Regression test for the in_subroutine == 0 case: my $i inside a for loop
# at package level, captured by a closure, must get a fresh binding per iteration.

test_io("package-level foreach: each closure captures its own \$i", '
my @foo;
for (qw(0 1 2 3 4)) {
    my $i = $_;
    $foo[$_] = sub { $i };
}
print $foo[0]->(), "\n";
print $foo[2]->(), "\n";
print $foo[4]->(), "\n";
', "0\n2\n4\n");

test_io("package-level foreach: mutable captured \$i per iteration", '
my @foo;
for (qw(0 1 2 3 4)) {
    my $i = $_;
    $foo[$_] = sub { $i = shift if @_; $i };
}
# Mutate via each closure independently
$foo[0]->(40);
$foo[4]->(99);
print $foo[0]->(), "\n";
print $foo[4]->(), "\n";
print $foo[2]->(), "\n";
', "40\n99\n2\n");

done_testing();
