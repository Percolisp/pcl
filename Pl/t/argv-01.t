#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# Tests for @ARGV and shift/pop context handling

use strict;
use warnings;

use Test::More;
use lib '.';
use Pl::Parser2;

# Helper to get just the generated code (skip preamble)
sub get_generated_code {
    my $code = shift;
    my $text = Pl::Parser2->parse_code($code);
    my @lines = split /\n/, $text;
    my @code;
    for my $line (@lines) {
        next if $line =~ /^\(in-package/;
        next if $line =~ /^\(setf pcl::\*pcl-pl2cl-path/;
        next if $line =~ /^\(setf pcl::\@INC/;
        next if $line =~ /^\(make-array/;
        next if $line =~ /^\(vector-push-extend/;
        next if $line =~ /^$/;
        push @code, $line;
    }
    return join("\n", @code);
}

diag "";
diag "-------- @ARGV variable:";

# Test 1: @ARGV is recognized as a variable
{
        my $output = get_generated_code('my @args = @ARGV;');
    like($output, qr/\@ARGV/, '@ARGV is recognized');
}

# Test 2: Access @ARGV elements
{
        my $output = get_generated_code('my $first = $ARGV[0];');
    like($output, qr/\@ARGV/, '$ARGV[0] accesses @ARGV');
}

# Test 3: @ARGV in foreach
{
        my $output = get_generated_code('foreach my $arg (@ARGV) { print $arg; }');
    like($output, qr/\@ARGV/, '@ARGV in foreach loop');
}

diag "";
diag "-------- shift/pop at top level (should use \@ARGV):";

# Test 4: shift at top level defaults to @ARGV
{
        my $output = get_generated_code('my $arg = shift;');
    like($output, qr/p-shift\s+\@ARGV/, 'shift at top level uses @ARGV');
}

# Test 5: pop at top level defaults to @ARGV
{
        my $output = get_generated_code('my $last = pop;');
    like($output, qr/p-pop\s+\@ARGV/, 'pop at top level uses @ARGV');
}

# Test 6: Multiple shifts at top level
{
        my $output = get_generated_code('my $a = shift; my $b = shift;');
    my @matches = ($output =~ /p-shift\s+\@ARGV/g);
    is(scalar @matches, 2, 'Multiple shifts at top level all use @ARGV');
}

diag "";
diag "-------- shift/pop inside subs (should use \@_):";

# Test 7: shift inside sub defaults to @_.  v2 optimizes a LEADING
# `my $x = shift;` into the raw-params calling convention — the sub's
# arguments are consumed directly as the parameter binding (never @ARGV).
{
        my $output = get_generated_code('sub foo { my $x = shift; }');
    like($output, qr/p-raw-params \(\$x\)/, 'shift inside sub uses @_');
}

# Test 8: pop inside sub defaults to @_
{
        my $output = get_generated_code('sub foo { my $x = pop; }');
    like($output, qr/p-pop\s+\@_/, 'pop inside sub uses @_');
}

# Test 9: shift used in typical constructor pattern
{
        my $output = get_generated_code('sub new { my $class = shift; bless {}, $class; }');
    like($output, qr/p-raw-params \(\$class\)/, 'shift in constructor uses @_');
}

# Test 10: Nested subs both consume THEIR OWN @_ — inner gets raw-params;
# outer (whose body holds a nested named sub, which vetoes the params
# extraction) keeps the explicit (p-shift @_).  Neither touches @ARGV.
{
        my $output = get_generated_code('
        sub outer {
            my $a = shift;
            sub inner {
                my $b = shift;
            }
        }
    ');
    # $b is exception-partition, so #296 renames the lexical before the
    # params fast path sees it — the suffix pins that the RENAMED name still
    # collapses to p-raw-params.
    ok($output =~ /p-raw-params \(\$b__excl__\d+\)/ && $output =~ /p-shift\s+\@_/,
       'Both nested subs use @_');
}

diag "";
diag "-------- Mixed context:";

# Test 11: Top level shift, then sub with shift
{
        my $output = get_generated_code('
        my $file = shift;
        sub process { my $x = shift; }
    ');
    like($output, qr/p-shift\s+\@ARGV/, 'Top level shift uses @ARGV');
    like($output, qr/p-raw-params \(\$x\)/, 'Sub shift uses @_');
}

# Test 12: Explicit @ARGV in sub (should stay @ARGV)
{
        my $output = get_generated_code('sub foo { my $x = shift @ARGV; }');
    like($output, qr/p-shift\s+\@ARGV/, 'Explicit shift @ARGV in sub stays @ARGV');
}

# Test 13: Explicit @_ at top level (should stay @_)
{
        my $output = get_generated_code('my $x = shift @_;');
    like($output, qr/p-shift\s+\@_/, 'Explicit shift @_ at top level stays @_');
}

diag "";
diag "-------- Scalar @ARGV:";

# Test 14: scalar @ARGV for count
{
        my $output = get_generated_code('my $count = scalar @ARGV;');
    like($output, qr/\@ARGV/, 'scalar @ARGV works');
}

# Test 15: if (@ARGV) check
{
        my $output = get_generated_code('if (@ARGV) { print "has args"; }');
    like($output, qr/\@ARGV/, '@ARGV in condition');
}

done_testing(16);
