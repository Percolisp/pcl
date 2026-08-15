#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# Tests for magic variables and related functions

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

# ============================================
# fc (fold case) function
# ============================================

# Test 1: Basic fc
{
        my $output = get_generated_code('my $x = fc("HELLO");');
    like($output, qr/p-fc.*"HELLO"/, 'fc("HELLO") generates p-fc');
}

# Test 2: fc with variable
{
        my $output = get_generated_code('my $s = "Test"; my $x = fc($s);');
    like($output, qr/p-fc.*\$s/, 'fc($s) generates p-fc with variable');
}

# Test 3: fc with $_
{
        my $output = get_generated_code('$_ = "TEST"; my $x = fc;');
    like($output, qr/p-fc.*\$_/, 'fc defaults to $_');
}

# ============================================
# $! in string interpolation
# ============================================

# Test 4: $! in double-quoted string
{
        my $output = get_generated_code('my $msg = "Error: $!";');
    like($output, qr/p-errno-string/, '$! interpolates in strings');
}

# Test 5: $! standalone
{
        my $output = get_generated_code('my $e = $!;');
    like($output, qr/p-errno-string/, '$! standalone generates p-errno-string');
}

# ============================================
# $$ in string interpolation (already worked, verify)
# ============================================

# Test 6: $$ in double-quoted string
{
        my $output = get_generated_code('my $msg = "PID: $$";');
    like($output, qr/\$\$/, '$$ interpolates in strings');
}

# ============================================
# Other magic variables in interpolation
# ============================================

# Test 7: $? in string
{
        my $output = get_generated_code('my $msg = "Exit: $?";');
    like($output, qr/\$\?/, '$? interpolates in strings');
}

# Test 8: $0 in string
{
        my $output = get_generated_code('my $msg = "Program: $0";');
    like($output, qr/\$0/, '$0 interpolates in strings');
}

# Test 9: $. (line number) in string
{
        my $output = get_generated_code('my $msg = "Line: $.";');
    like($output, qr/\|\$\.\|/, '$. interpolates in strings');
}

# Test 10: $@ in string
{
        my $output = get_generated_code('my $msg = "Error: $@";');
    like($output, qr/\$\@/, '$@ interpolates in strings');
}

# ============================================
# Caret variables
# ============================================

# Test 11: $^O (OS name) standalone
{
        my $output = get_generated_code('my $os = $^O;');
    like($output, qr/\|\$\^O\|/, '$^O generates pipe-quoted symbol');
}

# Test 12: $^O in string interpolation
{
        my $output = get_generated_code('my $msg = "OS: $^O";');
    like($output, qr/\|\$\^O\|/, '$^O interpolates in strings');
}

# Test 13: $^V (Perl version) standalone
{
        my $output = get_generated_code('my $v = $^V;');
    like($output, qr/\|\$\^V\|/, '$^V generates pipe-quoted symbol');
}

# ============================================
# caller() function
# ============================================

# Test 14: caller() with no args
{
        my $output = get_generated_code('my $pkg = caller();');
    like($output, qr/p-caller/, 'caller() generates p-caller');
}

# Test 15: caller() with level argument
{
        my $output = get_generated_code('my $pkg = caller(1);');
    like($output, qr/p-caller\s+1/, 'caller(1) generates p-caller with argument');
}

# Test 16: caller() in list context
{
        my $output = get_generated_code('my ($pkg, $file, $line) = caller();');
    like($output, qr/p-caller/, 'caller() in list context');
}

# ============================================
# Record separators
# ============================================

# Test 17: $/ (input record separator) standalone
{
        my $output = get_generated_code('my $sep = $/;');
    like($output, qr/\|\$\/\|/, '$/ generates pipe-quoted symbol');
}

# Test 18: $\ (output record separator) standalone
{
        my $output = get_generated_code('my $sep = $\\;');
    # Must be |$\\| (backslash escaped inside the |...| symbol).  The old buggy
    # form |$\| escapes the closing pipe -> an unreadable symbol that truncates
    # the whole file at the CL reader.  Must match the runtime's (defvar |$\\|).
    like($output, qr/\|\$\\\\\|/, '$\\ generates a correctly-escaped pipe-quoted symbol');
}

# ============================================
# Multiple magic vars in one string
# ============================================

# Test 19: Multiple magic vars
{
        my $output = get_generated_code('my $msg = "PID $$ on $^O";');
    like($output, qr/\$\$.*\|\$\^O\|/s, 'Multiple magic vars interpolate');
}

# Test 20: Magic var with regular var
{
        my $output = get_generated_code('my $name = "test"; my $msg = "Process $$ running $name";');
    like($output, qr/\$\$.*\$name/s, 'Magic var with regular var interpolate');
}

# ============================================
# Edge cases
# ============================================

# Test 21: $" (list separator) - tricky in double-quoted string
{
        my $output = get_generated_code('my $sep = $";');
    like($output, qr/\|\$"\|/, '$" generates pipe-quoted symbol');
}

# Test 22: $' (postmatch) standalone
{
    my $output = get_generated_code(q{my $x = $';});
    # $' should be treated as a magic variable
    ok(defined $output, q{$' parses without error});
}

# Test 23: $& (match) standalone
{
        my $output = get_generated_code('my $x = $&;');
    ok(defined $output, '$& parses without error');
}

# Test 24: $+ (last bracket) standalone
{
        my $output = get_generated_code('my $x = $+;');
    ok(defined $output, '$+ parses without error');
}

# Test 25: Using $! in die message
{
        my $output = get_generated_code('open(my $fh, "<", "file") or die "Cannot open: $!";');
    like($output, qr/p-errno-string/, '$! in die message interpolates');
}

done_testing(25);
