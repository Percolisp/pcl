#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

#          -*-Mode: CPerl -*-

# Test `use constant` parsing and usage
# Constants are implemented as zero-arg functions (like Perl does internally)

use v5.30;
use strict;
use warnings;

use lib ".";

use Test::More tests => 22;
BEGIN { use_ok('Pl::Parser2') };
BEGIN { use_ok('Pl::Environment') };


# Helper: parse code and return generated CL
sub parse_code {
    my $code = shift;
        return Pl::Parser2->parse_code($code);
}


# Helper: check if output contains expected string.  v2 pretty-prints long
# forms across lines, so any whitespace run in EXPECTED matches any
# whitespace run in the output (the assertion is about the FORM, not the
# line breaks).
sub output_contains {
    my $code     = shift;
    my $expected = shift;
    my $desc     = shift // "contains: $expected";

    my $result = parse_code($code);
    my $rx = join '\s+', map { quotemeta } split /\s+/, $expected;
    like($result, qr/$rx/, $desc);
}


# ========================================
diag "";
diag "-------- Single constant declaration:";
# Every constant sub carries `:prototype ""` in its facts slot since s500a
# (#2533): perl's prototype(\&CONST) is '', and the p-sub macro registers it.

output_contains('use constant PI => 3.14159;',
                '(p-sub pl-PI (&rest %_args) (:prototype "") (progn %_args 3.14159))',
                'Single constant: p-sub generated');

output_contains('use constant NAME => "hello";',
                '(p-sub pl-NAME (&rest %_args) (:prototype "") (progn %_args "hello"))',
                'String constant');

# A NON-literal value is evaluated ONCE, at the `use`, into a cell the sub
# answers from (s508a, #2681): the value is no longer the sub's BODY, which ran
# it at every use.  A literal (the two rows above) keeps the plain body.
output_contains('use constant TWO_PI => 2 * 3.14159;',
                '(p-use-constant pl-TWO_PI (p-* 2 3.14159))',
                'Expression constant: evaluated once (p-use-constant)');


# ========================================
diag "";
diag "-------- Hash-style constant declaration:";

{
    my $result = parse_code('use constant { A => 1, B => 2 };');
    like($result, qr/\(p-sub pl-A \(&rest %_args\) \(:prototype ""\) \(progn %_args 1\)/, 'Hash-style: A defined');
    like($result, qr/\(p-sub pl-B \(&rest %_args\) \(:prototype ""\) \(progn %_args 2\)/, 'Hash-style: B defined');
}

{
    my $result = parse_code('use constant { WIDTH => 100, HEIGHT => 200, DEPTH => 50 };');
    like($result, qr/\(p-sub pl-WIDTH \(&rest %_args\) \(:prototype ""\) \(progn %_args 100\)/, 'Hash-style: WIDTH defined');
    like($result, qr/\(p-sub pl-HEIGHT \(&rest %_args\) \(:prototype ""\) \(progn %_args 200\)/, 'Hash-style: HEIGHT defined');
    like($result, qr/\(p-sub pl-DEPTH \(&rest %_args\) \(:prototype ""\) \(progn %_args 50\)/, 'Hash-style: DEPTH defined');
}

# s508a (#2681): a hash-form NON-literal value is evaluated once too, and is ONE
# scalar (the trailing t); its literal sibling keeps the plain body.
{
    my $result = parse_code('use constant { CFG => { a => 1 }, LIM => 9 };');
    like($result, qr/\(p-use-constant pl-CFG \(make-p-box \(p-hash "a" 1\)\) t\)/,
         'Hash-style: a non-literal value is evaluated once, as one scalar');
    like($result, qr/\(p-sub pl-LIM \(&rest %_args\) \(:prototype ""\) \(progn %_args 9\)/,
         'Hash-style: its literal sibling keeps the plain body');
}


# ========================================
diag "";
diag "-------- Constant usage in expressions:";

# The constant use compiles to a scalar-context call of the constant sub
# (pl-PI) inside the my-init assignment.  (v1 spelled the binding box-set;
# v2 spells the same runtime write p-my-= inside the fresh let.)
output_contains('use constant PI => 3.14159;
my $x = PI;',
                '(p-my-= $x (p-scalar-ctx (pl-PI)))',
                'Constant in assignment');

# These two used to spell the write `p-my-=` into a boxed slot.  Since #759
# (Kind-A `raw-op-family`, s456af) the ARITH ROOT proves the stored value is a
# raw CL number whatever its operands are, so the declaration takes the raw
# let-init instead — a strictly stronger assertion, because it pins the
# constant's lowering AND the operand tree AND the slot's representation.
# What these rows are really about is unchanged and still asserted: a constant
# use compiles to a scalar-context call of the constant sub.
output_contains('use constant PI => 3.14;
my $area = PI * $r * $r;',
                '(p-let (($area :scalar (p-* (p-* (p-scalar-ctx (pl-PI)) $r) $r))))',
                'Constant in arithmetic');

output_contains('use constant { WIDTH => 100, HEIGHT => 200 };
my $size = WIDTH * HEIGHT;',
                '(p-let (($size :scalar (p-* (p-scalar-ctx (pl-WIDTH)) (p-scalar-ctx (pl-HEIGHT))))))',
                'Multiple constants in expression');


# ========================================
diag "";
diag "-------- Constant after print is a list element, not a filehandle:";

# `print FOO, ...` — a comma right after the ALL-CAPS bareword means FOO is a
# list element (here a constant), NOT a filehandle.  Regression: PCL used to
# swallow FOO as a filehandle and print nothing for it.
{
    my $cl = parse_code('use constant FOO => 1; print FOO, "x";');
    like($cl, qr/\(p-print \(p-list-ctx \(pl-FOO\)\) "x"\)/,
         'print FOO, ... treats FOO as a list element (constant), not a filehandle');
    unlike($cl, qr/p-print :fh.*pl-FOO/,
           'print FOO, ... does NOT route FOO as a filehandle');
}

# A bareword filehandle with NO comma is still a filehandle: `print STDERR LIST`.
{
    my $cl = parse_code('print STDERR "x";');
    like($cl, qr/\(p-print :fh 'STDERR "x"\)/,
         'print STDERR LIST still routes STDERR as a filehandle');
}


# ========================================
diag "";
diag "-------- Environment integration (prototype tracking):";

{
    my $parser = Pl::Parser2->new(code => 'use constant PI => 3.14159;');
    $parser->parse;

    my $env = $parser->environment;
    ok($env->has_prototype('PI'), 'PI registered as prototype');
    my $sig = $env->get_prototype('PI');
    is($sig->{min_params}, 0, 'PI has min_params = 0');
}

{
    my $parser = Pl::Parser2->new(code => 'use constant { A => 1, B => 2 };');
    $parser->parse;

    my $env = $parser->environment;
    ok($env->has_prototype('A'), 'A registered as prototype');
    ok($env->has_prototype('B'), 'B registered as prototype');
}


done_testing();
