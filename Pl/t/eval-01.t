#!/usr/bin/env perl
#          -*-Mode: CPerl -*-

# Test eval { } block handling and $@ error variable

use v5.30;
use strict;
use warnings;

use lib ".";

use Test::More tests => 12;
BEGIN { use_ok('Pl::Parser') };
BEGIN { use_ok('Pl::Environment') };


# Helper: parse code and return generated CL
sub parse_code {
    my $code = shift;
    my $parser = Pl::Parser->new(code => $code);
    return $parser->parse;
}


# Helper: check if output contains expected string
sub output_contains {
    my $code     = shift;
    my $expected = shift;
    my $desc     = shift // "contains: $expected";

    my $result = parse_code($code);
    like($result, qr/\Q$expected\E/, $desc);
}


# Helper: check if output matches a regex
sub output_matches {
    my $code     = shift;
    my $pattern  = shift;
    my $desc     = shift // "matches pattern";

    my $result = parse_code($code);
    like($result, $pattern, $desc);
}


# ========================================
diag "";
diag "-------- Basic eval { } block:";

output_contains('eval { 1 };',
                'p-eval-block',
                'eval block generates p-eval-block');

output_contains('eval { die "oops" };',
                'p-eval-block',
                'eval with die generates p-eval-block');

output_contains('eval { die "oops" };',
                'p-die',
                'die inside eval block');


# ========================================
diag "";
diag "-------- $@ error variable:";

output_contains('print $@;',
                '(p-print $@)',
                '$@ used as variable');

output_contains('if ($@) { print "error" }',
                '$@',
                '$@ in condition');


# ========================================
diag "";
diag "-------- eval { } return value:";

output_contains('my $result = eval { 42 };',
                'p-eval-block',
                'eval block in assignment');


# ========================================
diag "";
diag "-------- eval { } with multiple statements:";

# Multi-statement blocks are inlined directly in p-eval-block (no funcall)
output_matches('eval { my $x = 1; $x + 1 };',
               qr/p-eval-block/s,
               'eval with multiple statements generates p-eval-block');


# ========================================
diag "";
diag "-------- Nested eval blocks:";

output_matches('eval { eval { die "inner" }; print $@ };',
               qr/p-eval-block.*p-eval-block/s,
               'nested eval blocks');


# ========================================
diag "";
diag "-------- eval with exception objects:";

output_contains('eval { die $exception };',
                '(p-die $exception)',
                'die with variable exception object');


# ========================================
diag "";
diag "-------- eval combined with control flow:";

output_matches('if (eval { dangerous() }) { ok() }',
               qr/p-if.*p-eval-block/s,
               'eval in condition');
