#!/usr/bin/env perl
#          -*-Mode: CPerl -*-

# Test `use constant` parsing and usage
# Constants are implemented as zero-arg functions (like Perl does internally)

use v5.30;
use strict;
use warnings;

use lib ".";

use Test::More tests => 17;
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


# ========================================
diag "";
diag "-------- Single constant declaration:";

output_contains('use constant PI => 3.14159;',
                '(pl-sub pl-PI () 3.14159)',
                'Single constant: pl-sub generated');

output_contains('use constant NAME => "hello";',
                '(pl-sub pl-NAME () "hello")',
                'String constant');

output_contains('use constant TWO_PI => 2 * 3.14159;',
                '(pl-sub pl-TWO_PI () (pl-* 2 3.14159))',
                'Expression constant');


# ========================================
diag "";
diag "-------- Hash-style constant declaration:";

{
    my $result = parse_code('use constant { A => 1, B => 2 };');
    like($result, qr/\(pl-sub pl-A \(\) 1\)/, 'Hash-style: A defined');
    like($result, qr/\(pl-sub pl-B \(\) 2\)/, 'Hash-style: B defined');
}

{
    my $result = parse_code('use constant { WIDTH => 100, HEIGHT => 200, DEPTH => 50 };');
    like($result, qr/\(pl-sub pl-WIDTH \(\) 100\)/, 'Hash-style: WIDTH defined');
    like($result, qr/\(pl-sub pl-HEIGHT \(\) 200\)/, 'Hash-style: HEIGHT defined');
    like($result, qr/\(pl-sub pl-DEPTH \(\) 50\)/, 'Hash-style: DEPTH defined');
}


# ========================================
diag "";
diag "-------- Constant usage in expressions:";

# Top-level my now uses eval-when for declaration, box-set for initialization
output_contains('use constant PI => 3.14159;
my $x = PI;',
                '(box-set $x (pl-PI))',
                'Constant in assignment');

output_contains('use constant PI => 3.14;
my $area = PI * $r * $r;',
                '(box-set $area (pl-* (pl-* (pl-PI) $r) $r))',
                'Constant in arithmetic');

output_contains('use constant { WIDTH => 100, HEIGHT => 200 };
my $size = WIDTH * HEIGHT;',
                '(box-set $size (pl-* (pl-WIDTH) (pl-HEIGHT)))',
                'Multiple constants in expression');


# ========================================
diag "";
diag "-------- Environment integration (prototype tracking):";

{
    my $parser = Pl::Parser->new(code => 'use constant PI => 3.14159;');
    $parser->parse;

    my $env = $parser->environment;
    ok($env->has_prototype('PI'), 'PI registered as prototype');
    my $sig = $env->get_prototype('PI');
    is($sig->{min_params}, 0, 'PI has min_params = 0');
}

{
    my $parser = Pl::Parser->new(code => 'use constant { A => 1, B => 2 };');
    $parser->parse;

    my $env = $parser->environment;
    ok($env->has_prototype('A'), 'A registered as prototype');
    ok($env->has_prototype('B'), 'B registered as prototype');
}


done_testing();
