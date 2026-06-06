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
                '(p-sub pl-PI (&rest %_args) (progn %_args 3.14159))',
                'Single constant: p-sub generated');

output_contains('use constant NAME => "hello";',
                '(p-sub pl-NAME (&rest %_args) (progn %_args "hello"))',
                'String constant');

output_contains('use constant TWO_PI => 2 * 3.14159;',
                '(p-sub pl-TWO_PI (&rest %_args) (progn %_args (p-* 2 3.14159)))',
                'Expression constant');


# ========================================
diag "";
diag "-------- Hash-style constant declaration:";

{
    my $result = parse_code('use constant { A => 1, B => 2 };');
    like($result, qr/\(p-sub pl-A \(&rest %_args\) \(progn %_args 1\)/, 'Hash-style: A defined');
    like($result, qr/\(p-sub pl-B \(&rest %_args\) \(progn %_args 2\)/, 'Hash-style: B defined');
}

{
    my $result = parse_code('use constant { WIDTH => 100, HEIGHT => 200, DEPTH => 50 };');
    like($result, qr/\(p-sub pl-WIDTH \(&rest %_args\) \(progn %_args 100\)/, 'Hash-style: WIDTH defined');
    like($result, qr/\(p-sub pl-HEIGHT \(&rest %_args\) \(progn %_args 200\)/, 'Hash-style: HEIGHT defined');
    like($result, qr/\(p-sub pl-DEPTH \(&rest %_args\) \(progn %_args 50\)/, 'Hash-style: DEPTH defined');
}


# ========================================
diag "";
diag "-------- Constant usage in expressions:";

# Top-level my now uses eval-when for declaration, box-set for initialization
output_contains('use constant PI => 3.14159;
my $x = PI;',
                '(box-set $x (let ((*wantarray* nil)) (pl-PI)))',
                'Constant in assignment');

output_contains('use constant PI => 3.14;
my $area = PI * $r * $r;',
                '(box-set $area (p-* (p-* (let ((*wantarray* nil)) (pl-PI)) $r) $r))',
                'Constant in arithmetic');

output_contains('use constant { WIDTH => 100, HEIGHT => 200 };
my $size = WIDTH * HEIGHT;',
                '(box-set $size (p-* (let ((*wantarray* nil)) (pl-WIDTH)) (let ((*wantarray* nil)) (pl-HEIGHT))))',
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
