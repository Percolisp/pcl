#!/usr/bin/env perl
#          -*-Mode: CPerl -*-

# Test bless, ref, and package handling

use v5.30;
use strict;
use warnings;

use lib ".";

use Test::More tests => 21;
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
diag "-------- Basic bless:";

output_contains('bless({}, "MyClass");',
                '(pl-bless',
                'bless recognized as function');

output_contains('my $obj = bless {}, "MyClass";',
                '(pcl:pl-setf $obj (pl-bless',
                'bless in assignment');

output_contains('bless $ref, "Class";',
                '(pl-bless $ref "Class")',
                'bless with variable');


# ========================================
diag "";
diag "-------- ref function:";

output_contains('ref($obj);',
                '(pl-ref $obj)',
                'ref function');

output_contains('my $type = ref($x);',
                '(pcl:pl-setf $type (pl-ref $x))',
                'ref in assignment');


# ========================================
diag "";
diag "-------- Constructor pattern:";

output_contains('sub new { bless {}, shift; }',
                '(defun pl-new',
                'Constructor generates defun');

output_contains('sub new { bless {}, shift; }',
                '(pl-bless (pl-hash ) (pl-shift @_))',
                'Constructor bless pattern');


# ========================================
diag "";
diag "-------- Package declaration:";

{
    my $result = parse_code('package MyClass;');
    like($result, qr/;;; package MyClass/, 'Simple package declaration');
}

{
    my $result = parse_code('package MyClass { sub new {} }');
    like($result, qr/;;; package MyClass/, 'Block package start');
    like($result, qr/;;; end package MyClass/, 'Block package end');
}


# ========================================
diag "";
diag "-------- Package stack (environment):";

{
    my $env = Pl::Environment->new();
    is($env->current_package, 'main', 'Default package is main');

    $env->push_package('Foo');
    is($env->current_package, 'Foo', 'After push, current is Foo');

    $env->push_package('Bar');
    is($env->current_package, 'Bar', 'After second push, current is Bar');

    $env->pop_package();
    is($env->current_package, 'Foo', 'After pop, back to Foo');

    $env->pop_package();
    is($env->current_package, 'main', 'After pop, back to main');

    $env->pop_package();
    is($env->current_package, 'main', 'Cannot pop below main');
}


# ========================================
diag "";
diag "-------- Full class example:";

{
    my $code = q{
package MyClass {
    sub new {
        my $class = shift;
        bless {}, $class;
    }
}
};
    my $result = parse_code($code);
    like($result, qr/;;; package MyClass/, 'Full class: package start');
    like($result, qr/\(defun pl-new/, 'Full class: constructor defined');
    like($result, qr/;;; end package MyClass/, 'Full class: package end');
}


done_testing();
