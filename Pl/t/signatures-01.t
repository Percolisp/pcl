#!/usr/bin/env perl
#          -*-Mode: CPerl -*-

# Test subroutine signature/prototype parsing

use v5.30;
use strict;
use warnings;

use lib ".";

use Data::Dump qw/dump/;

use Test::More tests => 34;
BEGIN { use_ok('Pl::Parser') };
BEGIN { use_ok('Pl::Environment') };


# Helper: parse a signature string
sub parse_sig {
    my $sig_str = shift;
    my $parser = Pl::Parser->new(code => "1;");
    return $parser->parse_prototype_or_signature($sig_str);
}


# ========================================
diag "";
diag "-------- Simple signatures:";

{
    my $sig = parse_sig('($x)');
    is($sig->{min_params}, 1, 'Single param: min_params = 1');
    is(scalar(@{$sig->{params}}), 1, 'Single param: 1 param');
    is($sig->{params}[0]{name}, '$x', 'Single param: name is $x');
    is($sig->{is_proto}, 0, 'Single param: is_proto = 0');
}

{
    my $sig = parse_sig('($a, $b, $c)');
    is($sig->{min_params}, 3, 'Three params: min_params = 3');
    is(scalar(@{$sig->{params}}), 3, 'Three params: 3 params');
}

{
    my $sig = parse_sig('()');
    is($sig->{min_params}, 0, 'Empty: min_params = 0');
    is(scalar(@{$sig->{params}}), 0, 'Empty: 0 params');
}


# ========================================
diag "";
diag "-------- Signatures with defaults:";

{
    my $sig = parse_sig('($x = 10)');
    is($sig->{min_params}, 0, 'All optional: min_params = 0');
    is($sig->{params}[0]{default_cl}, '10', 'Default compiled to CL');
}

{
    my $sig = parse_sig('($x, $y = 5)');
    is($sig->{min_params}, 1, 'One required, one optional: min_params = 1');
    ok(!defined $sig->{params}[0]{default_cl}, 'First param has no default');
    is($sig->{params}[1]{default_cl}, '5', 'Second param has default');
}

{
    my $sig = parse_sig('($x, $y = $x * 2)');
    is($sig->{min_params}, 1, 'Complex default: min_params = 1');
    is($sig->{params}[1]{default_cl}, '(pcl:pl-* $x 2)', 'Complex default compiled');
}


# ========================================
diag "";
diag "-------- Slurpy parameters:";

{
    my $sig = parse_sig('(@args)');
    is($sig->{min_params}, 0, 'Slurpy only: min_params = 0');
    is($sig->{params}[0]{name}, '@args', 'Slurpy name is @args');
}

{
    my $sig = parse_sig('($first, @rest)');
    is($sig->{min_params}, 1, 'Scalar + slurpy: min_params = 1');
    is(scalar(@{$sig->{params}}), 2, 'Two params total');
}

{
    my $sig = parse_sig('(%opts)');
    is($sig->{min_params}, 0, 'Hash slurpy: min_params = 0');
    is($sig->{params}[0]{name}, '%opts', 'Hash slurpy name');
}


# ========================================
diag "";
diag "-------- Old-style prototypes:";

{
    my $sig = parse_sig('($$)');
    is($sig->{is_proto}, 1, 'Old-style: is_proto = 1');
    is($sig->{min_params}, 2, 'Old-style $$: min_params = 2');
    is(scalar(@{$sig->{params}}), 2, 'Old-style $$: 2 params');
}

{
    my $sig = parse_sig('($;$)');
    is($sig->{is_proto}, 1, 'Semicolon proto: is_proto = 1');
    is($sig->{min_params}, 1, 'Semicolon proto: min_params = 1');
    is(scalar(@{$sig->{params}}), 2, 'Semicolon proto: 2 params total');
}

{
    my $sig = parse_sig('(\@$)');
    is($sig->{is_proto}, 1, 'Ref proto: is_proto = 1');
    is($sig->{params}[0]{name}, '\@', 'Array ref sigil');
    is($sig->{params}[1]{name}, '$', 'Scalar sigil');
}


# ========================================
diag "";
diag "-------- Environment integration:";

{
    my $parser = Pl::Parser->new(code => 'sub foo($x, $y) { 1 }');
    $parser->parse;

    my $env = $parser->environment;
    my $sig_info = $env->get_prototype('foo');
    ok(defined $sig_info, 'foo stored in environment');
    is($env->get_min_params('foo'), 2, 'foo has min_params = 2');
}

done_testing();
