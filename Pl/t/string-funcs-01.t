#!/usr/bin/env perl

use v5.30;
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
# lcfirst - lowercase first character
# ============================================

# Test 1: Basic lcfirst
{
        my $output = get_generated_code('my $x = lcfirst("HELLO");');
    like($output, qr/p-lcfirst.*"HELLO"/, 'lcfirst("HELLO") generates p-lcfirst');
}

# Test 2: lcfirst with variable
{
        my $output = get_generated_code('my $s = "World"; my $x = lcfirst($s);');
    like($output, qr/p-lcfirst.*\$s/, 'lcfirst($s) generates p-lcfirst with variable');
}

# Test 3: lcfirst with $_
{
        my $output = get_generated_code('$_ = "TEST"; my $x = lcfirst;');
    like($output, qr/p-lcfirst.*\$_/, 'lcfirst defaults to $_');
}

# ============================================
# ucfirst - uppercase first character
# ============================================

# Test 4: Basic ucfirst
{
        my $output = get_generated_code('my $x = ucfirst("hello");');
    like($output, qr/p-ucfirst.*"hello"/, 'ucfirst("hello") generates p-ucfirst');
}

# Test 5: ucfirst with variable
{
        my $output = get_generated_code('my $s = "world"; my $x = ucfirst($s);');
    like($output, qr/p-ucfirst.*\$s/, 'ucfirst($s) generates p-ucfirst with variable');
}

# Test 6: ucfirst with $_
{
        my $output = get_generated_code('$_ = "test"; my $x = ucfirst;');
    like($output, qr/p-ucfirst.*\$_/, 'ucfirst defaults to $_');
}

# ============================================
# chop - remove last character (modifies in place)
# ============================================

# Test 7: Basic chop
{
        my $output = get_generated_code('my $s = "hello"; my $c = chop($s);');
    like($output, qr/p-chop.*\$s/, 'chop($s) generates p-chop');
}

# Test 8: chop with $_
{
        my $output = get_generated_code('$_ = "test"; chop;');
    like($output, qr/p-chop.*\$_/, 'chop defaults to $_');
}

# Test 9: chop return value (removed char)
{
        my $output = get_generated_code('my $s = "abc"; my $last = chop($s);');
    like($output, qr/p-chop/, 'chop returns removed character');
}

# ============================================
# quotemeta - escape regex metacharacters
# ============================================

# Test 10: Basic quotemeta
{
        my $output = get_generated_code('my $x = quotemeta("a.b*c");');
    like($output, qr/p-quotemeta.*"a\.b\*c"/, 'quotemeta generates p-quotemeta');
}

# Test 11: quotemeta with variable
{
        my $output = get_generated_code('my $pat = "foo[bar]"; my $x = quotemeta($pat);');
    like($output, qr/p-quotemeta.*\$pat/, 'quotemeta($pat) with variable');
}

# Test 12: quotemeta with $_
{
        my $output = get_generated_code('$_ = "test+"; my $x = quotemeta;');
    like($output, qr/p-quotemeta.*\$_/, 'quotemeta defaults to $_');
}

# Test 13: quotemeta in regex context
{
        my $output = get_generated_code('my $lit = quotemeta("$var"); $s =~ /$lit/;');
    like($output, qr/p-quotemeta/, 'quotemeta used in regex pattern');
}

# ============================================
# pos - get/set match position
# ============================================

# Test 14: pos getter
{
        my $output = get_generated_code('my $p = pos($str);');
    like($output, qr/p-pos.*\$str/, 'pos($str) generates p-pos');
}

# Test 15: pos with no args
{
        my $output = get_generated_code('my $p = pos();');
    like($output, qr/p-pos/, 'pos() generates p-pos');
}

# ============================================
# Combined tests - using functions together
# ============================================

# Test 16: ucfirst with lc
{
        my $output = get_generated_code('my $x = ucfirst(lc("HELLO WORLD"));');
    like($output, qr/p-ucfirst.*p-lc/, 'ucfirst(lc(...)) nests correctly');
}

# Test 17: quotemeta in substitution
{
        my $output = get_generated_code('my $lit = "foo.bar"; $s =~ s/\Q$lit\E/baz/;');
    # \Q...\E should use quotemeta or escape the pattern
    like($output, qr/p-subst/, '\Q...\E in substitution');
}

# Test 18: chop in loop
{
        my $output = get_generated_code('while (length($s) > 0) { chop($s); }');
    like($output, qr/p-while.*p-chop/s, 'chop in while loop');
}

# Test 19: lcfirst/ucfirst on empty string
{
        my $output = get_generated_code('my $x = lcfirst(""); my $y = ucfirst("");');
    like($output, qr/p-lcfirst.*"".*p-ucfirst.*""/s, 'lcfirst/ucfirst on empty strings');
}

# Test 20: chop on single character
{
        my $output = get_generated_code('my $s = "x"; my $c = chop($s);');
    like($output, qr/p-chop/, 'chop on single character string');
}

done_testing(20);
