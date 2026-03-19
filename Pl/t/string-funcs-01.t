#!/usr/bin/env perl

use v5.30;
use strict;
use warnings;

use Test::More;
use lib '.';
use Pl::Parser;

# Helper to get just the generated code (skip preamble)
sub get_generated_code {
    my $parser = shift;
    my @output = $parser->parse();
    my $text = join("\n", @output);
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
    my $parser = Pl::Parser->new(code => 'my $x = lcfirst("HELLO");');
    my $output = get_generated_code($parser);
    like($output, qr/p-lcfirst.*"HELLO"/, 'lcfirst("HELLO") generates p-lcfirst');
}

# Test 2: lcfirst with variable
{
    my $parser = Pl::Parser->new(code => 'my $s = "World"; my $x = lcfirst($s);');
    my $output = get_generated_code($parser);
    like($output, qr/p-lcfirst.*\$s/, 'lcfirst($s) generates p-lcfirst with variable');
}

# Test 3: lcfirst with $_
{
    my $parser = Pl::Parser->new(code => '$_ = "TEST"; my $x = lcfirst;');
    my $output = get_generated_code($parser);
    like($output, qr/p-lcfirst.*\$_/, 'lcfirst defaults to $_');
}

# ============================================
# ucfirst - uppercase first character
# ============================================

# Test 4: Basic ucfirst
{
    my $parser = Pl::Parser->new(code => 'my $x = ucfirst("hello");');
    my $output = get_generated_code($parser);
    like($output, qr/p-ucfirst.*"hello"/, 'ucfirst("hello") generates p-ucfirst');
}

# Test 5: ucfirst with variable
{
    my $parser = Pl::Parser->new(code => 'my $s = "world"; my $x = ucfirst($s);');
    my $output = get_generated_code($parser);
    like($output, qr/p-ucfirst.*\$s/, 'ucfirst($s) generates p-ucfirst with variable');
}

# Test 6: ucfirst with $_
{
    my $parser = Pl::Parser->new(code => '$_ = "test"; my $x = ucfirst;');
    my $output = get_generated_code($parser);
    like($output, qr/p-ucfirst.*\$_/, 'ucfirst defaults to $_');
}

# ============================================
# chop - remove last character (modifies in place)
# ============================================

# Test 7: Basic chop
{
    my $parser = Pl::Parser->new(code => 'my $s = "hello"; my $c = chop($s);');
    my $output = get_generated_code($parser);
    like($output, qr/p-chop.*\$s/, 'chop($s) generates p-chop');
}

# Test 8: chop with $_
{
    my $parser = Pl::Parser->new(code => '$_ = "test"; chop;');
    my $output = get_generated_code($parser);
    like($output, qr/p-chop.*\$_/, 'chop defaults to $_');
}

# Test 9: chop return value (removed char)
{
    my $parser = Pl::Parser->new(code => 'my $s = "abc"; my $last = chop($s);');
    my $output = get_generated_code($parser);
    like($output, qr/p-chop/, 'chop returns removed character');
}

# ============================================
# quotemeta - escape regex metacharacters
# ============================================

# Test 10: Basic quotemeta
{
    my $parser = Pl::Parser->new(code => 'my $x = quotemeta("a.b*c");');
    my $output = get_generated_code($parser);
    like($output, qr/p-quotemeta.*"a\.b\*c"/, 'quotemeta generates p-quotemeta');
}

# Test 11: quotemeta with variable
{
    my $parser = Pl::Parser->new(code => 'my $pat = "foo[bar]"; my $x = quotemeta($pat);');
    my $output = get_generated_code($parser);
    like($output, qr/p-quotemeta.*\$pat/, 'quotemeta($pat) with variable');
}

# Test 12: quotemeta with $_
{
    my $parser = Pl::Parser->new(code => '$_ = "test+"; my $x = quotemeta;');
    my $output = get_generated_code($parser);
    like($output, qr/p-quotemeta.*\$_/, 'quotemeta defaults to $_');
}

# Test 13: quotemeta in regex context
{
    my $parser = Pl::Parser->new(code => 'my $lit = quotemeta("$var"); $s =~ /$lit/;');
    my $output = get_generated_code($parser);
    like($output, qr/p-quotemeta/, 'quotemeta used in regex pattern');
}

# ============================================
# pos - get/set match position
# ============================================

# Test 14: pos getter
{
    my $parser = Pl::Parser->new(code => 'my $p = pos($str);');
    my $output = get_generated_code($parser);
    like($output, qr/p-pos.*\$str/, 'pos($str) generates p-pos');
}

# Test 15: pos with no args
{
    my $parser = Pl::Parser->new(code => 'my $p = pos();');
    my $output = get_generated_code($parser);
    like($output, qr/p-pos/, 'pos() generates p-pos');
}

# ============================================
# Combined tests - using functions together
# ============================================

# Test 16: ucfirst with lc
{
    my $parser = Pl::Parser->new(code => 'my $x = ucfirst(lc("HELLO WORLD"));');
    my $output = get_generated_code($parser);
    like($output, qr/p-ucfirst.*p-lc/, 'ucfirst(lc(...)) nests correctly');
}

# Test 17: quotemeta in substitution
{
    my $parser = Pl::Parser->new(code => 'my $lit = "foo.bar"; $s =~ s/\Q$lit\E/baz/;');
    my $output = get_generated_code($parser);
    # \Q...\E should use quotemeta or escape the pattern
    like($output, qr/p-subst/, '\Q...\E in substitution');
}

# Test 18: chop in loop
{
    my $parser = Pl::Parser->new(code => 'while (length($s) > 0) { chop($s); }');
    my $output = get_generated_code($parser);
    like($output, qr/p-while.*p-chop/s, 'chop in while loop');
}

# Test 19: lcfirst/ucfirst on empty string
{
    my $parser = Pl::Parser->new(code => 'my $x = lcfirst(""); my $y = ucfirst("");');
    my $output = get_generated_code($parser);
    like($output, qr/p-lcfirst.*"".*p-ucfirst.*""/s, 'lcfirst/ucfirst on empty strings');
}

# Test 20: chop on single character
{
    my $parser = Pl::Parser->new(code => 'my $s = "x"; my $c = chop($s);');
    my $output = get_generated_code($parser);
    like($output, qr/p-chop/, 'chop on single character string');
}

done_testing(20);
