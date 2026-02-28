#!/usr/bin/env perl
# Tests for magic variables and related functions

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
# fc (fold case) function
# ============================================

# Test 1: Basic fc
{
    my $parser = Pl::Parser->new(code => 'my $x = fc("HELLO");');
    my $output = get_generated_code($parser);
    like($output, qr/pl-fc.*"HELLO"/, 'fc("HELLO") generates pl-fc');
}

# Test 2: fc with variable
{
    my $parser = Pl::Parser->new(code => 'my $s = "Test"; my $x = fc($s);');
    my $output = get_generated_code($parser);
    like($output, qr/pl-fc.*\$s/, 'fc($s) generates pl-fc with variable');
}

# Test 3: fc with $_
{
    my $parser = Pl::Parser->new(code => '$_ = "TEST"; my $x = fc;');
    my $output = get_generated_code($parser);
    like($output, qr/pl-fc.*\$_/, 'fc defaults to $_');
}

# ============================================
# $! in string interpolation
# ============================================

# Test 4: $! in double-quoted string
{
    my $parser = Pl::Parser->new(code => 'my $msg = "Error: $!";');
    my $output = get_generated_code($parser);
    like($output, qr/pl-errno-string/, '$! interpolates in strings');
}

# Test 5: $! standalone
{
    my $parser = Pl::Parser->new(code => 'my $e = $!;');
    my $output = get_generated_code($parser);
    like($output, qr/pl-errno-string/, '$! standalone generates pl-errno-string');
}

# ============================================
# $$ in string interpolation (already worked, verify)
# ============================================

# Test 6: $$ in double-quoted string
{
    my $parser = Pl::Parser->new(code => 'my $msg = "PID: $$";');
    my $output = get_generated_code($parser);
    like($output, qr/\$\$/, '$$ interpolates in strings');
}

# ============================================
# Other magic variables in interpolation
# ============================================

# Test 7: $? in string
{
    my $parser = Pl::Parser->new(code => 'my $msg = "Exit: $?";');
    my $output = get_generated_code($parser);
    like($output, qr/\$\?/, '$? interpolates in strings');
}

# Test 8: $0 in string
{
    my $parser = Pl::Parser->new(code => 'my $msg = "Program: $0";');
    my $output = get_generated_code($parser);
    like($output, qr/\$0/, '$0 interpolates in strings');
}

# Test 9: $. (line number) in string
{
    my $parser = Pl::Parser->new(code => 'my $msg = "Line: $.";');
    my $output = get_generated_code($parser);
    like($output, qr/\|\$\.\|/, '$. interpolates in strings');
}

# Test 10: $@ in string
{
    my $parser = Pl::Parser->new(code => 'my $msg = "Error: $@";');
    my $output = get_generated_code($parser);
    like($output, qr/\$\@/, '$@ interpolates in strings');
}

# ============================================
# Caret variables
# ============================================

# Test 11: $^O (OS name) standalone
{
    my $parser = Pl::Parser->new(code => 'my $os = $^O;');
    my $output = get_generated_code($parser);
    like($output, qr/\|\$\^O\|/, '$^O generates pipe-quoted symbol');
}

# Test 12: $^O in string interpolation
{
    my $parser = Pl::Parser->new(code => 'my $msg = "OS: $^O";');
    my $output = get_generated_code($parser);
    like($output, qr/\|\$\^O\|/, '$^O interpolates in strings');
}

# Test 13: $^V (Perl version) standalone
{
    my $parser = Pl::Parser->new(code => 'my $v = $^V;');
    my $output = get_generated_code($parser);
    like($output, qr/\|\$\^V\|/, '$^V generates pipe-quoted symbol');
}

# ============================================
# caller() function
# ============================================

# Test 14: caller() with no args
{
    my $parser = Pl::Parser->new(code => 'my $pkg = caller();');
    my $output = get_generated_code($parser);
    like($output, qr/pl-caller/, 'caller() generates pl-caller');
}

# Test 15: caller() with level argument
{
    my $parser = Pl::Parser->new(code => 'my $pkg = caller(1);');
    my $output = get_generated_code($parser);
    like($output, qr/pl-caller\s+1/, 'caller(1) generates pl-caller with argument');
}

# Test 16: caller() in list context
{
    my $parser = Pl::Parser->new(code => 'my ($pkg, $file, $line) = caller();');
    my $output = get_generated_code($parser);
    like($output, qr/pl-caller/, 'caller() in list context');
}

# ============================================
# Record separators
# ============================================

# Test 17: $/ (input record separator) standalone
{
    my $parser = Pl::Parser->new(code => 'my $sep = $/;');
    my $output = get_generated_code($parser);
    like($output, qr/\|\$\/\|/, '$/ generates pipe-quoted symbol');
}

# Test 18: $\ (output record separator) standalone
{
    my $parser = Pl::Parser->new(code => 'my $sep = $\\;');
    my $output = get_generated_code($parser);
    like($output, qr/\|\$\\\|/, '$\\ generates pipe-quoted symbol');
}

# ============================================
# Multiple magic vars in one string
# ============================================

# Test 19: Multiple magic vars
{
    my $parser = Pl::Parser->new(code => 'my $msg = "PID $$ on $^O";');
    my $output = get_generated_code($parser);
    like($output, qr/\$\$.*\|\$\^O\|/s, 'Multiple magic vars interpolate');
}

# Test 20: Magic var with regular var
{
    my $parser = Pl::Parser->new(code => 'my $name = "test"; my $msg = "Process $$ running $name";');
    my $output = get_generated_code($parser);
    like($output, qr/\$\$.*\$name/s, 'Magic var with regular var interpolate');
}

# ============================================
# Edge cases
# ============================================

# Test 21: $" (list separator) - tricky in double-quoted string
{
    my $parser = Pl::Parser->new(code => 'my $sep = $";');
    my $output = get_generated_code($parser);
    like($output, qr/\|\$"\|/, '$" generates pipe-quoted symbol');
}

# Test 22: $' (postmatch) standalone
{
    my $parser = Pl::Parser->new(code => q{my $x = $';});
    my $output = get_generated_code($parser);
    # $' should be treated as a magic variable
    ok(defined $output, q{$' parses without error});
}

# Test 23: $& (match) standalone
{
    my $parser = Pl::Parser->new(code => 'my $x = $&;');
    my $output = get_generated_code($parser);
    ok(defined $output, '$& parses without error');
}

# Test 24: $+ (last bracket) standalone
{
    my $parser = Pl::Parser->new(code => 'my $x = $+;');
    my $output = get_generated_code($parser);
    ok(defined $output, '$+ parses without error');
}

# Test 25: Using $! in die message
{
    my $parser = Pl::Parser->new(code => 'open(my $fh, "<", "file") or die "Cannot open: $!";');
    my $output = get_generated_code($parser);
    like($output, qr/pl-errno-string/, '$! in die message interpolates');
}

done_testing(25);
