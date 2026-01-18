#!/usr/bin/env perl
# Tests for @ARGV and shift/pop context handling

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

diag "";
diag "-------- @ARGV variable:";

# Test 1: @ARGV is recognized as a variable
{
    my $parser = Pl::Parser->new(code => 'my @args = @ARGV;');
    my $output = get_generated_code($parser);
    like($output, qr/\@ARGV/, '@ARGV is recognized');
}

# Test 2: Access @ARGV elements
{
    my $parser = Pl::Parser->new(code => 'my $first = $ARGV[0];');
    my $output = get_generated_code($parser);
    like($output, qr/\@ARGV/, '$ARGV[0] accesses @ARGV');
}

# Test 3: @ARGV in foreach
{
    my $parser = Pl::Parser->new(code => 'foreach my $arg (@ARGV) { print $arg; }');
    my $output = get_generated_code($parser);
    like($output, qr/\@ARGV/, '@ARGV in foreach loop');
}

diag "";
diag "-------- shift/pop at top level (should use \@ARGV):";

# Test 4: shift at top level defaults to @ARGV
{
    my $parser = Pl::Parser->new(code => 'my $arg = shift;');
    my $output = get_generated_code($parser);
    like($output, qr/pl-shift\s+\@ARGV/, 'shift at top level uses @ARGV');
}

# Test 5: pop at top level defaults to @ARGV
{
    my $parser = Pl::Parser->new(code => 'my $last = pop;');
    my $output = get_generated_code($parser);
    like($output, qr/pl-pop\s+\@ARGV/, 'pop at top level uses @ARGV');
}

# Test 6: Multiple shifts at top level
{
    my $parser = Pl::Parser->new(code => 'my $a = shift; my $b = shift;');
    my $output = get_generated_code($parser);
    my @matches = ($output =~ /pl-shift\s+\@ARGV/g);
    is(scalar @matches, 2, 'Multiple shifts at top level all use @ARGV');
}

diag "";
diag "-------- shift/pop inside subs (should use \@_):";

# Test 7: shift inside sub defaults to @_
{
    my $parser = Pl::Parser->new(code => 'sub foo { my $x = shift; }');
    my $output = get_generated_code($parser);
    like($output, qr/pl-shift\s+\@_/, 'shift inside sub uses @_');
}

# Test 8: pop inside sub defaults to @_
{
    my $parser = Pl::Parser->new(code => 'sub foo { my $x = pop; }');
    my $output = get_generated_code($parser);
    like($output, qr/pl-pop\s+\@_/, 'pop inside sub uses @_');
}

# Test 9: shift used in typical constructor pattern
{
    my $parser = Pl::Parser->new(code => 'sub new { my $class = shift; bless {}, $class; }');
    my $output = get_generated_code($parser);
    like($output, qr/pl-shift\s+\@_/, 'shift in constructor uses @_');
}

# Test 10: Nested subs both use @_
{
    my $parser = Pl::Parser->new(code => '
        sub outer {
            my $a = shift;
            sub inner {
                my $b = shift;
            }
        }
    ');
    my $output = get_generated_code($parser);
    my @matches = ($output =~ /pl-shift\s+\@_/g);
    is(scalar @matches, 2, 'Both nested subs use @_');
}

diag "";
diag "-------- Mixed context:";

# Test 11: Top level shift, then sub with shift
{
    my $parser = Pl::Parser->new(code => '
        my $file = shift;
        sub process { my $x = shift; }
    ');
    my $output = get_generated_code($parser);
    like($output, qr/pl-shift\s+\@ARGV/, 'Top level shift uses @ARGV');
    like($output, qr/pl-shift\s+\@_/, 'Sub shift uses @_');
}

# Test 12: Explicit @ARGV in sub (should stay @ARGV)
{
    my $parser = Pl::Parser->new(code => 'sub foo { my $x = shift @ARGV; }');
    my $output = get_generated_code($parser);
    like($output, qr/pl-shift\s+\@ARGV/, 'Explicit shift @ARGV in sub stays @ARGV');
}

# Test 13: Explicit @_ at top level (should stay @_)
{
    my $parser = Pl::Parser->new(code => 'my $x = shift @_;');
    my $output = get_generated_code($parser);
    like($output, qr/pl-shift\s+\@_/, 'Explicit shift @_ at top level stays @_');
}

diag "";
diag "-------- Scalar @ARGV:";

# Test 14: scalar @ARGV for count
{
    my $parser = Pl::Parser->new(code => 'my $count = scalar @ARGV;');
    my $output = get_generated_code($parser);
    like($output, qr/\@ARGV/, 'scalar @ARGV works');
}

# Test 15: if (@ARGV) check
{
    my $parser = Pl::Parser->new(code => 'if (@ARGV) { print "has args"; }');
    my $output = get_generated_code($parser);
    like($output, qr/\@ARGV/, '@ARGV in condition');
}

done_testing(16);
