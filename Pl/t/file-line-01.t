#!/usr/bin/env perl
# Tests for __FILE__ and __LINE__ compile-time tokens

use strict;
use warnings;

use Test::More;
use File::Temp qw(tempfile);

use lib '.';
use Pl::Parser;

# Helper to get just the generated code (skip preamble)
sub get_generated_code {
    my $parser = shift;
    my @output = $parser->parse();
    my $text = join("\n", @output);
    # Skip preamble lines (in-package, @INC setup, etc.)
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

# Helper to run CL code and get output
sub run_lisp {
    my $code = shift;
    my ($fh, $filename) = tempfile(SUFFIX => '.lisp');
    print $fh $code;
    close $fh;
    my $output = `sbcl --noinform --non-interactive --load cl/pcl-runtime.lisp --load "$filename" 2>&1`;
    unlink $filename;
    return $output;
}

diag '-------- __FILE__ Token:';

# Test 1: __FILE__ from stdin returns '-'
{
    my $parser = Pl::Parser->new(code => 'print __FILE__;');
    my $output = get_generated_code($parser);
    like($output, qr/"-"/, '__FILE__ from code string returns "-"');
}

# Test 2: __FILE__ in expression
{
    my $parser = Pl::Parser->new(code => 'my $f = __FILE__;');
    my $output = get_generated_code($parser);
    like($output, qr/\$f\s+"-"/, '__FILE__ can be assigned to variable');
}

# Test 3: __FILE__ with concatenation
{
    my $parser = Pl::Parser->new(code => 'print "File: " . __FILE__;');
    my $output = get_generated_code($parser);
    like($output, qr/pl-\.\s+"File: "\s+"-"/, '__FILE__ can be concatenated');
}

diag '-------- __LINE__ Token:';

# Test 4: __LINE__ basic
{
    my $parser = Pl::Parser->new(code => 'print __LINE__;');
    my $output = get_generated_code($parser);
    like($output, qr/pl-print\s+1/, '__LINE__ returns line number');
}

# Test 5: __LINE__ preserves actual line number
{
    my $code = "# comment\nprint __LINE__;\n";  # __LINE__ on line 2
    my $parser = Pl::Parser->new(code => $code);
    my $output = get_generated_code($parser);
    like($output, qr/pl-print\s+2/, '__LINE__ reflects actual source line');
}

# Test 6: __LINE__ in expression
{
    my $parser = Pl::Parser->new(code => 'my $l = __LINE__;');
    my $output = get_generated_code($parser);
    like($output, qr/\$l\s+1/, '__LINE__ can be assigned to variable');
}

# Test 7: Multiple __LINE__ on different lines
{
    my $code = "print __LINE__;\nprint __LINE__;\nprint __LINE__;\n";
    my $parser = Pl::Parser->new(code => $code);
    my @output = $parser->parse();
    my $text = join("\n", @output);
    like($text, qr/pl-print\s+1/, 'First __LINE__ is 1');
    like($text, qr/pl-print\s+2/, 'Second __LINE__ is 2');
    like($text, qr/pl-print\s+3/, 'Third __LINE__ is 3');
}

diag '-------- __FILE__ and __LINE__ Together:';

# Test 8: Both tokens in same statement
{
    my $parser = Pl::Parser->new(code => 'print __FILE__, ":", __LINE__;');
    my $output = get_generated_code($parser);
    like($output, qr/"-".*":".*1/s, '__FILE__ and __LINE__ work together');
}

# Test 9: In conditional
{
    my $parser = Pl::Parser->new(code => 'die "Error at " . __FILE__ . ":" . __LINE__ if $err;');
    my $output = get_generated_code($parser);
    like($output, qr/pl-die.*pl-\.\s+.*"-".*":".*1/s, '__FILE__ and __LINE__ work in die message');
}

diag '-------- Runtime Execution:';

# Test 10: Actually run the code
{
    my $parser = Pl::Parser->new(code => 'print __FILE__, "\n"; print __LINE__, "\n";');
    my @output = $parser->parse();
    my $lisp_code = join("\n", @output);
    my $result = run_lisp($lisp_code);
    like($result, qr/-/, '__FILE__ outputs stdin marker');
    like($result, qr/1/, '__LINE__ outputs line number');
}

# Test 11: __LINE__ preserves correct line in runtime
{
    my $code = <<'PERL';
# First line
# Second line
print __LINE__, "\n";
PERL
    my $parser = Pl::Parser->new(code => $code);
    my @output = $parser->parse();
    my $lisp_code = join("\n", @output);
    my $result = run_lisp($lisp_code);
    like($result, qr/^3\s*$/m, '__LINE__ on third line outputs 3');
}

# Test 12: __FILE__ with real file
{
    my ($fh, $filename) = tempfile(SUFFIX => '.pl');
    print $fh 'print __FILE__, "\n";';
    close $fh;

    my $parser = Pl::Parser->new(filename => $filename);
    my @output = $parser->parse();
    my $lisp_code = join("\n", @output);
    my $result = run_lisp($lisp_code);
    like($result, qr/\Q$filename\E/, '__FILE__ outputs actual filename');
    unlink $filename;
}

diag '-------- Edge Cases:';

# Test 13: __FILE__ inside string interpolation (should NOT interpolate)
{
    my $parser = Pl::Parser->new(code => 'print "__FILE__";');
    my $output = get_generated_code($parser);
    like($output, qr/"__FILE__"/, '__FILE__ in quotes is literal string');
}

# Test 14: __LINE__ inside string interpolation (should NOT interpolate)
{
    my $parser = Pl::Parser->new(code => 'print "__LINE__";');
    my $output = get_generated_code($parser);
    like($output, qr/"__LINE__"/, '__LINE__ in quotes is literal string');
}

# Test 15: __FILE__ and __LINE__ are not valid identifiers (barewords)
{
    my $parser = Pl::Parser->new(code => 'my $line = __LINE__ + 1;');
    my $output = get_generated_code($parser);
    like($output, qr/pl-\+\s+\d+\s+1/, '__LINE__ works in arithmetic');
}

diag '-------- __PACKAGE__ Token:';

# Test 16: __PACKAGE__ in main returns "main"
{
    my $parser = Pl::Parser->new(code => 'print __PACKAGE__;');
    my $output = get_generated_code($parser);
    like($output, qr/"main"/, '__PACKAGE__ in main returns "main"');
}

# Test 17: __PACKAGE__ in named package
{
    my $parser = Pl::Parser->new(code => 'package Foo; print __PACKAGE__;');
    my $output = get_generated_code($parser);
    like($output, qr/"Foo"/, '__PACKAGE__ returns current package name');
}

# Test 18: __PACKAGE__ in nested package
{
    my $parser = Pl::Parser->new(code => 'package Foo::Bar::Baz; print __PACKAGE__;');
    my $output = get_generated_code($parser);
    like($output, qr/"Foo::Bar::Baz"/, '__PACKAGE__ works with nested packages');
}

# Test 19: __PACKAGE__ can be assigned
{
    my $parser = Pl::Parser->new(code => 'my $pkg = __PACKAGE__;');
    my $output = get_generated_code($parser);
    like($output, qr/\$pkg\s+"main"/, '__PACKAGE__ can be assigned to variable');
}

# Test 20: __PACKAGE__ in string (should NOT interpolate)
{
    my $parser = Pl::Parser->new(code => 'print "__PACKAGE__";');
    my $output = get_generated_code($parser);
    like($output, qr/"__PACKAGE__"/, '__PACKAGE__ in quotes is literal string');
}

# Test 21: __PACKAGE__ used with bless (common pattern)
{
    my $parser = Pl::Parser->new(code => 'package MyClass; sub new { bless {}, __PACKAGE__ }');
    my $output = get_generated_code($parser);
    like($output, qr/pl-bless.*"MyClass"/, '__PACKAGE__ works in bless');
}

# Test 22: __PACKAGE__ in arithmetic (concatenation)
{
    my $parser = Pl::Parser->new(code => 'package Foo; my $s = __PACKAGE__ . "::method";');
    my $output = get_generated_code($parser);
    like($output, qr/pl-\.\s+"Foo"\s+"::method"/, '__PACKAGE__ works in concatenation');
}

# Test 23: Multiple packages - __PACKAGE__ tracks correctly
{
    my $code = 'package A; my $a = __PACKAGE__; package B; my $b = __PACKAGE__;';
    my $parser = Pl::Parser->new(code => $code);
    my $output = get_generated_code($parser);
    like($output, qr/\$a\s+"A"/, '__PACKAGE__ is "A" in package A');
    like($output, qr/\$b\s+"B"/, '__PACKAGE__ is "B" in package B');
}

# Test 24: Runtime execution
{
    my $parser = Pl::Parser->new(code => 'package TestPkg; print __PACKAGE__, "\n";');
    my @output = $parser->parse();
    my $lisp_code = join("\n", @output);
    my $result = run_lisp($lisp_code);
    like($result, qr/TestPkg/, '__PACKAGE__ outputs package name at runtime');
}

done_testing();
