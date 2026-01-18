#!/usr/bin/env perl
# Tests for $! (errno) system error variable

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

diag '-------- $! (errno) Variable:';

# Test 1: $! generates pl-errno-string call
{
    my $parser = Pl::Parser->new(code => 'my $e = $!;');
    my $output = get_generated_code($parser);
    like($output, qr/pl-errno-string/, '$! generates pl-errno-string call');
}

# Test 2: $! in print statement
{
    my $parser = Pl::Parser->new(code => 'print $!;');
    my $output = get_generated_code($parser);
    like($output, qr/pl-print.*pl-errno-string/, '$! works in print');
}

# Test 3: $! in concatenation
{
    my $parser = Pl::Parser->new(code => 'my $msg = "Error: " . $!;');
    my $output = get_generated_code($parser);
    like($output, qr/pl-\.\s+"Error: "\s+\(pl-errno-string\)/, '$! works in concatenation');
}

# Test 4: $! in die statement with string interpolation
{
    my $parser = Pl::Parser->new(code => 'die "Failed: $!";');
    my $output = get_generated_code($parser);
    like($output, qr/pl-die.*pl-errno-string/s, '$! works in die with interpolation');
}

diag '-------- Runtime Execution:';

# Test 5: $! works at runtime (value may vary)
{
    my $parser = Pl::Parser->new(code => 'my $e = $!; print "got-errno\n";');
    my @output = $parser->parse();
    my $lisp_code = join("\n", @output);
    my $result = run_lisp($lisp_code);
    like($result, qr/got-errno/, '$! can be read at runtime');
}

# Test 6: $! returns error after failed operation
{
    my $code = <<'PERL';
open my $fh, '<', '/nonexistent/file/that/does/not/exist/12345';
print "error:", $!, "\n";
PERL
    my $parser = Pl::Parser->new(code => $code);
    my @output = $parser->parse();
    my $lisp_code = join("\n", @output);
    my $result = run_lisp($lisp_code);
    # Should contain some error message (varies by system)
    like($result, qr/error:.+/, '$! contains error message after failed open');
    unlike($result, qr/error:\s*\n/, '$! is not empty after failed open');
}

# Test 7: Common pattern: open or die
{
    my $parser = Pl::Parser->new(code => 'open my $fh, "<", $file or die "Cannot open: $!";');
    my $output = get_generated_code($parser);
    like($output, qr/pl-open/, 'open is present');
    like($output, qr/pl-die/, 'die is present');
    like($output, qr/pl-errno-string/, '$! is interpolated in string');
}

done_testing();
