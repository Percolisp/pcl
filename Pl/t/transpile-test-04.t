#!/usr/bin/env perl
# Transpile tests part 4: math and string functions

use v5.30;
use strict;
use warnings;
use Test::More;
use File::Temp qw(tempfile);
use FindBin qw($RealBin);

# Path to pl2cl and runtime
my $project_root = "$RealBin/../..";
my $pl2cl = "$project_root/pl2cl";
my $runtime = "$project_root/cl/pcl-runtime.lisp";

# Check dependencies
plan skip_all => "pl2cl not found" unless -x $pl2cl;
plan skip_all => "sbcl not found" unless `which sbcl 2>/dev/null`;

# Run a Perl snippet and return output
sub run_perl {
    my ($code) = @_;
    # Add common 'use' statements for features we support
    my $full_code = 'use feature "state"; use Cwd; ' . $code;
    my $output = `perl -e '$full_code' 2>&1`;
    return $output;
}

# Transpile and run CL, return output
sub run_cl {
    my ($code) = @_;

    # Write Perl code to temp file
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;

    # Transpile
    my $cl_code = `$pl2cl $pl_file 2>&1`;

    # Write CL to temp file
    my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cl_fh $cl_code;
    close $cl_fh;

    # Run with sbcl
    my $output = `sbcl --noinform --non-interactive --load $runtime --load $cl_file 2>&1`;

    # Filter out warnings and "PCL Runtime loaded"
    $output =~ s/^;.*\n//gm;
    $output =~ s/^caught .*\n//gm;
    $output =~ s/^compilation unit.*\n//gm;
    $output =~ s/^\s*Undefined.*\n//gm;
    $output =~ s/^-->.*\n//gm;
    $output =~ s/^==>.*\n//gm;
    $output =~ s/^PCL Runtime loaded\n//gm;
    $output =~ s/^\s*\n//gm;

    return $output;
}

# Test helper: compare Perl and CL output
sub test_transpile {
    my ($name, $code) = @_;
    my $perl_out = run_perl($code);
    my $cl_out = run_cl($code);
    is($cl_out, $perl_out, $name) or diag("Perl: $perl_out\nCL: $cl_out");
}

# ============ MATH FUNCTIONS ============

# Use int() to avoid float formatting differences between Perl and CL
test_transpile("math: int(sin(0))", 'print int(sin(0)), "\n";');
test_transpile("math: int(cos(0))", 'print int(cos(0)), "\n";');
test_transpile("math: int(sqrt(16))", 'print int(sqrt(16)), "\n";');
test_transpile("math: sqrt(2) comparison", 'print sqrt(2) > 1.41 && sqrt(2) < 1.42 ? "ok" : "fail", "\n";');
test_transpile("math: int(exp(0))", 'print int(exp(0)), "\n";');
test_transpile("math: exp(1) comparison", 'print exp(1) > 2.71 && exp(1) < 2.72 ? "ok" : "fail", "\n";');
test_transpile("math: int(log(1))", 'print int(log(1)), "\n";');
test_transpile("math: log(exp(1)) comparison", 'print log(exp(1)) > 0.99 && log(exp(1)) < 1.01 ? "ok" : "fail", "\n";');
test_transpile("math: atan2(1,1) comparison", 'print atan2(1,1) > 0.78 && atan2(1,1) < 0.79 ? "ok" : "fail", "\n";');
test_transpile("math: int(atan2(0,1))", 'print int(atan2(0,1)), "\n";');
test_transpile("math: int(3.7)", 'print int(3.7), "\n";');
test_transpile("math: int(-3.7)", 'print int(-3.7), "\n";');
test_transpile("math: abs(-42)", 'print abs(-42), "\n";');
test_transpile("math: abs(42)", 'print abs(42), "\n";');

# ============ STRING FUNCTIONS ============

test_transpile("string: chr(65)", 'print chr(65), "\n";');
test_transpile("string: chr(97)", 'print chr(97), "\n";');
test_transpile("string: ord A", 'print ord("A"), "\n";');
test_transpile("string: ord a", 'print ord("a"), "\n";');
test_transpile("string: hex 0xff", 'print hex("ff"), "\n";');
test_transpile("string: hex 0x10", 'print hex("10"), "\n";');
test_transpile("string: oct 0777", 'print oct("777"), "\n";');
test_transpile("string: oct 0o10", 'print oct("10"), "\n";');
test_transpile("string: lcfirst", 'print lcfirst("HELLO"), "\n";');
test_transpile("string: ucfirst", 'print ucfirst("hello"), "\n";');
test_transpile("string: lc", 'print lc("HELLO"), "\n";');
test_transpile("string: uc", 'print uc("hello"), "\n";');
test_transpile("string: length", 'print length("hello"), "\n";');
test_transpile("string: substr 2 args", 'print substr("hello", 2), "\n";');
test_transpile("string: substr 3 args", 'print substr("hello", 1, 3), "\n";');
test_transpile("string: index", 'print index("hello", "l"), "\n";');
test_transpile("string: index with offset", 'print index("hello", "l", 3), "\n";');
test_transpile("string: rindex", 'print rindex("hello", "l"), "\n";');

# ============ SPRINTF ============

test_transpile("sprintf: string %s", 'print sprintf("%s", "hello"), "\n";');
test_transpile("sprintf: integer %d", 'print sprintf("%d", 42), "\n";');
test_transpile("sprintf: float %f", 'print sprintf("%.2f", 3.14159), "\n";');
test_transpile("sprintf: multiple args", 'print sprintf("%s is %d", "answer", 42), "\n";');

# ============ FILE I/O ============

test_transpile("file I/O: write and read back", '
my $file = "/tmp/pcl-test-file.txt";

# Write to file using print and say
open(FH, ">", $file);
print FH "line one";
print FH "\n";
say FH "line two";
print FH "line three\n";
close(FH);

# Read back and verify
my $content = "";
open(FH, "<", $file);
while (my $line = <FH>) {
    $content = $content . $line;
}
close(FH);

# Clean up
unlink($file);

# Print what we read
print $content;
');

test_transpile("file I/O: readline returns undef at EOF", '
my $file = "/tmp/pcl-test-eof.txt";
open(FH, ">", $file);
print FH "only line\n";
close(FH);

open(FH, "<", $file);
my $line1 = <FH>;
my $line2 = <FH>;
close(FH);
unlink($file);

print "line1 defined: ", defined($line1) ? "yes" : "no", "\n";
print "line2 defined: ", defined($line2) ? "yes" : "no", "\n";
');

# ============ REGEX ============

test_transpile("regex: simple match true", '
my $str = "hello world";
if ($str =~ /world/) { print "yes\n"; } else { print "no\n"; }
');

test_transpile("regex: simple match false", '
my $str = "hello world";
if ($str =~ /foo/) { print "yes\n"; } else { print "no\n"; }
');

test_transpile("regex: case insensitive", '
my $str = "Hello World";
if ($str =~ /hello/i) { print "yes\n"; } else { print "no\n"; }
');

test_transpile("regex: negation", '
my $str = "hello world";
if ($str !~ /foo/) { print "yes\n"; } else { print "no\n"; }
');

test_transpile("regex: substitution single", '
my $str = "foo bar foo";
$str =~ s/foo/baz/;
print $str, "\n";
');

test_transpile("regex: substitution global", '
my $str = "foo bar foo";
$str =~ s/foo/baz/g;
print $str, "\n";
');

test_transpile("regex: substitution case insensitive", '
my $str = "Hello World";
$str =~ s/hello/hi/i;
print $str, "\n";
');

test_transpile("regex: tr lowercase to uppercase", '
my $str = "hello";
$str =~ tr/a-z/A-Z/;
print $str, "\n";
');

test_transpile("regex: tr uppercase to lowercase", '
my $str = "HELLO";
$str =~ tr/A-Z/a-z/;
print $str, "\n";
');

test_transpile("regex: tr single chars", '
my $str = "abc";
$str =~ tr/abc/xyz/;
print $str, "\n";
');

# ============ REGEX: METACHARACTERS ============

test_transpile("regex: dot matches any char", '
my $str = "abc";
if ($str =~ /a.c/) { print "yes\n"; } else { print "no\n"; }
');

test_transpile("regex: dot does not match newline by default", '
my $str = "a\nc";
if ($str =~ /a.c/) { print "yes\n"; } else { print "no\n"; }
');

test_transpile("regex: star quantifier", '
my $str = "goooal";
if ($str =~ /go*al/) { print "yes\n"; } else { print "no\n"; }
');

test_transpile("regex: star matches zero", '
my $str = "gal";
if ($str =~ /go*al/) { print "yes\n"; } else { print "no\n"; }
');

test_transpile("regex: plus quantifier", '
my $str = "goooal";
if ($str =~ /go+al/) { print "yes\n"; } else { print "no\n"; }
');

test_transpile("regex: plus requires one", '
my $str = "gal";
if ($str =~ /go+al/) { print "yes\n"; } else { print "no\n"; }
');

test_transpile("regex: question mark optional", '
my $str = "color";
if ($str =~ /colou?r/) { print "yes\n"; } else { print "no\n"; }
');

test_transpile("regex: question mark with char", '
my $str = "colour";
if ($str =~ /colou?r/) { print "yes\n"; } else { print "no\n"; }
');

# ============ REGEX: ANCHORS ============

test_transpile("regex: caret anchor match", '
my $str = "hello world";
if ($str =~ /^hello/) { print "yes\n"; } else { print "no\n"; }
');

test_transpile("regex: caret anchor no match", '
my $str = "say hello";
if ($str =~ /^hello/) { print "yes\n"; } else { print "no\n"; }
');

test_transpile("regex: dollar anchor match", '
my $str = "hello world";
if ($str =~ /world$/) { print "yes\n"; } else { print "no\n"; }
');

test_transpile("regex: dollar anchor no match", '
my $str = "world hello";
if ($str =~ /world$/) { print "yes\n"; } else { print "no\n"; }
');

test_transpile("regex: both anchors", '
my $str = "hello";
if ($str =~ /^hello$/) { print "yes\n"; } else { print "no\n"; }
');

# ============ REGEX: CHARACTER CLASSES ============

test_transpile("regex: character class", '
my $str = "cat";
if ($str =~ /[cb]at/) { print "yes\n"; } else { print "no\n"; }
');

test_transpile("regex: negated character class", '
my $str = "cat";
if ($str =~ /[^d]at/) { print "yes\n"; } else { print "no\n"; }
');

test_transpile("regex: digit class", '
my $str = "abc123";
if ($str =~ /\d+/) { print "yes\n"; } else { print "no\n"; }
');

test_transpile("regex: word class", '
my $str = "hello_world";
if ($str =~ /^\w+$/) { print "yes\n"; } else { print "no\n"; }
');

test_transpile("regex: whitespace class", '
my $str = "hello world";
if ($str =~ /\s/) { print "yes\n"; } else { print "no\n"; }
');

done_testing();
