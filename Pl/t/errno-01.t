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

# Test 1: $! generates p-errno-string call
{
    my $parser = Pl::Parser->new(code => 'my $e = $!;');
    my $output = get_generated_code($parser);
    like($output, qr/p-errno-string/, '$! generates p-errno-string call');
}

# Test 2: $! in print statement
{
    my $parser = Pl::Parser->new(code => 'print $!;');
    my $output = get_generated_code($parser);
    like($output, qr/p-print.*p-errno-string/, '$! works in print');
}

# Test 3: $! in concatenation
{
    my $parser = Pl::Parser->new(code => 'my $msg = "Error: " . $!;');
    my $output = get_generated_code($parser);
    like($output, qr/p-\.\s+"Error: "\s+\(p-errno-string\)/, '$! works in concatenation');
}

# Test 4: $! in die statement with string interpolation
{
    my $parser = Pl::Parser->new(code => 'die "Failed: $!";');
    my $output = get_generated_code($parser);
    like($output, qr/p-die.*p-errno-string/s, '$! works in die with interpolation');
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
    like($output, qr/p-open/, 'open is present');
    like($output, qr/p-die/, 'die is present');
    like($output, qr/p-errno-string/, '$! is interpolated in string');
}

diag '-------- $! dualvar survives @_, array, and hash:';

# Test 8: $! keeps its numeric (errno) half when passed through @_, stored in an
# array/hash, or copied.  Regression: p-aref-unbox-elem / %p-flatten-list /
# %p-array-store-scalar / %p-make-hash-entry used to unbox the dualvar to its
# string value, dropping the numeric side (so $!+0 became 0 downstream).
{
    my $code = <<'PERL';
open(my $fh, "<", "/nonexistent/xyz/12345") or 1;
my $en = $! + 0;                      # the numeric errno (e.g. 2)
print "direct:$en\n";
sub thru { my ($g) = @_; return $g + 0; }
print "thru_at:", thru($!), "\n";
sub elem0 { return $_[0] + 0; }
print "elem0:", elem0($!), "\n";
my @a = ($!);
print "array:", $a[0] + 0, "\n";
my %h = (e => $!);
print "hash:", $h{e} + 0, "\n";
PERL
    my $parser = Pl::Parser->new(code => $code);
    my $lisp_code = join("\n", $parser->parse());
    my $result = run_lisp($lisp_code);
    my ($direct) = $result =~ /direct:(\d+)/;
    ok($direct && $direct > 0, "errno is non-zero after failed open (got $direct)");
    like($result, qr/thru_at:$direct/, '$! numeric survives passing through @_ (my ($g)=@_)');
    like($result, qr/elem0:$direct/,   '$! numeric survives $_[0] read');
    like($result, qr/array:$direct/,   '$! numeric survives storage in an array');
    like($result, qr/hash:$direct/,    '$! numeric survives storage in a hash');
}

diag '-------- Scalar::Util::dualvar:';

# Test 9: Scalar::Util::dualvar builds a real dualvar (numeric AND string), and
# the two halves survive arrays, hashes and @_ (regression: the shim used to
# return only the string).  Uses ./runpl because `use Scalar::Util` needs the
# module-loading path that the bare run_lisp helper does not set up.
{
    my $code = <<'PERL';
use Scalar::Util qw(dualvar);
my $dv = dualvar(42, "forty-two");
print "n:", $dv + 0, " s:$dv\n";
my @a = ($dv);
print "an:", $a[0] + 0, " as:$a[0]\n";
my %h = (k => $dv);
print "hn:", $h{k} + 0, " hs:$h{k}\n";
sub g { my ($x) = @_; print "gn:", $x + 0, " gs:$x\n"; }
g($dv);
PERL
    my ($cfh, $cfile) = tempfile(SUFFIX => '.pl');
    print $cfh $code;
    close $cfh;
    my $result = `./runpl "$cfile" 2>&1`;
    unlink $cfile;
    like($result, qr/n:42 s:forty-two/,   'dualvar: direct numeric + string');
    like($result, qr/an:42 as:forty-two/, 'dualvar: survives array storage');
    like($result, qr/hn:42 hs:forty-two/, 'dualvar: survives hash storage');
    like($result, qr/gn:42 gs:forty-two/, 'dualvar: survives @_');
}

done_testing();
