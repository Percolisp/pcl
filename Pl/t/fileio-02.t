#!/usr/bin/env perl
# Tests for lexical filehandle I/O (open/read/write/close/eof with $fh).

use v5.30;
use strict;
use warnings;
use Test::More;
use File::Temp qw(tempfile);
use FindBin qw($RealBin);

my $project_root = "$RealBin/../..";
my $pl2cl        = "$project_root/pl2cl";
my $runtime      = "$project_root/cl/pcl-runtime.lisp";

plan skip_all => "pl2cl not found" unless -x $pl2cl;
plan skip_all => "sbcl not found"  unless `which sbcl 2>/dev/null`;

sub run_cl {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;

    my $cl_code = `$pl2cl $pl_file 2>&1`;

    my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cl_fh $cl_code;
    close $cl_fh;

    my $output = `sbcl --noinform --non-interactive --load $runtime --load $cl_file 2>&1`;
    $output =~ s/^;.*\n//gm;
    $output =~ s/^caught .*\n//gm;
    $output =~ s/^compilation unit.*\n//gm;
    $output =~ s/^\s*Undefined.*\n//gm;
    $output =~ s/^PCL Runtime loaded\n//gm;
    $output =~ s/^# PCL Test library loaded\n//gm;
    $output =~ s/^WARNING:.*\n//gm;
    $output =~ s/^\s*\n//gm;
    return $output;
}

# Create a temp file with known content, return its path.
# The path is embedded as a literal string in the generated Perl code.
sub make_temp_data_file {
    my (@lines) = @_;
    my ($fh, $path) = tempfile(SUFFIX => '.txt', UNLINK => 1);
    print $fh join('', @lines);
    close $fh;
    return $path;
}

sub test_io {
    my ($name, $code) = @_;
    my ($fh, $file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;
    my $perl_out = `perl $file 2>&1`;
    my $cl_out   = run_cl($code);
    is($cl_out, $perl_out, $name) or diag("Perl: $perl_out\nCL:   $cl_out");
}

plan tests => 9;

# --- Test 1: Bareword write + read (baseline) ---
{
    my $tmpfile = make_temp_data_file();
    test_io('bareword fh: write and read back', <<"PERL");
open(F, '>', '$tmpfile') or die "open: \$!";
print F "line1\n";
print F "line2\n";
close F;
open(F, '<', '$tmpfile') or die "open: \$!";
while (my \$line = <F>) {
    print \$line;
}
close F;
PERL
}

# --- Test 2: Lexical \$fh write + read ---
{
    my $tmpfile = make_temp_data_file();
    test_io('lexical fh: write and read back', <<"PERL");
open(my \$fh, '>', '$tmpfile') or die "open: \$!";
print \$fh "hello\n";
print \$fh "world\n";
close \$fh;
open(my \$fh2, '<', '$tmpfile') or die "open: \$!";
while (my \$line = <\$fh2>) {
    print \$line;
}
close \$fh2;
PERL
}

# --- Test 3: eof on lexical handle ---
{
    my $tmpfile = make_temp_data_file("alpha\n", "beta\n", "gamma\n");
    test_io('lexical fh: eof detection in while loop', <<"PERL");
open(my \$fh, '<', '$tmpfile') or die;
my \@lines;
while (!eof(\$fh)) {
    my \$line = <\$fh>;
    chomp \$line;
    push \@lines, \$line;
}
close \$fh;
print join(',', \@lines), "\n";
PERL
}

# --- Test 4: <\$var> where \$var is undef (not a filehandle) ---
test_io('readline on undef handle returns undef gracefully', <<'PERL');
no warnings;
my $fh;   # not opened
my $result = readline($fh);
if (!defined($result)) {
    print "undef\n";
} else {
    print "got: $result\n";
}
PERL

# --- Test 5: Read all lines into array from lexical handle ---
{
    my $tmpfile = make_temp_data_file("one\n", "two\n", "three\n");
    test_io('lexical fh: read all lines into array via while loop', <<"PERL");
open(my \$fh, '<', '$tmpfile') or die;
my \@lines;
while (my \$line = <\$fh>) {
    chomp \$line;
    push \@lines, \$line;
}
close \$fh;
print scalar(\@lines), "\\n";
print join(':', \@lines), "\\n";
PERL
}

# --- Test 6: tell/seek on lexical handle ---
{
    my $tmpfile = make_temp_data_file("ABCDE\nFGHIJ\n");
    test_io('lexical fh: tell and seek', <<"PERL");
open(my \$fh, '<', '$tmpfile') or die;
my \$line1 = <\$fh>;
my \$pos = tell(\$fh);
my \$line2 = <\$fh>;
seek(\$fh, \$pos, 0);
my \$line2again = <\$fh>;
close \$fh;
chomp \$line1; chomp \$line2; chomp \$line2again;
print "\$line1\n";
print "\$line2\n";
print "\$line2again\n";
print \$line2 eq \$line2again ? "same\n" : "different\n";
PERL
}

# --- Test 7: binmode on lexical handle (must not crash) ---
{
    my $tmpfile = make_temp_data_file("data\n");
    test_io('lexical fh: binmode does not crash', <<"PERL");
open(my \$fh, '<', '$tmpfile') or die;
binmode(\$fh);
my \$line = <\$fh>;
close \$fh;
chomp \$line;
print "\$line\n";
PERL
}

# --- Test 8: lexical fh with 2-arg open ---
{
    my $tmpfile = make_temp_data_file("twoline\n");
    test_io('lexical fh: 2-arg open for reading', <<"PERL");
open(my \$fh, '<$tmpfile') or die;
my \$line = <\$fh>;
close \$fh;
chomp \$line;
print "\$line\n";
PERL
}

# --- Test 9: Reuse lexical \$fh variable for multiple opens ---
{
    my $tmpfile = make_temp_data_file("first\n");
    my $tmpfile2 = make_temp_data_file("second\n");
    test_io('lexical fh: reuse variable for second open', <<"PERL");
open(my \$fh, '<', '$tmpfile') or die;
my \$a = <\$fh>;
close \$fh;
open(\$fh, '<', '$tmpfile2') or die;
my \$b = <\$fh>;
close \$fh;
chomp \$a; chomp \$b;
print "\$a \$b\n";
PERL
}
