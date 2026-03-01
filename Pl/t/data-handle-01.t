#!/usr/bin/env perl
# Tests for __DATA__ and __END__ filehandle support.

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

# Transpile and run CL, return output
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

sub test_data {
    my ($name, $code) = @_;
    my ($fh, $file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;
    my $perl_out = `perl $file 2>&1`;
    my $cl_out   = run_cl($code);
    is($cl_out, $perl_out, $name) or diag("Perl: $perl_out\nCL:   $cl_out");
}

plan tests => 7;

# Basic __DATA__ readline
test_data('__DATA__: single line read', <<'PERL');
my $line = <DATA>;
print $line;
__DATA__
hello
PERL

# Multiple reads from __DATA__
test_data('__DATA__: multiple readline calls', <<'PERL');
my $line1 = <DATA>;
my $line2 = <DATA>;
chomp $line1;
chomp $line2;
print "$line1 $line2\n";
__DATA__
foo
bar
PERL

# __DATA__ with while loop
test_data('__DATA__: while loop over all lines', <<'PERL');
my @lines;
while (my $line = <DATA>) {
    chomp $line;
    push @lines, $line;
}
print join(',', @lines), "\n";
__DATA__
alpha
beta
gamma
PERL

# __DATA__ with eof detection
test_data('__DATA__: eof at end of data', <<'PERL');
my @lines;
while (!eof(DATA)) {
    my $line = <DATA>;
    chomp $line;
    push @lines, $line;
}
print scalar(@lines), "\n";
__DATA__
one
two
three
PERL

# __END__ works the same as __DATA__
test_data('__END__: readline from DATA handle', <<'PERL');
my $line = <DATA>;
chomp $line;
print "got: $line\n";
__END__
end-data
PERL

# Read all DATA lines via while loop into array, then process
test_data('__DATA__: read all lines via while loop', <<'PERL');
my @lines;
while (my $line = <DATA>) {
    chomp $line;
    push @lines, $line;
}
print scalar(@lines), "\n";
print join('|', @lines), "\n";
__DATA__
foo
bar
baz
PERL

# Chomp in while loop over __DATA__
test_data('__DATA__: chomp in while loop', <<'PERL');
my @out;
while (my $line = <DATA>) {
    chomp $line;
    push @out, uc($line);
}
print join(',', @out), "\n";
__DATA__
apple
banana
cherry
PERL
