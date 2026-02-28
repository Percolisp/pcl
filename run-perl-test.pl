#!/usr/bin/env perl
#
# Run a Perl test file with PCL transpiler
#
# Usage: ./run-perl-test.pl <test.t>
#
# This script:
# 1. Copies the test file to perl-tests/ if not already there
# 2. Runs pl2cl from perl-tests/ directory so t/test.pl stub is found
# 3. Runs the generated CL with SBCL
#
# The perl-tests/t/test.pl stub provides function declarations for
# plan, is, ok, etc. - the actual implementations are in pcl-test.lisp

use strict;
use warnings;
use File::Temp qw(tempfile);
use File::Basename;
use File::Copy;
use File::Spec;
use Cwd qw(abs_path getcwd);

my $test_file = shift or die "Usage: $0 <test.t>\n";
die "Test file not found: $test_file\n" unless -f $test_file;

# Get absolute paths
my $project_root = abs_path(dirname($0));
my $pl2cl = "$project_root/pl2cl";
my $runtime = "$project_root/cl/pcl-runtime.lisp";
my $testlib = "$project_root/cl/pcl-test.lisp";
my $perl_tests_dir = "$project_root/perl-tests";

# Copy test file to perl-tests/ if it's not already there
my $test_basename = basename($test_file);
my $local_test = "$perl_tests_dir/$test_basename";

if (abs_path($test_file) ne abs_path($local_test)) {
    copy($test_file, $local_test) or die "Cannot copy $test_file to $local_test: $!\n";
    print "Copied $test_file to $local_test\n";
}

# Save current directory and change to perl-tests
my $original_dir = getcwd();
chdir $perl_tests_dir or die "Cannot chdir to $perl_tests_dir: $!\n";

print "=== Working in $perl_tests_dir ===\n";

# Transpile from perl-tests directory
# Need to add project root to Perl's @INC so pl2cl can find Pl/ modules
# Capture stderr separately to avoid mixing warnings into generated CL code
my ($err_fh, $err_file) = tempfile(SUFFIX => '.err', UNLINK => 1);
close $err_fh;
my $cl_code = `perl -I$project_root $pl2cl --no-cache $test_basename 2>$err_file`;

if ($? != 0) {
    my $errmsg = do { local $/; open my $f, '<', $err_file; $f ? <$f> : '' };
    print STDERR "Transpilation failed:\n$cl_code\n$errmsg\n";
    chdir $original_dir;
    exit 1;
}

# Write CL code to temp file
my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
print $cl_fh $cl_code;
close $cl_fh;

# Show the transpiled code for debugging
print "=== Transpiled to $cl_file ===\n";

# Run with SBCL
print "=== Running with SBCL ===\n";
my $output = `sbcl --noinform --non-interactive --load $runtime --load $testlib --load $cl_file 2>&1`;

# Return to original directory
chdir $original_dir;

# Filter SBCL noise
$output =~ s/^;.*\n//gm;
$output =~ s/^\s*\n//gm;
$output =~ s/PCL Runtime loaded\n?//g;
$output =~ s/STYLE-WARNING.*\n//g;

print $output;

# Check TAP output
my $pass = 0;
my $fail = 0;
while ($output =~ /^(not )?ok \d+/gm) {
    if ($1) { $fail++; } else { $pass++; }
}

print "\n=== Results ===\n";
print "Passed: $pass, Failed: $fail\n";

exit($fail > 0 ? 1 : 0);
