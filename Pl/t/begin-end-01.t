#!/usr/bin/env perl

# Tests for BEGIN and END blocks
# BEGIN blocks run at compile/load time (via eval-when)
# END blocks run at program exit (via *end-blocks*)

use strict;
use warnings;
use Test::More;
use File::Temp qw(tempfile);

my $pl2cl = './pl2cl';
my $runtime = 'cl/pcl-runtime.lisp';

# Helper to run transpiled code and capture output
sub run_pcl {
  my ($code) = @_;

  my ($fh, $pl_file) = tempfile(SUFFIX => '.pl');
  print $fh $code;
  close $fh;

  my $cl_code = `$pl2cl --no-cache $pl_file 2>&1`;

  my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp');
  print $cl_fh $cl_code;
  close $cl_fh;

  my $output = `sbcl --noinform --non-interactive --load $runtime --load $cl_file 2>&1`;

  # Filter SBCL noise
  $output =~ s/^;.*\n//gm;
  $output =~ s/^\s*\n//gm;
  $output =~ s/PCL Runtime loaded\n?//g;

  unlink $pl_file, $cl_file;

  return $output;
}

# Test: Single BEGIN block
{
  my $output = run_pcl(<<'END_CODE');
BEGIN { print "start "; }
print "middle ";
END_CODE

  like($output, qr/start.*middle/s, 'BEGIN runs before main code');
}

# Test: Multiple BEGIN blocks run in order
{
  my $output = run_pcl(<<'END_CODE');
BEGIN { print "1 "; }
BEGIN { print "2 "; }
print "3 ";
END_CODE

  like($output, qr/1.*2.*3/s, 'multiple BEGIN blocks run in order');
}

# Test: BEGIN sets variable used later
{
  my $output = run_pcl(<<'END_CODE');
my $x;
BEGIN { $x = 42; }
print $x;
END_CODE

  like($output, qr/42/, 'BEGIN can set variables used by main code');
}

# Test: Single END block
{
  my $output = run_pcl(<<'END_CODE');
print "main ";
END { print "end"; }
END_CODE

  like($output, qr/main.*end/s, 'END runs after main code');
}

# Test: Multiple END blocks run in reverse order
{
  my $output = run_pcl(<<'END_CODE');
END { print "3"; }
END { print "2"; }
END { print "1"; }
print "main ";
END_CODE

  like($output, qr/main.*1.*2.*3/s, 'multiple END blocks run in reverse order');
}

# Test: BEGIN and END together
{
  my $output = run_pcl(<<'END_CODE');
BEGIN { print "B "; }
END { print " E"; }
print "M";
END_CODE

  like($output, qr/B.*M.*E/s, 'BEGIN runs first, END runs last');
}

# Test: BEGIN runs before runtime code even when declared after it
# PCL phase-2 reordering moves eval-when forms (including BEGIN blocks)
# before runtime forms, so BEGIN always runs first regardless of source order.
{
  my $output = run_pcl(<<'END_CODE');
print "code ";
BEGIN { print "begin "; }
END_CODE

  # Phase-2 reordering: BEGIN eval-when is classified as compile-time,
  # so it moves before the print "code" runtime form.
  like($output, qr/begin.*code/s, 'BEGIN runs before later runtime code');
}

# Test: Generated code structure for BEGIN
{
  my $code = 'BEGIN { my $x = 1; }';
  my ($fh, $pl_file) = tempfile(SUFFIX => '.pl');
  print $fh $code;
  close $fh;

  my $cl_code = `$pl2cl $pl_file 2>&1`;

  like($cl_code, qr/eval-when.*:compile-toplevel.*:execute/s,
       'BEGIN generates eval-when with compile-toplevel and execute');

  unlink $pl_file;
}

# Test: Generated code structure for END
{
  my $code = 'END { my $x = 1; }';
  my ($fh, $pl_file) = tempfile(SUFFIX => '.pl');
  print $fh $code;
  close $fh;

  my $cl_code = `$pl2cl $pl_file 2>&1`;

  like($cl_code, qr/push.*lambda.*\*end-blocks\*/s,
       'END generates push to *end-blocks*');

  unlink $pl_file;
}

# Test: BEGIN with built-in function call
{
  my $output = run_pcl(<<'END_CODE');
my $x;
BEGIN { $x = "hello"; }
print "$x world";
END_CODE

  like($output, qr/hello.*world/s, 'BEGIN can set variables');
}

# Test: BEGIN can call subs defined before it
# This works because subs are wrapped in eval-when via pl-sub macro
{
  my $output = run_pcl(<<'END_CODE');
sub greet { print "hello "; }
BEGIN { greet(); }
print "world";
END_CODE

  like($output, qr/hello.*world/s, 'BEGIN can call subs defined before it');
}

# Test: BEGIN can call subs with arguments
{
  my $output = run_pcl(<<'END_CODE');
sub double { my ($n) = @_; return $n * 2; }
my $result;
BEGIN { $result = double(21); }
print $result;
END_CODE

  like($output, qr/42/, 'BEGIN can call subs with arguments');
}

# Test: BEGIN can access 'our' variables defined before it
{
  my $output = run_pcl(<<'END_CODE');
our $config = "default";
BEGIN { $config = "from_begin"; }
print $config;
END_CODE

  # In interpreted mode: init runs first (default), then BEGIN (from_begin)
  # Result: from_begin
  like($output, qr/from_begin/, 'BEGIN can modify our variables');
}

# Test: Multiple subs before BEGIN, all accessible
{
  my $output = run_pcl(<<'END_CODE');
sub a { print "A"; }
sub b { print "B"; }
sub c { print "C"; }
BEGIN { a(); b(); c(); }
END_CODE

  like($output, qr/ABC/, 'BEGIN can call multiple subs defined before it');
}

# Test: BEGIN with nested sub calls
{
  my $output = run_pcl(<<'END_CODE');
sub inner { return "inner"; }
sub outer { return inner() . "-outer"; }
my $x;
BEGIN { $x = outer(); }
print $x;
END_CODE

  like($output, qr/inner-outer/, 'BEGIN can use nested sub calls');
}

# Test: Generated code for pl-sub uses eval-when
{
  my $code = 'sub foo { 1 }';
  my ($fh, $pl_file) = tempfile(SUFFIX => '.pl');
  print $fh $code;
  close $fh;

  my $cl_code = `$pl2cl $pl_file 2>&1`;

  like($cl_code, qr/pl-sub.*pl-foo/s,
       'sub generates pl-sub macro call');

  unlink $pl_file;
}

# ============================================================
# Corner Cases
# ============================================================

# Test: BEGIN populates array (no runtime re-initialization)
# Using 'our' without initializer so BEGIN's work is not overwritten at runtime.
{
  my $output = run_pcl(<<'END_CODE');
our @data;
BEGIN { push @data, 1, 2, 3, 4; }
print join(",", @data);
END_CODE

  like($output, qr/1,2,3,4/, 'BEGIN can populate array before runtime code');
}

# Test: BEGIN populates hash (no runtime re-initialization)
{
  my $output = run_pcl(<<'END_CODE');
our %config;
BEGIN { $config{a} = 1; $config{b} = 2; }
print $config{a}, $config{b};
END_CODE

  like($output, qr/12/, 'BEGIN can populate hash before runtime code');
}

# Test: Multiple BEGINs with dependencies (second uses result of first)
# Using 'our' without runtime initializer so BEGIN results survive.
{
  my $output = run_pcl(<<'END_CODE');
our $x;
BEGIN { $x = 10; }
BEGIN { $x = $x * 2; }
print $x;
END_CODE

  like($output, qr/20/, 'Multiple BEGINs can build on each other');
}

# Test: BEGIN using constant defined before it
{
  my $output = run_pcl(<<'END_CODE');
use constant MULTIPLIER => 5;
my $result;
BEGIN { $result = MULTIPLIER * 3; }
print $result;
END_CODE

  like($output, qr/15/, 'BEGIN can use constant defined before it');
}

# Test: Sub in BEGIN calls another sub (mutual dependency)
{
  my $output = run_pcl(<<'END_CODE');
sub double { return $_[0] * 2; }
sub quad { return double(double($_[0])); }
my $x;
BEGIN { $x = quad(5); }
print $x;
END_CODE

  like($output, qr/20/, 'BEGIN with nested sub calls works');
}

# Test: END block can access variable set by main code
{
  my $output = run_pcl(<<'END_CODE');
my $final = "not set";
END { print "final=$final"; }
$final = "was set";
END_CODE

  like($output, qr/final=was set/, 'END block sees variable set by main code');
}

# Test: Multiple END blocks share state
{
  my $output = run_pcl(<<'END_CODE');
my $count = 0;
END { print "count=$count "; }
END { $count++; }
END { $count++; }
END_CODE

  # ENDs run in reverse order: increment, increment, print
  like($output, qr/count=2/, 'Multiple END blocks share state');
}

# Test: BEGIN with package variable from different package
{
  my $output = run_pcl(<<'END_CODE');
package Config;
our $value = 100;

package main;
my $result;
BEGIN { $result = $Config::value; }
print $result;
END_CODE

  like($output, qr/100/, 'BEGIN can access package variable from other package');
}

# Test: Sub defined in BEGIN is callable later
{
  my $output = run_pcl(<<'END_CODE');
BEGIN {
  sub greet { print "Hello "; }
}
greet();
print "World";
END_CODE

  like($output, qr/Hello.*World/s, 'Sub defined in BEGIN is callable in main code');
}

# Test: BEGIN with complex expression
{
  my $output = run_pcl(<<'END_CODE');
sub add { return $_[0] + $_[1]; }
sub mul { return $_[0] * $_[1]; }
my $x;
BEGIN { $x = add(mul(2, 3), mul(4, 5)); }
print $x;
END_CODE

  # 2*3 + 4*5 = 6 + 20 = 26
  like($output, qr/26/, 'BEGIN with complex nested function calls');
}

done_testing();
