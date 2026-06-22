#!/usr/bin/env perl

# Regression tests for two bugs found while getting Data::Dumper to run
# (session after s264):
#
#  1. local($ref->{key}) = EXPR  (parenthesized list-form local on a
#     subscripted lvalue) used to clobber the base scalar instead of
#     localizing the element.
#
#  2. A BEGIN block inside an expression-level do{} that sits in an elsif
#     CONDITION, inside a named sub of a non-main package, used to hoist the
#     BEGIN into the middle of the generated p-if chain ("too many elements
#     in p-if").

use strict;
use warnings;
use Test::More;
use File::Temp qw(tempfile);

my $pl2cl = './pl2cl';
my $runtime = 'cl/pcl-runtime.lisp';

plan skip_all => "pl2cl not found" unless -x $pl2cl;
plan skip_all => "sbcl not found"  unless `which sbcl 2>/dev/null`;

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
  $output =~ s/^;.*\n//gm;
  $output =~ s/PCL Runtime loaded\n?//g;
  unlink $pl_file, $cl_file;
  return $output;
}

plan tests => 5;

# 1. local($ref->{key}) = EXPR localizes the element and restores it,
#    WITHOUT clobbering the base hashref.
{
  my $out = run_pcl(<<'CODE');
my $s = { apad => "x", other => "keep" };
{
  local($s->{apad}) = "new";
  print "in:", (ref($s) ? "H" : "N"), ":", $s->{apad}, ":", $s->{other}, "\n";
}
print "out:", $s->{apad}, ":", $s->{other}, "\n";
CODE
  like($out, qr/in:H:new:keep/, 'local($ref->{k}) keeps $s a hashref inside');
  like($out, qr/out:x:keep/,    'local($ref->{k}) restores the element after');
}

# 2. local($hash{key}) = EXPR (direct hash element, paren form)
{
  my $out = run_pcl(<<'CODE');
our %h = (a => 1, b => 2);
{
  local($h{a}) = 99;
  print "in:", $h{a}, ":", $h{b}, "\n";
}
print "out:", $h{a}, ":", $h{b}, "\n";
CODE
  like($out, qr/in:99:2.*out:1:2/s, 'local($h{k}) paren form localizes element');
}

# 3. BEGIN inside do{} in an elsif condition, inside a packaged named sub.
{
  my $out = run_pcl(<<'CODE');
package Foo;
use constant SC => 1;
sub classify {
  my ($val) = @_;
  my $out = "";
  my $type = ref $val;
  if ($type) { $out = "ref"; }
  elsif (!defined($val)) {
    $out .= "undef";
  }
  elsif (SC && do {
    BEGIN { SC and warnings->unimport("experimental::builtin") }
    $val > 3
  }) {
    $out .= "big";
  }
  else {
    $out .= "small";
  }
  return $out;
}
package main;
print Foo::classify(undef), "\n";
print Foo::classify(5), "\n";
print Foo::classify(1), "\n";
CODE
  like($out, qr/undef\nbig\nsmall/, 'BEGIN in do{} in elsif condition compiles & runs');
}

# 4. Data::Dumper end-to-end (it stresses all of the above plus the XSLoader
#    pure-Perl fallback).  Skip gracefully if the core module is unavailable.
SKIP: {
  skip "Data::Dumper not installed", 1
    unless eval { require Data::Dumper; 1 };
  my $out = run_pcl(<<'CODE');
use Data::Dumper;
$Data::Dumper::Sortkeys = 1;
$Data::Dumper::Indent = 0;
print Dumper({ a => 1, b => [2,3] }), "\n";
CODE
  like($out, qr/\$VAR1 = \{'a' => 1,'b' => \[2,3\]\};/,
       'Data::Dumper runs via pure-Perl fallback');
}
