#!/usr/bin/env perl
# Tests for scalar tie implementation.
# Exercises TIESCALAR, FETCH, STORE, UNTIE, tied(), untie().

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

# Run a Perl file and return output
sub run_perl {
    my ($code) = @_;
    my ($fh, $file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;
    return `perl $file 2>&1`;
}

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
    $output =~ s/^-->.*\n//gm;
    $output =~ s/^==>.*\n//gm;
    $output =~ s/^PCL Runtime loaded\n//gm;
    $output =~ s/^WARNING:.*\n//gm;
    $output =~ s/^\s*\n//gm;
    return $output;
}

sub test_tie {
    my ($name, $code) = @_;
    my $perl_out = run_perl($code);
    my $cl_out   = run_cl($code);
    is($cl_out, $perl_out, $name) or diag("Perl: $perl_out\nCL:   $cl_out");
}

# ============ TESTS ============

# Basic FETCH
test_tie('tie basic FETCH', <<'PERL');
package Counter;
sub TIESCALAR { my ($class, $val) = @_; bless \$val, $class }
sub FETCH      { ${$_[0]} }
sub STORE      { ${$_[0]} = $_[1] }
package main;
tie my $x, 'Counter', 42;
print $x, "\n";
PERL

# STORE via assignment
test_tie('tie STORE via assignment', <<'PERL');
package Counter;
sub TIESCALAR { my ($class, $val) = @_; bless \$val, $class }
sub FETCH      { ${$_[0]} }
sub STORE      { ${$_[0]} = $_[1] }
package main;
tie my $x, 'Counter', 0;
$x = 99;
print $x, "\n";
PERL

# STORE call count (using hash-based object to avoid cross-package my vars)
test_tie('tie STORE call count', <<'PERL');
package Magic;
sub TIESCALAR { bless {val => $_[1], count => 0}, $_[0] }
sub FETCH      { $_[0]->{val} }
sub STORE      { $_[0]->{count}++; $_[0]->{val} = $_[1] }
package main;
tie my $x, 'Magic', 0;
$x = 1; $x = 2; $x = 3;
print tied($x)->{count}, "\n";
print "$x\n";
PERL

# tied() returns the tie object
test_tie('tied() returns tie object', <<'PERL');
package Obj;
sub TIESCALAR { bless {val => $_[1]}, $_[0] }
sub FETCH      { $_[0]->{val} }
sub STORE      { $_[0]->{val} = $_[1] }
package main;
tie my $x, 'Obj', 7;
my $obj = tied($x);
print ref($obj), "\n";
print $obj->{val}, "\n";
PERL

# tied() on untied var returns undef
test_tie('tied() on untied var is undef', <<'PERL');
my $x = 5;
my $t = tied($x);
print defined($t) ? "defined" : "undef", "\n";
PERL

# untie restores plain variable
test_tie('untie restores plain variable', <<'PERL');
package Loud;
sub TIESCALAR { bless \(my $v = $_[1]), $_[0] }
sub FETCH      { "fetched" }
sub STORE      { ${$_[0]} = $_[1] }
package main;
tie my $x, 'Loud', 0;
print "$x\n";
untie $x;
$x = 42;
print "$x\n";
PERL

# UNTIE is called on untie()
test_tie('UNTIE method called by untie()', <<'PERL');
package WithUntie;
sub TIESCALAR { bless \(my $v = 0), $_[0] }
sub FETCH      { ${$_[0]} }
sub STORE      { ${$_[0]} = $_[1] }
sub UNTIE      { print "UNTIE called\n" }
package main;
tie my $x, 'WithUntie';
untie $x;
PERL

# String interpolation triggers FETCH
test_tie('string interpolation calls FETCH', <<'PERL');
package Tval;
sub TIESCALAR { bless \(my $v = $_[1]), $_[0] }
sub FETCH      { "hello" }
sub STORE      { ${$_[0]} = $_[1] }
package main;
tie my $x, 'Tval', 0;
print "value is $x\n";
PERL

# ++ increments through FETCH/STORE
test_tie('++ on tied scalar', <<'PERL');
package Inc;
sub TIESCALAR { bless \(my $v = $_[1]), $_[0] }
sub FETCH      { ${$_[0]} }
sub STORE      { ${$_[0]} = $_[1] }
package main;
tie my $x, 'Inc', 10;
$x++;
print "$x\n";
PERL

# tie return value is the tie object
test_tie('tie() return value', <<'PERL');
package Ret;
sub TIESCALAR { bless {n => 99}, $_[0] }
sub FETCH      { $_[0]->{n} }
sub STORE      {}
package main;
my $obj = tie my $x, 'Ret';
print ref($obj), "\n";
print $obj->{n}, "\n";
PERL

# defined() on tied scalar uses FETCH (using instance var to avoid cross-package my)
test_tie('defined() on tied scalar', <<'PERL');
package MaybeUndef;
sub TIESCALAR { bless {give_undef => 0}, $_[0] }
sub FETCH      { $_[0]->{give_undef} ? undef : 1 }
sub STORE      {}
package main;
tie my $x, 'MaybeUndef';
print defined($x) ? "defined" : "undef", "\n";
tied($x)->{give_undef} = 1;
print defined($x) ? "defined" : "undef", "\n";
PERL

# tie with extra constructor args (clamping example)
test_tie('tie passes extra args to TIESCALAR', <<'PERL');
package Clamp;
sub TIESCALAR {
    my ($class, $min, $max) = @_;
    bless {min => $min, max => $max, val => $min}, $class;
}
sub FETCH { $_[0]->{val} }
sub STORE {
    my ($self, $val) = @_;
    $val = $self->{min} if $val < $self->{min};
    $val = $self->{max} if $val > $self->{max};
    $self->{val} = $val;
}
package main;
tie my $x, 'Clamp', 0, 10;
$x = 5;  print "$x\n";
$x = 20; print "$x\n";
$x = -3; print "$x\n";
PERL

# tied() after untie is undef
test_tie('tied() after untie is undef', <<'PERL');
package Simple;
sub TIESCALAR { bless \(my $v = 0), $_[0] }
sub FETCH      { ${$_[0]} }
sub STORE      { ${$_[0]} = $_[1] }
package main;
tie my $x, 'Simple';
untie $x;
print defined(tied($x)) ? "tied" : "untied", "\n";
PERL

# FETCH in numeric context
test_tie('FETCH in numeric context', <<'PERL');
package Num;
sub TIESCALAR { bless \(my $v = $_[1]), $_[0] }
sub FETCH      { ${$_[0]} }
sub STORE      { ${$_[0]} = $_[1] }
package main;
tie my $x, 'Num', 21;
my $y = $x * 2;
print "$y\n";
PERL

# STORE receives the correct value
test_tie('STORE receives assigned value', <<'PERL');
package Spy;
sub TIESCALAR { bless {val => '', last => undef}, $_[0] }
sub FETCH      { $_[0]->{val} }
sub STORE      { $_[0]->{last} = $_[1]; $_[0]->{val} = $_[1] }
package main;
tie my $x, 'Spy';
$x = "hello";
print "stored:", tied($x)->{last}, "\n";
PERL

done_testing;
