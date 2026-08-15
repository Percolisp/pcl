#!/usr/bin/env perl
# refaliasing-01.t — `use feature 'refaliasing' / 'declared_refs'`: a \-cast in
# LVALUE position is an ALIAS, not a value write (task #325).
#
# Before this existed, `\$x = \$y` emitted (p-setf (p-backslash $x) …), whose
# place was a FRESH ref box — so the write landed in a temporary and vanished.
# Four of the six spellings were SILENT WRONG that way; the other two were hard
# refusals.  The fix is one arm in p-setf's place dispatch: a \-cast place
# rebinds the NAME'S STORAGE to the right-hand referent, which in PCL's model
# is exactly "both names now hold the same box / vector / hash-table".
#
# Every expectation here was probed against real perl 5.40.3 first.
#
# The INVERSE rows matter as much as the positive ones: `my $x = \$y` must stay
# an ordinary reference (a \-cast in RVALUE position is untouched), and a
# right-hand side of the wrong kind must still die perl's death.

use v5.30;
use strict;
use warnings;
use Test::More;
use File::Temp qw(tempfile);
use FindBin qw($RealBin);
use lib $RealBin;
use PCLCore;

my $project_root = "$RealBin/../..";
my $pl2cl        = "$project_root/pl2cl";
my $runtime      = "$project_root/cl/pcl-runtime.lisp";
my @sbcl_rt = PCLCore::sbcl_prefix($runtime);

plan skip_all => "pl2cl not found" unless -x $pl2cl;
plan skip_all => "sbcl not found"  unless `which sbcl 2>/dev/null`;

plan tests => 16;

my $PREAMBLE = "use feature 'refaliasing', 'declared_refs';\n"
             . "no warnings 'experimental';\n";

sub run_cl {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $PREAMBLE, $code;
    close $fh;
    my $cl_code = `$pl2cl $pl_file 2>/dev/null`;
    my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cl_fh $cl_code;
    close $cl_fh;
    my $output = `sbcl @sbcl_rt --load $cl_file 2>&1`;
    $output =~ s/^;.*\n//gm;
    $output =~ s/^PCL Runtime loaded\n//gm;
    $output =~ s/^\s*\n//gm;
    return $output;
}

sub test_cl {
    my ($name, $code, $expected) = @_;
    is(run_cl($code), $expected, $name);
}

sub transpile {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $PREAMBLE, $code;
    close $fh;
    return scalar `$pl2cl $pl_file 2>/dev/null`;
}

# --- codegen: the \-cast place, and the one-element \(…) lvalue -------------

like(transpile('my $y; my $x; \$x = \$y;'),
    qr/\(p-setf \(p-backslash \$x\) /,
    'a \-cast LVALUE reaches p-setf as a (p-backslash …) place');
# `\($x) = LIST` is a LIST assignment even with one element (perl gives it list
# context); the rvalue shortcut \(scalar) == \scalar must not reach an lvalue.
like(transpile('my $x; my @l; \($x) = @l;'),
    qr/\(p-list-= \(vector \(p-backslash \$x\)\)/,
    '\($x) = LIST keeps its list-ness as an lvalue');

# --- scalars ---------------------------------------------------------------

test_cl('\$x = \$y aliases (write through either name)',
    q{my $y = 5; my $x; \$x = \$y; $x = 7; print "$y $x\n";}, "7 7\n");
test_cl('\my $x = \$y aliases at the declaration',
    q{my $y = 5; \my $x = \$y; $x = 7; print "$y $x\n";}, "7 7\n");
test_cl('an alias makes the two refs IDENTICAL',
    q{my $y; \my $x = \$y; print \$x == \$y ? "same\n" : "different\n";},
    "same\n");
# The right-hand side may be a VARIABLE holding the reference, which sits one
# box deeper than the \-wrapper — is-ref is what tells the two apart.
test_cl('alias through a variable that holds the ref',
    q{my $z = 3; my $r = \$z; \my $al = $r; $al = 7; print "$z\n";}, "7\n");
test_cl('alias through a ref-to-ref does not peel too far',
    q{my $w = 1; my $rr = \\\\$w; \my $x = $$rr; $x = 5; print "$w\n";}, "5\n");

# --- aggregates ------------------------------------------------------------

test_cl('\my @b = \@a aliases the array',
    q{my @a = (1,2); \my @b = \@a; push @b, 3; print "@a\n";}, "1 2 3\n");
test_cl('\my %g = \%h aliases the hash',
    q{my %h = (k=>1); \my %g = \%h; $g{j} = 2;
      print join(",", map {"$_=$h{$_}"} sort keys %h), "\n";}, "j=2,k=1\n");

# --- package variables, and the `our \$T` declaration ----------------------

test_cl('our \$T = \$::TODO declares the cell and aliases it',
    q{our $TODO = "orig"; our \$T = \$::TODO; $T = "tv"; print "$::TODO\n";},
    "tv\n");

# --- container slots -------------------------------------------------------

test_cl('\$h{k} = \$v aliases the hash SLOT',
    q{my $v = 5; my %h; \$h{k} = \$v; $h{k} = 8; print "$v\n";}, "8\n");
test_cl('\$a[i] = \$v aliases the array SLOT',
    q{my $v = 5; my @a; \$a[0] = \$v; $a[0] = 11; print "$v\n";}, "11\n");

# --- list forms ------------------------------------------------------------

test_cl('(\$x) = @list aliases through a list assignment',
    q{my $t = 1; @_ = \$t; my $x; (\$x) = @_; $x = 9; print "$t\n";}, "9\n");
test_cl('\(my $p) = @list aliases the fresh lexical',
    q{my $t = 1; @_ = \$t; \(my $p) = @_; $p = 9; print "$t\n";}, "9\n");

# --- INVERSE: a \-cast in RVALUE position is untouched ---------------------

test_cl('my $r = \$y is still a REFERENCE, not an alias',
    q{my $y = 5; my $r = \$y; $r = 7; print "$y $r\n";}, "5 7\n");
# A wrong-kind right-hand side must die perl's death rather than alias anything.
like(run_cl(q{my $x; my @a; eval { \$x = \@a; 1 } or print "died\n";}),
    qr/died/, 'aliasing a scalar to an ARRAY ref dies');
