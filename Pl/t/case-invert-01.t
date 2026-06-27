#!/usr/bin/env perl
# Regression tests for case-sensitive identifiers under the :invert readtable.
#
# Perl identifiers are case-sensitive; PCL reads its generated CL under
# (readtable-case :invert) so that names differing only in case map to distinct
# CL symbols.  Every place the RUNTIME builds a CL symbol from a string — or
# reconstructs a Perl name from a CL symbol — must apply the same invert
# transform.  These tests lock in the spots that were broken and fixed:
#   * case-distinct variables / subs / packages
#   * loop/block labels (uniform-upper, mixed, lowercase)
#   * caller()[3] sub-name case
#   * AUTOLOAD dispatch (mixed-case symbol pl-AUTOLOAD)
#   * symbol-table (stash) key case
#   * Exporter/glob CODE-slot install of an UPPERCASE-named sub/constant
#   * method dispatch to an UPPERCASE method name (tie-style)
# Each case is checked DIFFERENTIALLY against real perl.

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

# Differential: run the snippet through real perl and through PCL, compare.
sub test_case {
    my ($name, $code) = @_;
    my ($fh, $file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;
    my $perl_out = `perl $file 2>&1`;
    my $cl_out   = run_cl($code);
    is($cl_out, $perl_out, $name) or diag("Perl: [$perl_out]\nCL:   [$cl_out]");
}

# --- case-distinct variables and subs ---------------------------------------
test_case('case-distinct lexical vars do not collide',
    'my $base_len = 1; my $BASE_LEN = 2; print "$base_len $BASE_LEN\n";');

test_case('case-distinct subs do not collide',
    'sub foo { 1 } sub FOO { 2 } print foo(), FOO(), "\n";');

test_case('case-distinct package globals do not collide',
    'our $val = "lo"; our $VAL = "UP"; print "$main::val $main::VAL\n";');

# --- case-distinct packages / methods ---------------------------------------
test_case('packages and methods differing only in case dispatch correctly',
    'package Aa; sub new { bless {}, shift } sub who { "Aa" }'
  . ' package AA; sub new { bless {}, shift } sub who { "AA" }'
  . ' package main; print Aa->new->who, AA->new->who, "\n";');

# --- loop / block labels in every case --------------------------------------
test_case('uniform-uppercase label (SKIP) last works',
    'SKIP: { print "a"; last SKIP; print "b"; } print "c\n";');

test_case('mixed-case label last works',
    'MyB: { print "a"; last MyB; print "b"; } print "c\n";');

test_case('lowercase label last works',
    'lbl: { print "a"; last lbl; print "b"; } print "c\n";');

test_case('labeled loop next/last with uppercase label',
    'OUTER: for my $i (1..3) { for my $j (1..3) { next OUTER if $j == 2; print "$i$j "; } } print "\n";');

# --- caller()[3] sub-name case ----------------------------------------------
test_case('caller sub-name preserves case (lowercase, mixed, upper)',
    'sub low { (caller(0))[3] } sub Mix { (caller(0))[3] } sub UP { (caller(0))[3] }'
  . ' print low(), "\n", Mix(), "\n", UP(), "\n";');

# --- AUTOLOAD dispatch ------------------------------------------------------
test_case('AUTOLOAD dispatch for lowercase and mixed-case method names',
    'package P; sub new { bless {}, shift } our $AUTOLOAD;'
  . ' sub AUTOLOAD { my $n = $AUTOLOAD; $n =~ s/.*:://; return "auto:$n"; }'
  . ' sub DESTROY {} package main; my $p = P->new;'
  . ' print $p->foo, " ", $p->BarBaz, "\n";');

# --- symbol-table (stash) key case ------------------------------------------
test_case('stash keys keep original sub-name case',
    'package Foo; sub Bar { 1 } sub baz { 2 } sub QUUX { 3 } package main;'
  . ' for my $k (sort keys %Foo::) { print "$k\n" if defined &{"Foo::$k"}; }');

# --- glob CODE-slot install of an UPPERCASE-named sub -----------------------
test_case('glob CODE-slot install of an uppercase-named sub is callable',
    'no strict "refs"; *{"main::SEEK_SET"} = sub { 7 }; print SEEK_SET(), "\n";');

# --- tie: dispatch to ALL-UPPERCASE methods ---------------------------------
test_case('tie dispatches to uppercase TIESCALAR/FETCH/STORE',
    'package Counter;'
  . ' sub TIESCALAR { my $c = shift; my $v = 0; bless \$v, $c }'
  . ' sub FETCH { my $s = shift; $$s }'
  . ' sub STORE { my ($s, $v) = @_; $$s = $v * 2 }'
  . ' package main; tie my $x, "Counter"; $x = 21; print "$x\n";');

# --- \u/\l composed with \U/\L case-span escapes (op/lex_assign.t test 1) -----
# \u\L$x and \L\u$x both mean ucfirst(lc($x)) — the one-shot \u/\l applies to the
# first character of the span's OUTPUT, not the first element inside it (which
# would give lc(ucfirst($x)), with the span's lc overriding the ucfirst).
test_case('backslash-u backslash-L composes as ucfirst(lc(...))',
    'my $a = "AB"; print "[", "\u\L$a", "]\n";');
test_case('backslash-L backslash-u composes as ucfirst(lc(...))',
    'my $a = "AB"; print "[", "\L\u$a", "]\n";');
test_case('backslash-u backslash-L with literal text and trailing E',
    'print "[", "\u\LJOHN\E SMITH", "]\n";');
test_case('backslash-l backslash-U composes as lcfirst(uc(...))',
    'my $a = "ab"; print "[", "\l\U$a", "]\n";');
test_case('plain backslash-u and U-span unaffected',
    'my $a = "hELLo"; print "[", "\u$a", "][", "\U$a\E-x", "]\n";');

done_testing();
