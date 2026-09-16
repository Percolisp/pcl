#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# tie-fetch-once-01.t — guard for task #1813: AN OPERATOR READS ITS OPERAND
# ONCE.
#
# perl calls a tied scalar's FETCH exactly once per operator.  An operator that
# first INSPECTS its operand — to pick a string arm over a numeric one, or to
# find the old value of a postfix `++` — and then COERCES the same PLACE again
# with to-string / to-number is reading it TWICE, and for a tie that second
# read is a second FETCH with the program's side effects in it.  #1430 fixed
# one member of this family (the range operator); these were the rest:
#
#     unary -   x   post ++   post --   &   ^   |   ~   length
#
# all measured at 2 FETCHes against perl's 1 (t/op/tie_fetch_count.t rows 4,
# 13, 20, 21, 38, 39, 40, 41).  The fix is `%p-read-operand`, which hands the
# coercion the value the inspection already produced — except for a box that
# carries a bless class, the is-ref flag, or a cached numeric half, where the
# CONTAINER is what must be coerced.  Rows 10-14 below are that exception and
# are the cases the fix would break if it were spelled as a plain `unbox`.
#
# The expectations are perl 5.40.3's own output for the same two programs.
#
# Both programs run in ONE SBCL launch each — a Pl/t file's cost is its wall
# time, not its row count.

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

plan tests => 15;

sub run_pcl {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;
    my $cl_code = `$pl2cl $pl_file 2>/dev/null`;
    my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cl_fh $cl_code;
    close $cl_fh;
    my $out = `sbcl @sbcl_rt --load $cl_file 2>&1`;
    $out =~ s/^;.*\n//gm;
    $out =~ s/^PCL Runtime loaded\n//gm;
    return $out;
}

# ---------------------------------------------------------------- program 1
# The FETCH counts themselves.  Every row is 1 under perl 5.40.3.
my $COUNTS = <<'PERL';
my $count = 0;
sub TIESCALAR { my $pack = shift; bless [@_], $pack }
sub FETCH { $count++; ${$_[0]}[0] }
sub STORE { ${$_[0]}[0] = $_[1] }
sub chk { print "$_[0]=$count\n"; $count = 0 }
my $dummy;
tie my $var => 'main', 1;
$dummy = -$var    ; chk 'neg';
$dummy = $var x 1 ; chk 'x';
$dummy = $var++   ; chk 'postinc';
$dummy = $var--   ; chk 'postdec';
$dummy = $var & 1 ; chk 'and';
$dummy = $var ^ 1 ; chk 'xor';
$dummy = $var | 1 ; chk 'or';
$dummy = ~$var    ; chk 'not';
tie my $s => 'main', "abc";
$dummy = length $s; chk 'length';
PERL

my $counts = run_pcl($COUNTS);
for my $op (qw(neg x postinc postdec and xor or not length)) {
    like($counts, qr/^\Q$op\E=1$/m, "FETCH called just once using '$op'")
        or diag("counts output:\n$counts");
}

# ---------------------------------------------------------------- program 2
# THE CASES THE NARROWING PROTECTS.  A plain `unbox` before the coercion would
# answer each of these wrongly, because the identity these operands coerce
# through lives on the CONTAINER, not in the value:
#   an overloaded object (`""` / `0+` handlers live on the box);
#   a dualvar (`$!` numifies to the errno, not by re-reading its string);
#   a reference (perl numifies one to the referent's ADDRESS, and PCL's
#   address comes from the box — so two distinct arrays must differ).
my $KEEP = <<'PERL';
package Str;
use overload '""' => sub { "STR" }, '0+' => sub { 7 }, fallback => 1;
sub new { bless {}, shift }
package main;
no warnings;
my $o = Str->new;
print "ov-neg=",  (-$o),        "\n";
print "ov-x=",    ($o x 2),     "\n";
print "ov-len=",  length($o),   "\n";
print "ov-and=",  ($o & 255),   "\n";
$! = 2;
my $e = $!;
print "dual-and=", ($e & 255),  "\n";
print "ref-not=",  ((~ [1]) == (~ [1]) ? "same" : "diff"), "\n";
PERL

my $keep = run_pcl($KEEP);
like($keep, qr/^ov-neg=-7$/m,    'unary - on an overloaded object numifies through 0+');
like($keep, qr/^ov-x=STRSTR$/m,  'x on an overloaded object repeats its "" string');
like($keep, qr/^ov-len=3$/m,     'length of an overloaded object measures its "" string');
like($keep, qr/^ov-and=7$/m,     '& on an overloaded object numifies through 0+');
like($keep, qr/^dual-and=2$/m,   '& on a dualvar uses its cached numeric half');
like($keep, qr/^ref-not=diff$/m, '~ of two distinct refs gives two distinct addresses');
