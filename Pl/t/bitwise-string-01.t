#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# bitwise-string-01.t — guard for task #1028: the string-vs-numeric MODE
# DECISION of `&` `|` `^` `~` (perlop, "Bitwise String Operators").
#
# perl asks ONE question of the two operands — does either carry a NUMBER
# (SvNIOKp)?  If not, `do_vop' STRINGIFIES both and operates byte by byte.  A
# reference, glob, qr//, blessed object or undef carries neither a number nor
# a string body, so it goes to the STRING side and stringifies into it.  PCL
# used to send every NON-string state to the NUMERIC side, so `undef | "abc"'
# was 0 where perl says "abc" and `[1] | "\0"x40' was 1 where perl says
# "ARRAY(0x…)" — 229 blessed perl-tests/bop.t rows, the largest single cluster
# in the sweep baseline, behind one predicate.
#
# UNARY `~' IS A DIFFERENT RULE and rows 22–29 pin the difference: `~ [1]' is
# the complemented ADDRESS (numeric) while `[1] | "x"' is the string op,
# because `pp_complement' takes its string branch only for an SV that HAS a
# PV, and a reference has neither.  Rows 32–35 pin the over-0xFF fatal, which
# belongs INSIDE the string path — `5 | "\x{100}"' is the numeric op and must
# NOT die.
#
# Every expectation below is the OUTPUT OF THE SAME PROGRAM under perl 5.40.3
# (scratch/guard-prog.pl), not a hand-derivation.
#
# The rows run in TWO SBCL launches, one per emission path, rather than one
# per assertion — a Pl/t file's cost is its wall time, not its row count.

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

plan tests => 74;

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

# One program, many tagged rows.  A byte-string answer is printed as hex so a
# NUL or a high byte survives the comparison as text; an answer whose text
# embeds a heap address is reduced to its FIRST character, which is exactly
# what perl-tests/bop.t's own `is_first' asks of the same shapes.
my $PROG = <<'PERL';
no warnings;
sub h { join('', map { sprintf "%02x", ord } split //, ($_[0] // '')) }
sub first { substr($_[0], 0, 1) }
my $u;
my ($u1, $u2);
our $foo = 1;
my $r  = [1];
my $sr = \1;
my $hr = {};
my $qr = qr/x/;
my $gl = *foo;
my $gr = \*foo;
my $ob = bless {}, 'K';
my $cr = sub { 1 };
my $pad = "\0" x 40;
print "01=", ($u | "abc"), "\n";
print "02=", h($u & "abc"), "\n";
print "03=", ($u ^ "abc"), "\n";
print "04=", first($r  | $pad), "\n";
print "05=", first($gl | $pad), "\n";
print "06=", first($qr | $pad), "\n";
print "07=", first($ob | $pad), "\n";
print "08=", first($sr | $pad), "\n";
print "09=", first($cr | $pad), "\n";
print "10=", first($gr | $pad), "\n";
print "11=", first($hr | $pad), "\n";
print "12=", ("abc" & "ABC"), "\n";
print "13=", (12 & 10), "\n";
print "14=", first("x" | $r), "\n";
my $n;
($n = $r) |= "x";   print "15=", first($n), "\n";
($n = $r) &= "x";   print "16=", first($n), "\n";
($n = $r) ^= "x";   print "17=", first($n), "\n";
($n = $u) |= "x";   print "18=", $n, "\n";
($n = $u) &= "x";   print "19=", h($n), "\n";
print "20=", h($u1 | $u2), "\n";
my $v = v65.66;
print "21=", h($v | "\0\0"), "\n";
print "22=", h(~$u), "\n";
print "23=", h(~$gl), "\n";
print "24=", ((~$r)  =~ /^\d\d+$/ ? "NUMERIC" : "STRING"), "\n";
print "25=", ((~$ob) =~ /^\d\d+$/ ? "NUMERIC" : "STRING"), "\n";
print "26=", ((~$gr) =~ /^\d\d+$/ ? "NUMERIC" : "STRING"), "\n";
print "27=", ((~$qr) =~ /^\d\d+$/ ? "NUMERIC" : "STRING"), "\n";
print "28=", h(~"ab"), "\n";
print "29=", h(~$v), "\n";
print "30=", (5 | "\x{100}"), "\n";
print "31=", h("ok \xFF\xFF\n" & "ok 19\n"), "\n";
for my $t (['&','and'], ['|','or'], ['^','xor']) {
  my ($op, $word) = @$t;
  local $@;
  eval "no warnings; my \$q = \"\\xFF\" $op \"\\x{100}\"; 1";
  print "32$word=", ($@ =~ /^Use of strings with code points over 0xFF as arguments to bitwise $word \(\Q$op\E\) operator is not allowed/ ? "ok" : "BAD"), "\n";
}
{ local $@; eval 'no warnings; my $q = ~ "\x{100}"; 1';
  print "33=", ($@ =~ /^Use of strings with code points over 0xFF as arguments to 1's complement \(~\) operator is not allowed/ ? "ok" : "BAD"), "\n"; }
{ local $@; eval 'no warnings; my $q = [1] | "\x{100}"; 1';
  print "34=", ($@ =~ /over 0xFF/ ? "ok" : "BAD"), "\n"; }
{ local $@; eval 'no warnings; my $q = 5 | "\x{100}"; 1';
  print "35=", ($@ eq '' ? "ok" : "BAD"), "\n"; }
PERL

my %EXPECT = (
    '01' => 'abc',       # THE HEADLINE: undef | "abc" — PCL used to say 0
    '02' => '',          # undef & "abc" — & truncates to the shorter operand
    '03' => 'abc',
    '04' => 'A',         # "ARRAY(0x…)" | NUL…
    '05' => '*',         # "*main::foo"
    '06' => '(',         # "(?^:x)"
    '07' => 'K',         # "K=HASH(0x…)"
    '08' => 'S',         # "SCALAR(0x…)"
    '09' => 'C',         # "CODE(0x…)"
    '10' => 'G',         # "GLOB(0x…)"
    '11' => 'H',         # "HASH(0x…)"
    '12' => 'ABC',       # plain strings were already right …
    '13' => '8',         # … and so were plain numbers
    '14' => 'y',         # the reference on the RIGHT stringifies too (x|A)
    '15' => 'y',         # the compound forms take the same decision
    '16' => '@',
    '17' => '9',
    '18' => 'x',
    '19' => '',
    '20' => '',          # undef | undef is "", not 0
    '21' => '4142',      # a vstring is PV-only
    '22' => '',          # ~undef is ""
    '23' => 'd5929e9691c5c5999090',   # ~"*main::foo", byte by byte
    '24' => 'NUMERIC',   # ~ REFERENCE is the complemented ADDRESS …
    '25' => 'NUMERIC',
    '26' => 'NUMERIC',
    '27' => 'NUMERIC',
    '28' => '9e9d',      # … while ~ STRING is the byte complement
    '29' => 'bebd',
    '30' => '5',         # ONE number operand forces the numeric op
    '31' => '6f6b2031390a',
    '32and' => 'ok',     # the over-0xFF fatal, with perl's own wording
    '32or'  => 'ok',
    '32xor' => 'ok',
    '33' => 'ok',
    '34' => 'ok',        # it fires for a REFERENCE operand too …
    '35' => 'ok',        # … and NOT on the numeric path
);

for my $opt ('default', 'none') {
    if ($opt eq 'none') { $ENV{PCL_OPT} = 'none' } else { delete $ENV{PCL_OPT} }
    my $out = run_pcl($PROG);
    my %got;
    $got{$1} = $2 while $out =~ /^(\d\d(?:and|or|xor)?)=(.*)$/mg;
    for my $k (sort keys %EXPECT) {
        is($got{$k} // "<MISSING; output was:\n$out>", $EXPECT{$k}, "[$opt] row $k");
    }
}
delete $ENV{PCL_OPT};
