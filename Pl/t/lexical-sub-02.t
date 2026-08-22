#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# lexical-sub-02.t — a lexical sub NAMED AFTER A STATEMENT MODIFIER
# (`my sub if () {…}`, `state sub unless`, `our sub for`) — task #374(b),
# B3.3 of docs/b3-operand-collapse-s428.md.
#
# perl's toke.c looks a bareword up as a lexical sub only where it is NOT in
# operator position (`PL_expect != XOPERATOR`), so in the sub's scope `my $x
# = if if if` reads `(my $x = if()) if if()` — the first and third `if` are
# the sub, the middle one is still the modifier (deparsed, perl 5.40.3;
# t/op/lexsub.t lines 72/210/577 assert it).  Parser2's lexical-sub rename is
# POSITION-AWARE for those six names (_word_in_operator_position), and the
# keyword-shaped structure PPI built around a term-position use (a Compound
# statement, a Condition after it) is re-classed in place
# (_reclass_keyword_call_site).  `our sub if` is the same rule with the
# qualified spelling `main::if` for the term-position uses.
#
# Every row runs the SAME source through perl and PCL and compares, as
# lexical-sub-01.t does — the oracle IS perl.  The last row guards the
# _ends_term arm the classifier needed (a POSTFIX `++`/`--` ends a term),
# which the #354 glob-multiply repair shares: `$i++*foo` used to be a glob.

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

plan tests => 6;

my $PREAMBLE = "use feature 'lexical_subs', 'state';\nno warnings;\n";

sub write_pl {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $PREAMBLE, $code;
    close $fh;
    return $pl_file;
}

sub run_cl {
    my ($pl_file) = @_;
    my $cl_code = PCLCore::transpile("$pl2cl $pl_file");
    my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cl_fh $cl_code;
    close $cl_fh;
    my $output = `sbcl @sbcl_rt --load $cl_file 2>&1`;
    $output =~ s/^;.*\n//gm;
    $output =~ s/^PCL Runtime loaded\n//gm;
    $output =~ s/^\s*\n//gm;
    return $output;
}

sub test_lexsub {
    my ($name, $code) = @_;
    my $pl_file  = write_pl($code);
    my $perl_out = `perl $pl_file 2>&1`;
    my $cl_out   = run_cl($pl_file);
    is($cl_out, $perl_out, $name) or diag("perl: [$perl_out]\nPCL:  [$cl_out]");
}

# The three t/op/lexsub.t shapes: the sub in TERM position, the keyword in
# OPERATOR position, same and other package, all three declarators.
test_lexsub('#374b: `my $x = if if if` is (my $x = if()) if if() — my/state/our, both packages', <<'PERL');
{ our sub if() { 42 }   my $x = if if if; print "A $x\n"; package bar; my $y = if if if; print "B $y\n"; }
{ state sub if() { 44 } my $x = if if if; print "C $x\n"; package bar; my $y = if if if; print "D $y\n"; }
{ my sub if() { 44 }    my $x = if if if; print "E $x\n"; package bar; my $y = if if if; print "F $y\n"; }
my $z = 5 if 1; print "G $z\n";     # out of every scope: the plain modifier
PERL

# Every spelling of a term-position use, and the operator-position ones that
# must stay the keyword.
test_lexsub('#374b: the other spellings — `if()`, `&if`, `\&if`, `defined &if`, `if;`, in a block, as a modifier condition', <<'PERL');
{
  my sub if() { 44 }
  my $a = if();           print "a $a\n";
  my $b = &if;            print "b $b\n";
  my $c = &if();          print "c $c\n";
  my $d = \&if;           print "d ", $d->(), "\n";
  print "e ", (defined &if ? "def" : "undef"), "\n";
  if;                     # a call statement
  my @l = map { if } 1..2; print "f @l\n";
  my $h = 1 if if;        print "h $h\n";
}
{
  our sub if() { 42 }
  my $a = if();           print "A $a\n";
  my $b = &if;            print "B $b\n";
  my $d = \&if;           print "D ", $d->(), "\n";
  my $h = 1 if if;        print "H $h\n";
  my $i = main::if;       print "I $i\n";
  print "J ", (main->can('if') ? "can" : "cannot"), "\n";
}
PERL

# PPI splits a statement at a statement-initial keyword: `(for, for)` is a
# Compound + an orphan `, for` — joined back; `(unless, 2)` is one Compound.
test_lexsub('#374b: `unless`/`for`/`while` — in a list, after an operator, modifier + term in one statement, interpolated', <<'PERL');
{
  my sub unless() { 7 }
  my @l = (unless, 2);    print "H @l\n";
  my $v = 3 + unless;     print "I $v\n";
  my $w = unless unless 0; print "J $w\n";
  print "K ", unless, "\n" if unless;
  my $q = "@{[ unless ]}"; print "M $q\n";
}
{
  state sub for() { 3 }
  my $t = 0;
  $t += for for 1..2;     print "K $t\n";
  my @m = (for, for);     print "L @m\n";
}
{
  my sub while() { 9 }
  my $i = 0;
  $i++ while $i < 3;      print "N $i\n";   # postfix ++ then the MODIFIER
  my $t = while;          print "O $t\n";
}
PERL

# A keyword-named sub's call with explicit parens: PPI builds the `()` as a
# Condition; it is the argument list (this shape dropped on both trees before).
test_lexsub('#374b: `NAME()` of a `()`-prototyped lexical sub, keyword-named and not', <<'PERL');
sub foo() { 44 }
my $z = foo();  print "z $z\n";
{ my sub bar() { 45 } my $y = bar(); my $b = bar(); print "y $y b $b\n"; }
{ my sub if() { 46 } my $w = if(); print "w $w\n"; }
{ my sub foo() {3} my @m = (foo, foo); my @p = (foo, foo, foo); print "m @m p @p\n"; }
PERL

# The inverse: a NON-keyword lexical sub is renamed at every use, as before.
test_lexsub('the non-keyword rename is unchanged: every use, both packages, interpolated', <<'PERL');
{ my sub nm { "LEX" } print nm(), " ", "@{[ nm() ]}", " "; package Q; print nm(), "\n"; }
sub nm { "PKG" } print nm(), "\n";
PERL

# _ends_term: a POSTFIX `++`/`--` ends the term it steps, so `*name` after it
# is multiplication (the #354 repair); the prefix spelling still opens a term.
test_lexsub('_ends_term: postfix ++/-- end a term (`$i++*foo` multiplies); prefix ++ does not', <<'PERL');
sub foo {3} my $i=2; my $j = $i++*foo; print "S $j $i\n";
my $k=5; my $m = $k--*foo; print "T $m $k\n";
my $n = 1; ++$n; print "U $n\n";
my $o = 4; my $p = $o-- - --$o; print "V $p $o\n";
PERL
