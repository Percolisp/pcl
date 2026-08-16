#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# try-catch-01.t — perl 5.34's `use feature 'try'`: try/catch/finally (#340).
#
# Every row runs the SAME source through real perl and through PCL and compares
# the output, so the expectations cannot drift from the oracle.
#
# What makes try NOT "eval {} with different spelling", and why each of these
# rows exists:
#   * `return` / `last` / `next` inside try belong to the ENCLOSING sub or loop
#     (eval {} catches return), and `finally` still runs on that path;
#   * `$@` is localized — "" inside try and inside catch, its old value again by
#     the time finally runs and afterwards — and the error arrives ONLY in the
#     catch variable;
#   * catch runs on a FALSE exception (`die 0`, a bool-overloading object);
#   * the construct has a VALUE (the executed block's last statement), in the
#     caller's context;
#   * wantarray and caller() see straight through it — no new frame.
#
# The PPI half matters as much: PPI 1.291 leaves `finally {…}` OUT of the try
# Compound and lets the orphan swallow the next statement whole
# (docs/ppi-upstream-bugs.md §18), so the "statement after finally still runs"
# rows below are the guard on that repair.

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

plan tests => 26;

my $PREAMBLE = "use feature 'try';\nno warnings 'experimental::try';\n";

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

# The oracle IS perl: same source, same preamble, both sides.
sub test_try {
    my ($name, $code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $PREAMBLE, $code;
    close $fh;
    my $perl_out = `perl $pl_file 2>&1`;
    my $cl_out   = run_cl($code);
    is($cl_out, $perl_out, $name) or diag("perl: [$perl_out]\nPCL:  [$cl_out]");
}

sub transpile {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $PREAMBLE, $code;
    close $fh;
    return scalar `$pl2cl $pl_file 2>/dev/null`;
}

# --- codegen shape ---------------------------------------------------------

like(transpile('try { f() } catch ($e) { g($e) }'),
    qr/\(p-try\b/,
    'try/catch lowers to the p-try macro');
like(transpile('try { f() } catch ($e) { g($e) } finally { h() }'),
    qr/\(p-try\s+\(progn.*\(\$e\s+\(progn.*\(progn.*h/s,
    'the finally block reaches p-try as its third argument');

# --- which block runs ------------------------------------------------------

test_try('successful try runs try, not catch', <<'PERL');
my $x = "";
try { $x .= "try" } catch ($e) { $x .= "catch" }
print "$x\n";
PERL

test_try('die in try runs catch, which sees the exception', <<'PERL');
my $x = "";
try { $x .= "try"; die "Oopsie\n" } catch ($e) { $x .= "catch"; print "e=[$e]" }
print "$x\n";
PERL

test_try('catch sees a FALSE exception (die 0)', <<'PERL');
my $caught = 0;
try { die 0 } catch ($e) { $caught++ }
print "caught=$caught\n";
PERL

test_try('catch sees a bool-overloading FALSE object', <<'PERL');
package FALSE { use overload 'bool' => sub { 0 }; sub new { bless [], shift } }
my $caught = 0;
try { die FALSE->new } catch ($e) { $caught++; print "ref=", ref($e), "\n" }
print "caught=$caught\n";
PERL

test_try('an object exception reaches the catch variable unstringified', <<'PERL');
package Ex { sub new { bless { m => $_[1] }, $_[0] } }
try { die Ex->new("obj") } catch ($e) { print ref($e), " ", $e->{m}, "\n" }
PERL

# --- $@ ---------------------------------------------------------------------

test_try('$@ is empty inside try and inside catch, restored after', <<'PERL');
$@ = "before\n";
try { print "in try: [$@]\n"; die "boom\n" } catch ($e) { print "in catch: [$@]\n" }
print "after: [$@]\n";
PERL

test_try('a successful try leaves $@ alone', <<'PERL');
$@ = "kept\n";
try { 1 } catch ($e) { print "never\n" }
print "after: [$@]\n";
PERL

test_try('an eval inside try still sets $@ for the rest of the block', <<'PERL');
try { eval { die "inner\n" }; print "saw: [$@]" } catch ($e) { print "no\n" }
PERL

test_try('a die inside catch propagates to the enclosing eval', <<'PERL');
my $r = eval { try { die "x\n" } catch ($e) { die "from catch\n" }; 1 };
print "rc=", defined $r ? $r : "undef", " at=[$@]";
PERL

# --- control flow through try -----------------------------------------------

test_try('return inside try returns from the enclosing sub', <<'PERL');
sub f { try { return "inside" } catch ($e) { } return "after" }
print f(), "\n";
PERL

test_try('last/next inside try control the enclosing loop', <<'PERL');
for my $i (1 .. 4) {
    try { next if $i == 2; last if $i == 4; print "i=$i " } catch ($e) { }
}
print "\n";
PERL

test_try('a labelled loop control inside try reaches its label', <<'PERL');
OUTER: for my $i (1 .. 3) {
    for my $j (1 .. 3) {
        try { last OUTER if $i == 2 } catch ($e) { }
        print "$i$j ";
    }
}
print "\n";
PERL

test_try('wantarray inside try answers for the enclosing sub', <<'PERL');
my $ctx;
sub what { try { $ctx = wantarray ? "list" : defined wantarray ? "scalar" : "void" } catch ($e) { } }
what();          print "1=$ctx\n";
my $s = what();  print "2=$ctx\n";
my @l = what();  print "3=$ctx\n";
PERL

test_try('caller() does not see the try block', <<'PERL');
sub A { my @c = caller 1; print "$c[3]\n" }
sub B { try { A() } catch ($e) { } }
B();
PERL

# --- the construct's value ---------------------------------------------------

test_try('do { try } yields the try block value in scalar and list context', <<'PERL');
my $s = do { try { 123 } catch ($e) { 456 } };
my @l = do { try { 1, 2, 3 } catch ($e) { 4, 5, 6 } };
print "s=$s l=@l\n";
PERL

test_try('do { try/catch } yields the catch value when try died', <<'PERL');
my $s = do { try { die "Oops" } catch ($e) { 456 } };
my @l = do { try { die "Oops" } catch ($e) { 4, 5, 6 } };
print "s=$s l=@l\n";
PERL

test_try('the value is the LAST statement of a multi-statement block', <<'PERL');
my $s = do { try { my $x = 123; 456 } catch ($e) { 789 } };
my $t = do { try { die "Oops" } catch ($e) { my $x = 123; "result" } };
print "s=$s t=$t\n";
PERL

# --- finally -----------------------------------------------------------------

test_try('finally runs on success and on failure', <<'PERL');
my $x = "";
try { $x .= "try" } catch ($e) { $x .= "catch" } finally { $x .= "finally" }
print "$x\n";
$x = "";
try { $x .= "try"; die "d\n" } catch ($e) { $x .= "catch" } finally { $x .= "finally" }
print "$x\n";
PERL

test_try('finally runs on return, and the sub still returns the try value', <<'PERL');
my $ran = 0;
sub ff { try { return "R" } catch ($e) { } finally { $ran++ } return "after" }
my $v = ff();
print "v=$v ran=$ran\n";
PERL

test_try('finally runs on last out of the enclosing loop', <<'PERL');
for my $i (1 .. 3) {
    try { last if $i == 2; print "i=$i " } catch ($e) { } finally { print "[f$i]" }
}
print "\n";
PERL

# The PPI repair's own guard: without it the statement AFTER a finally block is
# swallowed into the finally statement and never runs at all.
test_try('the statement after finally still runs (PPI swallow repair)', <<'PERL');
my $x = "";
try { $x .= "t" } catch ($e) { } finally { $x .= "f" }
print "after=$x\n";
sub g { try { 1 } catch ($e) { } finally { print "fin\n" } my $y = "reached"; print "$y\n" }
g();
PERL

test_try('a nested try inside a catch block keeps both variables', <<'PERL');
try { die "outer\n" }
catch ($e) { try { die "inner\n" } catch ($f) { print "e=[$e] f=[$f]" } }
PERL

# Statement positions and spellings the construct turns up in: inside an `if`
# block, as the value of a `do` in a hash assignment, with a redundant trailing
# `;` (people write it out of habit), and with a `return` out of the try block.
test_try('try in every statement position, including a trailing semicolon', <<'PERL');
sub s1 { try { die "a\n" } catch ($e) { return "c" }; return "n" }
print s1(), "\n";
if (1) { try { print "in-if " } catch ($e) { } }
my %h; $h{k} = do { try { "v" } catch ($e) { "x" } };
print "h=$h{k}\n";
try { print "semi " } catch ($e) { };
try { print "fin " } catch ($e) { } finally { print "F " };
print "done\n";
PERL

# The catch variable is scoped to the catch block: it shadows an outer variable
# of the same name and must not leak past the construct — at file scope and
# inside a sub (probed both).
test_try('the catch variable shadows, and does not leak', <<'PERL');
my $e = "outer";
try { die "x\n" } catch ($e) { print "in: $e" }
print "after: $e\n";
sub f { my $e = "sub"; try { die "y\n" } catch ($e) { print "insub: $e" } return $e }
print "ret: ", f(), "\n";
PERL
