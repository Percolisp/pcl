#!/usr/bin/env perl
# bareword-call-01.t — task #266: when is a bare NAME (no parens, no args) a
# CALL and when is it the string "NAME"?
#
# Perl decides at COMPILE time, top-down: a call only if the name is already
# known callable where the call site is compiled; otherwise (no `strict subs`)
# the bareword is simply its own text.  PCL got both halves wrong:
#
#   * a QUALIFIED name never matched the sub tables (they are keyed by the bare
#     name plus a package), so `Foo::init` read as the string even with
#     `sub init` in package Foo ABOVE it — perl calls;
#   * the whole-file pre-scan made the answer position-blind, so a call site
#     ABOVE `sub foo {…}` was told the name is callable — perl reads "foo".
#     Qualified, the two compounded into a SILENT WRONG: a call to a sub not
#     yet defined, which returned EMPTY.
#   * and an unknown name at the END of a list (`print "x=", nosuch;`) fell
#     through to a funcall that CRASHED at load with an undefined function.
#
# The INVERSE guards matter as much: PCL's compile-time name knowledge is
# incomplete (builtins outside the arity table, subs from a `require`d file,
# `:DEFAULT` imports), so a lone bareword starting its own run must still be a
# CALL — `next;`, `goto again;`, `rel2abs(curdir)`.
#
# Every expectation below is the live `perl` answer (probed s374).

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

plan tests => 18;

sub run_cl {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
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

my $DECL_Q = 'package Foo; sub init { return 7 } package main;';

# ── QUALIFIED name, declared ABOVE: perl CALLS ───────────────────────────────
test_cl('qualified bareword declared above, mid-list, is a call',
    qq{$DECL_Q\nprint "R=", Foo::init, "!\\n";}, "R=7!\n");

test_cl('qualified bareword declared above, end of list, is a call',
    qq{$DECL_Q\nprint "R=", Foo::init; print "!\\n";}, "R=7!\n");

# ── QUALIFIED name, declared BELOW: perl STRINGIFIES ─────────────────────────
test_cl('qualified bareword declared below, mid-list, is the string',
    qq{print "R=", Foo::init, "!\\n";\n$DECL_Q}, "R=Foo::init!\n");

# This one was the filed #266 silent wrong: PCL called a sub not yet defined
# and printed nothing at all.
test_cl('qualified bareword declared below, end of list, is the string',
    qq{print "R=", Foo::init; print "!\\n";\n$DECL_Q}, "R=Foo::init!\n");

# ── name declared NOWHERE: perl STRINGIFIES (this used to CRASH) ─────────────
test_cl('unknown qualified bareword at end of list is the string, not a crash',
    qq{print "R=", Nope::nope; print "!\\n";}, "R=Nope::nope!\n");

test_cl('unknown plain bareword at end of list is the string, not a crash',
    qq{print "R=", nosuch; print "!\\n";}, "R=nosuch!\n");

# ── PLAIN name, position decides ─────────────────────────────────────────────
test_cl('plain bareword declared above is a call',
    qq{sub init { return 7 }\nprint "R=", init, "!\\n";}, "R=7!\n");

test_cl('plain bareword declared below is the string',
    qq{print "R=", init, "!\\n";\nsub init { return 7 }}, "R=init!\n");

# ── INVERSE GUARDS: the shapes the widening must NOT touch ───────────────────

# A control-flow operator is callable everywhere, including after a comma —
# `last` here exits the loop, so nothing is ever pushed (perl: @a is empty).
test_cl('`push @a, last;` is a last, not the string "last"',
    'my @a; for (1) { push @a, last; } print "n=", scalar(@a), "\n";', "n=0\n");

test_cl('`redo` after && is a redo, not the string "redo"',
    'my $n = 2; my $c = 0; { $c++; $n-- && redo; } print "c=$c\n";', "c=3\n");

# A lone bareword starting its own run keeps CALLING: this compiler cannot see
# every name perl can (here: declared later in the file, reached at runtime).
test_cl('a bare statement-position call to a sub declared below still calls',
    'my $r = 0; sub later { $r = 5 } later(); print "r=$r\n";', "r=5\n");

# A class-name invocant is not a value bareword — it must keep its runtime
# sub-or-class resolution, not be flattened to a string by the string reading.
test_cl('bareword class invocant before -> still dispatches',
    'sub new { my $c = shift; bless {}, $c } my $o = main -> new (sub {1});'
  . ' print "r=", ref($o), "\n";', "r=main\n");

# `Foo::` (trailing ::) is perl's explicit class-name string, value "Foo".
test_cl('bless into a trailing-:: bareword class',
    'my $x = 5; my $r = bless \$x, Foo::; print "ref=", ref($r), "\n";',
    "ref=Foo\n");

# Under `strict subs` perl would refuse an unknown bareword at compile time;
# PCL does not reproduce compile-time refusal (principle 9) and keeps the CALL,
# because a sub installed through a dynamic glob is invisible here but real at
# runtime (task #193).  Guard that the strict path did not move.
test_cl('under strict subs a declared sub is still a plain-bareword call',
    'use strict; use warnings; sub helper { 42 } print "h=", helper;'
  . ' print "\n";', "h=42\n");

# ── `-BAREWORD` autoquoting: the fat comma and the hash subscript (#234) ─────
#
# Perl autoquotes a `-BAREWORD` in the two positions that autoquote at all —
# before `=>` and inside a hash subscript — and the STRING reading wins over
# the operator one.  PPI hands a single-letter `-f` over as the FILETEST
# operator token, so neither autoquote site recognised it: the `$_` default
# turned it into `-f($_)` and the result ATE the next list element, or produced
# an empty key.  `-foo` and `-1` tokenize differently and were always right.

test_cl('a filetest letter before => is the key "-f", not a filetest',
    'my %h = (-f => 4, abc => 3);'
  . ' print join(",", map { "$_=$h{$_}" } sort keys %h), "\n";',
    "-f=4,abc=3\n");

test_cl('a filetest letter is a hash KEY in a subscript and in interpolation',
    'my %h; $h{-f} = 1; $h{-foo} = 2; $h{-1} = 4;'
  . ' print join(",", sort keys %h), " i=$h{-f}\n";',
    "-1,-f,-foo i=1\n");

# INVERSE: a REAL filetest whose RESULT the fat comma follows keeps running —
# perl only autoquotes the word IMMEDIATELY before `=>`.
test_cl('a filetest applied to an operand is still a filetest before =>',
    'sub g { return "[@_]" } my $f = "/etc/hostname";'
  . ' print g(-e $f => 1), " ", (-e $f ? "y" : "n"), "\n";',
    "[1 1] y\n");

# INVERSE: an ARRAY subscript is not autoquoted in perl, and a numeric key is
# still the number.
test_cl('array subscripts and numeric keys are untouched',
    'my @a = (10,20,30); my %h = (-1 => 5);'
  . ' print "$a[-1] $a[1] ", join(",", %h), "\n";',
    "30 20 -1,5\n");
