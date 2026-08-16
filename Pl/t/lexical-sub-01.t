#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# lexical-sub-01.t — `my sub NAME {…}` / `state sub NAME {…}` are LEXICALS (#337).
#
# PCL compiles every named sub as a PACKAGE sub.  Before the scope-unique
# rename, two same-named lexical subs in DIFFERENT scopes clobbered each other
# and every reference — including one captured in a closure built before the
# second declaration ran — resolved to whichever was defined LAST.  Silent: no
# warning, no die, a wrong value (perl `8 3`, PCL `3 3`).
#
# Every row below runs the SAME source through real perl and through PCL and
# compares, so no expectation here can drift away from the oracle.  The four
# shapes the rename exists for are the first four rows; the rest are the cases
# a rename could BREAK — a method or hash key spelled like the sub, a package
# sub of the same name still reachable outside the region, a call BEFORE the
# declaration (which perl sends to the package sub), the declaration's own
# region ending at a sibling redeclaration, and — the one this pass caused and
# a probe caught — a call inside INTERPOLATED CODE (`"@{[ f() ]}"`), which is
# compiled from the string's text and not from the token stream.
#
# Registered divergences, deliberately NOT tested for equality here (each has
# a docs/not-supported.md entry): a body's call to its own name, which perl
# rejects and PCL allows; a lexical sub reached from a STRING eval, which perl
# finds in the pad; and a fresh closure per loop iteration.

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

plan tests => 24;

my $PREAMBLE = "use feature 'lexical_subs';\nno warnings 'experimental::lexical_subs';\n";

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

# The oracle IS perl: same source, same preamble, both sides.
sub test_lexsub {
    my ($name, $code) = @_;
    my $pl_file  = write_pl($code);
    my $perl_out = `perl $pl_file 2>&1`;
    my $cl_out   = run_cl($pl_file);
    is($cl_out, $perl_out, $name) or diag("perl: [$perl_out]\nPCL:  [$cl_out]");
}

# --- the four shapes the rename exists for ---------------------------------

test_lexsub('two scopes, one name: each \&x is its OWN sub (#337)', <<'PERL');
my $g1 = sub { my sub x () { 8 } \&x };
my $g2 = sub { my sub x () { 3 } \&x };
print &{$g1->()}, " ", &{$g2->()}, "\n";
PERL

test_lexsub('a lexical sub shadows a package sub of the same name', <<'PERL');
sub nm { "PKG" }
{ my sub nm { "LEX" } print nm(), " "; }
print nm(), "\n";
PERL

test_lexsub('a nested lexical sub shadows the outer one, which survives it', <<'PERL');
my sub f { "outer" }
{ my sub f { "inner" } print f(), " "; }
print f(), "\n";
PERL

test_lexsub('a call BEFORE the declaration still reaches the package sub', <<'PERL');
sub w { "PKG" }
{ print w(), " "; my sub w { "LEX" } print w(), "\n"; }
PERL

# --- the region's own edges ------------------------------------------------

test_lexsub('a sibling redeclaration ends the earlier region', <<'PERL');
my sub f { "A" }
{ print f(); my sub f { "B" } print f(); }
print f(), "\n";
PERL

test_lexsub('two sibling blocks, same name, independent subs', <<'PERL');
{ my sub f { "A" } print f(); }
{ my sub f { "B" } print f(); }
print "\n";
PERL

test_lexsub('a redeclaration in the SAME scope wins from there on', <<'PERL');
my sub f { "F1" }
my sub f { "F2" }
print f(), "\n";
PERL

test_lexsub('the declaration does not leak out of its block', <<'PERL');
sub f { "PKG" }
{ my sub f { "L" } }
print f(), "\n";
PERL

# --- what the rename must NOT touch ----------------------------------------

test_lexsub('a hash key spelled like the sub stays a key', <<'PERL');
my sub f { "L" }
my %h = (f => 1, x => f());
print "$h{f} $h{x}\n";
PERL

test_lexsub('a METHOD spelled like the sub is still a method', <<'PERL');
package C; sub f { "M" }
package main;
my sub f { "L" }
my $o = bless {}, "C";
print $o->f(), " ", f(), "\n";
PERL

test_lexsub('a package sub declared INSIDE the region is still a package sub', <<'PERL');
{ my sub f { "L" } print f(); }
sub f { "PKG" }
print f(), "\n";
PERL

# --- the ways a lexical sub is referenced -----------------------------------

test_lexsub('\&NAME, &NAME and defined &NAME all reach the lexical', <<'PERL');
my sub f { "L" }
print defined(&f) ? "D" : "U";
my $r = \&f;
print &$r(), " ", &f, "\n";
PERL

test_lexsub('goto &NAME reaches the lexical', <<'PERL');
my sub f { "L" }
sub g { goto &f }
print g(), "\n";
PERL

test_lexsub('a sort comparator names the lexical sub', <<'PERL');
my sub cmpf { $a <=> $b }
my @s = sort cmpf (3,1,2);
print "@s\n";
PERL

test_lexsub('a closure and a named sub in the same scope both see it', <<'PERL');
my sub f { "L" }
sub caller1 { f() }
my $c = sub { f() };
print caller1(), $c->(), "\n";
PERL

# INTERPOLATED CODE is compiled from the string's TEXT, not from the token
# stream — the rename has to reach into it or the call points at a package sub
# that no longer exists ("the function main::pl-f is undefined").
test_lexsub('a call inside "@{[ … ]}" is renamed too', <<'PERL');
my sub f { "L" }
print "got @{[ f() ]}\n";
PERL

test_lexsub('…and inside an interpolating heredoc', <<'PERL');
my sub f { "L" }
my $h = <<"E";
val @{[ f() ]}
E
print $h;
PERL

# A lexical sub is NOT in the stash — before the rename it was, and ->can lied.
test_lexsub('a lexical sub is not reachable through ->can', <<'PERL');
my sub f { "L" }
print __PACKAGE__->can('f') ? "CAN" : "NOCAN", "\n";
PERL

# A lexical sub named after a KEYWORD (#374).  perl allows it and t/op/lexsub.t
# asserts it; PCL's statement grammar owns those words, so `my $x = if if if`
# cannot be lowered.  What matters is HOW it fails: it used to emit a
# zero-argument `(p-if)` — the p-if MACRO called as a function, because `if` is
# in ExprToCL's %RUNTIME_NAMES — and that form's macroexpansion error killed
# the WHOLE FILE at load.  Now the statement is a counted, announced DROP and
# the program runs on.
{
    # `our sub if` is a PACKAGE sub, so #337's rename deliberately leaves it
    # alone and the words stay keywords — this is the half that used to reach
    # the macro.  (`my`/`state sub if` renames, and then drops in the term
    # grammar instead; both halves are #374, both are counted in the census.)
    my $pl_file = write_pl(<<'PERL');
sub is { }
{ our sub if() { 42 }
  my $x = if if if;
  is $x, 42; }
print "after\n";
PERL
    my ($cl, $err, $rc) = PCLCore::transpile_raw("$pl2cl $pl_file");
    unlike($cl, qr/\(p-if\)/,
           '#374: a keyword-named sub never emits a zero-argument (p-if), whose '
         . 'macroexpansion error used to kill the whole file at load');
    like($cl, qr/PARSE ERROR: statement keyword/,
         '#374: …the statement is a counted DROP instead');
    like($err, qr/PCL: statement dropped/,
         '#374: …and the drop is announced, so it is not silent');
}

# ── #377: a lexical sub reading the enclosing sub's PARAMETER ────────────────
#
# The shape a `my sub` user writes first — a private helper that reads what the
# enclosing sub was called with.  All three spellings of "take a param" used to
# go three different ways: `my $x = shift` CRASHED on an unbound $x__file__0
# (the promotion renamed the captured lexical, but the raw-params fast path
# bound the promoted name LEXICALLY and emitted no cell), `my ($x) = @_` killed
# the whole FILE with "lexical 'x' possibly captured by nested sub" (the
# promoter never saw a one-element list declaration — it is not
# _single_scalar_decl's shape, the container branch wants a [@%] sigil, and the
# list branch wanted two names), and `my $x = 70` worked.  One program so the
# three sit next to each other, and perl is the oracle for all of them.
test_lexsub('#377: a lexical sub reads its enclosing sub\'s parameter, in every spelling', <<'PERL');
sub o1 { my $x = shift;        my sub i { $x * 2 }  return i(); }
sub o2 { my ($x) = @_;         my sub i { $x * 2 }  return i(); }
sub o3 { my ($x, $y) = @_;     my sub i { $x + $y } return i(); }
sub o4 { my $x = 70;           my sub i { $x * 2 }  return i(); }
sub o5 { my $x = shift;        my sub i { 7 }       return i() + $x; }
print o1(3), " ", o1(4), "\n";
print o2(3), " ", o2(4), "\n";
print o3(1,2), " ", o3(10,20), "\n";
print o4(), " ", o4(), "\n";
print o5(1), " ", o5(2), "\n";
PERL

# The counterpart the fix must NOT disturb: a sub whose parameter no nested sub
# captures keeps the p-raw-params fast path (no promotion, no cell).  Asserted
# on the EMISSION, because the VALUE is the same either way.
{
    my $pl_file = write_pl("sub plain { my \$x = shift; return \$x * 3 }\nprint plain(5), \"\\n\";\n");
    my $cl = PCLCore::transpile("$pl2cl $pl_file");
    like($cl, qr/\(p-raw-params \(\$x\)/,
         '#377: an uncaptured shift-param still takes the raw-params fast path');
    unlike($cl, qr/\$x__file__/,
           '#377: …and is not promoted to a cell');
}
