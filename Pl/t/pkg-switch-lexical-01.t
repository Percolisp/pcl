#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# pkg-switch-lexical-01.t — task #593: a `my` declared BEFORE an in-block
# `package NAME;` switch must still be readable AFTER it.
#
#     {   package p73626;
#         sub TIESCALAR { bless {} }
#         sub FETCH { "$Perl hi" }
#         tie my $p, 'p73626';
#         package main;
#         open(my $f, '-|', $p);          # perl: the FETCHed command
#     }                                   # PCL:  read an EMPTY cell
#
# v2 splits such a block into per-package SECTIONS (separate top-level CL
# forms, because `in-package` is READ-time), so a `let` cannot span the
# switch: a lexical that crosses one is PROMOTED to a package cell by
# `_rename_spanning_lexicals`.  That pass never fired here, and the reason is
# the finding — the span detector asked whether the later segment REFERENCES
# `$p`, and `_symbol_is_declarator` answered "no, that token is a
# DECLARATION".
#
# PPI wraps the parenthesised argument list of `open(my $f, "<", $p)` in a
# PPI::Statement::Variable (it begins with `my`), and the predicate's own
# hand-rolled walk called every Symbol before a top-level `=` a declarator —
# there is no `=` in an argument list, so EVERY argument came back as a
# declaration.  perl declares only the leading name there (`my $f, "<", $p`
# warns "Parenthesize"; `$p` is an ordinary use), which is exactly what
# `_declared_names` already said.  Two predicates, one question, different
# answers — so the fix is ONE resolver, `_declarator_syms`, read by both
# (CLAUDE.md 11).
#
# WHY IT MATTERED BEYOND THE VALUE: with a global cell of the name in
# existence the second section read an EMPTY box instead of dying, which is
# perl's own t/io/open.t test 118 ("open -| magic": 136/25 -> 137/24 with this
# fix), and an empty pipe command used to send PCL down the bare fork-open
# path (#535).
#
# THE NEGATIVES ARE THE POINT (rows 4..7): widening "this token is a USE"
# must not turn a real declarator into a use.  Each of those rows fails if
# _declarator_syms ever starts reporting a token to the right of the declared
# name (or of the top-level `=`).  Every expectation is the live perl 5.40.3
# answer.

use v5.30;
use strict;
use warnings;
use Test::More;
use File::Temp qw(tempfile tempdir);
use FindBin qw($RealBin);
use lib $RealBin;
use PCLCore;

my $project_root = "$RealBin/../..";
my $pl2cl        = "$project_root/pl2cl";
my $runtime      = "$project_root/cl/pcl-runtime.lisp";
my @sbcl_rt = PCLCore::sbcl_prefix($runtime);

plan skip_all => "pl2cl not found" unless -x $pl2cl;
plan skip_all => "sbcl not found"  unless `which sbcl 2>/dev/null`;
plan tests => 8;

my $dir = tempdir(CLEANUP => 1);

sub write_pl {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;
    return $pl_file;
}

# Transpile (a DROPPED statement fails the row, via PCLCore) and run.
sub run_cl {
    my ($code) = @_;
    my $cl_code = PCLCore::transpile("$pl2cl " . write_pl($code));
    my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cl_fh $cl_code;
    close $cl_fh;
    my $output = `sbcl @sbcl_rt --load $cl_file 2>&1`;
    $output =~ s/^;.*\n//gm;
    $output =~ s/^PCL Runtime loaded\n//gm;
    $output =~ s/^\s*\n//gm;
    return $output;
}

sub emitted { return PCLCore::transpile("$pl2cl " . write_pl($_[0])) }

# ── 1. the reproducer: a tied `my` read after the switch, by a statement
#       that declares a `my` of its OWN (that second `my` is what hid the
#       first one's use from the span detector).
is(run_cl(<<'PL'), "seen:[tied-ok\n]",
my $Perl = "/bin/echo";
{
    package p73626;
    sub TIESCALAR { bless {} }
    sub FETCH { "$Perl tied-ok" }

    tie my $p, 'p73626';

    package main;

    open(my $f, '-|', $p) or die "open: $!";
    my $line = <$f>;
    close $f;
    print "seen:[$line]";
}
PL
   'a `my` before an in-block package switch is READ after it (#593)');

# ── 2. the same shape spelled the way perl's t/io/open.t writes it: the
#       later statement's own `my` sits in a PAREN list, which is the PPI
#       shape that made the predicate answer "declarator" for every argument.
is(run_cl(<<'PL'), "arg:[VALUE]\n",
{
    package Holder;
    my $p = "VALUE";

    package main;

    sub show { print "arg:[$_[1]]\n" }
    show(my $slot, $p);
}
PL
   'the paren-list `my` does not hide a sibling argument\'s USE (#593)');

# ── 3. the variant with NO capture of an outer file lexical must keep
#       working — it always did, and it is the half a narrow fix could break.
is(run_cl(<<'PL'), "p=[5]\nget=[5]\n",
{
    package Foo;
    my $p = 5;
    sub get { $p }

    package main;

    print "p=[$p]\n";
    print "get=[", Foo::get(), "]\n";
}
PL
   'the no-capture in-block switch keeps working (#593 inverse)');

# ── 4..7 THE NEGATIVES: what a declarator still is ───────────────────────────

# `my $x = $x` — the RHS reads the OUTER $x, so the RHS token must NOT be
# treated as the declarator (it never was; this pins it).
is(run_cl(<<'PL'), "inner:[outer-in]\n",
my $x = "outer";
{
    my $x = $x . "-in";
    print "inner:[$x]\n";
}
PL
   '`my $x = $x` still reads the outer $x on the RHS');

# `my ($a1, $b1) = ($p, 1)` — the RHS list's `$p` is a USE, the LHS list's
# two names are the declarators.
is(run_cl(<<'PL'), "a=[P] b=[1]\n",
my $p = "P";
my ($a1, $b1) = ($p, 1);
print "a=[$a1] b=[$b1]\n";
PL
   'a list declaration declares its LHS list and uses its RHS');

# `my $a1, $b1;` declares ONLY $a1 — $b1 is the PACKAGE variable, and a sub
# that reads it must see the package variable (#314's shape).
is(run_cl(<<'PL'), "g=[PKG]\n",
$b1 = "PKG";
my $a1, $b1;
sub g { return $b1 }
print "g=[", g(), "]\n";
PL
   '`my VAR, REST` declares only VAR; REST stays the package variable');

# an embedded `my` in a paren list declares only its own name: the trailing
# argument is the outer lexical, evaluated as a use.
is(run_cl(<<'PL'), "two:[|SRC]\n",
my $src = "SRC";
sub two { return "$_[0]|$_[1]" }
print "two:[", two(my $d, $src), "]\n";
PL
   'an embedded `my` in a paren list declares only its own name');

# ── 8. the EMISSION shape: the promoted cell, not a per-statement `let`.
#       A `(let (($p …)) …)` around the tie form is the bug (the next
#       section's read is then free), so assert the promotion directly.
{
    my $cl = emitted(<<'PL');
{
    package Pkg593;
    my $p = 7;

    package main;

    open(my $h, '<', \$p) or print "no\n";
}
PL
    like($cl, qr/Pkg593::\$p/,
         'the spanning lexical is promoted to a package cell, not let-bound');
}
