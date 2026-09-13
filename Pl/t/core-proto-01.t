#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# core-proto-01.t — `prototype("CORE::NAME")` answers perl's own prototype
# string, and an unknown keyword throws (task #1586).
#
# Every `prototype("CORE::…")` used to be undef, and
# `prototype("CORE::nosuchthing")` used not to die — so t/op/cproto.t's 183
# rows were ONE cluster with ONE cause.  The table is LANGUAGE data and lives
# in the runtime (CLAUDE.md 9a's core-builtin exception), GENERATED from
# perl's own answers by tools/gen-core-protos.pl.
#
# TWO KINDS OF ROW HERE, deliberately:
#   * the SPELLINGS (rows 1-7) assert the mechanism against values probed from
#     perl 5.40.3 by hand, so they hold even where no perl is installed;
#   * the TABLE row re-asks the LIVE perl for all ~250 keywords and diffs
#     (`--check`), which is what catches a stale committed table.  It skips
#     when perl's own t/ tree is absent, because without perl's keyword LIST
#     the generator legitimately produces a smaller table (its candidates fall
#     back to Pl::PExpr::Config alone) and the diff would be a false DRIFT.

use v5.30;
use strict;
use warnings;
use Test::More;
use File::Temp qw(tempfile);
use FindBin qw($RealBin);
use lib $RealBin;
use lib "$RealBin/../../tools/lib";
use PCLCore;

my $project_root = "$RealBin/../..";
my $pl2cl        = "$project_root/pl2cl";
my $runtime      = "$project_root/cl/pcl-runtime.lisp";
my @sbcl_rt = PCLCore::sbcl_prefix($runtime);

plan skip_all => "pl2cl not found" unless -x $pl2cl;
plan skip_all => "sbcl not found"  unless `which sbcl 2>/dev/null`;

plan tests => 11;

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

# ── the prototype STRINGS (probed: perl 5.40.3) ──────────────────────────────
test_cl('prototype("CORE::abs") is (_), and so are lc / length / ref',
    'print join(" ", map { prototype("CORE::$_") } qw(abs lc length ref)), "\n";',
    "_ _ _ _\n");

test_cl('prototype("CORE::push") is (\\@@) — a reference-typed first slot',
    'print prototype("CORE::push"), "\n"; print prototype("CORE::splice"), "\n";',
    "\\\@\@\n\\\@;\$\$\@\n");

test_cl('prototype("CORE::sprintf") is ($@)',
    'print prototype("CORE::sprintf"), "\n"; print prototype("CORE::open"), "\n";',
    "\$\@\n*;\$\@\n");

# A zero-arg keyword's prototype is the DEFINED empty string.  Asserting the
# printed text alone would pass on the base tree too (undef prints as ""), so
# this row asks `defined` — which is the whole fact (#1173's family: a false
# answer from a builtin is "", never undef).
test_cl('a zero-arg keyword is the DEFINED empty prototype, not undef',
    'my @kw = qw(__FILE__ wantarray time);'
  . 'print join(" ", map { my $p = prototype("CORE::$_");'
  . '                      defined $p ? "[$p]" : "undef" } @kw), "\n";'
  . 'print scalar(grep { defined prototype("CORE::$_") && prototype("CORE::$_") eq "" }'
  . '             @kw), "\n";',
    "[] [] []\n3\n");

# ── the keywords whose real answer IS undef (~70 control-flow words) ─────────
test_cl('a control-flow keyword answers undef, and it is a DEFINED absence',
    'my $n = 0;'
  . 'for my $kw (qw(if my print split defined delete each_not_this)) {'
  . '  next if $kw eq "each_not_this";'
  . '  $n++ if !defined prototype("CORE::$kw");'
  . '} print "$n\n";',
    "6\n");

# ── the unknown keyword THROWS, with perl's message ──────────────────────────
test_cl('prototype("CORE::nosuchthing") dies "Can\'t find an opnumber for"',
    'my $v = eval { prototype("CORE::nosuchthing") };'
  . 'print +($@ =~ /^Can\'t find an opnumber for "nosuchthing"/ ? "died" : "no:[$@]"), "\n";'
  . 'my $w = eval { prototype("CORE::Foo::bar") };'
  . 'print +($@ =~ /^Can\'t find an opnumber for "Foo::bar"/ ? "died-qualified" : "no:[$@]"), "\n";',
    "died\ndied-qualified\n");

# The EMPTY name is perl's one exception: `prototype("CORE::")` is undef, NOT
# a fatal (probed).  A rule-12 die on every non-key would have got this wrong.
test_cl('prototype("CORE::") is undef, not a fatal',
    'my $v = eval { prototype("CORE::") };'
  . 'print +($@ ? "DIED:[$@]" : (defined $v ? "def" : "undef")), "\n";'
  . 'print +(defined prototype("nosuchsub") ? "def" : "undef"), "\n";'
  . 'print +(defined prototype(undef) ? "def" : "undef"), "\n";',
    "undef\nundef\nundef\n");

# ── the table itself, re-asked from the LIVE perl ────────────────────────────
my $gen = "$project_root/tools/gen-core-protos.pl";
my $suite_t = do { local $@; eval { require PCLPaths; PCLPaths::perl_suite_t() } };
SKIP: {
    skip "gen-core-protos.pl not found", 2 unless -f $gen;
    skip "perl's own t/op/cproto.t not found (no keyword list to ask about)", 2
      unless defined $suite_t && -f "$suite_t/op/cproto.t";

    my $check = `perl $gen --check 2>&1`;
    my $rc    = $?;
    is($rc, 0, 'the committed CORE:: prototype table is what this perl answers')
      or diag($check);

    # THE DISAGREEMENT MEASUREMENT (#1586's own bar): PCL has a SECOND table of
    # the same builtins — Pl::PExpr::Config's `known_no_of_params`, which
    # records ARITIES for parsing, not perl's prototype TEXT.  They are NOT
    # unified (that was explicitly out of scope); this row measures how far
    # apart they are, so the decision to unify is made from a list.  Measured
    # s484a: 151 keywords carry both facts, 88 are comparable, 0 disagree.
    my $arity = `perl $gen --arity 2>&1`;
    my $arc   = $?;
    is($arc, 0, 'the prototype arities and known_no_of_params do not contradict')
      or diag($arity);
}

# ── the classic-prototype gap is UNCHANGED (docs/not-supported.md) ───────────
# `sub f ($$)` prototypes are consumed at transpile time and still report
# undef; only the CORE:: half moved.  This row is the inverse guard: a fix that
# accidentally routed a user sub through the CORE table would break it.
test_cl('a user sub is not looked up in the CORE table',
    'sub p3 ($$) { 1 } sub plain { 1 }'
  . 'print +(defined prototype("p3") ? "def" : "undef"), " ",'
  . '      (defined prototype(\&plain) ? "def" : "undef"), "\n";'
  . 'sub abs2 { 1 } print +(defined prototype("abs2") ? "def" : "undef"), "\n";',
    "undef undef\nundef\n");

test_cl('a sub NAMED like a keyword is still the sub, not the keyword',
    'print +(defined prototype("length") ? "def" : "undef"), "\n";'
  . 'print prototype("CORE::length"), "\n";',
    "undef\n_\n");
