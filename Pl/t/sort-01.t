#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# sort-01.t - Tests for sort operator code generation and runtime behavior
#
# Documents failures identified from perl-tests/sort.t:
#   1. sort NAME LIST not generating function-reference form (generates wrong call)
#   2. $a/$b undefined in named sort comparator subs (no defvar emitted)
#   3. p-sort not binding $a/$b dynamically before calling comparator
#
# See docs/v1-implementation-plan.md B5 for context.

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

plan tests => 23;

# ── Helpers ─────────────────────────────────────────────────────────────────

sub transpile {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;
    return scalar `$pl2cl $pl_file 2>/dev/null`;
}

sub run_cl {
    my ($code) = @_;
    my $cl_code = transpile($code);
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
    my $got = run_cl($code);
    is($got, $expected, $name);
}

# ── Transpilation tests ─────────────────────────────────────────────────────

# Test 1: sort { BLOCK } LIST → inline lambda with $a/$b params
{
    my $cl = transpile('sort { $a cmp $b } @arr;');
    like($cl, qr/p-sort.*p-sort-cmp.*\$a.*\$b/s,
         'sort { BLOCK } LIST generates a p-sort-cmp over $a/$b');
}

# Test 2: sort NAME LIST → wrapped in lambda, not a bare call
{
    my $cl = transpile('sort compare @arr;');
    unlike($cl, qr/\(p-sort \(pl-compare/,
           'sort NAME LIST: comparator not called as bare function');
    like($cl, qr/p-sort.*pl-compare/s,
         'sort NAME LIST: comparator name appears in p-sort call');
}

# Test 3: $a and $b are declared (defvar) so named comparator subs can use them
{
    my $cl = transpile('sub backwards { $b cmp $a } my @s = sort backwards qw/c a b/;');
    like($cl, qr/defvar.*\$[ab]|\$[ab].*defvar/s,
         '$a and $b get defvar declarations for named comparator subs');
}

# Test 4: default sort (no comparator) emits p-sort without function arg
{
    my $cl = transpile('my @s = sort @arr;');
    like($cl, qr/p-sort/,
         'sort without comparator emits p-sort');
    unlike($cl, qr/p-sort.*lambda/s,
           'sort without comparator has no lambda');
}

# ── Runtime tests ────────────────────────────────────────────────────────────

# Test 5: basic numeric sort with block comparator
test_cl('sort { $a <=> $b } produces ascending order',
    'my @s = sort { $a <=> $b } (3, 1, 4, 1, 5, 9, 2);
     print join(" ", @s), "\n";',
    "1 1 2 3 4 5 9\n");

# Test 6: reverse numeric sort with block
test_cl('sort { $b <=> $a } produces descending order',
    'my @s = sort { $b <=> $a } (3, 1, 2);
     print join(" ", @s), "\n";',
    "3 2 1\n");

# Test 7: string sort with block comparator
test_cl('sort { $a cmp $b } produces lexical order',
    'my @s = sort { $a cmp $b } qw/banana apple cherry/;
     print join(" ", @s), "\n";',
    "apple banana cherry\n");

# Test 8: default sort (no comparator) is lexical
test_cl('default sort is lexical',
    'my @s = sort qw/banana apple cherry/;
     print join(" ", @s), "\n";',
    "apple banana cherry\n");

# Test 9: sort returns new list (original unchanged)
test_cl('sort does not modify original array',
    'my @orig = (3, 1, 2);
     my @s = sort { $a <=> $b } @orig;
     print join(" ", @orig), "\n";
     print join(" ", @s), "\n";',
    "3 1 2\n1 2 3\n");

# Test 10: sort with named comparator sub
test_cl('sort with named comparator sub',
    'sub num_cmp { $a <=> $b }
     my @s = sort num_cmp (3, 1, 2);
     print join(" ", @s), "\n";',
    "1 2 3\n");

# Test 11: sort with named reverse comparator
test_cl('sort with named reverse comparator',
    'sub rev_cmp { $b cmp $a }
     my @s = sort rev_cmp qw/banana apple cherry/;
     print join(" ", @s), "\n";',
    "cherry banana apple\n");

# Test 12: Schwartzian transform (map/sort/map)
# Use words with distinct lengths to avoid stability dependence
test_cl('Schwartzian transform',
    'my @words = qw/hi hello bye/;
     my @sorted = map { $_->[0] }
                  sort { $a->[1] <=> $b->[1] }
                  map { [$_, length($_)] } @words;
     print join(" ", @sorted), "\n";',
    "hi bye hello\n");

# Test 13: sort empty list
test_cl('sort empty list returns empty list',
    'my @s = sort ();
     print scalar(@s), "\n";',
    "0\n");

# Test 14: sort single element
test_cl('sort single element',
    'my @s = sort { $a <=> $b } (42);
     print join(" ", @s), "\n";',
    "42\n");

# Tests 15-16: sort of a postfix-deref list (sort $ar->@*). A scalar
# immediately followed by -> is one term (postfix deref), NOT a bare
# comparator, so this sorts the elements of $ar. Previously the
# sort-$scalar-comparator detection mis-fired and parse-errored.
# Found by tools/difftest-ops.pl axis 23.
test_cl('sort $ar->@* sorts the arrayref elements (no comparator)',
    'my $ar = [3, 1, 2];
     print join(",", sort $ar->@*), "\n";',
    "1,2,3\n");
test_cl('sort {block} $ar->@* (block comparator + postfix-deref list)',
    'my $ar = [[3],[1],[2]];
     print join(",", map { $_->[0] } sort { $a->[0] <=> $b->[0] } $ar->@*), "\n";',
    "1,2,3\n");

# ── Tests 19-21: `sort NAME LIST` is a PLAIN CALL (task #501) ───────────────
# The named-comparator lowering used to wrap the call in its own AUTOLOAD
# dispatch — `handler-case` + `(intern "PL-AUTOLOAD" |sort--pkg|)`, citing
# [perl #30661] — a THIRD copy of the "a call reached no body" rule whose
# no-AUTOLOAD arm returned nil, so `sort nonexistent LIST` compared everything
# EQUAL where perl dies.  It could not even fire: "PL-AUTOLOAD" is not the
# symbol %pcl-cl-sub-name produces for AUTOLOAD.  Since task #468 the CALL
# answers the question, in perl's own order (the sub's own package's AUTOLOAD
# with $AUTOLOAD set, else perl's die), so the wrapper is gone.
# Tests 19-20 are the INVERSE guard: both fail on a tree that still emits it.
{
    my $cl = transpile('sub cmpx { $a <=> $b } my @s = sort cmpx (3,1,2);');
    unlike($cl, qr/PL-AUTOLOAD/,
           'sort NAME LIST: no AUTOLOAD wrapper in the emission (#501)');
    like($cl, qr/p-sort-cmp \(\$a \$b\)\s*\(p-scalar-ctx\s*\(pl-cmpx\)\)/s,
         'sort NAME LIST: the comparator is a plain call');
}

# Test 21: every `sort NAME` shape, one program, all probed against perl
# 5.40.3 (s442d).  perl agrees line for line except the die's LOCATION, which
# is " at F line N." — PCL carries none (message fidelity is not a goal,
# memory: project_error_message_fidelity_not_required).  The message itself
# became perl's own in s446j: the resolution now happens where perl does it,
# at sort ENTRY (task #514), so it is the sort-specific diagnostic and not
# the generic "Undefined subroutine &main::nope_cmp called" the first
# comparison used to raise.
# `nosub:` and `fwd:` are the rows the deleted wrapper got WRONG: before #468
# they were LIVED with the list unsorted.  `autoload:` is perl's own answer —
# a sort comparator name DOES reach the package's AUTOLOAD.
test_cl('sort NAME: named / ($$) / reverse / qualified / no-sub dies / fwd-decl dies / AUTOLOAD',
    'my @l = (3,1,2,10);
     sub by_num { $a <=> $b }
     sub by_p ($$) { $_[0] <=> $_[1] }
     sub fwd_cmp;
     print "named:",  join(",", sort by_num @l), "\n";
     print "proto:",  join(",", sort by_p @l), "\n";
     print "rev:",    join(",", reverse sort by_num @l), "\n";
     package Other; sub by_len { length($main::a) <=> length($main::b) }
     package main;
     print "qual:",   join(",", sort Other::by_len @l), "\n";
     my $ok = eval { my @s = sort nope_cmp @l; 1 };
     print "nosub:", ($ok ? "LIVED" : "DIED"), ":",
           ($@ =~ /Undefined sort subroutine "main::nope_cmp" called/
              ? "msg-ok" : "msg=[$@]"), "\n";
     my $ok2 = eval { my @s = sort fwd_cmp @l; 1 };
     print "fwd:", ($ok2 ? "LIVED" : "DIED"), "\n";
     package P;
     our $AUTOLOAD;
     sub AUTOLOAD { our ($a, $b); return $a <=> $b }
     sub run { my @m = (5,4,6); return join(",", sort auto_cmp @m) }
     package main;
     print "autoload:", P::run(), "\n";',
    "named:1,2,3,10\nproto:1,2,3,10\nrev:10,3,2,1\nqual:3,1,2,10\n"
  . "nosub:DIED:msg-ok\nfwd:DIED\nautoload:4,5,6\n");

# ── Tests 22-23: `sort NAME LIST` resolves on ENTRY (task #514) ─────────────
# perl resolves the comparator when the sort STARTS, not at the first
# comparison: `sort nonexistent (7)` and even `sort nonexistent ()` die
# although no pair is ever compared.  PCL built a lambda that called the name
# and only died once it ran, so both short-list spellings quietly succeeded —
# a die-TIMING divergence, invisible to any test with three elements.
# The AUTOLOAD rows are perl's own answer at the SAME two list sizes (perl
# #30661), and they are why the check asks "body OR the package's AUTOLOAD",
# never fboundp alone.
test_cl('sort NAME: entry resolution — short lists die, AUTOLOAD does not',
    'our ($a, $b);
     sub by_num { $a <=> $b }
     sub fwd_only;
     for my $t (["one", sub { my @s = sort nope1 (7); "@s" }],
                ["zero", sub { my @s = sort nope2 (); "@s" }],
                ["two", sub { my @s = sort nope3 (1,2); "@s" }],
                ["fwd", sub { my @s = sort fwd_only (7); "@s" }],
                ["ok1", sub { my @s = sort by_num (7); "@s" }],
                ["ok3", sub { my @s = sort by_num (3,1,2); "@s" }],
                ["al1", sub { my @s = sort AL::cmp1 (7); "@s" }],
                ["al3", sub { my @s = sort AL::cmp1 (3,1,2); "@s" }]) {
       my ($nm, $c) = @$t;
       my $r = eval { $c->() };
       print "$nm:", (defined $r ? "[$r]" : "DIED"), "\n";
     }
     package AL; sub AUTOLOAD { return $main::a <=> $main::b }',
    "one:DIED\nzero:DIED\ntwo:DIED\nfwd:DIED\n"
  . "ok1:[7]\nok3:[1 2 3]\nal1:[7]\nal3:[1 2 3]\n");

# perl runs the LIST first and dies afterwards (probed with $|=1), which is
# why the check belongs to p-sort and not to the comparator form — that form
# is an ARGUMENT, evaluated BEFORE the list.
test_cl('sort NAME: the LIST runs before the entry die',
    '$| = 1;
     our ($a, $b);
     sub side { print "LIST\n"; return (3,1) }
     my $ok = eval { my @s = sort nope_ord (side()); 1 };
     print "died:", ($ok ? 0 : 1), "\n";',
    "LIST\ndied:1\n");
