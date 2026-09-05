#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# array-facts-01.t — task #1140, the VarAnnotator ARRAY-fact family, and its
# three consumers.
#
# WHAT THE FAMILY IS.  Until s470bj `Pl::VarAnnotator` was scalar-only by
# construction (`next unless $var =~ /^\$\w+$/`), so no `@name` ever got a
# verdict.  It now answers two questions per `my @name` declared in a region:
#
#   escapes       — the container or an element is reachable under another
#                   name: `\@a`, `\$a[i]`, passed whole to a call or method,
#                   named in a nested sub (anon OR named), tied, blessed
#                   through a ref, a string eval in the region, a
#                   redeclaration, the list of a foreach whose variable is not
#                   proven read-only, or ANY position the walk has not
#                   classified.  Unclassified defaults to ESCAPE.
#   written_in(R) — a direct write inside a `for`/`foreach` body block.
#
# THE THREE CONSUMERS:
#   * Kind-A `local-push` (#996 half A3): `push @a, SCALAR` on a non-escaping
#     `my @a` emits `(%p-push1 @a X)`.  Bench `pushloc` 0.48x -> 0.28x.
#   * `foreach-raw`: its read-only LOOP-VARIABLE verdict gains
#     `!escapes && !written_in(body)` for every array named in the list.  This
#     closes #1140's hole: `for my $x (@fa) { $fa[0] = 99; print $x; last }`
#     printed 1 where perl prints 99.
#   * `classic-sort`'s foreach licence consults the `p-foreach-raw` head, so
#     `for my $x (sort { $a <=> $b } @fs) { $fs[1] = 99 … }` is fixed with it —
#     which is why the conjunct asks about EVERY array named in the list, not
#     just a bare `@a` (`sort`, `reverse`, `values` and a comma list all hand
#     the SOURCE array's elements through; probed).
#
# TWO KINDS OF ROW.  SHAPE rows assert which form is emitted and legitimately
# differ under `PCL_OPT=none` / `PCL_OPT=-local-push` — that is the registry's
# contract.  BEHAVIOUR rows assert the program's OUTPUT and every expectation
# is the live `perl` answer (probed s470bj, perl 5.40.3); those must hold in
# every mode, because a verdict may never change what a program prints.
#
# WHY EACH NEGATIVE IS ITS OWN PROGRAM.  `escapes` is a REGION fact: one
# `eval "…"` or one nested sub anywhere in the region revokes the licence for
# every array in it.  A single multi-case file would therefore "pass" all its
# negative rows on one region-wide veto and prove nothing about the others.
#
# THE ONE SHAPE THAT LOOKS LIKE A HOLE AND IS NOT: `"@{[ push @a, 99 ]}"`.
# PExpr parses the interpolated block INTO the tree (probed in both the
# double-quote and the heredoc spelling, output identical to perl), so the
# ordinary classification applies and the licence is precise there — it is a
# POSITIVE row below, not a negative one.

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

plan tests => 61;

sub write_pl {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;
    return $pl_file;
}

sub emitted { return PCLCore::transpile("$pl2cl " . write_pl($_[0])) }

sub run_cl {
    my ($code) = @_;
    my $cl_code = emitted($code);
    my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cl_fh $cl_code;
    close $cl_fh;
    my $output = `sbcl @sbcl_rt --load $cl_file 2>&1`;
    $output =~ s/^;.*\n//gm;
    $output =~ s/^PCL Runtime loaded\n//gm;
    $output =~ s/^\s*\n//gm;
    return $output;
}

# ══ SHAPE — `local-push` FIRES ═════════════════════════════════════════════
# The item's shape is the second half of the licence, so each of these varies
# it while the array stays a clean `my @a`.

for my $case (
    ['push @a, 1'                     => 'a literal'],
    ['my $i=5; push @a, $i'           => 'a plain scalar'],
    ['my $i=5; push @a, $i * 2'       => 'an arithmetic operator result'],
    ['push @a, "x" . "y"'             => 'a concatenation'],
    ['my $i=5; push @a, "v$i"'        => 'an interpolated string'],
    ['push @a, [1,2]'                 => 'an anon-array constructor (ONE ref)'],
    ['push @a, {k=>1}'                => 'an anon-hash constructor (ONE ref)'],
    ['my %h; push @a, $h{k}'          => 'a hash element'],
    ['my @s=(1); push @a, $s[0]'      => 'an array element'],
    ['my $s; push @a, \$s'            => 'a reference to one named scalar'],
    ['my $i=5; push @a, -$i'          => 'a unary minus'],
    ['my $s = "@{[ push @a, 99 ]}"'   => 'a push INSIDE an interpolated block'],
) {
    my ($stmt, $why) = @$case;
    like(emitted("my \@a; $stmt; print scalar(\@a), \"\\n\";"),
         qr/\(%p-push1 \@a /, "local-push fires: $why");
}

# `sort` (with no block), `reverse` and `values` hand the caller ALIASES of the
# source elements — probed: `$_++ for reverse @a` increments @a — but they are
# not escapes on their own: they pass THEIR position's licence through, and an
# assignment COPIES.  A `sort` BLOCK is user code that sees `$a`/`$b`, so it
# escapes unless this region provably never writes or references either
# (the negatives below are the two halves of that).

for my $case (
    ['my @z = reverse @a'                 => 'reverse into a COPYING assignment'],
    ['my @z = sort @a'                    => 'sort (no block) into an assignment'],
    ['my @z = values @a'                  => 'values into an assignment'],
    ['my @z = sort { $a <=> $b } @a'      => 'a sort BLOCK that only READS $a/$b'],
    ['print sort @a'                      => 'sort into print'],
) {
    my ($stmt, $why) = @$case;
    like(emitted("my \@a; push \@a, 1; $stmt; print \"\\n\";"),
         qr/\(%p-push1 \@a /, "alias-transparent: $why");
}

# ══ SHAPE — the ITEM is not provably one scalar ════════════════════════════

for my $case (
    ['my @o=(1,2); push @a, @o'       => 'a whole array (must flatten)'],
    ['my %g=(k=>1); push @a, %g'      => 'a whole hash (must spread)'],
    ['my %g=(k=>1); push @a, keys %g' => 'a list-returning builtin'],
    ['push @a, (1,2)'                 => 'a parenthesised list'],
    ['push @a, 1, 2'                  => 'two arguments'],
    ['push @a, f(); sub f { (1,2) }'  => 'a user sub call (may return a list)'],
    ['my $r=[1]; push @a, @$r'        => 'a dereferenced array'],
) {
    my ($stmt, $why) = @$case;
    unlike(emitted("my \@a; $stmt; print scalar(\@a), \"\\n\";"),
           qr/%p-push1/, "local-push refused: $why");
}

# ══ SHAPE — the ARRAY escapes ══════════════════════════════════════════════
# Every spelling F1 lists, each ALONE in its own program (see the header).

for my $case (
    ['my $r = \@a'                        => '\@a — a reference to the array'],
    ['my $r = \$a[0]'                     => '\$a[i] — a reference to an element'],
    ['sub f { push @a, 9 }'               => 'a NAMED sub in the region names it'],
    ['my $c = sub { $a[0] }'              => 'an anon sub names an element'],
    ['g(@a); sub g { }'                   => 'passed WHOLE to a call'],
    ['my $o = bless {}, "K"; $o->m(@a)'   => 'passed whole to a METHOD'],
    ['my $c = \&g; $c->(@a); sub g { }'   => 'passed whole through a code ref'],
    ['eval "1"'                           => 'a string eval in the region'],
    ['my @a = ()'                         => 'a redeclaration of the same name'],
    ['for my $v (@a) { $v = 1 }'          => 'a foreach whose var WRITES'],
    ['for (@a) { print }'                 => 'a TOPIC foreach (a global var)'],
    ['$_++ for @a'                        => 'the foreach statement MODIFIER'],
    ['my @z = map { $_ } @a'              => 'map (its block sees $_ aliased)'],
    ['my @z = grep { 1 } @a'              => 'grep (its block sees $_ aliased)'],
    ['my @z = sort { $a = 1; $a <=> $b } @a'
                                          => 'a sort BLOCK that writes $a'],
    ['my $r; my @z = sort { $r = \$a; $a <=> $b } @a'
                                          => 'a sort BLOCK that takes \$a'],
    ['$_++ for reverse @a'                => 'reverse into an ALIASING consumer'],
    ['$_++ for sort @a'                   => 'sort into an ALIASING consumer'],
    ['$_++ for values @a'                 => 'values into an ALIASING consumer'],
    ['g(reverse @a); sub g { }'           => 'reverse into a CALL (aliases in @_)'],
    ['tie @a, "K"'                        => 'tie (magic on the container)'],
    ['my $o = bless \@a, "K"'             => 'blessed through a reference'],
) {
    my ($stmt, $why) = @$case;
    unlike(emitted("my \@a; $stmt; push \@a, 1; print scalar(\@a), \"\\n\";"),
           qr/%p-push1/, "escapes: $why");
}

# An `our @a` is a package cell, not a `my` declaration — no verdict at all.
unlike(emitted('our @a; push @a, 1; print scalar(@a), "\n";'),
       qr/%p-push1/, 'escapes: an `our` (package) array gets no verdict');

# ══ SHAPE — the foreach-raw conjunct ═══════════════════════════════════════

like(emitted('my @a=(1,2); for my $x (@a) { print $x } print "\n";'),
     qr/\(p-foreach-raw \(\$x \@a\)/,
     'control: a read-only loop over an UNWRITTEN array keeps the raw arm');

unlike(emitted('my @a=(1,2); for my $x (@a) { $a[0] = 9; print $x } print "\n";'),
       qr/p-foreach-raw/,
       'demoted: the BODY writes an element of the list array (#1140 repro A)');

unlike(emitted('my @a=(1,2); my $c=1; for my $x (@a) { if ($c) { push @a, 3 } last } print "\n";'),
       qr/p-foreach-raw/,
       'demoted: a write NESTED inside an if in the body still counts');

unlike(emitted('my @a=(1,2); for my $x (@a) { print $x } continue { $a[0]=9 } print "\n";'),
       qr/p-foreach-raw/,
       'demoted: a write in the CONTINUE block counts (it runs during the loop)');

like(emitted('my @a=(1,2); for my $x (@a) { print $x } $a[0] = 9; print "\n";'),
     qr/\(p-foreach-raw \(\$x \@a\)/,
     'NOT demoted: a write AFTER the loop is not in the body region');

unlike(emitted('my @a=(3,1); for my $x (sort { $a <=> $b } @a) { $a[1]=9; print $x; last } print "\n";'),
       qr/%p-sort-classic/,
       'classic-sort follows: the sorted SOURCE array is written in the body');

# …and the licence it inherits still WORKS where the body does not write: the
# row that would otherwise go silently missing when a conjunct is added.
like(emitted('my @a=(3,1); my @o; for my $x (sort { $a <=> $b } @a) { push @o, $x } print "@o\n";'),
     qr/p-foreach-raw.*\Q(%p-sort-classic :num-asc\E/s,
     'classic-sort keeps its foreach licence on an unwritten source array');

# ══ BEHAVIOUR — every expectation is the live perl answer ══════════════════

is(run_cl(<<'PL'), "plain:99\n", '#1140 repro A: a body write reaches the alias (perl: 99)');
my @fa = (1,2,3);
for my $x (@fa) { $fa[0] = 99; print "plain:$x\n"; last }
PL

is(run_cl(<<'PL'), "sorted:99\n", '#1140 repro B: the sort-fed twin (perl: 99)');
my @fs = (3,1,2);
for my $x (sort { $a <=> $b } @fs) { $fs[1] = 99; print "sorted:$x\n"; last }
PL

is(run_cl(<<'PL'), "fr-plain-aassign:2\n", 'a whole-array assignment during the loop (perl: 2)');
my @fb = (2,1); for my $x (@fb) { @fb = (7,8); print "fr-plain-aassign:$x\n"; last }
PL

is(run_cl(<<'PL'), "3|1 2 3|3\n", 'local-push: length, order and the array (perl: 3|1 2 3|3)');
my @a; my $n; for my $i (1..3) { $n = push @a, $i } print "$n|@a|", scalar(@a), "\n";
PL

is(run_cl(<<'PL'), "K|1|9\n", 'local-push stores a BLESSED ref and a live element (perl: K|1|9)');
my @a; my $o = bless {v=>1}, "K"; push @a, $o; my $s; push @a, \$s; $s = 9;
print ref($a[0]), "|", $a[0]{v}, "|", ${$a[1]}, "\n";
PL

is(run_cl(<<'PL'), "1 2|1 2\n", 'local-push COPIES the value it stores (perl: 1 2|1 2)');
my @a; my $v = 1; push @a, $v; $v = 2; push @a, $v; print "@a|1 2\n";
PL

is(run_cl(<<'PL'), "6|1 2 3\n", 'the raw foreach arm still sums an unwritten array (perl: 6|1 2 3)');
my @a=(1,2,3); my $s=0; for my $x (@a) { $s += $x } print "$s|@a\n";
PL
