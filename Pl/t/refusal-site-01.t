#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# refusal-site-01.t — A RULED REFUSAL IS STATEMENT-LEVEL (task #1037; RULED
# s465 by the USER, docs/plan-test-audit-s464.md §5.5 / DECIDED §s465).
#
# Until s466 a ruled refusal DIED AT TRANSPILE TIME, which cost the whole file.
# `perl-tests/state.t` and perl's own `t/op/state.t` each contain exactly ONE
# `given` block, in an otherwise supported file, and each lost every one of its
# ~160 rows to it.  The ruling: a refusal is an event at a STATEMENT, exactly
# like a drop (`docs/ir-spec.md` §9.3), so it takes the drop form's shape — a
# perl-shaped, trappable run-time die at the statement's own site, with the
# rest of the file compiling and running.  A refusal that aborts the transpile
# is a bug in the refusal, not a property of the feature.
#
# What each row is holding down:
#   1  the file TRANSPILES (pl2cl exits 0) — that is the whole change;
#   2  the emission marker is `;; RULED REFUSAL:`, never `;; PARSE ERROR:`.
#      That second string is the drop CENSUS key (tools/drop-census.pl,
#      tools/corpus-diff.pl's SILENT-DROP counter, both runners' `drops`
#      column), and a refused statement is not a compiler gap to be closed, so
#      it must not enter the census that exists to be shrunk;
#   3  the die is REACHED and TRAPPABLE — `$@` carries the byte-identical
#      ruled text with file and line, `eval` returns undef;
#   4  the statements BEFORE it ran and the program continues after the eval;
#   5  a refusal the program never REACHES costs nothing at all — this is the
#      shape that cost state.t its file;
#   6  the announcement and the emitted die name the SAME statement (one
#      `Pl::Parser::_drop_site` builder), and the announcement has its OWN verb,
#      `PCL: refused statement at …`: the runners, tools/gate-set-scan.pl and
#      tools/drop-harvest.pl key on the fixed prefix `PCL: statement dropped
#      at`, and a refusal must never be counted as a drop;
#   7  the four other statement-shaped families take the same route — `class
#      NAME ;` (which is refused on code that COMPILES, from Parser2's own
#      site, not the drop classifier), `defer { }` and infix `~~`;
#   8  STRING-EVAL MODE STILL DIES AT TRANSPILE (#363), and that is not an
#      exception but the same reasoning: the emission is produced by the
#      `pl2cl --server` subprocess and discarded on error, so there is nothing
#      to carry a run-time form — while perl's own contract for `eval STRING`
#      is that what does not compile sets `$@`;
#   9  the INVERSE — a file with no refusal gets no `RULED REFUSAL` marker and
#      no announcement, i.e. the change is confined to the refusal site.
#
# INVERSE GUARD, measured on a `57848f3` worktree (the commit this work sits
# on): 13 of the 19 rows FAIL there — 1, 2, 4, 5, 6, 7, 8, 9, 12, 13, 14, 15,
# 16 — because pl2cl exits nonzero and emits NOTHING for every one of those
# files.  The six that pass on the base do so for a reason each, and none of
# them is the change: rows 3, 10 and 11 are NEGATIVES over an empty emission
# (vacuous there, load-bearing here); row 17 is the string-eval half, which
# this change deliberately leaves alone; rows 18–19 are the inverse file, which
# has no refusal in it at all.
#
# NOT COVERED, and why: `format NAME = … .` never reaches this machinery
# end to end.  PPI swallows the format body AND every statement after it into
# one `PPI::Statement` (it does not honour the `.` terminator), and PCL then
# emits the trailing statements with the format part silently gone — a
# PRE-EXISTING silent drop, filed as its own task.  The classifier's `format`
# arm is exercised directly in `Pl/t/ruled-refusal-01.t`.

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

plan tests => 19;

sub write_pl {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;
    return $pl_file;
}

# transpile_raw, not transpile: these rows MEAN to read the refusal
# announcement, which PCLCore::transpile fails a row for (and rightly,
# everywhere else).  The .pl path comes back too — the rows assert that the
# refusal names it.
sub transpile_refusing {
    my ($code) = @_;
    my $pl = write_pl($code);
    my ($cl, $err, $rc) = PCLCore::transpile_raw("$pl2cl $pl");
    return ($cl, $err, $rc, $pl);
}

sub run_cl {
    my ($cl_code) = @_;
    my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cl_fh $cl_code;
    close $cl_fh;
    my $output = `sbcl @sbcl_rt --load $cl_file 2>&1`;
    $output =~ s/^;.*\n//gm;
    $output =~ s/^PCL Runtime loaded\n//gm;
    $output =~ s/^\s*\n//gm;
    return $output;
}

# ── The refusing program.  Line 4 is the `given`; the prints on either side
# are row 4's evidence, and the eval is row 3's. ─────────────────────────────
my $REFUSING = <<'PL';
use feature 'switch';
no warnings;
print "before\n";
my $ok = eval { given (3) { when (3) { print "in\n" } } ; 1 };
print "returned=", (defined $ok ? "value" : "undef"), "\n";
print "err=$@";
print "after\n";
PL

my ($cl, $err, $rc, $pl) = transpile_refusing($REFUSING);

# 1 ── the file transpiles: the refusal costs the statement, not the file.
is($rc, 0, 'pl2cl exits 0 on a ruled refusal — the file is no longer lost')
    or diag("stderr was: $err");

# 2 ── the census key does NOT grow.  A refusal has its own marker.
like($cl, qr/\(progn ;; RULED REFUSAL: given\/when \(feature 'switch'\) is not supported/,
     'the refused statement is replaced by the RULED REFUSAL form');
unlike($cl, qr/;; PARSE ERROR:/,
       'and NOT by a `;; PARSE ERROR:` — a refusal never enters the drop census');

# 3 + 4 ── reached, trappable, and the rest of the program runs.
my $out = run_cl($cl);
like($out, qr/^err=PCL: given\/when \(feature 'switch'\) is not supported -- removed in perl 5\.42, at \Q$pl\E line 4$/m,
     'the refused statement DIES when reached, and $@ carries the ruled text with file and line');
like($out, qr/^returned=undef$/m,
     'the eval returns undef — the refusal is a real die, not a value');
like($out, qr/before.*returned=undef.*after/s,
     'statements before the refusal ran, and the program continues after the eval');

# 5 ── a refusal the program never reaches costs nothing.  THIS is the shape
# that cost perl-tests/state.t and t/op/state.t their whole files.
my ($cl_unreached, undef, $rc_unreached) = transpile_refusing(<<'PL');
use feature 'switch';
no warnings;
sub unreached { given (1) { when (1) { print "no\n" } } }
print "a\n"; print "b\n"; print "c\n";
PL
is($rc_unreached, 0, 'a file whose `given` is never reached transpiles');
is(run_cl($cl_unreached), "a\nb\nc\n",
   'and runs to its end — every row of an otherwise supported file survives');

# 6 ── ONE builder, and the announcement has its own verb.
my ($ann_site) = $err =~ /^PCL: refused statement at (\S+ line \d+): /m;
my ($die_site) = $cl  =~ /\(pcl:p-die "PCL: [^"]*, at (\S+ line \d+)\n"\)/;
ok(defined $ann_site && length $ann_site,
   'the transpile-time announcement fires, with the refusal verb')
    or diag("stderr was: $err");
is($die_site, $ann_site,
   'the announcement and the emitted die name the SAME statement (one _drop_site builder)');
unlike($err, qr/^PCL: statement dropped at/m,
       'a refusal is never announced with the DROP verb the runners count on');

# 7 ── the other statement-shaped families take the same route.
#
# 7a `class NAME ;` is refused from Parser2's own site, on code that COMPILES
#    (it would otherwise be the indirect-object call `Foo->class`), so it is the
#    second route into the same emission — not the drop classifier.
my ($cl_class, $err_class, $rc_class, $pl_class) = transpile_refusing(<<'PL');
use feature 'class';
no warnings;
print "before\n";
class Point;
print "after\n";
PL
is($rc_class, 0, '`class NAME ;` refuses at its statement, not at the file')
    or diag("stderr was: $err_class");
like($cl_class,
     qr/\(progn ;; RULED REFUSAL: feature 'class' is not supported.*?PCL: feature 'class' is not supported, at \Q$pl_class\E line 4/s,
     'and emits the perl-shaped die at the statement site');

# 7b `defer { … }` (perl 5.36).  The trailing statement is load-bearing: PPI
# joins a block-form `defer` to the statement AFTER it, so a `defer` that ends
# the file is not one statement PPI can lose — it lowers as a CALL to a sub
# named `defer` and is a PRE-EXISTING silent wrong (filed separately, and not
# this change's to fix).
my ($cl_defer, $err_defer, $rc_defer) = transpile_refusing(<<'PL');
use feature 'defer';
no warnings;
print "before\n";
defer { print "deferred\n" }
print "after\n";
PL
is($rc_defer, 0, '`defer { }` refuses at its statement')
    or diag("stderr was: $err_defer");
like($cl_defer, qr/;; RULED REFUSAL: defer blocks are not supported/,
     'and takes the same emission');

# 7c infix `~~` (the smart match, removed in perl 5.42).
my ($cl_sm, $err_sm, $rc_sm) = transpile_refusing(<<'PL');
no warnings;
my @y = (1,2,3);
my $r = (2 ~~ @y);
PL
is($rc_sm, 0, 'infix `~~` refuses at its statement')
    or diag("stderr was: $err_sm");

# 8 ── string-eval mode is DELIBERATELY unchanged: it still refuses at
# transpile, because there is no emission to carry a run-time form (#363).
# This row passes on the base commit too — it is the boundary, not the change.
my ($cl_eval, undef, $rc_eval) = transpile_refusing(<<'PL');
no warnings;
print "before\n";
my $r = eval 'given (3) { when (3) { 1 } } ; 42';
print "r=", (defined $r ? $r : "undef"), "\n";
print "at=", ($@ =~ /given\/when/ ? "ruled" : "other"), "\n";
print "after\n";
PL
SKIP: {
    skip "outer file did not transpile", 1 if $rc_eval != 0;
    like(run_cl($cl_eval), qr/before.*r=undef.*at=ruled.*after/s,
         'a refusal inside `eval STRING` still sets $@ at transpile, and the program runs on');
}

# 9 ── the inverse: no refusal, no marker, no announcement.
my ($cl_clean, $err_clean, $rc_clean) = transpile_refusing(<<'PL');
no warnings;
my @y = (1,2,3);
my $r = ~~$y[0];
print "clean=$r\n";
PL
is($rc_clean, 0, 'a file with no refusal transpiles');
ok($cl_clean !~ /RULED REFUSAL/ && $err_clean !~ /^PCL: refused statement/m,
   'and gets no refusal marker and no announcement — the change is confined to the site')
    or diag("cl/stderr: $err_clean");
