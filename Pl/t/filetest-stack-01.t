#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# filetest-stack-01.t — task #372 (Option B phase 2, Track B1): STACKED
# FILETESTS.  Design: docs/b1-operand-grammar-s416.md.
#
# `-f -d $x` is one term in perl and was DROPPED WHOLE by PCL — 27 statements
# over the companion suite, every one of them "Bug. Fell through".  Two
# separate things had to be true for it to parse and mean the right thing, and
# both are guarded here:
#
#  1. A `-X` Operator STARTS A TERM.  `_is_print_term_start` answered 0 for
#     every Operator except `!`/`~`/`not`, so the pre-pass that supplies the
#     `$_` default read `-d` as "no operand" and spliced `$_` into the MIDDLE
#     of the run (`-f $_ -d $x`), which then fell through the operator loop.
#     The same oracle decides `print $fh -e $f`, where perl reads $fh as the
#     handle BECAUSE `-e` starts a term.
#
#  2. A stacked run is the `_`-CHAIN, not a nest (perldoc -f -X):
#
#         -f -w -x $file   ==   -x $file && -w _ && -f _
#
#     The RIGHTMOST test runs on the real operand; each earlier one re-uses the
#     stat buffer `_`, &&-short-circuited.  Nesting them instead — `-f` applied
#     to `-d`'s 1/undef — is SILENT WRONG and inverts the common case:
#     `-e -f $file` is TRUE in perl and undef nested.
#
# Every expectation below is the live `perl` answer (probed s417).
#
# NOT asserted here, and deliberately: perl's filetest FALSE is defined ("")
# when the stat succeeded and undef only when it failed, while PCL answers
# undef for both.  That is a property of the whole `p--*` runtime family, not
# of stacking — it diverges for a plain `-f "/tmp"` too — so it is filed with
# its own reproducer (task #403) rather than weakened into a wrong expectation
# here.  The one member of that family whose answer is a VALUE rather than a
# flag, `-s`, IS asserted (task #740, s456ah): an empty file's size is a
# defined 0, and only a FAILED stat is undef.

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

plan tests => 13;

# Fixtures built by the HARNESS (real perl), with their paths interpolated into
# the generated program — so the expectations below do not depend on anything
# about the machine the suite runs on.  $D is a directory, $F a 5-byte plain
# file, $O a path the print rows write to.
my $dir  = tempdir(CLEANUP => 1);
my $file = "$dir/plain.txt";
open my $mk, '>', $file or die "cannot create fixture: $!";
print $mk "hello";
close $mk;
my $empty = "$dir/empty.txt";
open my $mke, '>', $empty or die "cannot create empty fixture: $!";
close $mke;
my $FIX = qq{my \$D = "$dir"; my \$F = "$file"; my \$O = "$dir/printed.txt";\n}
        . qq{my \$E = "$empty"; my \$M = "$dir/nope.txt";\n};

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

# ── 1. The chain: value and short-circuit order ──────────────────────────────
# `-f -d $D` is `-d $D && -f _`: the dir IS a dir, so the chain runs `-f` on the
# stat buffer and is false.  `-e -f $F` is `-f $F && -e _` — TRUE, and the row
# a nest gets backwards.  `-s -f $F` yields the SIZE through the chain.
is(run_cl($FIX . <<'PL'), "1:\n2:\n3:1\n4:1\n5:\n6:5\n",
print "1:", (-f -d $D), "\n";
print "2:", (-f -d $F), "\n";
print "3:", (-e -f $F), "\n";
print "4:", (-e -r -f $F), "\n";
print "5:", (-r -w -x $F), "\n";
print "6:", (-s -f $F), "\n";
PL
   'stacked filetests chain over `_`, not nested');

# `-s` is the one filetest whose answer is a VALUE, so an EMPTY file is a
# defined 0 and only a FAILED stat is undef (task #740).  Every line below is
# the live perl answer; `b:` shows 0 is still boolean-false, and `z:` runs the
# size out through this file's own `_` chain.
is(run_cl($FIX . <<'PL'), "e:d0\nf:d5\nm:u\nu:d0\np:0\nb:F\nz:0.\n",
print "e:", (defined(-s $E) ? "d" : "u"), (-s $E), "\n";
print "f:", (defined(-s $F) ? "d" : "u"), (-s $F), "\n";
print "m:", (defined(-s $M) ? "d" : "u"), (-s $M), "\n";
stat($E);
print "u:", (defined(-s _) ? "d" : "u"), (-s _), "\n";
printf("p:%d\n", -s $E);
print "b:", ((-s $E) ? "T" : "F"), "\n";
print "z:", (-s -f $E), ".\n";
PL
   '-s on an EMPTY file is a defined 0; only a failed stat is undef (#740)');

like(emitted(q{my $x = "/tmp"; my $r = -e -r -f $x;}),
     qr/\(p-&&\s+\(p-&&\s+\(p--f\s+\$x\)\s+\(p--r\s+_\)\)\s+\(p--e\s+_\)\)/,
     'a three-deep run emits ((-f OPERAND && -r _) && -e _)');

# ── 2. The parse: a stacked run is a TERM wherever a term may stand ──────────
is(run_cl($FIX . <<'PL'), "T:1\nc:2:tail\nq:0\na:no\n",
sub take { print "T:", ($_[0] ? 1 : 0), "\n" }
take(-e -f $F);
my @a = (-f -d $D, "tail"); print "c:", scalar(@a), ":", $a[1], "\n";
print "q:", (-f -d $D ? 1 : 0), "\n";
print "a:", ((-f -d $D && 7) || "no"), "\n";
PL
   'a stacked run is a term (funcall arg, list, ternary, &&)');

is(run_cl($FIX . <<'PL'), "if\n",
if (-e -f $F) { print "if\n" } else { print "else\n" }
PL
   'a stacked run is a condition');

# ── 3. The print family: `-X` starting a term is what makes $fh the handle ───
is(run_cl($FIX . <<'PL'), "1\nn\n",
print STDOUT -e $F; print "\n";
print -f -d $D ? "y" : "n"; print "\n";
PL
   'print FH with a leading filetest argument');

# `print $fh -e $F` — PPI splits the `-e` here into Operator('-') + Word('e')
# (docs/ppi-upstream-bugs.md); without the token repair this printed to STDOUT
# a subtraction of a call to a sub named `e`.
is(run_cl($FIX . <<'PL'), "file:1\n",
open(my $fh, ">", $O) or die "open: $!";
print $fh -e $F;
close $fh;
open(my $in, "<", $O) or die "reopen: $!";
my $got = <$in>;
print "file:", $got, "\n";
PL
   'print $fh with a leading filetest (PPI split-token repair)');

is(run_cl($FIX . <<'PL'), "block:1\n",
open(my $fh, ">", $O) or die "open: $!";
print {$fh} -e $F;
close $fh;
open(my $in, "<", $O) or die "reopen: $!";
my $got = <$in>;
print "block:", $got, "\n";
PL
   'print {EXPR} with a leading filetest');

# ── 4. INVERSE GUARDS — shapes that must NOT move ───────────────────────────
# These all worked before B1 and are what a wider rule would break: the
# predicate keys on the ATTACHED LETTER, so a plain `-` stays binary minus.
is(run_cl($FIX . <<'PL'), "d:def\nn:F\nb:1\nm:5\nc:5\nu:T\ns:7\n",
print "d:", (defined(-e $F) ? "def" : "undef"), "\n";
print "n:", ((!-e $F) ? "T" : "F"), "\n";
my $y = 5;
print "b:", (!!$y), "\n";
print "m:", (- -$y), "\n";
print "c:", (~~$y), "\n";
$_ = $D; print "u:", (-d ? "T" : "F"), "\n";
my $x = 10; print "s:", ($x - 3), "\n";
PL
   'single filetests and the other prefix operators are untouched');

# `! -e -f $F` is `!(-f $F && -e _)` — a mixed run keeps per-op recursion
# AROUND the filetest sub-run; only the filetest part chains.
is(run_cl($FIX . <<'PL'), "not:F\n",
print "not:", ((! -e -f $F) ? "T" : "F"), "\n";
PL
   'a mixed prefix run nests around the filetest sub-run');

# perl does NOT stack through a VARIABLE holding a filetest's value (probed:
# `my $t = -f $F; -e $t` is false), so the chain mark must sit on the
# REDUCTION, never on the value.
is(run_cl($FIX . <<'PL'), "v:F\n",
my $t = -f $F;
print "v:", ((-e $t) ? "T" : "F"), "\n";
PL
   'no stacking through a variable holding a filetest value');

# `print $fh - e $f` — with a SPACE this is negation of a call to sub `e`, and
# perl agrees (deparse: `-(e(...))`).  The repair keys on adjacency, so this
# must keep parsing as a call and NOT become a filetest.
my $spaced = emitted(q{sub e { 7 } my $f = 1; print $fh - e $f;});
like($spaced, qr/\(pl-e\s+\$f\)/,
     '`print $fh - e $f` (spaced) stays a call to sub e, not a filetest');
unlike($spaced, qr/p--e/,
     '`print $fh - e $f` (spaced) emits no filetest operator');
