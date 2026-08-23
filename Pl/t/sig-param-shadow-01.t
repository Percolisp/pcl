#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# sig-param-shadow-01.t — task #454: a SIGNATURE PARAMETER is the sub's own
# declaration, so it shadows a same-named file lexical.
#
# perl desugars `sub f ($x) {…}` to `my $x = $_[0]` INSIDE the sub, so every
# `$x` in the body is the parameter and the file's `my $x` is not captured at
# all.  PCL refused the whole file — "Parser2 TODO: file lexical 'x' captured
# by sub f" — because the two scope questions were blind to signatures:
# `_check_sub_captures` scans the sub's BLOCK, and the signature is not in the
# block; `_ref_shadowed` climbs to the Statement::Sub and asks
# `_stmt_declares_canon` about the preceding siblings, of which the signature
# is one.  Both ask `_signature_param_canons` now — the detector and the
# rewriter share the resolver, which is the project's rule for this pair.
#
# ONLY THE PARAMETER NAMES: a DEFAULT is an expression evaluated in the sub,
# so `my $y = 5; sub f ($x = $y)` really does reference the outer `$y` — hence
# the top-level-comma split, and hence the last row here.
#
# PPI PRODUCES BOTH SHAPES FOR THE SAME SOURCE, and a guard that saw one would
# be half a guard: with `use feature "signatures"` on an EARLIER line `($x)`
# lexes as a PPI::Structure::Signature, on the pragma's own line as a
# PPI::Token::Prototype (that line dependence is task #455).  Row 5 is the
# one-line spelling on purpose.
#
# The shape emits IDENTICALLY across all four populations (951 files A/B'd, 0
# DIFF, plus corpus-diff over the 111), so no corpus guards it: these rows do.

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

sub write_pl {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;
    return $pl_file;
}

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

sub both_agree {
    my ($code, $desc) = @_;
    my $perl = `perl @{[ write_pl($code) ]} 2>&1`;
    my $pcl  = run_cl($code);
    is($pcl, $perl, "$desc (perl: " . ($perl =~ s/\n/\\n/gr) . ")");
}

# ---- the refusal is gone, and the answers are perl's ----------------------

both_agree(<<'PL', 'a signature param shadows a LATER file `my` of the same name');
use feature "signatures";
sub f ($x) { "f($x)" }
my $x = f 5;
print "$x\n";
PL

both_agree(<<'PL', '... with a DEFAULT parameter');
use feature "signatures";
sub f ($x = 1) { "f($x)" }
my $x = f 5;
print "$x\n";
PL

both_agree(<<'PL', '... with a slurpy tail');
use feature "signatures";
sub f ($x, @r) { "f($x)[@r]" }
my $x = f 5;
print "$x\n";
PL

both_agree(<<'PL', '... and through a nested anon sub that closes over the param');
use feature "signatures";
sub m1 ($x) { my $inner = sub { "in($x)" }; $inner->() }
my $x = 4;
print m1(2), " x=$x\n";
PL

both_agree('use feature "signatures"; sub f ($x) { "f($x)" } my $x = f 5; print "$x\n";',
           'the ONE-LINE spelling, where PPI gives a Token::Prototype instead');

# ---- the file lexical declared BEFORE the sub still belongs to the file ---

both_agree(<<'PL', 'a file `my $x` BEFORE the sub keeps its own value');
use feature "signatures";
my $x = 9;
sub k ($x) { "k($x)" }
print k(1), " x=$x\n";
PL

# ---- the narrowing must not go further: a REAL capture still refuses ------
#
# `($x)` declares `$x` and nothing else, so the body's `@x` is the file's
# `my @x` — a genuine capture, and the refusal is still the honest answer.
{
    my $pl = write_pl(<<'PL');
use feature "signatures";
sub g ($x) { "g($x)[@x]" }
my @x = (7, 8);
print g(1), "\n";
PL
    my $err = `$pl2cl $pl 2>&1 >/dev/null`;
    like($err, qr/file lexical 'x' captured by sub g/,
         'a DIFFERENT-sigil use of the same bare name is still a capture');
}

# ---- task #455: the pragma's OWN LINE is inside the feature's region ------
#
# PPI's feature tracking starts at the line AFTER `use feature "signatures"`,
# so a sub sharing that line came back as a Token::Prototype and was emitted
# through the OLD-prototype path — the params became a raw CL lambda list
# instead of arity-checked bindings from @_, and an empty slurpy then
# interpolated as an uninitialized value where perl is silent.
#
# THE REPAIR IS THE BOUNDARY, NOT THE TEXT, and the second row is why: perl
# reads `($a)` as an old-style PROTOTYPE where the feature is not yet on, so
# `$a` in that body is the package variable.  perl-tests/signatures.t:17
# asserts exactly that, and a purely textual rule broke it (measured).

both_agree('use feature "signatures"; sub f ($x, @r) { "[@r]" }' . "\n"
         . 'print f(0), "\n";',
           'a sub on the pragma\'s OWN line is a signature (no spurious warning)');

# s439 review fix: the enabling pragma in its `qw()` spelling.  The first cut of
# `_signatures_enabled_at` matched a QUOTED 'signatures' only, so this row
# took the old-prototype lowering (PCL `-u|2 3-2`, perl `0-u|2-2`).  Any
# quoting of the word, a `:5.NN` bundle >= 36 and `use v5.36`+ all enable.
both_agree('use feature qw(signatures say); sub f ($x, @r) { scalar(@r) . "-" . ($r[0] // "u") }' . "\n"
         . 'print f(1), "|", f(1,2,3), "\n";',
           'the same-line pragma spelled `use feature qw(signatures say)` enables too');

# ---- #497 (s440, found by the SHAPES corpus): a signature PARAMETER is a
# declaration to EVERY rewriter, not only to the capture detector.  PPI hands
# `($x = 1)` over as a Structure::Signature when the feature is in force from
# an earlier line, and then the parameter's own Symbol was offered to (a) the
# block-package requalifier, which asked `_binding_at` (a scope walk that
# cannot see a signature, a sibling of the body) and rewrote it to `$Pkg::x`;
# (b) the file-lexical span renamer, which asked `_ref_shadowed` (same blind
# spot) when a `my $x` in an earlier package spans into the sub's section.
# Either way the head came out as `sub f ($S02::x = 1)` / `($S01::x)`, arity
# 0/0, body reading the global: `f 5` dropped, `f(5)` died "Too many
# arguments".  One predicate (_symbol_is_signature_param) now answers both
# resolvers.  The rows are the four shapes, perl the oracle.
both_agree(<<'PL', '#497: the same signature sub NAME in two block-scoped packages');
{ package S01; use feature "signatures"; sub f ($x) { "f($x)" } my $x = f 5; print "$x\n"; }
{ package S02; use feature "signatures"; sub f ($x = 1) { "g($x)" } my $x = f 5; print "$x\n"; }
PL
both_agree(<<'PL', '#497: ... statement-form packages, a spanning file `my $x` with the parameter name');
package S01; use feature "signatures"; sub f ($x) { "f($x)" } my $x = f 5; print "$x\n";
package S02; use feature "signatures"; sub f ($x = 1) { "g($x)" } my $y = f 5; print "$y\n";
PL
both_agree(<<'PL', '#497: ... three packages, a slurpy, the feature enabled once at the top');
use feature "signatures";
{ package S01; sub f ($x) { "f($x)" } my $x = f 5; print "$x\n"; }
{ package S02; sub f ($x = 1) { "g($x)" } my $x = f 5; print "$x\n"; }
{ package S03; sub f ($x, @r) { "h($x|@r)" } my $x = f 5, 6; print "$x\n"; }
PL
both_agree(<<'PL', '#497 inverse: different names in two block packages, parenthesised and not');
{ package S01; use feature "signatures"; sub f ($x) { "f($x)" } print f(5), "\n"; }
{ package S02; use feature "signatures"; sub g ($x = 1) { "g($x)" } my $y = g 5; print "$y\n"; }
PL

# ---- #485: a signature DEFAULT is part of the sub's CAPTURE SET -----------
#
# The other half of the top-level-comma split above.  A default is an
# expression evaluated INSIDE the sub, so `my $y = 5; sub f ($x = $y)` really
# does capture the file lexical — and every capture question here scanned the
# BLOCK, of which the signature is a SIBLING.  Nobody saw it: no refusal, no
# promotion to a package cell, and the default read an unbound name (`f()`
# where perl says `f(5)`).  `_sub_captures_name` is now the one resolver the
# gate, the promoter and the nested-sub hoist share, and it asks the block AND
# the signature.
#
# Shadowing INSIDE a signature is the left-parameter rule, probed vs perl
# 5.40.3: a parameter is not in scope in its OWN default (`sub s1 ($x = $x)`
# reads the outer $x) and a LATER parameter never is either.

both_agree(<<'PL', '#485: a signature default reading a file lexical (the reproducer)');
use feature "signatures";
my $y = 5;
sub f ($x = $y) { "f($x)" }
print f(), "\n";
print f(9), "\n";
PL

both_agree(<<'PL', '#485: ... spelled `//=` (perl 5.38+), including an explicit undef');
use feature "signatures";
my $y = 5;
sub g ($x //= $y) { "g(" . (defined $x ? $x : "U") . ")" }
print g(), "\n", g(0), "\n", g(undef), "\n";
PL

both_agree(<<'PL', '#485: ... spelled `||=`');
use feature "signatures";
my $y = 5;
sub h ($x ||= $y) { "h($x)" }
print h(), "\n", h(0), "\n", h(7), "\n";
PL

both_agree(<<'PL', '#485: TWO defaults, two file lexicals, the second reading the first param');
use feature "signatures";
my $y = 5;
my $z = 11;
sub m2 ($a = $y, $b = $z + $a) { "m2($a,$b)" }
print m2(), "\n", m2(1), "\n", m2(1,2), "\n";
PL

both_agree(<<'PL', '#485: a default that CALLS a sub reading the lexical (already worked — the sub body is the capture)');
use feature "signatures";
my $y = 5;
sub base { $y * 2 }
sub k ($x = base()) { "k($x)" }
print k(), "\n", k(1), "\n";
PL

both_agree(<<'PL', '#485: a parameter is NOT in scope in its OWN default — it reads the outer lexical');
use feature "signatures";
my $x = "OUT";
sub s1 ($x = $x) { "s1($x)" }
print s1(), "\n", s1("P"), "\n";
PL

both_agree(<<'PL', '#485: a LATER parameter is not in scope in an earlier default either');
use feature "signatures";
my $q = "OUT";
sub s2 ($p = $q, $q = "PARAM") { "s2($p,$q)" }
print s2(), "\n", s2("A"), "\n", s2("A","B"), "\n";
PL

# The REWRITER half, and it was silently wrong on its own: with a file lexical
# of the parameter's name in scope, every rewriter that renamed that lexical
# renamed the default's `$p` with it, because `_ref_shadowed` saw only the
# parameter TOKEN as a declaration and not the parts to its left.  PCL printed
# `n3(P,)` where perl prints `n3(P,P)`.  The `$a` spelling is the same bug
# through a different rename (Pl::GlobalPartition's `__excl__` pass).
both_agree(<<'PL', '#485: an EARLIER PARAMETER default, with a same-named file lexical (rewriter half)');
use feature "signatures";
my $p = "FILE";
sub n3 ($p, $q = $p) { "n3($p,$q)" }
print n3("P"), "\n", n3("P","Q"), "\n", "p=$p\n";
PL
both_agree(<<'PL', '#485: ... the same through the $a/$b exception rename');
use feature "signatures";
my $a = "FILE";
sub n2 ($a, $b = $a) { "n2($a,$b)" }
print n2("P"), "\n", n2("P","Q"), "\n", "a=$a\n";
PL

# A BLOCK extent promotes under a MANGLED name ($y__file__N), so this row is
# the one that proves the rename reaches INTO the signature.
both_agree(<<'PL', '#485: two block-scoped lexicals, two subs — the mangled promotion reaches the default');
use feature "signatures";
{ my $y = 5; sub f11 ($x = $y) { "f11($x)" } }
{ my $y = 9; sub g11 ($x = $y) { "g11($x)" } }
print f11(), "\n", g11(), "\n", f11(1), "\n";
PL

# ... and the SAME shape in the Token::Prototype spelling, where the signature
# is ONE token with no Symbol for the rename to reach.  `_promote_captured`
# refuses that combination outright rather than renaming the declaration and
# leaving the default on the old name (which is what it did before the
# refusal: `f12()` for perl's `f12(5)`).
both_agree('use feature "signatures"; no warnings;' . "\n"
         . '{ my $y = 5;' . "\n"
         . '  use feature "signatures"; sub f12 ($x = $y) { "f12($x)" } }' . "\n"
         . 'print f12(), "\n", f12(1), "\n";',
           '#485: ... and in the Token::Prototype spelling of the same signature');

# INVERSES: a default that names nothing lexical must not promote anything,
# and `our`/`state` inside a default (v1's _parse_signature declares the cell)
# must keep working.
both_agree(<<'PL', '#485 inverse: a literal default and a default reading a GLOBAL of the same name');
use feature "signatures";
our $y = "GLOBAL";
sub lit ($x = 42) { "lit($x)" }
sub glob2 ($x = $main::y) { "glob2($x)" }
print lit(), "\n", lit(1), "\n", glob2(), "\n";
PL
both_agree(<<'PL', '#485 inverse: `our` and `state` declarations INSIDE a default still work');
use feature "signatures", "state";
our $Z = "ZED";
my $y = "FILE";
sub d1 ($x = our $Z) { "d1($x)" }
sub d2 ($x = state $n = "ST") { "d2($x)" }
sub d3 ($x = $y, $w = $x) { "d3($x,$w)" }
print d1(), "\n", d2(), "\n", d3(), "\n", d3("A"), "\n";
PL

# THE NEGATIVE THIS FILE DOES NOT ASSERT, and why: `sub t000 ($a)` written
# where the feature is NOT yet on is an old-style (illegal) PROTOTYPE in perl,
# which perl IGNORES — so `$a` in the body is the package variable and
# `&t000(456)` is 123.  PCL answers 456 on BOTH paths, because the
# old-prototype lowering binds a named prototype`s names as parameters too.
# That is a blessed baseline failure (`baselines/fail-baseline.tsv`,
# signatures.t "() not signature when not enabled") and task #486 — it is
# NOT what the boundary repair above changes (emission over the 111-file
# corpus is identical), so asserting it here would add a knowingly-failing row.
