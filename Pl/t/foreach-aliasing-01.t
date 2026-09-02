#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# foreach-aliasing-01.t — `for (LVALUE) { $_ = ... }` must bind $_ to the live
# container so writes propagate, exactly as `for (@a) { $_ = ... }` aliases array
# elements.  PCL binds the loop variable to the *same box object* the container
# holds; the fix is to make the foreach-list codegen surface that box (its
# box-returning form) instead of a fresh value-box.
#
# Covered here (the aliasable forms PCL supports):
#   - single hash element   $h{k}   -> p-gethash-box
#   - single array element  $a[i]   -> p-aref-box
#   - the same THROUGH A REF ($r->{k}, $$r{k})  -> p-gethash-deref-box / …
#   - subscript CHAINS ($h{a}{b}, $r->{a}[0], $a[0][1]) -> the outer access's
#     plain box head (its base is the previous access's value, not a name)
#   - substr/pos/vec lvalues are covered in lvalue-ref-01.t
# Both SPELLINGS are covered, because they are two lowering sites: the block
# form `for (LV) { … }` and the statement modifier `EXPR for (LV);` (#262/#263).
# Also pinned: forms that must NOT alias (computed temps, plain builtins) and the
# whole-array case, so the boundary doesn't silently drift.
#
# NOT yet aliased (deliberate, see docs/foreach-aliasing.md): slices @a[...]/@h{...}
# and `values %h` — they flatten through the shared list-copy machinery.

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

plan tests => 22;

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

sub transpile {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;
    return scalar `$pl2cl $pl_file 2>/dev/null`;
}

# --- codegen: element forms rewrite to their box-returning heads ---
like(transpile('my @a=(1,2,3); for ($a[1]) { $_=9 }'),
    qr/\(p-aref-box /, 'for($a[i]) compiles to p-aref-box');
like(transpile('my %h=(k=>1); for ($h{k}) { $_=9 }'),
    qr/\(p-gethash-box /, 'for($h{k}) compiles to p-gethash-box');

# --- hash element aliases (write-through) ---
test_cl('for($h{k}) write-through',
    q{my %h=(k=>1); for ($h{k}) { $_=99 } print "$h{k}\n";}, "99\n");
test_cl('for($h{k}) in-place s/// idiom',
    q{my %h=(name=>"bob"); for ($h{name}) { s/b/B/g } print "$h{name}\n";}, "BoB\n");

# --- array element aliases (write-through) ---
test_cl('for($a[i]) write-through',
    q{my @a=(1,2,3); for ($a[1]) { $_=99 } print "@a\n";}, "1 99 3\n");
test_cl('for($a[i]) increment',
    q{my @a=(1,2,3); for ($a[2]) { $_++ } print "@a\n";}, "1 2 4\n");

# --- whole-array aliasing must still work (guard against regression) ---
test_cl('for(@a) still aliases each element',
    q{my @a=(1,2,3); for (@a) { $_*=10 } print "@a\n";}, "10 20 30\n");

# --- forms that must NOT alias: a computed value is a temporary ---
test_cl('for($x+1) does not write back (computed temp)',
    q{my $x=5; for ($x+1) { $_++ } print "$x\n";}, "5\n");
test_cl('for(uc $x) does not write back (rvalue builtin)',
    q{my $x="abc"; for (uc $x) { $_="ZZ" } print "$x\n";}, "abc\n");
# perl 5.40.3 prints "7": pp_leavesub hands the caller a mortal COPY of the
# returned scalar, so the foreach alias writes into the copy (task #964).  The
# row this replaces used `sub f { my $v=7; return $v }` — a SUB-LOCAL my, the
# one shape where the copy is unobservable, since nobody else holds that box —
# and asserted only that the program printed "ok", so it could not fail on the
# bug it names.  The observable shape returns an OUTER lexical and reads it.
# The map / grep / \f() / @_-writer spellings of the same rule, and the list,
# @_, goto, anon-sub and method families, are Pl/t/return-copy-01.t.
test_cl('for(f()) does not write back (normal sub returns a copy)',
    q{my $v=7; sub f { $v } for (f()) { $_=99 } print "$v\n";}, "7\n");

# --- multi-element list must NOT trigger the single-element rewrite ---
test_cl('for($a[0], $a[1]) still iterates both (no false rewrite)',
    q{my @a=(1,2,3); my @seen; for ($a[0], $a[1]) { push @seen, $_ } print "@seen\n";},
    "1 2\n");

# --- #267 (s370): the WRAPPER and the per-element HEADS, in one pattern each.
# A list whose every depth-0 element is a single scalar has a statically known
# length, so it takes the `(vector …)` shape at every k — not the run-time
# flattener, which cannot be handed boxes (a box over a vector is
# indistinguishable from an @array box: the #262/#263 hazard one level up).
# With the wrapper switched (step 1) the sole-element verdict then applies PER
# ELEMENT (step 2): one verdict function, one head-swapper, k of them, the
# verdicts taken off the untouched tokens before any lowering and mapped onto
# the lowered elements by position.  Both spellings — two lowering sites.
like(transpile('my %h=(a=>1,b=>2); for ($h{a}, $h{b}) { $_="W" }'),
    qr/\(vector \(p-gethash-box %h "a"\) \(p-gethash-box %h "b"\)\)/,
    'multi-element scalar list: (vector …) + per-element box, block form (#267)');
like(transpile('my %h=(a=>1,b=>2); $_="W" for ($h{a}, $h{b});'),
    qr/\(vector \(p-gethash-box %h "a"\) \(p-gethash-box %h "b"\)\)/,
    'multi-element scalar list: (vector …) + per-element box, modifier form (#267)');
# INVERSE GUARD: a MIXED list keeps the flattener — `@a` really does spread, so
# a static length does not exist and the vector shape would be a miscompile.
like(transpile('my @a=(1,2); my $x=0; for ($x, @a) { }'),
    qr/\(p-flatten-args \(list \$x \@a\)\)/,
    'a list with an @array still flattens at run time (#267 inverse)');

# --- #263 (s365): the STATEMENT-MODIFIER spelling is a second lowering site,
# and it was not doing the rewrite at all — the two spellings wrap the list
# differently (Statement children vs the parens), and only one pass peeled
# both, so the AST verdict never fired for `EXPR for (LV)`.  One shared peeler
# now serves the rewrite, the single-scalar wrap and the annotator's veto.
# Widening it covered the ref/chain shapes NEITHER spelling handled before.
# Grouped into three snippets: each row costs an SBCL run (CLAUDE.md 6).
test_cl('modifier form aliases a named element (#263)',
    q{my %h=(k=>"o"); $_="w" for ($h{k}); my @a=("o"); $_="w" for ($a[0]);
      print "$h{k}$a[0]\n";}, "ww\n");
test_cl('both spellings alias an element THROUGH a ref',
    q{my $hr={k=>"o"}; $_="w" for ($hr->{k});
      my $ar=["o"];    for ($ar->[0]) { $_="w" }
      my %y=(k=>"o"); my $yr=\%y; for ($$yr{k}) { $_="w" }
      print "$hr->{k}$ar->[0]$y{k}\n";}, "www\n");
test_cl('both spellings alias a subscript CHAIN',
    q{my %d=(k=>{j=>"o"}); for ($d{k}{j}) { $_="w" }
      my $q={a=>{b=>"o"}};  $_="w" for ($q->{a}{b});
      my @f=(["o"]);        for ($f[0][0]) { $_="w" }
      print "$d{k}{j}$q->{a}{b}$f[0][0]\n";}, "www\n");

# INVERSE GUARD for that widening: the rewrite must claim no list that is not
# an aliasable ELEMENT.  An @array, `keys`, a slice and a call all flatten to
# VALUES through the shared copy machinery, and boxing one of their calls would
# hand a box where a container is expected — so no box / lvalue-cell head may
# appear at all.  (`for (@a)` aliases through p-foreach itself, checked above,
# not through this rewrite.)  NB: a two-element list of ELEMENTS is deliberately
# NOT in this snippet since #267 step 2 — it now boxes, and is guarded below.
unlike(transpile(q{
my @a=(1,2); my %h=(a=>1,b=>2); sub f { "x" }
for (@a) { $_="w" }  for (keys %h) { $_="w" }  for (@a[0,1]) { $_="w" }
for (f()) { $_="w" } for ($h{a}, @a) { $_="w" }
$_="w" for (@a);     $_="w" for (keys %h);
}), qr/p-(?:gethash|aref)(?:-deref)?-box|-lvalue-cell/,
    'alias rewrite claims no non-element list, either spelling');

# --- #267 step 2 (s370): the write-through those heads buy, at runtime.
test_cl('multi-element list aliases every element, both spellings (#267)',
    q{my %h=(a=>"o",b=>"o"); for ($h{a}, $h{b}) { $_="W" }
      my @a=("o","o","o"); $_="W" for ($a[0], $a[2]);
      my $r={x=>"o",y=>"o"}; for ($r->{x}, $r->{y}) { $_="W" }
      print "$h{a}$h{b}|@a|$r->{x}$r->{y}\n";}, "WW|W o W|WW\n");
# INVERSE GUARD: a scalar holding a REF is still ONE element after boxing — the
# #262/#263 hazard is exactly that a box over a vector looks like an @array box,
# so a boxed element must never be spread by the loop.
test_cl('a ref element stays ONE iteration, not spread (#267 inverse)',
    q{my $r=[1,2,3]; my $s=[4,5]; my $n=0; for ($r, $s) { $n++ } print "$n\n";},
    "2\n");

# --- #274 (s371 §2): the ANCHOR contract both callers now die on.
# _apply_alias_head swaps the head only when the head the AST predicted IS the
# outermost call in the emission (the `(vector ` wrap either site may add is
# allowed).  Anything else returns undef, and BOTH callers — k=1 and k>1 — turn
# that undef into a die, because a failed anchor is always a compiler
# self-inconsistency: either the verdict was right and the write silently lands
# on a copy (the #262/#263 silent-wrong), or the verdict and the lowering
# disagree about the same tokens.  These two rows pin the undef half; the die
# half is one line at each caller.  Pure perl — no SBCL, no transpile.
{
  local @INC = ($project_root, @INC);   # the compiler modules, not the shims
  require Pl::Parser;
  is(Pl::Parser::_apply_alias_head('(p-aref $a 0)', 'p-gethash', 'p-gethash-box'),
     undef, 'a head that is not outermost returns undef (#274 anchor contract)');
  is(Pl::Parser::_apply_alias_head('(vector (p-gethash %h "k"))',
                                   'p-gethash', 'p-gethash-box'),
     '(vector (p-gethash-box %h "k"))',
     'the outermost head swaps through a (vector …) wrap (#274)');
}
