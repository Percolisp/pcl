#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# Transpile tests part 10 — the direction-D global-cell flip (tasks #289/#290).
# An ORDINARY package global is now a symbol macro over its own global cell
# (p-defcell) instead of a `defvar` special, and `local` on one lowers to
# p-local-cell instead of a dynamic `let`.  The rows here are the SEMANTIC
# consequences, each probed against real perl (docs/direction-d-plan.md §3).
# NEW TESTS GO HERE — the biggest file bounds the parallel suite's wall time
# (one SBCL spawn per row), so start a new file rather than grow one.

use v5.30;
use strict;
use warnings;
use Test::More;
use File::Temp qw(tempfile);
use FindBin qw($RealBin);
use lib $RealBin;
use PCLCore;

# Path to pl2cl and runtime
my $project_root = "$RealBin/../..";
my $pl2cl = "$project_root/pl2cl";
my $runtime = "$project_root/cl/pcl-runtime.lisp";
# Optional saved-core fast path (PCL_TEST_CORE=1); source-load otherwise.
my @sbcl_rt = PCLCore::sbcl_prefix($runtime);

# Check dependencies
plan skip_all => "pl2cl not found" unless -x $pl2cl;
plan skip_all => "sbcl not found" unless `which sbcl 2>/dev/null`;

# Run a Perl snippet and return output
sub run_perl {
    my ($code) = @_;
    my $full_code = 'use feature "state"; use Cwd; ' . $code;
    # Shell-escape embedded single quotes ('…' -> '\''), or any tick in the
    # snippet truncates the -e arg.
    (my $sh_code = $full_code) =~ s/'/'\\''/g;
    my $output = `perl -e '$sh_code' 2>&1`;
    return $output;
}

# Transpile and run CL, return output
sub run_cl {
    my ($code) = @_;

    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;

    my $cl_code = PCLCore::transpile(qq{$pl2cl $pl_file});

    my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cl_fh $cl_code;
    close $cl_fh;

    my $output = `sbcl @sbcl_rt --load $cl_file 2>&1`;

    $output =~ s/^;.*\n//gm;
    $output =~ s/^caught .*\n//gm;
    $output =~ s/^compilation unit.*\n//gm;
    $output =~ s/^\s*Undefined.*\n//gm;
    $output =~ s/^-->.*\n//gm;
    $output =~ s/^==>.*\n//gm;
    $output =~ s/^PCL Runtime loaded\n//gm;
    $output =~ s/^\s*\n//gm;

    return $output;
}

# Test helper: compare Perl and CL output
sub test_transpile {
    my ($name, $code) = @_;
    my $perl_out = run_perl($code);
    my $cl_out = run_cl($code);
    is($cl_out, $perl_out, $name) or diag("Perl: $perl_out\nCL: $cl_out");
}

# Transpile-only helper (no SBCL spawn) for the two emission-shape rows.
sub transpile {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;
    return PCLCore::transpile(qq{$pl2cl $pl_file});
}

# …and its sibling for the ONE row that asserts pl2cl REFUSES an input: it
# wants the diagnostic, which is the stream transpile() judges.
sub transpile_stderr {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;
    my (undef, $err) = PCLCore::transpile_raw(qq{$pl2cl $pl_file});
    return $err;
}

# ---------------------------------------------------------------------------
# THE PARTITION, as emitted.  An ordinary user global is declared with
# p-defcell (symbol macro over its own global cell); $a/$b — bound by the sort
# lowering, so a dynamic binding IS the mechanism — stay defvar.  Pinning both
# arms in one row is the point: a partition that answers the same for both
# would still pass a one-arm test.
# ---------------------------------------------------------------------------
{
    my $cl = transpile('our $counter = 0; $counter++; print "$counter\n";');
    like($cl, qr/\(p-defcell \$counter /, 'ordinary global declared as a cell');
    like($cl, qr/\(defvar \$a \(make-p-box nil\)\)/,
         'the sort pair stays a defvar (exception set)');
}

# `local` follows the same partition: ordinary → p-local-cell (save/install/
# restore over the cell), exception → today's dynamic let.  Both in one file so
# the row fails if either arm drifts.
{
    my $cl = transpile('our $g = 1; sub w { local $g = 2; local $_ = "u"; f() } sub f {}');
    like($cl, qr/\(p-local-cell \$g /, 'local on an ordinary global uses the cell');
    like($cl, qr/\(let \(\(\$_ \(p-box-for-local/,
         'local on $_ keeps the dynamic let');
}

# ---------------------------------------------------------------------------
# §3.1 — a called sub must see the PACKAGE variable, not the caller's `my`
# shadow of the same name.  Under the old defvar model the caller's `let` was a
# dynamic binding, so the shadow leaked into the callee (this is what the
# poisoned-`my` rename machinery existed to paper over).
# ---------------------------------------------------------------------------
test_transpile('a my-shadow does not leak into a called sub', '
our $x = "global";
sub show { print "sub sees: $x\n" }
sub caller_with_my { my $x = "lexical"; show(); print "my sees: $x\n"; }
caller_with_my();
print "after: $x\n";
');

# §3.2 — a symbolic ref names the PACKAGE variable even where a `my` of the
# same name is in scope (perl: symbolic refs never see lexicals).
test_transpile('symbolic deref under a my-shadow reads the package variable', '
our $x = "global";
sub deref { my $x = "lexical"; my $n = "x"; no strict "refs"; print "symref: ${$n}\n"; }
deref();
');

# §3.3 — p-local-cell restores through a non-local exit, not just a normal one.
test_transpile('local on a cell restores after die', '
our $g = "outer";
sub boom { local $g = "inner"; die "bang\n" }
eval { boom() };
print "err=$@";
print "after die: $g\n";
');

# §3.4 — a declared-but-never-assigned global still reads as undef (p-defcell
# initializes the cell exactly once, like defvar).
test_transpile('never-assigned globals read as undef/empty', '
our $never; our @none; our %nohash;
print "never: [", (defined $never ? $never : "undef"), "]\n";
print "sizes: ", scalar(@none), " ", scalar(keys %nohash), "\n";
');

# ---------------------------------------------------------------------------
# `foreach $pkgvar (LIST)` IMPLICITLY LOCALIZES the package variable: the body
# (and anything it calls) sees the current element, and the old value is back
# after the loop.  With the loop var in a cell a plain `let` would install a
# lexical shadow the callee cannot see — so the loop macros localize the cell
# instead (%p-cell-loop-var-p).  Both loop flavors are here because they are
# separate expanders (p-foreach and p-foreach-range).
#
# NOT covered here: `for my $i (…)` where `$i` is ALSO a package global still
# lets the callee see the loop value (perl: it sees the global).  Probed at
# HEAD and after this change — identical, a pre-existing divergence; task #294.
# ---------------------------------------------------------------------------
test_transpile('foreach over a package global localizes the cell', '
our $i = "before";
sub peek { print "  callee sees: $i\n" }
for $i (1, 2) { print "body: $i\n"; peek() }
print "after list: $i\n";
for $i (7 .. 8) { print "range: $i\n"; peek() }
print "after range: $i\n";
');

# A loop that exits early must still restore — the restore is an
# unwind-protect, not a fall-through assignment.
test_transpile('foreach over a global restores after last/die', '
our $i = "before";
for $i (1, 2, 3) { last if $i == 2 }
print "after last: $i\n";
eval { for $i (1, 2, 3) { die "stop\n" if $i == 2 } };
print "after die: $i\n";
');

# ---------------------------------------------------------------------------
# One `local` statement can name several variables of BOTH partitions at once;
# the ordinary ones become nested cell opens and the exception ones stay in the
# let, and every one of them must restore.
# ---------------------------------------------------------------------------
test_transpile('mixed local: cells and dynamic bindings restore together', '
our @arr = (1,2,3); our %h = (a=>1); our $s = "S";
sub dump_all { print "in: @arr / ", join(",", map {"$_=$h{$_}"} sort keys %h), " / $s / $_\n" }
sub work { local @arr = (9); local %h = (z=>26); local $s = "T"; local $_ = "underscore"; dump_all() }
$_ = "outer_";
work();
dump_all();
');

# A multi-variable `local` with an initializer: the RHS reads the OLD values
# (it is evaluated in the let, before any cell is overwritten).
test_transpile('local (LIST) = RHS reads the old values', '
our ($p, $q) = ("P", "Q");
sub swap { local ($p, $q) = ($q, $p); print "in: $p $q\n" }
swap();
print "out: $p $q\n";
');

# ---------------------------------------------------------------------------
# #295 (the flip's one regression, fixed s383): an eval site inside a compiled
# eval body carries %p-eval-env% — the enclosing eval's capture alist, held as
# a LEXICAL the eval's subs close over (ir-spec §9.1's pad-chain continuation).
# fred1: sub defined inside an eval, called after it returns — its own eval
# must see the file's `my $zzz` (= 1), in both calls (the caller's block
# shadow is not in its pad chain).  fred2: same through TWO eval levels (the
# env threads).  fact: `my $fact` referenced by its own eval text at every
# recursion depth, plus `local` of an ordinary global inside eval mode
# (p-local-cell's lexical rebind must agree with the threaded env).
# ---------------------------------------------------------------------------
test_transpile('eval-defined subs keep the eval scope; env threads through nesting', '
$::zzz = $::zzz = 0;
my $zzz = 1;
eval q{ sub fred1 { print "f1: ", eval(q($zzz)), "\n" } };
fred1();
{ my $zzz = 2; fred1() }
eval q{ eval q{ sub fred2 { print "f2: ", eval(q($zzz)), "\n" } } };
fred2();
our $foo; $foo = 5;
my $fact = q{ local($foo) = $foo; $foo <= 1 ? 1 : $foo-- * (eval $fact) };
print "fact: ", eval($fact), "\n";
print "foo after: $foo\n";
');

# ---------------------------------------------------------------------------
# #294: `foreach MY $x` binds a LEXICAL even when a package variable of the
# same name exists.  Since the flip the two are spelled the same symbol, so
# the loop macro used to read the global cell out of its macroexpansion
# environment and localize it — a sub called from the body then saw the loop
# value, and a closure made in the body saw the post-loop restore.  The
# compiler now states the declaration (`:my t`).  The third row is the case
# the fix must NOT break: `foreach $x` over a package global, where perl DOES
# localize and a called sub MUST see the current element.
# ---------------------------------------------------------------------------
test_transpile('foreach my $x does not localize a same-named global', '
our $i = "global";
sub see { return $i }
for my $i (1..3) { print see(), "\n" }
print see(), "\n";
');

test_transpile('foreach my $x closure capture with a same-named global', '
our $n = "global";
my %f;
for my $n ("A".."C") { $f{$n} = sub { $n } }
print $f{A}->(), $f{B}->(), $f{C}->(), " $n\n";
');

test_transpile('foreach over a package global still localizes for callees', '
our $g = "before";
sub peek { return $g }
for $g (1..3) { print peek(), "\n" }
print peek(), "\n";
');

# ---------------------------------------------------------------------------
# #291 family 1 (`__shadow__`, s299 / postfixderef.t): a `my` in a nested BARE
# BLOCK whose name is also a package global at file level.  The block lexical
# used to be renamed to NAME__shadow__N so the global could keep its
# declaration; now both keep the name — the `let` shadows the symbol macro,
# and the global's value survives the block untouched.
# ---------------------------------------------------------------------------
test_transpile('block my shadows a same-named package global, all three sigils', '
@a = (1,2,3);
$s = "S-global";
%h = (k => "H-global");
{ my ($s, @a, %h); @a = (4,5); $s = "S-block"; $h{k} = "H-block";
  print "in: @a $s $h{k}\n"; }
print "out: @a $s $h{k}\n";
');

# ---------------------------------------------------------------------------
# #291 family 2 (`__cond__`, defins.t): a `my` in a CONDITION or C-for head
# whose name is also a package global.  Same story as family 1 — the head's
# `my` is a lexical shadow, the global keeps its cell and its value.  The
# `our` spelling is included because `our` is what genuinely creates a global,
# and it was the case the deleted pass treated as most certainly poisoned.
# ---------------------------------------------------------------------------
test_transpile('condition-my and C-for-my shadow same-named package globals', '
our $err = "E-global";
our $i   = "I-global";
my @l = (1,2);
while (my $l = shift @l) { print "w:$l\n" }
if (my $err = "E-inner") { print "if:$err\n" }
for (my $i = 0; $i < 2; $i++) { print "f:$i\n" }
print "out: $err $i\n";
');

# ---------------------------------------------------------------------------
# #291 family 3 (`__emb__`, #265/#272): an expression-embedded `my` inside a
# sub body whose name another sub mentions.  The let-hoist's veto — "that sub
# shares the forward-declared global as its cell" — is true at FILE level and
# false inside a sub body, so the veto is no longer asked there and the decl
# binds a plain lexical.  Rows: (a) named-sub body, (b) ANON-sub body (#272),
# (c) the FILE-level shape the veto exists for, which must still share the one
# cell (Capture-Tiny's Utils.pm, #199).
# ---------------------------------------------------------------------------
test_transpile('embedded my inside a sub body vs the file-level shared cell', '
sub setter { ($x, $y) = ("SX", "SY") }
sub foo3   { ++my $x->{foo}; return $x->{foo} }
setter();
print "named: ", foo3(), foo3(), " x=$x y=$y\n";
my $anon = sub { ++my $x->{foo}; return $x->{foo} };
print "anon: ", $anon->(), $anon->(), " x=$x\n";
my $tmp = "/tmp/pcl-291emb-$$.txt";
open my $fh, ">", $tmp or die "w: $!";
sub w { print $fh "shared\n" }
w();
close $fh;
open my $in, "<", $tmp or die "r: $!";
print "file: ", scalar(<$in>);
close $in;
unlink $tmp;
');

# ---------------------------------------------------------------------------
# #298: `my $c = bless $c, "C3"` — a SELF-REFERENTIAL init whose only depth-0
# low-prec token is a list-operator argument separator.  The syntactic
# pre-check reads that comma as a statement tail and used to refuse the whole
# file; PExpr, which owns the `my $c = h 1, 2` ambiguity, parses it as one
# assignment, so the run is lowered in the OUTER scope and its RHS goes into
# the p-box-init binding.  `our $c1` is what makes this reachable on main: it
# stops the block-shadow rename from covering the name.
# ---------------------------------------------------------------------------
test_transpile('self-referential my-init whose comma is a list-op separator', '
our $c1 = bless(\(my $s = "x"), "C");
{ my $c1 = bless $c1, "C3"; print "in:", ref($c1), "\n" }
print "out:", ref($c1), "\n";
our @l = (3, 1, 2);
{ my @l = sort { $a <=> $b } @l; print "sorted:@l\n" }
print "orig:@l\n";
');

# ---------------------------------------------------------------------------
# #297: EVERY `my` in a C-for HEAD scopes to the loop and needs its own `let`.
# The init counter always had one; a `my` in the CONDITION or the STEP, and a
# list/array init `_single_scalar_decl` declines (`my ($x) = …`, `my @a = …`),
# lowered to a bare write into the package cell — so the name stayed defined
# (and shared with the global) after the loop.  The `__cond__` rename was the
# only thing scoping them, which is why this had to be fixed before #291 can
# delete it.  Rows: condition-my, step-my, list-init, array-init, and a
# multi-counter init with a condition-my on top.
# ---------------------------------------------------------------------------
test_transpile('every my in a C-for head is scoped to the loop', '
my $j = 3;
for (my $i = 0; (my $k = $i) < $j; ++$i) { }
print defined($k) ? "k:DEFINED\n" : "k:undef\n";
for (my $n = 0; $n < 2; my $s = $n, ++$n) { }
print defined($s) ? "s:DEFINED\n" : "s:undef\n";
for (my ($x) = (0); $x < 2; $x++) { print "x=$x\n" }
print defined($x) ? "x:DEFINED\n" : "x:undef\n";
for (my @a = (1,2); @a; shift @a) { print "n=", scalar(@a), "\n" }
print defined($a[0]) ? "a:DEFINED\n" : "a:undef\n";
for (my $p = 0, my $q = 2; (my $r = $p) < $q; ++$p) { }
print defined($r) ? "r:DEFINED\n" : "r:undef\n";
');

# INVERSE: the same-named package GLOBAL must keep its cell AND its value —
# the head `my` is a lexical shadow, so a sub called from the body still reads
# the global (the #294 shape one construct over), and the global is untouched
# after the loop.  This is the row that fails if the new let were a dynamic
# rebind or if it swallowed the global's declaration.
test_transpile('C-for head my shadows a same-named global, callee still sees it', '
our $k = "global";
sub see { $k }
my $j = 2;
for (my $i = 0; (my $k = $i) < $j; ++$i) { print see(), "|", $k, "\n" }
print "after:", $k, "\n";
');

# ---------------------------------------------------------------------------
# #296: a `my`/`state` of an EXCEPTION-partition name binds a symbol that is
# PROCLAIMED SPECIAL, so its `let` was a DYNAMIC rebinding — a closure made
# inside lost the value at scope exit.  The declaration is renamed to a fresh
# non-special symbol.  Every shape in one row: sub-local (the reproducer),
# `%ENV` (the rename is name-decidable, not $a/$b-specific), `for my $a`
# (#294's `:my t` binds it lexically, which is the same trap), file-level
# captured by a named sub (the pass must run BEFORE the capture promotion),
# and a block shadow whose global must survive.
# ---------------------------------------------------------------------------
test_transpile('exception-named my binds a LEXICAL: closures, foreach, capture', '
sub mk { my $a = shift; return sub { $a } }
print "closure:", mk("F")->(), mk("G")->(), "\n";
sub mkb { my $b = shift; return sub { $b } }
print "closureb:", mkb("F")->(), mkb("G")->(), "\n";
sub mke { my %ENV = (K => $_[0]); return sub { $ENV{K} } }
print "closureE:", mke("F")->(), mke("G")->(), "\n";
my @c;
for my $a (1,2,3) { push @c, sub { $a } }
print "foreach:", join(",", map { $_->() } @c), "\n";
our $g = "GLOBAL";
sub see { $g }
{ my $g = "inner"; print "shadow:$g see:", see(), "\n" }
print "after:$g\n";
');

# INVERSE — the partition itself must not move: sort still binds the package
# $a/$b pair, in main AND (per #287) in the package the comparator was
# COMPILED in.  The middle row asserts what the comparator OBSERVED, never the
# resulting ORDER: with a `my $a` in scope perl gives the comparator the
# LEXICAL, and an inconsistent comparator's output order is the sort
# ALGORITHM's answer (mergesort vs stable-sort), not a claim PCL can make.
test_transpile('sort still binds the package $a/$b pair; a lexical $a wins inside', '
my @s = sort { $a <=> $b } (3, 1, 2);
print "plain:@s\n";
{
  my $a = "LEX";
  my %seen;
  my @t = sort { $seen{$a eq "LEX" ? "LEX" : "PKG"} = 1; 0 } (7, 9);
  print "observed:", join(",", sort keys %seen), "\n";
}
my @u = sort { $a <=> $b } (5, 4);
print "again:@u\n";
package Foo;
our @q = (30, 10, 20);
sub go { my @r = sort { $a <=> $b } @q; return "@r" }
package main;
print "pkg:", Foo::go(), "\n";
');

# ---------------------------------------------------------------------------
# #301 (not direction-D; lands here because this is the current NEW-TESTS file).
# A heredoc is RAW exactly when its terminator is SINGLE-quoted, and perl allows
# both `~` and whitespace between `<<` and a quoted terminator.  Four separate
# hand-written regexes used to answer "does this heredoc interpolate?" and every
# one was narrower than perl, so `<< 'E'` / `<<~'E'` / `<<~ 'E'` were run through
# string interpolation — variables vanished and `\n` collapsed, silently.  All
# seven spellings in ONE row: the interpolating three are the inverse, and they
# are what a fix that simply stopped interpolating heredocs would break.
# ---------------------------------------------------------------------------
test_transpile('heredoc raw-vs-interpolating, all seven marker spellings', <<'PL301');
my $x = "XX"; my @y = ("A","B");
my %r;
$r{"<<'E'"}   = <<'E1';
raw $x @y \n
E1
$r{"<< 'E'"}  = << 'E2';
raw $x @y \n
E2
$r{"<<~'E'"}  = <<~'E3';
    raw $x @y \n
    E3
$r{"<<~ 'E'"} = <<~ 'E4';
    raw $x @y \n
    E4
$r{'<<"E"'}   = <<"E5";
dq $x @y \n
E5
$r{'<< "E"'}  = << "E6";
dq $x @y \n
E6
$r{'<<~"E"'}  = <<~"E7";
    dq $x @y \n
    E7
for my $k (sort keys %r) { my $v = $r{$k}; $v =~ s/\n/N/g; printf "%-10s => [%s]\n", $k, $v }
PL301

# INVERSE: a genuine below-assignment TAIL must still be refused — its tail
# runs inside the new binding, which the p-box-init shape cannot express.
{
  my ($fh, $tmp) = tempfile(SUFFIX => '.pl');
  print $fh qq{our \$x = "O";\n{ my \$x = \$x, 1; print "\$x\\n"; }\n};
  close $fh;
  my $err = `$pl2cl \Q$tmp\E 2>&1 >/dev/null`;
  like($err, qr/self-referential my-init with a below-assignment tail/,
       'a real `my $x = $x, 1` tail is still refused');
  unlink $tmp;
}

# ---------------------------------------------------------------------------
# #296-B1: the STRING-EVAL half of the exception-name rename.  A `my $a` the
# rename moved to a fresh symbol must still be what an eval compiled in its
# scope reads — and an eval compiled with NO such lexical in scope must still
# read sort's dynamically-bound $a.  The discriminator is the capture alist,
# which is perl's own rule (fable-answers-s385.md §2a).  All five rows of the
# ruled acceptance table in one snippet; perl is the oracle.
# ---------------------------------------------------------------------------
test_transpile('string eval sees a renamed exception-name lexical; without one, sort still wins', '
our $seen;
{ my $a = "IN"; print "1: ", eval q{"[$a]"}, "\n"; }
{ my $f; { my $a = "CAP"; $f = eval q{sub { "[$a]" }}; } print "2: ", $f->(), "\n"; }
{ my $cmp = eval q{sub { $a <=> $b }}; my @s = sort $cmp (2,3,1);
  print "3: ", join(",", @s), "\n"; }
{ my $a = 5; my $cmp = eval q{sub { $seen = $a; 0 }}; my @s = sort $cmp (2,3,1);
  print "4: a=$seen\n"; }
{ my $a = "orig"; eval q{$a = "W"}; print "5: $a\n"; }
');

# The FILE-level twin: a named sub is hoisted out of the file-level scope, so
# only the promotion-to-cell path reaches it — the rename must stand aside
# there (task #296 reproducer 2), while a file-level closure over the same
# declaration keeps working.
test_transpile('a file lexical named $a reaches an eval inside a named sub', '
my $a = "FILE";
my $f = sub { $a };
sub g { my $b = "SUB"; return eval q{join(",", $a, $b)} }
print "closure:", $f->(), "\n";
print "insub:", g(), "\n";
print "eval:", eval q{$a}, "\n";
');

# ---------------------------------------------------------------------------
# #296-B2: a LATER declaration of the same exception name ends the earlier
# one's claim on the uses that follow it.  Two spellings, both live on the
# branch that introduced the rename and both correct before it:
#   - a SIBLING redeclaration in the same scope (perl-tests/split.t's three
#     `my ($a,$b) = split …` statements in one block);
#   - a CONSTRUCT-scoped one (`while (my $a = …)`, `for my $a (…)`) — the
#     head is a sibling of no Block, so the shadow reducer cannot see it.
# The redeclaration's own INITIALIZER still reads the earlier variable: perl
# does not introduce the new name until the statement finishes.
# ---------------------------------------------------------------------------
test_transpile('a later declaration of an exception name owns the uses after it', '
{ my $s = "1,2;3";
  my ($a,$b) = split(/,/, $s); print "1:[$a][$b]\n";
  my ($a,$b) = split(/;/, $s); print "2:[$a][$b]\n";
  my ($a,$b) = ($b,$a);        print "3:[$a][$b]\n"; }
{ my $a = "X"; my $a = "[$a]"; print "4:$a\n"; }
{ my $a = "p"; my $c1 = sub {$a}; my $a = "q"; my $c2 = sub {$a};
  print "5:", $c1->(), $c2->(), "\n"; }
{ my $a = "OUT"; my $n = 0;
  while (my $a = $n++ ? undef : "[$a]") { print "6:$a\n" }
  print "7:$a\n"; }
{ my $a = "L"; for my $a ($a, "z") { print "8:$a\n" } print "9:$a\n"; }
{ my $a = "O"; for (my $a = 0; $a < 2; $a++) { print "10:$a\n" } print "11:$a\n"; }
{ my $a = "o"; { my $a = "i"; print "12:$a\n"; } print "13:$a\n"; }
');


# ---------------------------------------------------------------------------
# #205: a section-let-bound name that is ALSO needed as a package global.  The
# forward-decl pass used to SKIP any name the section let-binds anywhere (a
# `defvar` would have proclaimed it special and turned that `let` into a
# dynamic rebind), so the global never existed: "$fh is unbound" at load.  The
# whole poisoned-`my` rename family existed to dodge that by renaming the
# LEXICAL; since the flip a `p-defcell` symbol macro and a `let` of the same
# name coexist, so the declaration is simply emitted (#291).
#
# The shape needs a HIDDEN use no Symbol-token scan sees: `<$fh>` readline of
# a file-level `open my $fh` (vetoed to the global by the let-hoist), inside a
# sub that ALSO block-shadows the name.  `looper` is the half that must keep
# working: a compound-header decl covering its own uses, no global needed.
# ---------------------------------------------------------------------------
test_transpile('let-bound name that is also the veto global gets its cell', '
my $tmp = "/tmp/pcl-205-probe-$$.txt";
open my $w, ">", $tmp or die "w: $!";
print $w "hello\n";
close $w;
open my $fh, "<", $tmp or die "r: $!";
sub tricky { my $line = <$fh>; { my $fh = "shadow"; } chomp $line; return "got:$line" }
sub looper { my $s = ""; for my $fh (1..2) { $s .= "i$fh" } return $s }
print tricky(), " ", looper(), "\n";
close $fh;
unlink $tmp;
');

# ---------------------------------------------------------------------------
# #291 family 1 (`__shadow__`, s299 / postfixderef.t): a `my` in a nested BARE
# BLOCK whose name is also a package global at file level.  The block lexical
# used to be renamed to NAME__shadow__N so the global could keep its
# declaration; now both keep the name — the `let` shadows the symbol macro,
# and the global's value survives the block untouched.
# ---------------------------------------------------------------------------
test_transpile('block my shadows a same-named package global, all three sigils', '
@a = (1,2,3);
$s = "S-global";
%h = (k => "H-global");
{ my ($s, @a, %h); @a = (4,5); $s = "S-block"; $h{k} = "H-block";
  print "in: @a $s $h{k}\n"; }
print "out: @a $s $h{k}\n";
');

# ---------------------------------------------------------------------------
# #291 family 2 (`__cond__`, defins.t): a `my` in a CONDITION or C-for head
# whose name is also a package global.  Same story as family 1 — the head's
# `my` is a lexical shadow, the global keeps its cell and its value.  The
# `our` spelling is included because `our` is what genuinely creates a global,
# and it was the case the deleted pass treated as most certainly poisoned.
# ---------------------------------------------------------------------------
test_transpile('condition-my and C-for-my shadow same-named package globals', '
our $err = "E-global";
our $i   = "I-global";
my @l = (1,2);
while (my $l = shift @l) { print "w:$l\n" }
if (my $err = "E-inner") { print "if:$err\n" }
for (my $i = 0; $i < 2; $i++) { print "f:$i\n" }
print "out: $err $i\n";
');

# ---------------------------------------------------------------------------
# #291 family 3 (`__emb__`, #265/#272): an expression-embedded `my` inside a
# sub body whose name another sub mentions.  The let-hoist's veto — "that sub
# shares the forward-declared global as its cell" — is true at FILE level and
# false inside a sub body, so the veto is no longer asked there and the decl
# binds a plain lexical.  Rows: (a) named-sub body, (b) ANON-sub body (#272),
# (c) the FILE-level shape the veto exists for, which must still share the one
# cell (Capture-Tiny's Utils.pm, #199).
# ---------------------------------------------------------------------------
test_transpile('embedded my inside a sub body vs the file-level shared cell', '
sub setter { ($x, $y) = ("SX", "SY") }
sub foo3   { ++my $x->{foo}; return $x->{foo} }
setter();
print "named: ", foo3(), foo3(), " x=$x y=$y\n";
my $anon = sub { ++my $x->{foo}; return $x->{foo} };
print "anon: ", $anon->(), $anon->(), " x=$x\n";
my $tmp = "/tmp/pcl-291emb-$$.txt";
open my $fh, ">", $tmp or die "w: $!";
sub w { print $fh "shared\n" }
w();
close $fh;
open my $in, "<", $tmp or die "r: $!";
print "file: ", scalar(<$in>);
close $in;
unlink $tmp;
');

# ---------------------------------------------------------------------------
# #153 FOLD chunk 2 — the intuit_curly boundary after grep/map/sort, and the
# eval/do result-deref (probed vs perl 5.40, s389).  A hash-constructor-shaped
# (or empty) `{…}` followed by `->` after grep/map/sort is an anon-hash EXPR
# term, not a block; for sort the deref'd value is simply a LIST ELEMENT
# (sort has no expr-comparator form).  An eval/do BLOCK followed by `->`
# derefs the block's RESULT.  Before chunk 2, the $deref_skip text-wrap
# consumed the chain into the lambda body: eval double-applied the deref
# (empty where perl prints 41) and sort swallowed the element ("3 1" where
# perl prints "1 1 3").
# ---------------------------------------------------------------------------
test_transpile('eval BLOCK -> deref applies to the RESULT, once', '
my $v = eval {[41]}->[0];
print "v=$v\n";
my $w = eval {[41,43]}->[1];
print "w=$w\n";
my $h = eval {{k=>7}}->{k};
print "h=$h\n";
my $c = eval {[41]}->[0] + do {[1]}->[0];
print "c=$c\n";
');

test_transpile('sort {ctor}->{k}, LIST — the deref is a list element', '
my @r = sort {a=>1}->{a}, (3,1);
print "block: @r\n";
my @p = sort({a=>1}->{a}, (3,1));
print "paren: @p\n";
');

test_transpile('grep/map ctor-deref incl. the empty-hash spelling', '
my @e = grep {}->{a}, (1,2);
print "empty: ", scalar(@e), "\n";
my @g = grep {a=>$_}->{a}, ("chobb", "", "foo");
print "grep: ", scalar(@g), " $g[0]\n";
my @m = map({x=>$_}->{x}, ("x", "y"));
print "map: @m\n";
');

test_transpile('inverse guards: real blocks and comparators unchanged', '
my @g = grep { $_ } (1, 0, 2);
print "grep: @g\n";
my @s = sort { $b <=> $a } (1, 3, 2);
print "sort: @s\n";
my $e = eval { 41 };
print "eval: $e\n";
my @m = map { $_ + 1 } (1, 2);
print "map: @m\n";
');

# ---------------------------------------------------------------------------
# #211: a leading scalar-deref cast binds WITH the ref target BEFORE a `->`
# subscript (probed vs perl 5.40): `$$rr->{k}` == `(${$rr})->{k}` — deref
# $rr FIRST, then the arrow derefs THAT value.  The arrow-as-sugar splice
# treated `$$rr->{k}` as `$$rr{k}` (== `$rr->{k}`), one level short.  Same
# rule Case 2 (`$$r->()`) already implements for calls.
# ---------------------------------------------------------------------------
test_transpile('#211: $$rr->{k} / ${$rr}->[i] keep the outer deref level', '
my $x = {k=>1};
my $rr = \$x;
print "a=", $$rr->{k}, "\n";
my @arr = (10,20);
my $ar = \@arr;
my $rra = \$ar;
print "b=", ${$rra}->[1], "\n";
print "c=", $$rra->[0], "\n";
');

test_transpile('inverse: $$r{k} / ${$r}[i] stay ONE deref level', '
my %h = (k=>5);
my $r = \%h;
print "a=", $$r{k}, "\n";
my @a = (7,8);
my $ra = \@a;
print "b=", ${$ra}[1], "\n";
print "c=", $ra->[0], "\n";
');

# A block-SHAPED `{…}` followed by `->` after grep/map/sort is a perl
# COMPILE-TIME syntax error (near "}->"); PCL must die at transpile, not
# silently deref the list-op result.  This row ASSERTS on the refusal, so it
# reads stderr itself (PCLCore::transpile_raw) instead of going through
# transpile(), whose job is to fail a row on exactly that stream (#355).
{
    my $err = transpile_stderr('my @r = grep { $_ > 1 }->{a}, (1,2,3); print "@r\n";');
    like($err, qr/syntax error near "\}->"/,
         'block-shaped {…}-> after grep dies perl-shaped at transpile');
}

# ---------------------------------------------------------------------------
# #305: a RUN of leading deref casts, not just one.  PPI lexes `$$` as the PID
# magic variable unless an identifier follows directly, so `$$$rr` arrived as
# Magic($$) Symbol($rr) — matched no case, and the "Missing case" die dropped
# the whole statement (these all printed NOTHING).  A pre-pass splits that
# Magic into two Casts; the term machinery then folds the run per perl's rule:
# the OUTERMOST cast picks the access kind, every inner one is a deref, and a
# real `->` makes all of them derefs on the base.  All rows probed vs perl.
# ---------------------------------------------------------------------------
test_transpile('#305: multi-cast deref before -> subscript/call', '
my %h = (k=>9); my $r = \%h; my $rr = \$r; my $rrr = \$rr; my $rrrr = \$rrr;
print "a=", $$rr->{k}, "\n";
print "b=", $$$rrr->{k}, "\n";
print "c=", $$$$rrrr->{k}, "\n";
my @a = (7,8,9); my $ar = \@a; my $arr = \$ar; my $arrr = \$arr;
print "d=", $$$arrr->[1], "\n";
my $c = sub { "c(@_)" }; my $cr = \$c; my $crr = \$cr;
print "e=", $$$crr->(1), "\n";
my %h2 = (k => {j=>"deep"}); my $r2 = \%h2; my $rr2 = \$r2; my $rrr2 = \$rr2;
print "f=", $$$rrr2->{k}{j}, "\n";
');

test_transpile('#305: multi-cast WITHOUT an arrow — outermost cast picks the kind', '
my %h = (a=>1,b=>2,k=>9); my $hr = \%h; my $hrr = \$hr;
my @a = (7,8,9); my $ar = \@a; my $arr = \$ar;
print "a=", $$$hrr{k}, "\n";
print "b=", $$$arr[1], "\n";
print "c=", join(",", @$$arr[0,1]), "\n";
print "d=", join(",", @$$hrr{qw(a b)}), "\n";
print "e=", join(",", %$$hrr{"a"}), "\n";
');

test_transpile('#305 inverse: bare $$ is still the PID, not a deref run', '
print "a=", ($$ > 0 ? "pid" : "bad"), "\n";
my @l = ($$, 2);      print "b=", scalar(@l), "\n";
my %h = ($$ => "v");  print "c=", (exists $h{$$} ? "key" : "bad"), "\n";
print "d=", ("pid=$$" =~ /^pid=\d+$/ ? "interp" : "bad"), "\n";
');

test_transpile('s398: arrow-less hash subscript after a LIST SLICE (PPI labels the {k} a Block) — was a silent PARSE-ERROR drop', '
sub f { return ({k=>"v", j=>{d=>"deep"}}, {k=>"w"}) }
my $x = ({a=>1})[0]{a};                 print "a=$x\n";
print "b=", ({foo => "bar"})[0]{foo}, "\n";
print "c=", (f())[1]{k}, "\n";
print "d=", (f())[0]{j}{d}, "\n";
print "e=", (f())[0]{j}->{d}, "\n";
my @l = ((f())[0]{k}, "z");             print "f=@l\n";
print "g=", ({a=>1})[0]{a} + 1, "\n";
print "h=", ([[1,2],[3,4]])[0][1]{x} // "u", "\n" if 0; print "h=", ([{x=>5}])[0][0]{x}, "\n";
print "i=", ("a", "b", {q=>7})[2]{q}, "\n";
');

# ── PPI operator-vs-term repairs (#354, #351) ────────────────────────────────
# Both shapes used to be DROPPED whole; each row runs the perl oracle too, so
# the inverse cases (real division, a real glob) are guarded by the same rows.
test_transpile('#354: `)*name` is multiplication, and a real glob still is not', '
my ($s, $k) = (0, "ab");
$s += length($k)*length($k);            print "a=$s\n";
my $x = 3;                              print "b=", $x*length($k), "\n";
my @a = (5);                            print "c=", $a[0]*length($k), "\n";
my %h = (x => 4);                       print "d=", $h{x}*length($k), "\n";
print "e=", 2*length($k), "\n";
print "f=", "3"*length($k), "\n";
sub gf { 7 } *bar = \&gf;               print "g=", bar(), "\n";
');

test_transpile('#351: /re/ after a paren-less call is a MATCH; a term keeps division', '
sub myok { print "CALL(@_)\n" }
$_ = "foofoo";
my $qr = qr/foo/;
myok /$qr/, "interp";
myok /foo/, "literal";
myok /foo/;
print /foo/, "\n";
myok /foo/x, "modifier";
myok /foo/ ? 1 : 0;
print "div1=", (time / 60 > 0 ? "ok" : "bad"), "\n";
use constant PIVAL => 6;
print "div2=", PIVAL / 2, "\n";
sub gz () { 10 }
print "div3=", gz / 2, "\n";
my $n = 12;
print "div4=", $n / 4, "\n";
');

# (The `$/`-dependent __DATA__ shape — ppi-upstream-bugs.md §13 — is guarded in
# Pl/t/data-handle-01.t, which runs perl on a real FILE: the helper here uses
# `perl -e`, where a __DATA__ section does not exist at all.)

test_transpile('s398 inverse: the list-slice shapes that already worked keep their reading', '
sub f { return (0,1,2) }
print "a=", ([qw/foo bar/])[0][1], "\n";
print "b=", ({foo=>"bar"})[0]->{foo}, "\n";
print "c=", qw(a b c)[1], "\n";
print "d=", (f())[2] + 1, "\n";
if ((f())[1]) { print "e=cond\n" }
for my $v ((f())[2]) { print "f=$v\n" }
my @s = (f())[1,2];  print "g=@s\n";
print "h=", (sub {"bar"})[0]->(), "\n";
print "i=", (map { {k=>$_} } 1..2)[1]{k}, "\n";
');

# s411 (Phase R, PCL_OPT=none found it): a defelem @_ alias — `$h{k}` passed
# to a sub — must be COPIED by value into `my ($x) = @_` / `my $x = shift`,
# never aliased: %p-flatten-list snapshotted the magic CELL, so a later write
# to $x went through the alias and vivified the caller's key.  The raw-params
# fast path hid it; a closure capture (or PCL_OPT=none) takes the general path.
test_transpile('s411: a defelem @_ alias is copied by value into a boxed my-param (no vivify on write)', '
no warnings;
my %h = (k => 1);
sub a4 { my ($x) = @_; my $f = sub { $x }; $x = 0; }   a4($h{a4}); print "a4:", (exists $h{a4} ? "BAD" : "ok"), "\n";
sub a5 { my $x = shift;  my $f = sub { $x }; $x = 0; }   a5($h{a5}); print "a5:", (exists $h{a5} ? "BAD" : "ok"), "\n";
sub a6 { my ($x, $y) = @_; my $f = sub { $x }; $y = 0 } a6($h{a6}, 1); print "a6:", (exists $h{a6} ? "BAD" : "ok"), "\n";
sub w  { $_[0] = 99 } w($h{w}); print "w:$h{w}\n";
package Cnt; sub TIESCALAR { bless {c=>0} } sub FETCH { my $s = shift; ++$s->{c} } sub STORE { print "STORE $_[1]\n" }
package main;
tie my $t, "Cnt";
my ($p, $q) = ($t, 7); print "p=$p q=$q\n"; $p = 5; print "p=$p t=$t\n";
');

# s412 (Phase B3): a `{ WORD => …` block after eval/do/sub is a BLOCK whose
# statement is a LIST — perl reads (k, 1); the hash-constructor route belongs
# to map/grep only (`map({k => $_}, LIST)`, where perl itself reads the braces
# as an anon hash).  Until s412 the check applied to every kind: `sub { a => 1 }`
# emitted a garbage lambda (run-time crash), `do { b => 2 }` and
# `eval { k => 1 }` yielded a HASH ref.
test_transpile('s412: `{ WORD => …` after eval/do/sub is a block returning a list; map/grep keep the hash-ctor route', '
my $f = sub { a => 1 };      print "sub:", join(",", $f->()), "|", scalar(() = $f->()), "\n";
my $d = do { b => 2 };       print "do:$d\n";
my @d = do { b => 2 };       print "do-list:@d\n";
my @x = eval { k => 1 };     print "eval-list:@x\n";
my $y = eval { k => 2 };     print "eval-scalar:$y\n";
my @m = map { {c => $_} } 1..2;      print "map:", ref($m[0]), $m[1]{c}, "\n";
my @n = map({ k => $_ }, 1..2);      print "map-paren:", ref($n[0]), $n[1]{k}, "\n";
my %h = map { $_ => 1 } qw(p q);     print "map-pairs:", join(",", sort keys %h), "\n";
');

# s412 (Phase C): an eval/do/sub body holding a `local` is lowered
# STRUCTURALLY (the v1 raw_wrap rides inside the lambda; until s412 the whole
# body took v1's text route for that alone) — the dynamic extent must still
# end with the block, and `eval { require X }` (a tail Include, also lifted)
# must yield require's value / undef + $@.
test_transpile('s412: local inside eval/do/sub bodies restores at block exit; eval { require X } tail value', '
our $x = 5; sub f { $x }
my $r = eval { local $x = 1; f() };           print "eval:$r $x\n";
my $d = do { local $x = 2; f() + 1 };         print "do:$d $x\n";
my $s = sub { local $x = 3; f() };            print "sub:", $s->(), " $x\n";
for my $i (7..8) { local $x = $i; print "loop:", f(), "\n" }  print "after:$x\n";
my @l = eval { local $x = 9; map { $_ + $x } 1..2 }; print "list:@l $x\n";
my $ok = eval { require Nope::Missing::Mod }; print defined $ok ? "req:$ok" : "req:undef", ($@ =~ /locate/ ? " err" : " ?"), "\n";
my $p = eval { require POSIX; 1 };            print "posix:$p\n";
our @arr = (1,2); my $c = eval { local @arr = (3); scalar @arr }; print "arr:$c ", scalar(@arr), "\n";
');

# `"\b"` in double-quoted context is BACKSPACE (0x08) — dq strings, qq{},
# heredocs, backticks and s/// replacements alike; tr/// already had it.
# PCL's dq escape decoder had no `\b` arm, so the "unknown \X → X" rule made
# it a plain `b` (silent wrong; found reviewing the two escape decoders for
# their merge, #387 family 44; task #393).  Every path that reaches the
# decoder is exercised: the non-interpolated string, an interpolated one, a
# heredoc, a plain and an interpolated s/// replacement.  Perl-probed.
test_transpile('#393: "\b" is backspace in every double-quoted context (dq, qq, heredoc, s/// replacement); tr keeps it', '
my $x = "Z";
my $s = "a\bc";                 print "dq:", join(",", map { ord } split //, $s), "\n";
my $i = "a\b$x";                print "interp:", join(",", map { ord } split //, $i), "\n";
my $q = qq{p\bq};               print "qq:", join(",", map { ord } split //, $q), "\n";
my $h = <<"E";
h\bh
E
chomp $h;                       print "heredoc:", join(",", map { ord } split //, $h), "\n";
(my $r = "aXc") =~ s/X/\b/;     print "subst:", join(",", map { ord } split //, $r), "\n";
(my $t = "aXc") =~ s/X/\b$x/;   print "isubst:", join(",", map { ord } split //, $t), "\n";
(my $u = "a\x08c") =~ tr/\b/B/; print "tr:$u\n";
print "still-unknown:", "\q\z", " and-f:", ord("\f"), "\n";
');

# The slice-argument flatten rule, applied by EVERY slice reader / KV reader /
# slice delete (runtime %p-flatten-slice-args): a range or an interpolated
# @list contributes its elements; a STRING is one index/key, never its
# characters.  Three siblings disagreed with it (task #394, found reviewing
# the six copies for their extraction, #387 family 21): p-aslice exploded a
# raw string index into characters (@a["12"] read element 0 twice), and the
# two array-slice deletes did not flatten at all (delete @a[1..2] deleted
# element 0).  Perl-probed.
test_transpile('#394: slice arguments flatten alike everywhere — string index is one index; delete @a[RANGE]/@a[@list]/%a[RANGE] flatten', '
my @a = (10,20,30,40,50,60,70,80,90,100,110,120,130);
print "lit:", join(",", @a["1"]), "\n";
my $s = "1"; print "cat:", join(",", @a[$s . "2"]), "\n";
my @b = @a["12", "0"]; print "two:@b\n";
my $i = "12"; print "var:", join(",", @a[$i]), "\n";
my @r = @a[1..3]; print "range:@r\n";
my %h = (ab => 1, cd => 2); print "hslice:", join(",", @h{"ab", "cd"}), "\n";
my @d = delete @a["12"]; print "delstr:@d ", scalar(@a), "\n";
my @x = (10,20,30,40,50,60);
my @dr = delete @x[1..2];   print "delrange:@dr | ", join(",", map { defined $_ ? $_ : "u" } @x), "\n";
my @ix = (4, 5);
my @dl = delete @x[@ix];    print "dellist:@dl | ", scalar(@x), "\n";
my @y = (1,2,3,4,5,6);
my @kv = delete %y[1..2];   print "delkv:@kv | ", scalar(@y), "\n";
');

# ---------------------------------------------------------------------------
# ONE decomposition of an element/slice access for every builtin arm
# (ExprToCL::_elem_container_key, #387 families 2+10).  The two rows below are
# the DIFFERENCES the copies had, each of which was a bug (task #397 and the
# empty-slice arity split) — probed against perl before unifying.
# ---------------------------------------------------------------------------

# tied/pos swapped the container's sigil UNCONDITIONALLY; exists/delete only
# for a bare Symbol/Magic container, because _swap_elem_sigil's regex is
# unanchored and otherwise rewrites the package-qualified INDEX inside a
# nested access's text — `$a[$i]{$k}` with `our $i` emitted `Pkg::%i`, a
# reference to a DIFFERENT variable (ac6fdc1 fixed exists/delete in s-June and
# never reached the twins).  A shape row because both consumers are inert
# today: pos() on an element always reads undef (#396) and `tie $a[0],…` is
# unimplemented (#155) — the wrong variable cannot show itself in output yet.
{
    my $cl = transpile('package Pkg; our @a = ({k=>"x"}); our $i = 1; our $k = "k";'
                       . ' my $p = pos($a[$i]{$k}); my $t = tied($a[$i]{$k});'
                       . ' my $e = exists($a[$i]{$k});');
    like($cl, qr/\(p-pos\s+\(p-gethash-box\s+\(p-aref\s+\@a\s+Pkg::\$i\)/,
         '#397: pos() keeps a nested package-qualified index as $i (was Pkg::%i)');
    like($cl, qr/\(p-tied\s+\(p-gethash-box\s+\(p-aref\s+\@a\s+Pkg::\$i\)/,
         '#397: tied() keeps it too — the same guard exists/delete always had');
    unlike($cl, qr/Pkg::%i/,
           '#397: no arm mis-sigils the index of a nested access');
}

# The empty slice, [perl #29127].  p-delete-hash-slice returned undef for it,
# but its three siblings disagreed: the two array-slice arms demanded a
# subscript child, so `delete @a[()]` fell through to the SCALAR delete and
# crashed on arity ("invalid number of arguments: 1"), and the KV runtimes had
# no empty-slice rule at all (0, not undef).  Perl-probed, all four spellings,
# both contexts — plus the read-only corner: perl allows `delete @ro[()]` and
# dies only on a real index, so the emptiness check comes FIRST.
test_transpile('#387 family 2: empty slice delete — all four spellings, scalar and list, per [perl #29127]', '
my %h = (a=>1); my @a = (1,2,3); my %g = (b=>2); my @c = (4,5,6);
my ($x1,$y1) = (1, scalar delete @h{()});  print "hslice:", (defined $y1 ? "def:$y1" : "undef"), "\n";
my ($x2,$y2) = (1, scalar delete @a[()]);  print "aslice:", (defined $y2 ? "def:$y2" : "undef"), "\n";
my ($x3,$y3) = (1, scalar delete %g{()});  print "kvh:",    (defined $y3 ? "def:$y3" : "undef"), "\n";
my ($x4,$y4) = (1, scalar delete %c[()]);  print "kva:",    (defined $y4 ? "def:$y4" : "undef"), "\n";
print "left:", scalar(keys %h), scalar(@a), scalar(keys %g), scalar(@c), "\n";
my @r1 = delete @h{()}; my @r2 = delete @a[()]; my @r3 = delete %g{()}; my @r4 = delete %c[()];
print "list:", scalar(@r1), scalar(@r2), scalar(@r3), scalar(@r4), "\n";
my @ro = (1,2,3); Internals::SvREADONLY(@ro,1);
print "ro-empty:", (eval { my @x = delete @ro[()]; 1 } ? "ok" : "died"), "\n";
print "ro-real:",  (eval { my @x = delete @ro[0];  1 } ? "ok" : "died"), "\n";
my @b = (10,20,30,40); my @d = delete @b[1..2];
print "still:@d | ", join(",", map { defined $_ ? $_ : "u" } @b), "\n";
');

done_testing();
