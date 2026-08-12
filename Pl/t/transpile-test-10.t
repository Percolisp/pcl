#!/usr/bin/env perl
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

    my $cl_code = `$pl2cl $pl_file 2>&1`;

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
    return `$pl2cl $pl_file 2>&1`;
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


done_testing();
