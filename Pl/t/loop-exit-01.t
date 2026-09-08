#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# loop-exit-01.t — task #1022: THE DYNAMIC LOOP EXIT.
#
# perl's unlabelled `last`/`next`/`redo` acts on the innermost DYNAMICALLY
# enclosing loop, so a bare `last` in a sub called from a loop exits the
# CALLER's loop.  PCL lowered it to a LEXICAL CL exit, which is not that, and
# it failed in two ways — one of them silent:
#
#   sub do_last { last }  for my $i (1..3) { $n++; do_last(); $n += 100 }
#     perl n=1    PCL n=303   — the `last` did NOTHING and the loop ran on
#   sub do_next { next }  while ($g++ < 3) { $n++; do_next(); $n += 100 }
#     perl n=3    PCL: SBCL "attempt to GO to nonexistent tag: :next"
#
# Half (a) (s470bi) made it a trappable die; half (b) (s470bl) PERFORMS the
# exit: a loop whose body can reach user code establishes ONE catch of
# `p-loop-dyn` per loop ENTRY (Kind-A gate `dyn-loop-exit`, `:dyn t`,
# %p-loop-driver / p-dyn-once) and the exit site throws to it.  Every
# expectation below is perl 5.40.3's, probed.
#
# The INVERSE rows are the point of this file as much as the positive ones:
# every ordinary spelling of a loop exit must be untouched, a loop with no
# call in its body must gain NO frame (that byte-identity is what keeps a
# counting loop free), and the two shapes PCL still refuses must stay LOUD.

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

plan tests => 48;

# Transpile (a DROPPED statement fails the row, via PCLCore) and run; stderr
# is kept, because some of these rows are about what lands on it.  $OPT is an
# optional PCL_OPT value for the transpile (the Kind-A gate rows).
sub transpile {
    my ($code, $opt) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;
    my $pre = defined $opt ? "PCL_OPT=$opt " : '';
    return PCLCore::transpile("$pre$pl2cl $pl_file");
}

sub run_cl {
    my ($code, $opt) = @_;
    my $cl_code = transpile($code, $opt);
    my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cl_fh $cl_code;
    close $cl_fh;
    my $output = `sbcl @sbcl_rt --load $cl_file 2>&1`;
    $output =~ s/^;.*\n//gm;
    $output =~ s/^PCL Runtime loaded\n//gm;
    $output =~ s/^\s*\n//gm;
    return $output;
}

# ---- 1. the two original repros, now ANSWERED ----------------------------
is(run_cl(<<'PERL'), "n=1\n",
sub do_last { last }
my $n = 0;
for my $i (1..3) { $n++; do_last(); $n += 100; }
print "n=$n\n";
PERL
   'repro A: a bare `last` in a called sub exits the caller\'s foreach (was n=303)');

is(run_cl(<<'PERL'), "n=3 g=4\n",
sub do_next { next }
my $n = 0; my $g = 0;
while ($g++ < 3) { $n++; do_next(); $n += 100; }
print "n=$n g=$g\n";
PERL
   'repro B: a bare `next` re-tests the while condition (was "GO to nonexistent tag")');

# ---- 2. every loop shape, every keyword ----------------------------------
is(run_cl(<<'PERL'), "n=1\n",
sub f { last }
my $n = 0;
for (my $i = 0; $i < 3; $i++) { $n++; f(); $n += 100 }
print "n=$n\n";
PERL
   'C-style for: `last` from a called sub leaves it');

is(run_cl(<<'PERL'), "n=3\n",
sub f { next }
my $n = 0;
for (my $i = 0; $i < 3; $i++) { $n++; f(); $n += 100 }
print "n=$n\n";
PERL
   'C-style for: a caught `next` runs the STEP, so the loop still terminates');

is(run_cl(<<'PERL'), "n=305 c=5\n",
my $c = 0;
sub f { redo if $c++ < 2 }
my $n = 0;
for my $i (1..3) { $n++; f(); $n += 100 }
print "n=$n c=$c\n";
PERL
   'foreach: a caught `redo` re-runs the body without advancing (perl n=305)');

is(run_cl(<<'PERL'), "1 1 1 2 3\n",
my $c = 0;
sub f { redo if $c++ < 2 }
my @seen;
for my $i (1..3) { push @seen, $i; f() }
print "@seen\n";
PERL
   '... on the SAME element: the index is backed up, not left advanced');

is(run_cl(<<'PERL'), "n=1\n",
sub f { last }
my $n = 0;
until ($n > 9) { $n++; f(); $n += 100 }
print "n=$n\n";
PERL
   'until is a while: same frame');

# ---- 3. the BARE BLOCK — loopctl.t\'s own row and rt119311.t\'s shape ------
is(run_cl(<<'PERL'), "ok=1\n",
sub test_last { last }
my $ok = 0;
TEST41: { $ok = 1; test_last(); $ok = 0 }
print "ok=$ok\n";
PERL
   'perl-tests/loopctl.t "dynamically scoped": a labelled BARE BLOCK is a loop-once');

is(run_cl(<<'PERL'), "n=1\n",
sub foo { my ($b) = @_; $b->() }
my $n = 0;
{ $n++; foo(sub { last }); $n += 100 }
print "n=$n\n";
PERL
   't/op/rt119311.t\'s shape: `{ foo(sub { … last }) }` leaves the bare block');

is(run_cl(<<'PERL'), "n=103 c=3\n",
sub foo { my ($b) = @_; $b->() }
my $c = 0; my $n = 0;
{ $n++; foo(sub { redo if $c++ < 2 }); $n += 100 }
print "n=$n c=$c\n";
PERL
   '... and `redo` RESTARTS the loop-once (a bare block carries no state)');

# ---- 4. which loop it is ---------------------------------------------------
is(run_cl(<<'PERL'), "n=2\n",
sub g { last }
sub f { my $m = 0; for my $j (1..3) { $m++; g(); $m += 100 } return $m }
my $n = 0;
for my $i (1..2) { $n += f() }
print "n=$n\n";
PERL
   'the INNERMOST dynamically enclosing loop wins (f\'s, not the caller\'s)');

is(run_cl(<<'PERL'), "n=0\n",
sub g { last }
sub f { my $t = 0; for my $j (1..2) { $t += $j } g(); return $t }
my $n = 0;
for my $i (1..3) { $n += f(); $n += 100 }
print "n=$n\n";
PERL
   '... and a loop that has ALREADY FINISHED does not catch it');

is(run_cl(<<'PERL'), "n=1\n",
sub f { last }
my $n = 0;
for my $i (1..3) { $n++; my @y = map { f(); $_ } (1,2); $n += 100 }
print "n=$n\n";
PERL
   'a map BLOCK is transparent: the exit reaches the enclosing loop');

is(run_cl(<<'PERL'), "n=1 err=\n",
my $n = 0;
for my $i (1..3) { $n++; eval q{last}; $n += 100 }
print "n=$n err=$@\n";
PERL
   'a `last` in a STRING EVAL reaches the loop (task #1244 (b), s473e): the eval\'s text is another compilation unit, so the loop is framed on the SPELLING — this row used to assert the #1162 residue, `Can\'t "last" outside a loop block` in $@ with n=303');

# ---- 5. what is NOT a loop — LOUD, with perl\'s own text -------------------
like(run_cl(<<'PERL'), qr/Can't "last" outside a loop block/,
last;
print "after\n";
PERL
     'no loop ANYWHERE: perl\'s own text');

like(run_cl(<<'PERL'), qr/^caught: Can't "last" outside a loop block/m,
sub f { last }
eval { f() };
print "caught: $@";
PERL
     '... trappable, at the exit\'s own site');

like(run_cl(<<'PERL'), qr/^caught: Can't "last" outside a loop block/m,
sub f { last }
my $g = 0;
eval { do { f() } while ($g++ < 2) };
print "caught: $@";
PERL
     '`do BLOCK while` is NOT a loop (perl agrees, and says so)');

# ---- 6. INVERSES: nothing ordinary moved ----------------------------------
is(run_cl(<<'PERL'), "sum=3\nc=1\nlabelled=1\nn=3\n",
my $sum = 0;
for my $i (1..5) { last if $i > 2; $sum += $i }     # foreach
print "sum=$sum\n";
my $c = 0;
for (my $j = 0; $j < 9; $j++) { last if $j > 1; $c = $j }   # C-style for
print "c=$c\n";
my $ok = 0;
sub exit_outer { last OUTER }
OUTER: { $ok = 1; exit_outer(); $ok = 0 }
print "labelled=$ok\n";
my $n = 0;
for my $i (1..3) { $n++; next; $n += 100 }
print "n=$n\n";
PERL
   'inverse: lexical exits, a LABELLED exit from a sub, a plain `next`');

is(run_cl(<<'PERL'), "1 after1 2 after2 3 after3\n",
my @s;
for my $i (1..3) { L: { push @s, $i; last; push @s, "X" } push @s, "after$i" }
print "@s\n";
PERL
   'task #1160: a bare `last` in a LABELLED bare block leaves THAT block (it used to exit the enclosing for loop, silently — and died at load with no loop around it)');

# ---- 7. the Kind-A gate, and the byte-identity it protects -----------------
like(run_cl(<<'PERL', '-dyn-loop-exit'), qr/PCL: unsupported: "last" exiting subroutine do_last/,
sub do_last { last }
for my $i (1..3) { do_last() }
PERL
     'PCL_OPT=-dyn-loop-exit: no frame is emitted, so the site keeps half (a)\'s die');

{
    # ONE source file, transpiled twice: the emitted preamble embeds the input
    # path, so two tempfiles could never compare byte-identical.
    my ($fh, $pl) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh 'my $s = 0; for my $i (1..3) { $s += $i } print "$s\n";';
    close $fh;
    my $a = PCLCore::transpile("$pl2cl $pl");
    my $b = PCLCore::transpile("PCL_OPT=-dyn-loop-exit $pl2cl $pl");
    ok($a eq $b, 'a call-free loop is emitted BYTE-IDENTICALLY with the gate on or off');
    like(transpile('sub f { last } my $s = 0; for my $i (1..3) { $s += f() } print "$s\n";'),
         qr/:dyn\s+t/, '... and a loop that can REACH a dynamic exit carries :dyn t');
}

# ---- 8. THE #1162 LICENCE: only a loop that can REACH a dynamic exit -------
#
# The frame's `catch` costs ~4.8 MB of SBCL compile IR, so a loop is framed
# only when its body can reach a dynamic exit: a direct call to a sub in
# MAY-DYN-EXIT (the fixpoint over the unit's call graph), a nested `sub {…}`
# carrying a site, a STRING EVAL (#1244 (b), section 10) or an INDIRECT CALL
# (#1244 (c), section 11) — the last two because the callee is not a name this
# walk can follow, so "it may" is the only sound answer.
#
# What is left of the ruled residue is ONE shape: a DIRECT named call into
# another compilation unit.  It meets no frame and takes perl's own die at the
# site, LOUDLY — never a silently un-taken exit.  (The name test is textual, so
# a word that merely COLLIDES with a MAY-DYN-EXIT sub name costs a frame —
# never a missing one.)

is(run_cl(<<'PERL'), "n=1\n",
sub g { last }
sub f { g() }
my $n = 0;
for my $i (1..3) { $n++; f(); $n += 100 }
print "n=$n\n";
PERL
   'a TWO-LEVEL named chain is followed: the loop is framed and the exit works');

{
    # a sub from ANOTHER compilation unit: the unit being compiled cannot see
    # that the module's `mod_last` performs an exit, so its loop takes no frame
    my $dir = File::Temp::tempdir(CLEANUP => 1);
    open my $mfh, '>', "$dir/DynExitMod.pm" or die $!;
    print $mfh "package DynExitMod;\nsub mod_last { last }\n1;\n";
    close $mfh;
    like(run_cl(<<"PERL"), qr/Can't "last" outside a loop block/,
use lib '$dir';
use DynExitMod;
my \$n = 0;
for my \$i (1..3) { \$n++; DynExitMod::mod_last(); \$n += 100 }
print "n=\$n\\n";
PERL
         'a sub from a `use`d MODULE is a different compilation unit: no frame, and the exit dies LOUDLY');
}

{
    # the whole point of the licence, in emission terms
    my $callee_cannot_exit = 'sub f { last } sub g { 1 } my $s = 0;'
                           . ' for my $i (1..3) { $s += g() } print "$s\n";';
    unlike(transpile($callee_cannot_exit), qr/:dyn/,
           'a loop that calls a sub which CANNOT exit takes no frame — that is what keeps the frame off 88 of t/op/loopctl.t\'s 89 loops (task #1162)');
}

# ---- 9. THE `continue` BLOCK IS INSIDE THE FRAME (task #1161) --------------
#
# A `continue` block used to make a foreach or a bare block DECLINE the frame,
# so a dynamic exit into such a loop died with perl's own text instead of
# performing the exit.  It could not simply be left where it was: it reads the
# loop variable, so it sits inside the per-iteration binding, and a CL `go`
# may not jump into a binding form.  On a FRAMED loop it is now created there
# as a THUNK and called from the driver's post-forms, which is where every
# re-entry lands (%p-foreach-continue-thunk); a bare block hands its continue
# to p-dyn-once, which runs it on a caught `next` and skips it on a `last`.
# Every expectation below is perl 5.40.3's, probed.

is(run_cl(<<'PERL'), "n=3003\n",
sub f { next }
my $n = 0;
for my $i (1..3) { $n++; f(); $n += 100 } continue { $n += 1000 }
print "n=$n\n";
PERL
   'foreach + continue: a caught `next` RUNS the continue block (was `Can\'t "next" outside a loop block`)');

is(run_cl(<<'PERL'), "n=1\n",
sub f { last }
my $n = 0;
{ $n++; f(); $n += 100 } continue { $n += 1000 }
print "n=$n\n";
PERL
   'bare block + continue: a caught `last` SKIPS the continue block (was a die)');

is(run_cl(<<'PERL'), "n=1001\n",
sub f { next }
my $n = 0;
{ $n++; f(); $n += 100 } continue { $n += 1000 }
print "n=$n\n";
PERL
   '... and a caught `next` ends the loop-once but only AFTER its continue block');

is(run_cl(<<'PERL'), "n=3003 g=4\n",
sub f { next }
my $n = 0; my $g = 0;
while ($g++ < 3) { $n++; f(); $n += 100 } continue { $n += 1000 }
print "n=$n g=$g\n";
PERL
   'inverse: `while` + continue was ALREADY right (its continue IS a post-body form) and is unchanged');

is(run_cl(<<'PERL'), "seen=1 1 2 c=1 2\n",
sub h { redo }
my $r = 0; my @seen; my @c;
for my $i (1..2) { push @seen, $i; if ($r++ < 1) { h() } } continue { push @c, $i }
print "seen=@seen c=@c\n";
PERL
   'a caught `redo` re-runs the body and does NOT run the continue block');

is(run_cl(<<'PERL'), "n=1 c=1\n",
sub f { last }
my $n = 0; my @c;
for my $i (1..3) { $n++ } continue { push @c, $i; f(); push @c, "x" }
print "n=$n c=@c\n";
PERL
   'a dynamic exit performed BY the continue block is caught too — the block runs inside the frame');

is(run_cl(<<'PERL'), "n=4 c=j1 j2 i1 j1 j2 i2\n",
sub f { next }
my @c; my $n = 0;
OUTER: for my $i (1..2) { for my $j (1..2) { $n++; f(); $n += 100 } continue { push @c, "j$j" } }
continue { push @c, "i$i" }
print "n=$n c=@c\n";
PERL
   'nested continue\'d loops: the INNERMOST frame catches, and both continue blocks still run in order');

is(run_cl(<<'PERL'), "c=99 99 99\n",
sub f { last if $_[0] }
my @c;
for my $i (1..3) { $i = 99; f(0) } continue { push @c, $i }
print "c=@c\n";
PERL
   'the continue block sees the body\'s WRITE to the loop variable — the thunk closes over the binding, so an unboxable `setf $i 99` reaches it (a saved value would read 1 2 3)');

is(run_cl(<<'PERL'), "n=3 c=10 20 30 a=10 20 30\n",
sub f { next }
my @c; my @a = (1,2,3); my $n = 0;
for my $x (@a) { $n++; $x = $x * 10; f(); $n += 100 } continue { push @c, $x }
print "n=$n c=@c a=@a\n";
PERL
   'the foreach ALIAS still writes through to the array, and the continue block sees the written element after a caught `next`');

like(transpile('sub f { last } my $n = 0; for my $i (1..3) { f() } continue { $n++ } print "$n\n";'),
     qr/:dyn\s+t/,
     'emission: a foreach WITH a continue block now carries `:dyn t` — the licence used to be stripped by the continue block (`@dyn = () if @cont`)');

# ---- 10. A STRING EVAL IS TRANSPARENT TO LOOP CONTROL (task #1244 (b)) -----
#
# perl compiles an eval'd string as its own unit, and a `last` in it still acts
# on the innermost DYNAMICALLY enclosing loop — the loop around the eval.  This
# compiler cannot read that text, so the only sound licence is "a string eval
# MAY exit": a loop whose body string-evals, or calls a sub in this unit that
# does, is framed.  Nothing had to change in the eval CATCHER — it is a
# `handler-case`, and a `throw` is not a condition, so the loop-control throw
# was already passing through it; the loop simply had no frame to reach.
#
# `eval BLOCK` needs no licence of its own: its `last` is either lexical (and
# always worked) or a call this unit follows by name.

is(run_cl(<<'PERL'), "n=3 err=[]\n",
my $n = 0;
for my $i (1..3) { $n++; eval q{next}; $n += 100 }
print "n=$n err=[$@]\n";
PERL
   '... and `eval q{next}` re-tests the loop, so all three iterations run');

is(run_cl(<<'PERL'), "p=1 err=[]\n",
sub f { last }
my $p = 0;
for my $i (1..3) { $p++; eval q{ f() }; $p += 100 }
print "p=$p err=[$@]\n";
PERL
   'a sub CALLED from the eval\'d text exits the loop too');

is(run_cl(<<'PERL'), "m=1 err=[]\n",
my $m = 0;
for my $i (1..3) { $m++; eval { last }; $m += 100 }
print "m=$m err=[$@]\n";
PERL
   'inverse: `eval BLOCK` + `last` was already right (a lexical exit through the handler-case)');

is(run_cl(<<'PERL'), "r=303 err=[boom\n]\n",
my $r = 0;
for my $i (1..3) { $r++; eval q{ die "boom\n" }; $r += 100 }
print "r=$r err=[$@]\n";
PERL
   'inverse: an ordinary die inside a string eval is still CAUGHT — only loop control passes through');

like(run_cl(<<'PERL'), qr/Can't "last" outside a loop block/,
eval q{last};
print "err=[$@]\n";
PERL
     'inverse: `eval q{last}` with NO loop anywhere dies inside the eval, and $@ carries perl\'s own text');

like(transpile('my $n = 0; for my $i (1..3) { eval q{last}; $n++ } print "$n\n";'),
     qr/:dyn\s+t/,
     'emission: a loop whose body string-evals carries `:dyn t`');

unlike(transpile('my $n = 0; for my $i (1..3) { eval { $n++ } } print "$n\n";'),
       qr/:dyn/,
       'emission: a loop whose body only `eval BLOCK`s carries NO frame — the widening is keyed on the STRING spelling, not on the word');

# ---- 11. AN INDIRECT CALL CARRIES THE FRAME (task #1244 (c)) ---------------
#
# `$c->()`, `&$c`, `&{$c}` and `$o->$m` name no sub, so the walk cannot follow
# the callee — and perl exits the loop from every one of them (probed).  Same
# answer as the string eval: "it may", so the loop is framed on the CALL SHAPE.
# The call PROTOCOL never needed anything: `*p-dyn-loop-frames*` is a dynamic
# variable, so it is carried by every call already; only the licence was
# missing.  `$o->NAME` was already right by accident — the bare method name is
# read by the textual name test.
#
# Rows 24 and 25 of section 8 used to assert that the coderef and the computed
# method name DIE; they are gone, replaced by these (the s416 stale-guard rule).

is(run_cl(<<'PERL'), "n=1\n",
sub f { last }
my %d = (k => \&f);
my $n = 0;
for my $i (1..3) { $n++; $d{k}->(); $n += 100 }
print "n=$n\n";
PERL
   'a CODEREF out of a data structure exits the loop (this row used to assert the #1162 residue die)');

is(run_cl(<<'PERL'), "n=1\n",
package C;
sub hop { last }
package main;
my $o = bless {}, 'C';
my $m = "hop";
my $n = 0;
for my $i (1..3) { $n++; $o->$m(); $n += 100 }
print "n=$n\n";
PERL
   'a COMPUTED method name exits the loop too (also used to assert the die)');

is(run_cl(<<'PERL'), "amp=1 blk=1 anon=1\n",
sub f { last }
my $c = \&f;
my $a = 0; for my $i (1..3) { $a++; &$c(); $a += 100 }
my $b = 0; for my $i (1..3) { $b++; &{$c}(); $b += 100 }
my $s = sub { last };
my $d = 0; for my $i (1..3) { $d++; $s->(); $d += 100 }
print "amp=$a blk=$b anon=$d\n";
PERL
   '`&$c()`, `&{$c}()` and an ANON sub through a coderef all reach the loop');

like(transpile('my $c; my $n = 0; for my $i (1..3) { $c->(); $n++ } print "$n\n";'),
     qr/:dyn\s+t/,
     'emission: a loop whose body calls through a coderef carries `:dyn t`');

unlike(transpile('my $r; my $n = 0; for my $i (1..3) { $n += $r->[0] + $r->{k} } print "$n\n";'),
       qr/:dyn/,
       'emission: `->[…]` and `->{…}` are SUBSCRIPTS, not calls — no frame');

{
    # THE BONUS the shape licence buys: a module sub reached INDIRECTLY has a
    # frame to throw to (the frame is in THIS unit; the callee's own unit emits
    # the throw), where the DIRECT named call still has none — the one residue
    # left, guarded by the `use`d-module row in section 8.
    my $dir = File::Temp::tempdir(CLEANUP => 1);
    open my $mfh, '>', "$dir/DynExitMod2.pm" or die $!;
    print $mfh "package DynExitMod2;\nsub mod_last { last }\n1;\n";
    close $mfh;
    is(run_cl(<<"PERL"), "n=1\n",
use lib '$dir';
use DynExitMod2;
my \$cr = \\&DynExitMod2::mod_last;
my \$n = 0;
for my \$i (1..3) { \$n++; \$cr->(); \$n += 100 }
print "n=\$n\\n";
PERL
       'a MODULE sub called through a CODEREF exits the loop, where the direct named call does not');
}
