#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# stmt-modifier-01.t — task #464: a trailing STATEMENT MODIFIER on a statement
# class whose lowering slices the statement's tokens by position.
#
#     require $m if 1;                        # perl: loads it   PCL: DROPPED
#     require $m unless $INC{$m};             # perl: loads it   PCL: DROPPED
#     local($\, $,) = (undef, "") if 1;       # perl: localizes  PCL: DROPPED
#
# while `require $m;`, `require Foo if 1;` (the BAREWORD spelling),
# `if (1) { require $m }` and `local $x = V if 1;` (the single-variable branch)
# all worked.  ONE cause: the modifier split lived inside
# _process_expression_statement, so every per-class handler that takes "the
# tokens after `require`" / "the tokens after the `=`" handed the modifier
# straight to the expression parser, which then had a bare `if` Word with no
# operator around it — "Bug. Fell through. Missing case: [", statement gone,
# and SILENTLY gone in module mode (Sub::Uplevel, Test2::API::Context,
# Test2::API::InterceptResult(::Event), Test2::Formatter::TAP).
#
# Two shared halves now, one copy each (Pl/Parser.pm):
#   _split_trailing_modifier  — take the modifier off a token run, with the set
#                               of words THIS caller can lower
#   _wrap_statement_modifier  — wrap an already-lowered form in it
#
# Every expectation below is the live `perl` answer (probed s441b, 5.40.3).
#
# WHY `require` accepts all six modifiers and `local` only if/unless: `require`
# is an ordinary runtime op, so p-if/p-unless/p-while/p-until/p-foreach say
# exactly what perl says.  `local` compiles to a let/p-local-cell that stays
# OPEN over the rest of the block, and perl's `local $x = 5 for (1,2)`
# localizes and RESTORES once per iteration — the value does not survive the
# statement (probed).  An open let cannot express that, so a loop modifier on
# `local` keeps DROPPING, loudly; row 9 is that promise.
#
# The three residues this file used to record as unfixed are FIXED — s446i,
# tasks #508 / #509 / #510, rows 13-19 below:
#   * a BARE `local $x if 0;` (no initializer) used to localize anyway, where
#     perl does not run the statement at all.  The split now happens ONCE at
#     the head of _process_local_declaration, over the whole token run, so the
#     branches with no RHS see it too; and with no assignment all six modifiers
#     lower (a loop one runs its loop with an EMPTY body and localizes nothing,
#     which is exactly perl, because the slot is restored per iteration).
#   * an ELEMENT or SLICE target inside a `local` LIST (`local($p,$h{a}) =
#     (5,6)`) was localized and restored but never ASSIGNED — the scan
#     flattened `$h{a}` to the bare Symbol `$h`.  A target is a structured item
#     now: the CONTAINER for the save/restore macro, the setf PLACE for the
#     assignment.
#   * `$!` inside a `local` LIST assignment was likewise never assigned,
#     because storage and place differ for it alone (*p-stored-errno* vs
#     (p-errno-string)) and the list took the storage name.
#
# STILL divergent, pre-existing, filed: a WRITE inside the scope of a
# false-conditioned `local` does not survive the block (`local $p if 0; $p = 9`
# — perl keeps 9, PCL restores), because "do not localize" is spelled
# "localize to the current value" and a save+restore is only invisible while
# nothing writes.

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

plan tests => 21;

# Fixture modules built by the HARNESS (real perl), with their directory
# interpolated into the generated program: the require rows then depend on
# nothing about which core modules this machine has, or how noisy they are.
my $dir = tempdir(CLEANUP => 1);
for my $n (qw(m1 m2 m3 m4 m5)) {
    open my $fh, '>', "$dir/$n.pm" or die "fixture $n: $!";
    print $fh "\$main::LOADED{'$n'}++;\n1;\n";
    close $fh;
}
my $FIX = qq{my \$D = "$dir";\n};
sub loaded { return qq{sub L { my \$n = shift; (\$main::LOADED{\$n} // 0) }\n} }

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

# ── 1. `local(LIST) = RHS if/unless COND` — the value AND the dynamic scope ──
# `s1()` is called from inside the block, so a row cannot pass by shadowing
# lexically: what it prints is what a CALLED SUB sees.
is(run_cl(<<'PL'), "t1:in:7,8\nt1:out:1,2\nt0:in:1,2\nt0:out:1,2\nu1:in:1,2\nu0:in:7,8\n",
our ($p,$q);
sub s1 { my $t = shift; print "$t:in:$p,$q\n" }
$p=1; $q=2; { local($p,$q)=(7,8) if 1;      s1("t1"); } print "t1:out:$p,$q\n";
$p=1; $q=2; { local($p,$q)=(7,8) if 0;      s1("t0"); } print "t0:out:$p,$q\n";
$p=1; $q=2; { local($p,$q)=(7,8) unless 1;  s1("u1"); }
$p=1; $q=2; { local($p,$q)=(7,8) unless 0;  s1("u0"); }
PL
   'local LIST = RHS if/unless COND: localizes only when the condition holds');

# A false condition means perl never reaches the statement, so the RHS never
# runs; and the CONDITION is evaluated first, and exactly once.
is(run_cl(<<'PL'), "f:in:1,2\nt:RHS\nt:in:7,8\nC\nR\ndone\n",
our ($p,$q);
sub r { my $t = shift; print "$t:RHS\n"; (7,8) }
$p=1; $q=2; { local($p,$q)=r("f") if 0; print "f:in:$p,$q\n"; }
$p=1; $q=2; { local($p,$q)=r("t") if 1; print "t:in:$p,$q\n"; }
my $n = 0;
sub c { print "C\n"; $n++; 1 }
sub r2 { print "R\n"; (7,8) }
{ local($p,$q)=r2() if c(); }
print "done\n" if $n == 1;
PL
   'the RHS runs only when the condition holds; the condition runs once, first');

# The RHS still reads the OLD values (it is evaluated before the localization).
is(run_cl(<<'PL'), "swap:2,1\nout:1,2\n",
our ($p,$q); $p=1; $q=2;
{ local($p,$q)=($q,$p) if 1; print "swap:$p,$q\n"; }
print "out:$p,$q\n";
PL
   'the RHS of a conditional local LIST sees the pre-localization values');

# Containers, `undef` skip slots and a short RHS behave as they do without a
# modifier: the gated assignment writes every element, undef included.
is(run_cl(<<'PL'), "a1:9,10 11\na0:1,4 5\nh0:1,k=4\ns1:1,8\ns0:1,2\nshort:7,U\n",
our ($p,@z,%h,$a,$b);
$p=1; @z=(4,5); { local($p,@z)=(9,10,11) if 1; print "a1:$p,@z\n"; }
$p=1; @z=(4,5); { local($p,@z)=(9,10,11) if 0; print "a0:$p,@z\n"; }
$p=1; %h=(k=>4); { local($p,%h)=(9,'j',10) if 0;
                   print "h0:$p,", join(",", map {"$_=$h{$_}"} sort keys %h), "\n"; }
$a=1; $b=2; { local(undef,$b)=(7,8) if 1; print "s1:$a,$b\n"; }
$a=1; $b=2; { local(undef,$b)=(7,8) if 0; print "s0:$a,$b\n"; }
$a=1; $b=2; { local($a,$b)=(7) if 1;
              print "short:", defined($a)?$a:'U', ",", defined($b)?$b:'U', "\n"; }
PL
   'arrays, hashes, skip slots and a short RHS under a conditional local LIST');

# The wild line, both branches (Test2::Formatter::TAP:111 and :155).
is(run_cl(<<'PL'), "abzE",
$\ = "E"; $, = "C";
{ local($\, $,) = (undef, "") if $\ || $,; print "a","b"; }
print "z";
PL
   'the Test2::Formatter::TAP line: `local($\, $,) = (undef, "") if $\ || $,`');

# ── 2. `require EXPR` with a modifier — all six ─────────────────────────────
# Each fixture bumps $main::LOADED{NAME} when it is loaded, so the observable
# is "did the require happen", counted, not merely "does %INC mention it".
is(run_cl($FIX . loaded() . <<'PL'), "if1:1\nif0:0\nunless:1\nparen:1\nconcat:1\nkey:1\n",
{ my $m = "$D/m1.pm"; require $m if 1;             print "if1:",    L('m1'), "\n"; }
{ my $m = "$D/m2.pm"; require $m if 0;             print "if0:",    L('m2'), "\n"; }
{ my $m = "$D/m2.pm"; require $m unless $INC{$m};  print "unless:", L('m2'), "\n"; }
{ my $m = "$D/m3.pm"; require($m) unless $INC{$m}; print "paren:",  L('m3'), "\n"; }
{ my $m = "$D/m4";    require $m . ".pm" if 1;     print "concat:", L('m4'), "\n"; }
{ my %h = (if => "$D/m5.pm"); require $h{if} if 1; print "key:",    L('m5'), "\n"; }
PL
   'require SCALAR with if/unless (and a hash key spelled `if` inside it)');

# A loop modifier really loops; %INC still makes the second require a no-op,
# so the fixture's counter stays 1 while the loop variable advances.
is(run_cl($FIX . loaded() . <<'PL'), "for:1\nwhile:1,c=2\n",
{ my $m = "$D/m1.pm"; require $m for (1,2); print "for:", L('m1'), "\n"; }
{ my $c = 0; my $m = "$D/m2.pm"; require $m while $c++ < 1;
  print "while:", L('m2'), ",c=$c\n"; }
PL
   'require SCALAR with the loop modifiers for/while');

# A LITERAL path with a modifier is a RUNTIME conditional, not the
# compile-time (p-eval-always …) reading the unmodified spelling gets.
is(run_cl(qq{require "$dir/m1.pm" if 0;  print "lit0:", (\$main::LOADED{m1} // 0), "\\n";
require "$dir/m1.pm" if 1;  print "lit1:", (\$main::LOADED{m1} // 0), "\\n";
require 5.008 if 1;
require 99.9  if 0;
print "ver:ok\\n";
}), "lit0:0\nlit1:1\nver:ok\n",
   'require "literal" / require VERSION under a modifier are runtime-gated');

# ── 3. what must NOT change ─────────────────────────────────────────────────
# A loop modifier on `local` is still refused LOUDLY.  perl restores the
# localization at the end of each implicit iteration, so the value does not
# survive the statement; PCL's open let cannot say that, and a silent
# mis-lowering would be worse than the drop.
{
    my ($cl, $err, $rc) =
      PCLCore::transpile_raw("$pl2cl " . write_pl(q{our $p; local $p = 5 for (1,2);}));
    like($err, qr/^PCL: statement dropped/m,
         'a LOOP modifier on `local` still drops loudly, it is not mis-lowered');
}

# `local *glob = RHS if COND` uses the same splitter now; it worked before and
# must still work, in both directions.
is(run_cl(<<'PL'), "if1:2\nif0:1\nun1:1\nplain:2\nout:1\n",
sub tgt { 1 }
{ local *tgt = sub { 2 } if 1;     print "if1:",   tgt(), "\n"; }
{ local *tgt = sub { 2 } if 0;     print "if0:",   tgt(), "\n"; }
{ local *tgt = sub { 2 } unless 1; print "un1:",   tgt(), "\n"; }
{ local *tgt = sub { 2 };          print "plain:", tgt(), "\n"; }
print "out:", tgt(), "\n";
PL
   'local *glob = RHS if/unless COND still lowers through the shared splitter');

# ── 4. the emission shapes, so a silent re-reading is visible ───────────────
like(emitted(q{my $m = "x.pm"; require $m unless $INC{$m};}),
     qr/\(p-unless .*\(p-require-file/s,
     'require SCALAR + unless emits (p-unless COND (p-require-file …))');

like(emitted(q{our ($p,$q); local($p,$q) = (7,8) if 1;}),
     qr/\(when pcl-local-cond-\d+ \(p-list-=/,
     'a conditional local LIST gates the assignment on one condition temporary');

# ── 5. task #508 — a BARE `local` (no initializer) takes its modifier ────────
# `s1()` is called from inside the block, so no row can pass by shadowing
# lexically: what it prints is what a CALLED SUB sees.  Every expectation is
# the live `perl` answer (probed s446i, 5.40.3); on the pre-fix parser every
# `if 0` / `unless 1` row below read U instead of the value.
is(run_cl(<<'PL'), "t1:U\nt1:out:1\nt0:1\nt0:out:1\nu1:1\nu1:out:1\nu0:U\nu0:out:1\na0:2\na1:0\nh0:1\nh1:0\n",
our ($p,@z,%h);
sub s1 { my $t = shift; print "$t:", defined($p) ? $p : 'U', "\n" }
$p=1; { local $p if 1;      s1("t1"); } print "t1:out:$p\n";
$p=1; { local $p if 0;      s1("t0"); } print "t0:out:$p\n";
$p=1; { local $p unless 1;  s1("u1"); } print "u1:out:$p\n";
$p=1; { local $p unless 0;  s1("u0"); } print "u0:out:$p\n";
@z=(4,5); { local @z if 0; print "a0:", scalar(@z), "\n"; }
@z=(4,5); { local @z if 1; print "a1:", scalar(@z), "\n"; }
%h=(k=>4); { local %h if 0; print "h0:", scalar(keys %h), "\n"; }
%h=(k=>4); { local %h if 1; print "h1:", scalar(keys %h), "\n"; }
PL
   'bare `local $x/@a/%h if|unless COND` localizes only when the condition holds');

is(run_cl(<<'PL'), "l0:1,2\nl1:U,U\nout:1,2\nsub:1\n",
our ($p,$q); $p=1; $q=2;
{ local ($p,$q) if 0; print "l0:$p,$q\n"; }
{ local ($p,$q) if 1;
  print "l1:", defined($p)?$p:'U', ",", defined($q)?$q:'U', "\n"; }
print "out:$p,$q\n";
sub f { local $p if 0; return defined($p) ? $p : 'U' }
print "sub:", f(), "\n";
PL
   'bare `local (LIST) if COND`, and the same inside a sub');

# A LOOP modifier on a bare `local` is lowerable exactly: perl restores the
# localization at the end of each implicit iteration, so nothing survives the
# statement and the only observable effect is the loop's own evaluation.  The
# `seen` row proves the list IS evaluated (it is not simply skipped).
is(run_cl(<<'PL'), "for:1\nwhile:1,c=3\nseen:7 8\nout:1\n",
our $p; $p = 1;
my @seen;
{ local $p for (1,2,3); print "for:", defined($p)?$p:'U', "\n"; }
my $c = 0;
{ local $p while $c++ < 2; print "while:", defined($p)?$p:'U', ",c=$c\n"; }
{ local $p foreach (grep { push @seen, $_; 1 } 7,8);
  print "seen:@seen\n"; }
print "out:$p\n";
PL
   'a LOOP modifier on a bare `local` runs the loop and localizes nothing');

# The two other no-RHS branches: a hash ELEMENT and a GLOB.
is(run_cl(<<'PL'), "e0:1\ne1:U\neout:1\ng0:1\ngout:1\n",
our %h; our $w; $h{a} = 1; $w = 0;
{ local $h{a} if 0; print "e0:", defined($h{a})?$h{a}:'U', "\n"; }
{ local $h{a} if 1; print "e1:", defined($h{a})?$h{a}:'U', "\n"; }
print "eout:$h{a}\n";
sub tgt { 1 }
{ local *tgt if 0; print "g0:", tgt(), "\n"; }
print "gout:", tgt(), "\n";
PL
   'a bare `local $h{k}` / `local *glob` under a modifier is conditional too');

# ── 6. task #509 / #510 — ELEMENT, SLICE and $! targets in a `local` LIST ────
# s2() is called from inside the block: a `local` a called sub cannot see is
# not a `local`.  Pre-fix every element read its OLD value inside the block
# (the assignment went to a phantom scalar $h) — perl-tests/readline.t:285's
# `local($SIG{__WARN__},$^W) = (sub {…}, 1)` never installed its handler.
is(run_cl(<<'PL'), "in:5,6,7,8\nout:0,1,2,1\nsl:9,10,11\nslout:0,1,2\n",
our %h; our @a; our $p;
$h{a}=1; $h{b}=2; @a=(1,2); $p=0;
sub s2 { print "in:$p,$h{a},$h{b},$a[0]\n" }
{ local ($p, $h{a}, $h{b}, $a[0]) = (5,6,7,8); s2(); }
print "out:$p,$h{a},$h{b},$a[0]\n";
{ local ($p, @h{'a','b'}) = (9,10,11); print "sl:$p,$h{a},$h{b}\n"; }
print "slout:$p,$h{a},$h{b}\n";
PL
   'element, array-element and hash-SLICE targets in a local LIST are assigned and restored');

is(run_cl(<<'PL'), "m0:0,1\nm1:5,6\nout:0,1\n",
our %h; our $p; $h{a}=1; $p=0;
{ local ($p, $h{a}) = (5,6) if 0; print "m0:$p,$h{a}\n"; }
{ local ($p, $h{a}) = (5,6) if 1; print "m1:$p,$h{a}\n"; }
print "out:$p,$h{a}\n";
PL
   'an element target in a local LIST under a modifier: assigned only when the condition holds');

is(run_cl(<<'PL'), "in:3,4\nout:2\nswap:6,5\nout2:2\n",
our $p; $p = 0; $! = 2;
sub s3 { print "in:", ($!+0), ",$p\n" }
{ local ($!, $p) = (3,4); s3(); }
print "out:", ($!+0), "\n";
{ local ($p, $!) = (5,6); print "swap:", ($!+0), ",$p\n"; }
print "out2:", ($!+0), "\n";
PL
   '$! in a local LIST is assigned in either slot, and restored (task #510)');

# Rule 12: a target shape with no lowering is a DROP — loud at compile time
# and a perl-shaped die when reached — never the old silent flattening to
# whatever Symbol it happened to contain.  (A `local` statement handler cannot
# just die: nothing above it catches, so the whole FILE would be lost.)
{
    my ($cl, $err, $rc) = PCLCore::transpile_raw("$pl2cl " . write_pl(
        q{our %h; our $p; $h{a}={b=>1}; local ($p, $h{a}{b}) = (5,6);}));
    like($err, qr/^PCL: statement dropped.*unsupported `local` target/m,
         'a chained-subscript target in a local LIST drops loudly (rule 12)');
    like($cl, qr/PARSE ERROR: .*unsupported `local` target/,
         '…and the file still compiles: only that statement is replaced');
}
