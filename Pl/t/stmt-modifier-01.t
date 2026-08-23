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
# NOT asserted here, and deliberately, all three PRE-EXISTING and filed:
#   * a BARE `local $x if 0;` (no initializer) still localizes where perl does
#     not run the statement at all — a different branch, untouched here;
#   * an ELEMENT or SLICE target inside a `local` LIST (`local($p,$h{a}) =
#     (5,6)`) is localized and restored but never ASSIGNED, with or without a
#     modifier;
#   * `$!` inside a `local` LIST assignment is likewise never assigned
#     (`local $! = 3` alone is right).

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

plan tests => 12;

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
