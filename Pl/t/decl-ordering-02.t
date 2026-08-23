#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# decl-ordering-02.t — locks the PERMANENT fix for the sub/use ordering bug.
#
# The invariant (see docs/declaration-ordering-fix-plan.md):
#   Within a package, generated CL reproduces Perl's two timelines —
#   a COMPILE-TIME stream (use/BEGIN/sub) in SOURCE ORDER (each form sees only
#   names defined EARLIER), then a RUNTIME stream.  A `use`/`BEGIN` that
#   introspects the current package's subs must see exactly what Perl would:
#   subs written before it, never subs written after it.  Forward stubs
#   (p-declare-sub) exist only to resolve a forward \&foo and are INVISIBLE to
#   introspection (keys %Pkg::, exists/defined &, ->can).
#
# These tests are differential vs real perl wherever possible.

use v5.30;
use strict;
use warnings;
use Test::More;
use File::Temp qw(tempfile);

my $pl2cl   = './pl2cl';
my $runtime = 'cl/pcl-runtime.lisp';

plan skip_all => "pl2cl not found" unless -x $pl2cl;
plan skip_all => "sbcl not found"  unless `which sbcl 2>/dev/null`;

plan tests => 28;

# Run a Perl snippet through PCL and return filtered stdout.
sub run_pcl {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;
    my $cl_code = `$pl2cl --no-cache $pl_file 2>/dev/null`;
    my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cl_fh $cl_code;
    close $cl_fh;
    my $output = `sbcl --noinform --non-interactive --load $runtime --load $cl_file 2>&1`;
    unlink $cl_file;
    $output =~ s/^;.*\n//gm;
    $output =~ s/^\s*\n//gm;
    $output =~ s/PCL Runtime loaded\n?//g;
    $output =~ s/^caught .*\n//gm;
    $output =~ s/^compilation unit.*\n//gm;
    chomp $output;
    return $output;
}

# Run the same snippet through real perl.
sub run_perl {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;
    my $out = `perl $pl_file 2>&1`;
    chomp $out;
    return $out;
}

# Differential assertion.
sub both_agree {
    my ($name, $code) = @_;
    my $p = run_perl($code);
    my $c = run_pcl($code);
    is($c, $p, $name) or diag "perl=[$p] pcl=[$c]";
}

# ---- C1: a runtime call before the sub's source position works ----
both_agree('C1 forward runtime call', <<'PL');
print foo(), "\n";
sub foo { return 42 }
PL

# ---- C4: a BEGIN/compile-time form sees subs defined EARLIER in source ----
both_agree('C4 BEGIN sees earlier sub', <<'PL');
sub early { return "E" }
BEGIN { print "begin: ", early(), "\n" }
PL

# ---- C5: a BEGIN must NOT see a sub defined LATER in source (via ->can) ----
both_agree('C5 BEGIN does not see later sub (can)', <<'PL');
BEGIN { print "in begin can later: ", (main->can("later") ? "yes" : "no"), "\n" }
sub later { 1 }
print "at runtime can later: ", (main->can("later") ? "yes" : "no"), "\n";
PL

# ---- C5b: introspection canary — the Moo bug distilled to plain Perl. ----
# A package whose use-time code snapshots its own subs must see only subs
# written BEFORE the use, not after.  This is exactly what Moo::Role's
# make_role does; if it regresses, roles break again.
both_agree('C5b use-time stash snapshot sees only earlier subs', <<'PL');
package Thing;
sub before_use { 1 }
our @SNAP;
BEGIN { @SNAP = sort grep { defined &{"Thing::$_"} } qw(before_use after_use) }
sub after_use { 1 }
package main;
print "snapshot: @Thing::SNAP\n";
PL

# ---- C7: a forward \&later taken in a BEGIN resolves once defined ----
both_agree('C7 forward coderef from BEGIN resolves', <<'PL');
our $r;
BEGIN { $r = \&later }
sub later { return 7 }
print $r->(), "\n";
PL

# ---- stubs invisible to keys %Pkg:: until really defined ----
both_agree('stash keys exclude not-yet-defined subs at use-time', <<'PL');
package P;
sub a_method { 1 }
our @K;
BEGIN { @K = sort grep { !/::/ } keys %P:: }
sub z_method { 1 }
package main;
# only subs defined before the BEGIN should be present
print "keys: ", join(",", grep { $_ eq "a_method" || $_ eq "z_method" } @P::K), "\n";
PL

# ---- defined &sub is false for a not-yet-defined sub, true after ----
both_agree('defined &sub respects source order', <<'PL');
my @r;
BEGIN { push @r, (defined &mysub ? 1 : 0) }
sub mysub { 1 }
push @r, (defined &mysub ? 1 : 0);
print "@r\n";
PL

# ---- C2 regression: local/dynamic scope still works (defvar hoist intact) ----
both_agree('C2 local dynamic binding intact', <<'PL');
our $x = "global";
sub show { return $x }
sub wrap { local $x = "wrapped"; return show() }
print wrap(), " ", show(), "\n";
PL

# ---- interleaved subs + runtime: all subs available at runtime ----
both_agree('interleaved subs and runtime', <<'PL');
sub a { 1 }
print "x", a();
sub b { a() + 1 }
print "y", b(), "\n";
PL

# ---- mutual recursion across source-order ----
both_agree('mutual recursion', <<'PL');
sub ev { my $n = shift; $n == 0 ? 1 : od($n-1) }
sub od { my $n = shift; $n == 0 ? 0 : ev($n-1) }
print ev(10), od(7), "\n";
PL

# ---- #456 half (b) / #469: perl's PHASE MODEL, across package sections ----
# A block that switches package (`{ package Q; ... }`) becomes its own emission
# section.  PCL used to emit one section at a time — decls, defs, BEGINs, RUN —
# so a file-level `sub nm {...}` written after such a block was DEFINED only
# after that block had already run, and the call died on the forward stub.
# Perl compiles the WHOLE FILE first: every named sub is defined and every
# BEGIN has run before the first run-time statement, wherever they sit.  s436
# emits every section's compile phase before any section's run phase, so these
# now agree with perl instead of diverging (the two rows this replaces asserted
# the loud death that was the best PCL could do before).
both_agree('#456(b) cross-section forward call — perl compiles the file first', <<'PL');
{ package Q; print main::nm(), "|\n"; } sub nm {"PKG"}
PL

# #469, the same bug through a BEGIN instead of a sub: a BEGIN in a LATER
# section must NOT see an EARLIER section's RUN-TIME assignment.  This was
# SILENT WRONG (PCL printed 5, perl prints the empty string) — the compile
# phase running after run-time code is visible in both directions.
both_agree('#469 a later section\'s BEGIN does not see earlier run-time state', <<'PL');
our $x = 5;
{ package Q; sub q1 { 1 } }
BEGIN { print "B=[$main::x]\n" }
print "end\n";
PL

# The INVERSES that pin the phase model down, all four probed against perl:
# source order INSIDE the compile phase is kept (a BEGIN sees the subs above it
# and none below), and a later section's compile phase does see an earlier
# section's compile phase.
both_agree('#469 inverse: a BEGIN sees an EARLIER section\'s sub', <<'PL');
sub early { "E" }
package Q;
BEGIN { print "B=", main::early(), "\n" }
package main;
print "end\n";
PL

both_agree('#469 inverse: a BEGIN does NOT see a sub defined below it', <<'PL');
package Q;
BEGIN { print "B=", (defined &main::late ? "yes" : "no"), "\n" }
package main;
sub late { "L" }
print "end\n";
PL

# The run phase keeps its own package sequence: __PACKAGE__ and the runtime
# current-package tracking must survive the reordering.
both_agree('#469 inverse: __PACKAGE__ across sections in the run phase', <<'PL');
package Q; sub who { return __PACKAGE__ }
package R; sub who { return __PACKAGE__ }
package main;
print Q::who(), R::who(), __PACKAGE__, "\n";
PL

both_agree('#469 inverse: CHECK/INIT still bracket the phase boundary', <<'PL');
package Q; INIT { print "initQ\n" } CHECK { print "checkQ\n" }
package main; INIT { print "initM\n" }
print "run\n";
PL
# ---- s437 review fix: `package NAME VERSION;` sets $VERSION as the package
# statement is COMPILED -- before any BEGIN, `use` or sub of that section.
# s436 assigned it at the END of the section's compile phase (and before the
# phase model, at the front of the run phase), so a BEGIN in the same section
# read undef where perl reads 1.5.  Both spellings, against live perl.
both_agree('package NAME VERSION is set before the section\'s own BEGIN runs', <<'PL');
package Foo 1.5; BEGIN { print "V=[$Foo::VERSION]\n" } sub ver { $Foo::VERSION }
package main; print "run=", Foo::ver(), "\n";
PL

both_agree('package NAME VERSION BLOCK: same, block form', <<'PL');
package Foo 1.5 { BEGIN { print "V=[$Foo::VERSION]\n" } }
print "run=$Foo::VERSION\n";
PL

# The INVERSE, which must keep working: the same call with NO package switch in
# the block -- one section, so the sub def is hoisted above the runtime forms.
both_agree('#456 inverse: same call without the package switch', <<'PL');
{ print main::nm(), "|\n"; } sub nm {"PKG"}
PL

# ---- the stub falls through to AUTOLOAD before it dies (#456 half (a)) ----
# perl's order for a plain call to a name with no body: run the package's
# AUTOLOAD with $AUTOLOAD set and the original arguments; only a package with no
# AUTOLOAD makes it fatal.  PCL answered the forward stub's undef before s432.
both_agree('forward-declared sub falls through to AUTOLOAD', <<'PL');
sub foo;
our $AUTOLOAD;
sub AUTOLOAD { return "AUTO($AUTOLOAD)" }
print foo(), "\n";
PL

# ... and the sort-comparator spelling of the same thing ([perl #30661], the
# assertion perl-tests/sort.t carries).
both_agree('forward-declared sort comparator reaches AUTOLOAD', <<'PL');
AUTOLOAD { $b <=> $a }
sub stubbedsub;
print join("", sort stubbedsub split//, '04381091'), "\n";
PL

# ---- and the same for a NEVER-declared name (#468) ----------------------
# The two rows above go through p-declare-sub's stub, which only exists for a
# name the file DECLARED.  A plain call to a name nothing ever mentioned
# emitted a direct `(pl-nope 1)` and reached SBCL's raw undefined-function:
# AUTOLOAD was never consulted and `$@` read "The function main::pl-nope is
# undefined."  Since s441c SBCL's `sb-kernel::restart-undefined` is
# encapsulated so the same `%p-call-of-undefined-sub` answers at the CALL —
# one mechanism, no emission change, every runner.

both_agree('#468 never-declared call reaches AUTOLOAD, qualified and not', <<'PL');
our $AUTOLOAD; sub AUTOLOAD { return "A($AUTOLOAD)[@_]" }
print nope(1), "\n";
print main::nope2(2,3), "\n";
print "rv:", scalar(nope3()), "\n";
PL

# The message is perl's, not SBCL's.  Asserted by MATCHING inside the program
# so the row compares a boolean: PCL cannot append perl's " at FILE line N."
# (the emitted call carries no location) and that suffix is not a goal.
both_agree('#468 no AUTOLOAD: eval {} sees perl\'s "Undefined subroutine"', <<'PL');
my $r = eval { nope(1); 1 };
print "ok=", ($r ? 1 : 0),
      " msg=", ($@ =~ /^Undefined subroutine &main::nope called/ ? 1 : 0), "\n";
PL

# A plain call consults the sub's OWN package's AUTOLOAD and walks nothing —
# that is the METHOD rule, not this one.  P2 has no AUTOLOAD, so P2::gone dies
# even though P1 and main are reachable classes in perl's sense.
both_agree('#468 AUTOLOAD is per-package for a plain call: no @ISA walk', <<'PL');
package P1; our $AUTOLOAD; sub AUTOLOAD { return "P1($AUTOLOAD)" }
package P2; sub go { return P2::gone(7) }
package main;
our $AUTOLOAD; sub AUTOLOAD { return "MAIN($AUTOLOAD)" }
print "p1:", P1::missing(1), "\n";
print "p2:", (eval { P2::go() } // "DIED"),
      " msg=", ($@ =~ /^Undefined subroutine &P2::gone called/ ? 1 : 0), "\n";
PL

# `\&NAME` on a name that has no body anywhere: perl's coderef is late-bound to
# the glob, so CALLING it asks the same question.  This branch of
# p-backslash-sub was a SECOND COPY of the AUTOLOAD logic and disagreed with it
# on five points (it looked AUTOLOAD up in the runtime *package*, interned
# instead of find-symbol'ing, never set $AUTOLOAD, dropped the arguments, and
# raised a raw CL error) — it now calls %p-call-of-undefined-sub.
both_agree('#468 \\&NAME with no body reaches its own package\'s AUTOLOAD', <<'PL');
package WA; our $AUTOLOAD; sub AUTOLOAD { "WA($AUTOLOAD)[@_]" }
sub take { return \&WA::gone }
package WB; sub take { return \&WB::gone }
package main;
print "auto:", WA::take()->(5,6), "\n";
print "none:", (eval { WB::take()->(7) } // "DIED"),
      " msg=", ($@ =~ /^Undefined subroutine &WB::gone called/ ? 1 : 0), "\n";
# late binding still wins over the fallback
my $r = \&later; sub later { "L(@_)" }
print "late:", $r->(1), "\n";
PL

# INVERSE GUARD: nothing about a never-declared name's EXISTENCE changed, and a
# missing METHOD keeps the method rule (\"Can't locate object method\"), which
# is a different diagnostic from a plain call's.
both_agree('#468 inverse: exists/defined/can and the method rule are untouched', <<'PL');
package Thing; sub new { bless {}, shift }
package main;
print "de:", (defined &nope ? 1 : 0), (exists &nope ? 1 : 0),
      (main->can('nope') ? 1 : 0), (Thing->can('new') ? 1 : 0), "\n";
my $o = Thing->new;
eval { $o->missing_method(1) };
print "meth:", ($@ =~ /^Can't locate object method "missing_method"/ ? 1 : 0), "\n";
eval { Thing->missing_class(1) };
print "clsm:", ($@ =~ /^Can't locate object method "missing_class"/ ? 1 : 0), "\n";
PL

# ── #503: an UNQUALIFIED symbolic sub name resolves in the CURRENT package ──
# perlmod: "If the string is unqualified, it is looked up in the current
# package."  PCL's resolver read the CL reader's *package* — MAIN for every
# form the loader reads after the file's last (in-package …) — so `package NA;
# sub p { my $s = "nafun"; &$s(3) }` looked up main::nafun.  Every conjunct
# below fails on a base tree, and three of them fail LOUDLY: `sort $string`
# inside a package CRASHED the whole file ((funcall nil …)), `\&$s` came back
# as a ref to nil, and a `package X;` string eval resolved in the caller.
# The die MESSAGE is matched inside the program (as the #468 rows do): PCL
# cannot append perl's " at FILE line N.".
both_agree('#503 an unqualified symbolic sub name resolves in the current package', <<'PL');
no strict 'refs';
package Third; sub tfun { return "Third::tfun(@_)" }
package main;  sub mfun { return "main::mfun(@_)" }
package NA;
sub nafun { return "NA::nafun(@_)" }
sub p1 { my $s = "nafun";      return &$s(1) }
sub p2 { my $s = "nafun";      return $s->(2) }
sub p3 {                       return &{"nafun"}(3) }
sub p4 { my $s = "main::mfun"; return &$s(4) }
sub p5 { my $s = "nafun"; my $r = \&$s; return $r->(5) }
sub p6 { my $s = "mfun";  my $ok = eval { &$s(6) };
         return defined($ok) ? "LIVED"
              : ($@ =~ /^Undefined subroutine &NA::mfun called/ ? "DIED-NA" : "DIED?$@") }
package WA; our $AUTOLOAD; sub AUTOLOAD { return "WA($AUTOLOAD)[@_]" }
sub w1 { my $s = "gone_sym"; return &$s(7) }
package X;
sub xf   { return "X::xf(@_)" }
sub xcmp { return $X::a <=> $X::b }
sub run_sort { my $c = "xcmp"; return join(",", sort $c (3,1,2)) }
package main;
print join("|", NA::p1(), NA::p2(), NA::p3(), NA::p4(), NA::p5(), NA::p6()), "\n";
print "auto:", WA::w1(), "\n";
print "sort:", X::run_sort(), "\n";
my $ev = eval q{ package X; my $s = "xf"; &$s(8) };
print "eval:", (defined $ev ? $ev : "undef:$@"), "\n";
PL

# INVERSE GUARD for #503: a QUALIFIED string still names its own package from
# anywhere, and the defined/exists answers are unchanged — the fix must not
# turn "Pkg::name" into a current-package lookup, nor make a missing name
# exist.  Passes on a base tree too.
both_agree('#503 inverse: qualified strings, defined/exists and \\&{"Pkg::name"}', <<'PL');
no strict 'refs';
package X; sub xf { return "X::xf(@_)" }
package Y;
sub from_y { my $r = \&{"X::xf"};
             return join(",", $r->(1), &{"X::xf"}(2),
                         (defined &{"X::nope"} ? 1 : 0),
                         (exists  &{"X::xf"}   ? 1 : 0),
                         (defined &{"xf"}      ? 1 : 0)) }
package main;
print "y:", Y::from_y(), "\n";
print "m:", &{"X::xf"}(3), "\n";
PL

done_testing();
