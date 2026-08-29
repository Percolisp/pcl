#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# symref-package-01.t — task #525: an UNQUALIFIED symbolic VARIABLE name
# resolves in the perl-level current package.
#
# perlmod's rule is one rule for every symbolic name: "if the string is
# unqualified, it is looked up in the current package".  PCL had two answers.
# The symbolic SUB resolver was fixed to read `*pcl-current-package*` in task
# #503, and the typeglob paths (`*{"n"}`, p-glob-assign-dynamic) always did —
# but `%p-symref-symbol`, behind `${"n"}` / `@{"n"}` / `%{"n"}` and their
# assignment forms, read `*package*`: the CL READER's package, which after a
# file's last `(in-package …)` is whatever that was.  So
#
#     package X; our $v = "X-value"; sub g { no strict 'refs'; ${"v"} }
#
# called from main returned main's $v — and a WRITE through the same spelling
# created $main::v while leaving $X::v alone, which is the silent half.
# `${"n"}` and `*{"n"}` disagreed about which stash `n` was in.
#
# A LEADING `::` is perl's ROOT stash — `${"::v"}` IS `$main::v` — and used to
# name no package at all (the split saw an empty prefix) and read undef.
#
# The rows are differential vs real perl.  The last two are the INVERSE guard:
# a qualified string, a `main::` prefix, a sub whose home package IS main, a
# top-level read and a string-eval region must all answer exactly as before —
# the fix must not turn "Pkg::name" into a current-package lookup, nor move a
# resolution that was already right.
#
# ir-spec §7.1 records the rule (it used to record this resolver as the one
# exception).  A residual gap in the same area is task #574: `local ${"n"} = …`
# does not localise the cell an unqualified symbolic read then sees — it fails
# on a base tree too, so it is not this fix's residue.
#
# ---------------------------------------------------------------------------
# Rows 7–9, task #685: the SAME resolver's other half — a FOREIGN-qualified
# name never reaches main's magic.  A Perl package PCL makes is
# `(:use :cl :pcl)`, so every sigil-named symbol the runtime exports (`|$!|`,
# `|%!|`, `%SIG`, `$_`, `|$<|`, the punctuation arrays …) is INHERITED into it
# and find-symbol answered main's variable for `%{"foo::!"}` — 134 errno keys
# where perl says foo's `!` glob has no hash at all — while the hash/array
# writers REPLACED that inherited binding, so `%{"foo::ENV"}` destroyed the
# process environment.  Twelve specials were probed vs perl: every
# `foo::`-qualified spelling is separate, every `main::`-qualified one shared.
# Row 9 is the INVERSE guard (main-qualified + unqualified keep the magic).
#
# NOT covered here, and not this fix's residue: the LITERAL spelling
# `%foo::SIG` reaches the same symbol through the CL READER, not through this
# resolver, so it still leaks — task #700, an emission-side fix.

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

plan tests => 9;

sub write_pl {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;
    return $pl_file;
}

# Transpile (PCLCore::transpile FAILS the row on a dropped statement) and run.
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
    my ($desc, $code) = @_;
    my $perl = `perl @{[ write_pl($code) ]} 2>&1`;
    my $pcl  = run_cl($code);
    is($pcl, $perl, "$desc (perl: " . ($perl =~ s/\n/\\n/gr) . ")");
}

# ---- the read half: all three sigils, in a NAMED sub --------------------

both_agree('#525 an unqualified ${"n"} / @{"n"} / %{"n"} reads the sub\'s package', <<'PL');
no strict 'refs';
package X;
our $v = "X-value";
our @a = ("X-a");
our %h = (k => "X-h");
sub gs { return ${"v"} }
sub ga { return join(",", @{"a"}) }
sub gh { return ${"h"}{k} }
sub gvar { my $n = "v"; return ${$n} }
package main;
our $v = "main-value";
our @a = ("main-a");
our %h = (k => "main-h");
print join("|", X::gs(), X::ga(), X::gh(), X::gvar()), "\n";
PL

# ---- the WRITE half — the silent one -----------------------------------

both_agree('#525 a write through an unqualified symbolic name lands in the sub\'s package', <<'PL');
no strict 'refs';
package X;
sub sets { ${"w"} = "X-written"; return }
sub seta { @{"wa"} = (1,2); return }
sub seth { %{"wh"} = (k => "X-h"); return }
package main;
X::sets(); X::seta(); X::seth();
print "s:", (defined $X::w ? $X::w : "u"), "/", (defined $main::w ? $main::w : "u"), "\n";
print "a:", scalar(@X::wa), "/", scalar(@main::wa), "\n";
print "h:", (defined $X::wh{k} ? $X::wh{k} : "u"), "/", (defined $main::wh{k} ? $main::wh{k} : "u"), "\n";
PL

# ---- every other way of arriving in a non-main package -----------------

both_agree('#525 anon sub, method, nested call, multi-segment and mixed-case packages', <<'PL');
no strict 'refs';
package Foo::Bar;  our $v = "FB"; sub g { return ${"v"} }
package MixedCase; our $v = "MC"; sub g { return ${"v"} }
package X;
our $v = "X";
sub m1 { my $c = shift; return ${"v"} }
my $anon = sub { return ${"v"} };
sub viaanon { return $anon->() }
sub inmap   { return join(",", map { ${"v"} . $_ } (1,2)) }
package Caller;
sub reach { return X::m1("X") }
package main;
our $v = "main";
print join("|", Foo::Bar::g(), MixedCase::g(), X->m1, X::viaanon(), X::inmap(), Caller::reach()), "\n";
PL

# ---- the root stash ----------------------------------------------------

both_agree('#525 a LEADING :: is the root stash — ${"::v"} is $main::v', <<'PL');
no strict 'refs';
package X;
our $v = "X-value";
sub root  { return ${"::v"} }
sub roota { return join(",", @{"::a"}) }
package main;
our $v = "main-value";
our @a = ("main-a");
print join("|", X::root(), X::roota()), "\n";
PL

# ---- the two spellings must now agree about the stash ------------------

both_agree('#525 ${"n"} and *{"n"}{SCALAR} name the same variable', <<'PL');
no strict 'refs';
package X;
our $v = "X-value";
sub via_scalar { return ${"v"} }
sub via_glob   { return ${*{"v"}{SCALAR}} }
sub agree      { return via_scalar() eq via_glob() ? "same" : "DIFFER" }
package main;
our $v = "main-value";
print join("|", X::via_scalar(), X::via_glob(), X::agree()), "\n";
PL

# ---- INVERSE: what must NOT move --------------------------------------

both_agree('#525 inverse: qualified names, a main-home sub, top level and a string eval', <<'PL');
no strict 'refs';
package X;
our $v = "X-value";
sub qual     { return ${"X::v"} }
sub mainqual { return ${"main::v"} }
sub inev     { return eval q{ ${"v"} } }
package main;
our $v = "main-value";
sub mainhome { return ${"v"} }
package Y;
sub reach_main { return main::mainhome() }
package main;
print join("|", X::qual(), X::mainqual(), X::inev(), mainhome(), Y::reach_main(), ${"v"}), "\n";
{ package X; print "inX:", ${"v"}, "\n"; }
print "inmain:", ${"v"}, "\n";
PL

# ---- #685: a FOREIGN-qualified magic name is its own variable -----------

both_agree('#685 a foo::-qualified magic name does not see main\'s magic', <<'PL');
no strict 'refs';
$! = 2;
$SIG{__WARN__} = sub { };
$_ = "underscore";
print "h:",   scalar(keys %{"foo::!"}), "\n";
print "s:",   (defined ${"foo::!"} ? "def" : "undef"), "\n";
print "sig:", scalar(keys %{"foo::SIG"}), "\n";
print "u:",   (defined ${"foo::_"} ? "def" : "undef"), "\n";
print "lt:",  (defined ${"foo::<"} ? "def" : "undef"), "\n";
print "arr:", scalar(@{"foo::+"}), "\n";
print "deep:", (defined ${"Deep::Pkg::,"} ? "def" : "undef"), "\n";
PL

# ---- #685: and a WRITE through it cannot reach main's ------------------

both_agree('#685 a write through a foo::-qualified magic name stays in foo', <<'PL');
no strict 'refs';
$! = 2;
%{"foo::!"} = (aaa => 1);
${"foo::,"} = "ZORK";
@{"foo::+"} = (1,2,3);
print "own-h:", scalar(keys %{"foo::!"}), "\n";
print "main-h:", (exists $!{aaa} ? "leaked" : "clean"), "\n";
print "own-s:", ${"foo::,"}, "\n";
print "main-s:", (defined $, ? "leaked" : "clean"), "\n";
print "own-a:", scalar(@{"foo::+"}), " main-a:", scalar(@+), "\n";
PL

# ---- #685 INVERSE: main-qualified and unqualified keep the magic -------

both_agree('#685 inverse: ${"main::!"} and ${"!"} still ARE the magic', <<'PL');
no strict 'refs';
$! = 2;
$SIG{__WARN__} = sub { };
$_ = "underscore";
print "mq-h:", (scalar(keys %{"main::!"}) > 0 ? "many" : "zero"), "\n";
print "mq-s:", ${"main::!"} + 0, "\n";
print "uq-s:", ${"!"} + 0, "\n";
print "uq-h:", (scalar(keys %{"!"}) > 0 ? "many" : "zero"), "\n";
print "mq-sig:", (exists ${"main::SIG"}{__WARN__} ? "shared" : "separate"), "\n";
print "mq-u:", (defined ${"main::_"} ? ${"main::_"} : "undef"), "\n";
print "root-u:", (defined ${"::_"} ? ${"::_"} : "undef"), "\n";
PL
