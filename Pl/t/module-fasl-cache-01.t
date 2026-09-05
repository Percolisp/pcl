#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# module-fasl-cache-01.t — the module cache stores COMPILED fasls (task #1188),
# and a fasl load must be indistinguishable from the source load it replaces.
#
# WHY THESE ROWS.  From session 251 until #1188 a cached module was CL TEXT
# that SBCL recompiled on every run, because compiling and loading in one
# image ran a `sub NAME` install at both passes and a `BEGIN` block at the
# compile pass only — so the load pass re-installed the plain body OVER a
# BEGIN-time replacement whose guard now skipped (docs/module-double-exec-bug.md).
# The fix is an ORDER (`*pcl-fasl-build*`), so every row here is an ORDER or an
# IDENTITY question, and each is run BOTH ways: through the fasl (twice — the
# build pass and the later hit) and through the text (PCL_NO_FASL_CACHE=1, the
# pre-#1188 path).  A row that answered differently in the two modes would be
# the bug back; a row that answered the same but WRONG would be a divergence
# from perl, and every expectation below is perl 5.40.3's own, probed.
#
# Each module lives in a fresh tempdir, so its cache key (absolute path +
# generation + runtime identity) is COLD on the first pass of every run — the
# rows measure the build and the hit, not whatever the developer's cache holds.

use v5.30;
use strict;
use warnings;
use Test::More;
use File::Temp qw(tempdir);
use File::Spec;
use FindBin qw($RealBin);

my $root = File::Spec->rel2abs("$RealBin/../..");
my $pcl  = "$root/pcl";

plan skip_all => "pcl not found"  unless -x $pcl;
plan skip_all => "sbcl not found" unless `which sbcl 2>/dev/null`;

plan tests => 22;

my $dir = tempdir(CLEANUP => 1);

sub write_mod {
    my ($name, $body) = @_;
    my $path = File::Spec->catfile($dir, "$name.pm");
    open my $fh, '>', $path or die "write $path: $!";
    print $fh $body;
    close $fh;
    return $path;
}

# Run CODE under `pcl` with $dir on @INC.  %opt: text => 1 forces the
# pre-#1188 source path; debug => 1 keeps the PCL_FASL_DEBUG trace lines
# (which name, per module, which of the three paths the load took).
sub run_pcl {
    my ($code, %opt) = @_;
    local $ENV{PCL_NO_FASL_CACHE} = $opt{text} ? 1 : undef;
    delete $ENV{PCL_NO_FASL_CACHE} unless $opt{text};
    local $ENV{PCL_FASL_DEBUG} = $opt{debug} ? 1 : undef;
    delete $ENV{PCL_FASL_DEBUG} unless $opt{debug};
    my $out = `$pcl -I '$dir' -E '$code' 2>&1`;
    $out =~ s/^;.*\n//gm;
    $out =~ s/^PCL Runtime loaded\n//gm;
    $out =~ s/^PCL: module .*\n//gm unless $opt{debug};
    return $out;
}

# THE ORDER, both directions.  perl 5.40.3: OrdA prints B (the BEGIN redefine
# comes after the sub, so it wins), OrdB prints A (the sub definition comes
# after the BEGIN, so IT wins).  The pre-fix fasl path answered A for OrdA —
# that is the whole bug, and OrdB is the row that stops the "fix" from being
# "run BEGIN blocks last".
#
# OrdA IS THE SHARP ONE, measured: neutering %p-begin-situations (so a BEGIN
# block keeps its :compile-toplevel while the fasl is built) fails these two
# rows and the FASL-HIT row below, while GuardBoot's rows still pass — the
# guarded repro of the bug report needs a particular interleaving to show it,
# the plain ordering pair does not.
write_mod('OrdA', <<'PM');
package OrdA;
sub f { "A" }
BEGIN { no warnings 'redefine'; *OrdA::f = sub { "B" }; }
1;
PM
write_mod('OrdB', <<'PM');
package OrdB;
BEGIN { no warnings 'redefine'; *OrdB::f = sub { "B" }; }
sub f { "A" }
1;
PM

# THE GUARDED redefine — docs/module-double-exec-bug.md's own repro, the shape
# that broke Moo subclasses.  perl: REPLACED.  Both ingredients matter: the
# redefine is at BEGIN time AND idempotency-guarded, so a second pass skips it.
write_mod('GuardBoot', <<'PM');
package GuardBoot; our $DONE;
sub greet { return "BOOTSTRAP"; }
BEGIN { $DONE ||= do { no warnings 'redefine';
  *GuardBoot::greet = sub { return "REPLACED"; }; 1; }; }
1;
PM

# THE READER's stake in the compile pass.  A module sub whose name collides
# with a PCL builtin must get a package-LOCAL symbol (p-sub's shadow), or the
# module's body would clobber the runtime's own `push` for the whole program.
# That shadow is the one thing the fasl build keeps at compile time.
# perl: "2MODPUSH".
write_mod('ShadowMod', <<'PM');
package ShadowMod;
sub push { return "MODPUSH" }
1;
PM

# A module that `use`s another: it is `(p-use "Dep")` running in the COMPILE
# pass that creates Dep's package before the reader meets a symbol in it, so
# this row is what makes p-eval-always keep its :compile-toplevel.  perl: dep7.
write_mod('Dep', <<'PM');
package Dep;
our @ISA = ();
sub val { 7 }
1;
PM
write_mod('User', <<'PM');
package User;
use Dep;
sub show { return "dep" . Dep::val() }
1;
PM

for my $case (
    ['OrdA: a BEGIN redefine AFTER the sub wins',   'use OrdA; print OrdA::f();',            'B'],
    ['OrdB: a sub definition AFTER the BEGIN wins', 'use OrdB; print OrdB::f();',            'A'],
    ['GuardBoot: the guarded BEGIN redefine holds', 'use GuardBoot; print GuardBoot->greet;', 'REPLACED'],
    ['ShadowMod: a module sub named push is local', 'use ShadowMod; my @a=(1); push @a, 2; print scalar(@a), ShadowMod::push();', '2MODPUSH'],
    ['User: a module that uses another loads',      'use User; print User::show();',          'dep7'],
) {
    my ($name, $code, $want) = @$case;
    is(run_pcl($code), $want, "$name (fasl build pass)");
    is(run_pcl($code), $want, "$name (fasl hit)");
    is(run_pcl($code, text => 1), $want, "$name (text path, pre-#1188)");
}

# THE MECHANISM IS ACTUALLY TAKEN.  Without this the five rows above would
# still pass with fasl caching silently disabled — which is exactly how a
# perf change dies unnoticed.  PCL_FASL_DEBUG is the ONE place the cache says
# anything, and it names the path per module.
{
    my $trace = run_pcl('use GuardBoot; print GuardBoot->greet;', debug => 1);
    like($trace, qr/^PCL: module GuardBoot\.pm -> FASL HIT/m,
         'a warm run loads the module from its FASL');
    unlike($trace, qr/^PCL: module GuardBoot\.pm -> TEXT/m,
           '... and does not fall back to the text');

    my $text = run_pcl('use GuardBoot; print GuardBoot->greet;', debug => 1, text => 1);
    like($text, qr/^PCL: module GuardBoot\.pm -> TEXT/m,
         'PCL_NO_FASL_CACHE=1 takes the text path (the negative)');
}

# STALENESS.  The fasl is invalidated by the SOURCE's mtime exactly as the
# .lisp is, so editing a module must be seen on the very next run — the
# failure this would hide is a program running yesterday's module.
{
    is(run_pcl('use Dep; print Dep::val();'), '7', 'Dep answers 7 before the edit');
    write_mod('Dep', <<'PM');
package Dep;
our @ISA = ();
sub val { 99 }
1;
PM
    # No utime: write_mod's own mtime is `now`, and p-cache-valid-p wants the
    # cache STRICTLY newer than the source, so a same-second rewrite already
    # invalidates.  (Stamping the source into the FUTURE would make every
    # later run re-transpile, which is a property of the mtime check and not
    # something to assert here.)
    is(run_pcl('use Dep; print Dep::val();'), '99',
       'an edited module is re-transpiled and re-compiled, not served stale');
    is(run_pcl('use Dep; print Dep::val();'), '99', '... and stays right on the next run');
}

# The fasl NAME carries this runtime's identity, because a fasl has this
# runtime's macro expansions baked in and only this SBCL can load it.  A
# cached fasl for another runtime must be unreachable, not merely stale.
{
    my $home = $ENV{HOME} // '';
    my @fasls = glob("$home/.pcl-cache/*-*.fasl");
    ok(scalar(@fasls) > 0,
       'a cached module fasl is named <text-key>-<runtime-identity>.fasl');
}
