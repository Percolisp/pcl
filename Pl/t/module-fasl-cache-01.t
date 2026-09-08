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

use lib "$RealBin/../../tools/lib";
use PCLPaths ();   # cache_root — the ONE Perl-side reading of $PCL_CACHE_DIR
use PCLSbcl ();    # cached_core — the saved core the #1303 rows run through

plan skip_all => "pcl not found"  unless -x $pcl;
plan skip_all => "sbcl not found" unless `which sbcl 2>/dev/null`;

plan tests => 59;

my $dir = tempdir(CLEANUP => 1);

sub write_mod {
    my ($name, $body) = @_;
    my $path = File::Spec->catfile($dir, "$name.pm");
    open my $fh, '>', $path or die "write $path: $!";
    print $fh $body;
    close $fh;
    return $path;
}

# Run CODE under `pcl` with $dir on @INC.  %opt:
#   text    => 1  forces the pre-#1188 source path (PCL_NO_FASL_CACHE=1)
#   debug   => 1  keeps the PCL_FASL_DEBUG trace lines (which name, per
#                 module, which of the three paths the load took)
#   compile => S  the PCL_COMPILE_DIRS value for the run
#   nocomp  => S  the PCL_NO_COMPILE_DIRS value for the run
#   env     => H  any further environment for the child (the #1303 rows set
#                 PCL_CACHE_DIR and PCL_CORE this way)
#
# EVERY FASL-PATH ROW PASSES compile => '*' (task #1261).  These fixtures live
# in a tempdir, and since #1261 the DEFAULT is to compile only modules under
# perl's installed library directories or PCL's own lib/ — so without the
# override the "fasl build" and "fasl hit" passes below would both take the
# TEXT path and stop testing what they say.  The default itself is asserted by
# its own rows further down.
#
# NO PERL5LIB (task #1284, fixed s473i).  This helper used to set PERL5LIB=$dir
# as well, because `pcl -I` reached the PROGRAM's transpile while the runtime
# spawned `pl2cl --module` with no -I at all — so a module's own `use` did not
# resolve at transpile time and the dependency rows had nothing to depend on.
# PERL5LIB worked only because the child inherits the environment.  The module
# transpile now gets this program's own @INC as -I, so `-I $dir` alone is the
# whole story, and these rows exercise the real path.
sub run_pcl {
    my ($code, %opt) = @_;
    local $ENV{PCL_NO_FASL_CACHE} = $opt{text} ? 1 : undef;
    delete $ENV{PCL_NO_FASL_CACHE} unless $opt{text};
    local $ENV{PCL_FASL_DEBUG} = $opt{debug} ? 1 : undef;
    delete $ENV{PCL_FASL_DEBUG} unless $opt{debug};
    local $ENV{PCL_COMPILE_DIRS} = $opt{compile};
    delete $ENV{PCL_COMPILE_DIRS} unless defined $opt{compile};
    local $ENV{PCL_NO_COMPILE_DIRS} = $opt{nocomp};
    delete $ENV{PCL_NO_COMPILE_DIRS} unless defined $opt{nocomp};
    local $ENV{PERL5LIB};
    delete $ENV{PERL5LIB};
    my %saved;
    for my $k (sort keys %{ $opt{env} || {} }) {
        $saved{$k} = $ENV{$k};
        $ENV{$k} = $opt{env}{$k};
    }
    my $out = `$pcl -I '$dir' -E '$code' 2>&1`;
    for my $k (sort keys %saved) {
        if (defined $saved{$k}) { $ENV{$k} = $saved{$k} } else { delete $ENV{$k} }
    }
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
    is(run_pcl($code, compile => '*'), $want, "$name (fasl build pass)");
    is(run_pcl($code, compile => '*'), $want, "$name (fasl hit)");
    is(run_pcl($code, text => 1), $want, "$name (text path, pre-#1188)");
}

# THE MECHANISM IS ACTUALLY TAKEN.  Without this the five rows above would
# still pass with fasl caching silently disabled — which is exactly how a
# perf change dies unnoticed.  PCL_FASL_DEBUG is the ONE place the cache says
# anything, and it names the path per module.
{
    my $trace = run_pcl('use GuardBoot; print GuardBoot->greet;',
                        debug => 1, compile => '*');
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
# (Under <cache>/modules/ since #1261, beside <cache>/core/ and <cache>/proto/.)
my $cache = PCLPaths::cache_root() . '/modules';
{
    my @fasls = glob("$cache/*-*.fasl");
    ok(scalar(@fasls) > 0,
       'a cached module fasl is named <text-key>-<runtime-identity>.fasl');
}

# ─────────────────────────────────────────────────────────────────────────
# THE DEPENDENCY MANIFEST (task #1261)
#
# A module A's TRANSPILE reads the prototypes and exports of the modules it
# `use`s — a `(&@)` makes a trailing block a code ref, an empty prototype
# makes a bareword a TERM, and a bareword is a CALL only for a known exported
# sub.  So A's cached CL encodes facts about B, and before #1261 nothing about
# B was in A's key or its validity check.  Measured on the base tree, both
# cache paths: editing B left A's cached parse in force — the prototype case
# SILENTLY (a code ref still passed where perl now passes a hash ref) and the
# export case as a CRASH ("Undefined subroutine &A2::zap called").
#
# perl is the oracle for every row here, and perl re-parses A on every run.
{
    # B2 carries two PARSE facts about itself: `zap`'s EMPTY prototype (which
    # makes a bareword a TERM, so `zap + 1` is zap()+1) and its EXPORT list (a
    # bareword is a CALL only for a known sub, so an unexported `tag` is the
    # STRING "tag").  Every expectation below is perl 5.40.3's, probed.
    my $b2_full = <<'PM';
package B2;
use Exporter 'import';
our @EXPORT = qw(zap tag);
sub zap () { return 7 + (@_ ? 100 : 0) }
sub tag { "TAG" }
1;
PM
    # Prototype dropped: `zap + 1` is now zap(+1), and zap sees an argument.
    (my $b2_noproto = $b2_full) =~ s/sub zap \(\)/sub zap/;
    # `tag` no longer exported: the bareword is the string.
    (my $b2_noexport = $b2_full) =~ s/qw\(zap tag\)/qw(zap)/;

    write_mod('A3', <<'PM');
package A3;
use B2;
sub go  { return zap + 1 }
sub go2 { return join("|", tag, 1) }
1;
PM

    my $prog = 'use A3; print A3::go(), " ", A3::go2();';

    for my $mode (['fasl', compile => '*'], ['text', text => 1]) {
        my ($label, @opt) = @$mode;
        write_mod('B2', $b2_full);
        # A3 is NOT rewritten between the passes: its own mtime must stay put,
        # or its own key would invalidate the entry and hide the hole.
        is(run_pcl($prog, @opt), '8 TAG|1', "A3 reads B2's prototype and export ($label)");

        write_mod('B2', $b2_noproto);
        is(run_pcl($prog, @opt), '107 TAG|1',
           "dropping B2's PROTOTYPE re-transpiles A3 ($label)");

        write_mod('B2', $b2_noexport);
        is(run_pcl($prog, @opt), '8 tag|1',
           "dropping B2's EXPORT re-transpiles A3 ($label)");
    }

    # NO SPURIOUS MISS: with nothing edited, the very next run is a HIT.  A
    # manifest that re-hashed to a different answer, or a check that failed
    # open, would show up here as an endless re-transpile.
    write_mod('B2', $b2_full);
    run_pcl($prog, compile => '*');
    my $warm = run_pcl($prog, debug => 1, compile => '*');
    like($warm, qr/^PCL: module A3\.pm -> FASL HIT/m,
         'an untouched module and an untouched dependency are a HIT');

    # THE SIDECAR exists, and it names the dependency it read.
    my @deps = grep { my $t = _slurp($_); $t =~ m{^source\t\S+\t\Q$dir\E/A3\.pm$}m }
               glob("$cache/*.deps");
    is(scalar(@deps), 1, "A3's cache entry has exactly one dependency manifest");
    like(_slurp($deps[0]), qr{^dep\tmod\tB2\t\w+\t[0-9a-f]{32}\t\Q$dir\E/B2\.pm$}m,
         '... naming B2 with its content hash');

    # A MISSING MANIFEST IS AN INVALID ENTRY — never a trusted one.  That is
    # also what every entry written before #1261 looks like.
    unlink $deps[0];
    is(run_pcl($prog, compile => '*'), '8 TAG|1',
       'a cache entry with no manifest still answers correctly');
    ok(scalar(grep { my $t = _slurp($_); $t =~ m{^source\t\S+\t\Q$dir\E/A3\.pm$}m }
              glob("$cache/*.deps")),
       '... by re-transpiling, which writes the manifest again');
}

sub _slurp {
    my ($p) = @_;
    open my $fh, '<', $p or return '';
    my $t = do { local $/; <$fh> };
    close $fh;
    return $t // '';
}

# ─────────────────────────────────────────────────────────────────────────
# WHICH MODULES ARE COMPILED TO NATIVE CODE — two directory lists (#1261).
#
# PCL_COMPILE_DIRS names the directories whose modules are compiled and
# cached (`*` = all); unset means perl's installed library directories plus
# PCL's own lib/.  PCL_NO_COMPILE_DIRS names directories that are NEVER
# compiled and WINS on any match.  A module that is not compiled still gets
# the transpile cache and the manifest check; it loads from the cached text.
{
    my $local = 'use GuardBoot; print GuardBoot->greet;';
    my $inst  = 'use Carp; print 1;';

    my $t = run_pcl($local, debug => 1);
    like($t, qr/^PCL: module GuardBoot\.pm -> TEXT/m,
         'DEFAULT: a module reached through -I / use lib is NOT compiled');
    my $ti = run_pcl($inst, debug => 1);
    like($ti, qr/^PCL: module Carp\.pm -> (?:FASL HIT|fasl-build)/m,
         'DEFAULT: a module from an installed library directory IS compiled');

    like(run_pcl($local, debug => 1, compile => '*'),
         qr/^PCL: module GuardBoot\.pm -> (?:FASL HIT|fasl-build)/m,
         "PCL_COMPILE_DIRS='*' compiles the local module too");

    like(run_pcl($local, debug => 1, compile => $dir),
         qr/^PCL: module GuardBoot\.pm -> (?:FASL HIT|fasl-build)/m,
         'a local directory LISTED in PCL_COMPILE_DIRS is compiled');
    like(run_pcl($inst, debug => 1, compile => $dir),
         qr/^PCL: module Carp\.pm -> TEXT/m,
         '... and the list REPLACES the installed default (Carp is not)');

    like(run_pcl($local, debug => 1, nocomp => '*'),
         qr/^PCL: module GuardBoot\.pm -> TEXT/m,
         "PCL_NO_COMPILE_DIRS='*' compiles nothing (local)");
    like(run_pcl($inst, debug => 1, nocomp => '*'),
         qr/^PCL: module Carp\.pm -> TEXT/m,
         "... and nothing installed either");

    like(run_pcl($local, debug => 1, compile => '*', nocomp => $dir),
         qr/^PCL: module GuardBoot\.pm -> TEXT/m,
         'PCL_NO_COMPILE_DIRS wins over PCL_COMPILE_DIRS on a match');

    # A listed directory that does not exist is not an error — perl's @INC
    # tolerates the same — it simply never matches.
    like(run_pcl($local, debug => 1, compile => "/no/such/dir:$dir"),
         qr/^PCL: module GuardBoot\.pm -> (?:FASL HIT|fasl-build)/m,
         'a nonexistent entry in the list is ignored, not fatal');
}

# ─────────────────────────────────────────────────────────────────────────
# WHERE THE CACHE LIVES — $PCL_CACHE_DIR is a PROCESS-START fact (task #1303).
#
# It reached proto/, core/ and `pcl --clear-cache` (all Perl-side) and NOT the
# module cache, because the runtime's *pcl-cache-dir* was a DEFPARAMETER
# INITFORM: evaluated when the runtime loads, which on the normal path is when
# a saved CORE is built.  So the variable was a lie for the biggest cache, and
# an installed core built by another user would have sent every user's modules
# to the BUILDER's home.  The fix is %p-default-cache-dir called from an
# sb-ext:*init-hooks* entry, the same shape $$, the FP modes, the standard
# handles and PCL_RAW_ELEMS already use.
#
# THE SECOND BLOCK IS THE ONE THAT MATTERS: it runs through a saved core built
# while a DIFFERENT cache directory was in force (the ambient one — this box's
# ~/.pcl-cache, or whatever $PCL_CACHE_DIR the gate itself ran under), which is
# the installed-core shape no source-mode rehearsal reproduces.
write_mod('CacheHome', <<'PM');
package CacheHome;
sub v { 42 }
1;
PM
{
    my $alt = tempdir(CLEANUP => 1);
    is(run_pcl('use CacheHome; print CacheHome::v();',
               compile => '*', env => { PCL_CACHE_DIR => $alt }),
       '42', 'a module loads with PCL_CACHE_DIR pointing at a fresh directory');
    ok(scalar(glob("$alt/modules/*.lisp")),
       '... its transpile is written UNDER PCL_CACHE_DIR');
    ok(scalar(glob("$alt/modules/*.fasl")),
       '... and so is its fasl');
    ok(scalar(glob("$alt/modules/*.deps")),
       '... and so is its dependency manifest');
    # NOT a file COUNT of the default cache: it is shared with every other
    # gate file running in parallel (and with any other PCL on this box), so a
    # count is a race.  The precise question is whether THIS module reached it,
    # and the manifest answers it by name — the fixture lives in a tempdir, so
    # only this run could have written a manifest naming that path.
    my @leaked = grep { _slurp($_) =~ m{^source\t\S+\t\Q$dir\E/CacheHome\.pm$}m }
                 glob("$cache/*.deps");
    is(scalar(@leaked), 0,
       '... and NOTHING about it was written to the default cache directory');

    # Through a SAVED CORE, which is how PCL normally starts.  The core was
    # built under the ambient cache directory; this run names another one.
    my $core = PCLSbcl::cached_core("$root/cl/pcl-runtime.lisp");
    ok($core && -f $core, 'a saved core is available for the init-hook row');
    my $alt2 = tempdir(CLEANUP => 1);
    run_pcl('use CacheHome; print CacheHome::v();',
            compile => '*', env => { PCL_CACHE_DIR => $alt2,
                                     ($core ? (PCL_CORE => $core) : ()) });
    ok(scalar(glob("$alt2/modules/*.lisp")),
       'a SAVED CORE writes the module cache where THIS run says, not where it was built');
}

# ─────────────────────────────────────────────────────────────────────────
# THE MODULE TRANSPILE SEARCHES THIS PROGRAM'S @INC (task #1284, s473i).
#
# A module A's transpile reads the prototypes and exports of every module it
# `use`s.  The runtime spawns `pl2cl --module` for it, and that child used to
# get NO `-I` at all — so `pcl -I DIR prog.pl` reached the PROGRAM's transpile
# and not A's: a dependency B beside A in DIR did not resolve,
# `_extract_module_prototypes` returned undef, and every parse fact B carries
# was SILENTLY missing.  Measured: with B's `sub blk (&)` unseen, `blk { 42 }`
# passed the block's VALUE where perl passes a CODE ref.
#
# The oracle is perl on the same fixtures.  These rows deliberately do NOT set
# PERL5LIB — that was the workaround this task removes (see run_pcl's comment),
# and with it set they would pass either way.
write_mod('IncB', <<'PM');
package IncB;
use Exporter 'import';
our @EXPORT = qw(blk);
sub blk (&) { my ($c) = @_; return ref($c) eq 'CODE' ? "CODE:" . $c->() : "VAL:$c" }
1;
PM
write_mod('IncA', <<'PM');
package IncA;
use IncB;
sub go { return blk { 42 } }
1;
PM
{
    my $code   = 'use IncA; print IncA::go();';
    my $oracle = `perl -I '$dir' -e '$code' 2>&1`;
    is($oracle, 'CODE:42',
       'perl passes a code ref for a (&) prototype declared in a dependency');
    is(run_pcl($code, compile => '*'), $oracle,
       'a module transpile resolves its OWN dependency through the program -I');
    is(run_pcl($code, text => 1), $oracle, '... on the text path too');
}

# AND A NAME THAT DID NOT RESOLVE IS A DEPENDENCY.  With the search path now
# shared, "B was not findable" is a fact the runtime can re-check, so a cache
# entry written while B was missing must not be served once B appears.  The
# manifest has always recorded `missing<TAB>mod<TAB>NAME`; until this task it
# was parsed and IGNORED.
{
    my $only_a = tempdir(CLEANUP => 1);   # SplitA alone
    my $with_b = tempdir(CLEANUP => 1);   # SplitB, added on the second run
    my $cdir   = tempdir(CLEANUP => 1);   # a cache of this block's own

    _write_at("$only_a/SplitA.pm", <<'PM');
package SplitA;
use SplitB;
sub go { return blk2 { 7 } }
1;
PM
    _write_at("$with_b/SplitB.pm", <<'PM');
package SplitB;
use Exporter 'import';
our @EXPORT = qw(blk2);
sub blk2 (&) { my ($c) = @_; return ref($c) eq 'CODE' ? "CODE:" . $c->() : "VAL:$c" }
1;
PM

    my $prog = 'use SplitA; print SplitA::go();';
    my $env  = "PCL_CACHE_DIR='$cdir' PCL_COMPILE_DIRS='*'";

    # Pass 1: SplitB is not on the path at all, so SplitA is transpiled with it
    # unresolved and the entry records `missing mod SplitB`.  The program then
    # dies at SplitA's own `use SplitB` — expected, and not what is measured
    # here; the CACHE ENTRY is.
    `$env $pcl -I '$only_a' -E '$prog' 2>&1`;
    my $recorded = grep { _slurp($_) =~ /^missing\tmod\tSplitB$/m }
                   glob("$cdir/modules/*.deps");
    ok($recorded, 'a dependency that did not resolve is recorded in the manifest');

    # Pass 2: SplitB's directory joins the path, against the SAME cache.
    my $out = `$env $pcl -I '$only_a' -I '$with_b' -E '$prog' 2>&1`;
    $out =~ s/^;.*\n//gm;
    $out =~ s/^PCL Runtime loaded\n//gm;
    my $oracle = `perl -I '$only_a' -I '$with_b' -e '$prog' 2>&1`;
    is($oracle, 'CODE:7',
       'perl resolves the dependency once its directory joins the path');
    is($out, $oracle,
       'a cache entry written while a dependency was MISSING is not served once it resolves');
}

sub _write_at {
    my ($path, $body) = @_;
    open my $fh, '>', $path or die "write $path: $!";
    print $fh $body;
    close $fh;
    return $path;
}

# ─────────────────────────────────────────────────────────────────────────
# WHICH COMPILER WROTE THIS ENTRY (task #1119, s473i).
#
# The key used to be the module's path + *pcl-cache-generation*, and that
# string says what a SESSION INTENDED, not which compiler ran.  Two worktrees
# on the same string SHARED the cache (a gate row died calling a function that
# existed only in the sibling's tree), and a stopped session's entries outlived
# its own compiler.  The key now carries a fingerprint of the compiler — its
# root PATH plus every Pl/**.pm's mtime and size, the rule
# Pl::ProtoCache::_compiler_stamp already uses for its own memo.
#
# Asked of the runtime directly, with two FAKE compiler trees, because that is
# the only way to have two "compilers" in one gate row: a real second checkout
# is minutes of setup and this file's whole point is the cache, not git.
{
    my $t1 = tempdir(CLEANUP => 1);
    my $t2 = tempdir(CLEANUP => 1);
    for my $t ($t1, $t2) {
        mkdir "$t/Pl" or die "mkdir $t/Pl: $!";
        _write_at("$t/pl2cl", "#!/usr/bin/perl\n1;\n");
        _write_at("$t/Pl/Fake.pm", "package Fake; 1;\n");
    }
    my $src = write_mod('StampSrc', "package StampSrc;\nsub v { 1 }\n1;\n");

    my @prefix = PCLSbcl::sbcl_prefix(runtime => "$root/cl/pcl-runtime.lisp",
                                      env_core => 1);
    # ONE FRESH PROCESS PER ANSWER, deliberately: the stamp is memoised for the
    # process, and clearing the memo by hand would make these rows depend on a
    # variable that does not exist on the base they are inverse-verified
    # against — they would error there instead of DISAGREEING there, which is
    # the whole point of a guard.  Four starts off the saved core, ~0.3 s each.
    #
    # NO APOSTROPHE anywhere in the form: it goes through `sbcl --eval '...'`,
    # and a Lisp quote would end the shell's own string (measured —
    # `(concatenate 'string ...)` made sh answer "Syntax error").
    my $key = sub {
        my ($p2c) = @_;
        my $probe = qq{(progn (setf pcl::*pcl-pl2cl-path* "$p2c") }
                  . qq{(format t "KEY ~A~%" }
                  . qq{(namestring (pcl::p-compute-cache-path "$src" t))))};
        my $out = `sbcl @prefix --eval '$probe' 2>&1`;
        return $out =~ /^KEY (\S+)$/m ? $1 : '';
    };

    my $k1 = $key->("$t1/pl2cl");
    my $k2 = $key->("$t2/pl2cl");
    my $k3 = $key->("$t1/pl2cl");
    my $then = time + 100;
    utime($then, $then, "$t1/Pl/Fake.pm") or die "utime: $!";
    my $k4 = $key->("$t1/pl2cl");

    ok(length $k1, 'the runtime answers with a cache path for a compiler tree');
    isnt($k1, $k2,
         'two compiler trees with the SAME generation get DIFFERENT keys (#1119)');
    is($k1, $k3,
       '... while the same tree, untouched, keeps its key (the cache still works)');
    isnt($k1, $k4,
         '... and editing a Pl/*.pm changes it, without touching the generation');
}
