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

plan tests => 15;

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

# ---- #456: an EARLIER section's call to a sub the file defines LATER ----
# A block that switches package (`{ package Q; ... }`) becomes its own emission
# section, so a file-level `sub nm {...}` written after it is DEFINED after that
# section has already run.  Perl compiles every named sub before run time and
# prints PKG.  PCL cannot yet (the hoist-order half of #456) -- but what it must
# never do again is answer the forward STUB's value, which made this SILENT: the
# program printed the empty string and exited 0.  s432 made the stub DIE
# (CLAUDE.md rule 12), so the failure is loud and trappable.
#
# WHEN THE HOIST HALF OF #456 LANDS: delete these two rows and put the same
# program through both_agree instead (it will then print PKG like perl).
{
    my $code = qq[{ package Q; print main::nm(), "|\\n"; } sub nm {"PKG"}\n];
    my $out  = run_pcl($code);
    like($out, qr/Undefined subroutine &main::nm called/,
         '#456 cross-section forward call dies loudly');
    unlike($out, qr/^\|/m,
           '#456 ... and never answers the forward stub, which printed empty');
}

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

done_testing();
