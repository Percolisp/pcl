#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# local-glob-01.t — `local *foo = RHS` typeglob localization, RHS eval order,
# and the deprecated conditional form `local *foo = RHS if COND`.
#
# Two bugs fixed (session 262):
#  1. RHS eval order: localizing *_ clears ALL slots of `_` (including @_), so an
#     RHS that reads @_ (e.g. local *_ = \join('', @_)) must be evaluated BEFORE
#     the slots are cleared.  PCL now binds the RHS in a wrapping let.
#  2. Conditional form: `local *_ = RHS if @_` (Text::ParseWords::old_shellwords).
#     When COND is false Perl does NOT localize at all (the rest of the scope sees
#     the outer slots); when true it localizes+assigns.  p-local-glob-if handles
#     both, evaluating RHS while the slots are still intact.

use v5.30;
use strict;
use warnings;

use Test::More;
use File::Temp qw(tempfile);

use lib ".";
use Pl::Parser2;
use FindBin;
use lib "$FindBin::Bin";
use PCLCore;

# The sbcl command line comes from the ONE builder every runner shares
# (tools/lib/PCLSbcl.pm via PCLCore::sbcl_prefix, task #344): the saved core
# with the runtime already compiled in, and the 512 MB control stack.  This
# file used to spell `sbcl --noinform --non-interactive --load
# cl/pcl-runtime.lisp` itself, which recompiles the whole runtime on EVERY row
# -- 2.89 CPU-s a row against 0.007 s from the core (measured s473u, #1544) --
# and ran on the default 2 MB stack, which is exactly the drift #344 exists to
# stop.
my @sbcl_rt = PCLCore::sbcl_prefix("$FindBin::Bin/../../cl/pcl-runtime.lisp");

sub run_pl {
    my $code = shift;
        my $cl_code = Pl::Parser2->parse_code($code);
    my ($fh, $filename) = tempfile(SUFFIX => '.lisp');
    print $fh $cl_code;
    close $fh;
    my $output = `sbcl @sbcl_rt --load "$filename" 2>&1`;
    unlink $filename;
    $output =~ s/^;.*\n//gm;
    $output =~ s/^\s*\n//gm;
    $output =~ s/PCL Runtime loaded\n?//g;
    $output =~ s/^caught .*\n//gm;
    $output =~ s/^compilation unit.*\n//gm;
    $output =~ s/^\s+//;
    return $output;
}

plan tests => 11;

# 1. RHS reads @_, which *_ localization would otherwise clear first.
is(run_pl(<<'PL'), "foobar\n", 'local *_ = \join("",@_) sees old @_');
sub t { local *_ = \join('', @_); print "$_\n"; }
t("foo","bar");
PL

# 2. \$_[0] as the RHS (also reads @_).
is(run_pl(<<'PL'), "foo\n", 'local *_ = \$_[0] sees old @_');
sub t { local *_ = \$_[0]; print "$_\n"; }
t("foo","bar");
PL

# 3. \"@_" interpolation RHS.
is(run_pl(<<'PL'), "a b\n", 'local *_ = \"@_" sees old @_');
sub t { local *_ = \"@_"; print "$_\n"; }
t("a","b");
PL

# 4. Plain literal RHS still works (no @_ involved).
is(run_pl(<<'PL'), "hi\n", 'local *_ = \"literal" still works');
sub t { local *_ = \"hi"; print "$_\n"; }
t();
PL

# 5. Conditional, COND true: localize + assign.
is(run_pl(<<'PL'), "Z\n", 'local *_ = RHS if COND (true) localizes');
sub t { local *_ = \"Z" if $_[0]; print "$_\n"; }
$_ = "outer";
t(1);
PL

# 6. Conditional, COND false: do NOT localize — rest of scope sees outer $_.
is(run_pl(<<'PL'), "outer\n", 'local *_ = RHS if COND (false) keeps outer $_');
sub t { local *_ = \"Z" if $_[0]; print "$_\n"; }
$_ = "outer";
t(0);
PL

# 7. The old_shellwords idiom: local *_ = \join('', @_) if @_.
is(run_pl(<<'PL'), "foobar|done\n", 'conditional local *_ = \join if @_ (with args)');
sub t {
    local *_ = \join('', @_) if @_;
    print "$_|done\n";
}
t("foo","bar");
PL

# 8. Same idiom, no args: falls through to the caller's $_.
is(run_pl(<<'PL'), "preset|done\n", 'conditional local *_ if @_ (no args) uses outer $_');
sub t {
    local *_ = \join('', @_) if @_;
    print "$_|done\n";
}
$_ = "preset";
t();
PL

# ── A glob STRINGIFIES to its perl spelling (#316, s395) ────────────────────
# Single-segment package names are upcased into CL packages and a glob's name
# is stored case-INVERTED (the symbol spelling), so both halves came back
# upcased: `print *plain` gave `*MAIN::PLAIN` where perl gives `*main::plain`.
# %pcl-invert-case is its own inverse, so one call restores each half — except
# for the package, where the inversion is applied on the way in only to names
# WITHOUT "::", so an all-lowercase multi-segment package must be left alone.
# All five expectations are the live perl answers.
is(run_pl(<<'PL'), "*main::plain\n*Foo::bar\n*main::STDOUT\n", 'globs stringify in perl case');
print *plain, "\n";
print *Foo::bar, "\n";
print *STDOUT, "\n";
PL

is(run_pl(<<'PL'), "*version::regex::thing\n", 'an all-lowercase multi-segment package is not upcased');
package version::regex;
sub q1 { return *thing }
print q1(), "\n";
PL

is(run_pl(<<'PL'), "STDOUT|main\n", '*FOO{NAME} and {PACKAGE} still answer the same way');
print *STDOUT{NAME}, "|", *STDOUT{PACKAGE}, "\n";
PL
