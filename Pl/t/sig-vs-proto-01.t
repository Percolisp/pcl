#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# sig-vs-proto-01.t -- task #2872: with the `signatures` feature in force,
# EVERY parenthesised list after a sub name is a SIGNATURE.  `($)`, `($, $)`,
# `(@)`, `()` are unnamed parameters with an arity check; a prototype is
# spelled only `:prototype(...)`.  PCL decided by SHAPE ("looks like a
# prototype -> prototype") where PPI could not tell it the feature was on --
# the pragma's own line, and a module whose import enables the feature -- so
# `use feature 'signatures'; sub f ($) { 42 } f(@two)` returned 42 where perl
# dies "Too many arguments".  One predicate now answers for every classifier
# (Pl::Parser::head_is_signature).
#
# Each row compares against perl's STDOUT + the text of a trapped death with
# its " at FILE line N." stripped (the s494 ruling: the death must happen in
# the same place; its location text need not match).  No 5.40 syntax: CI's
# perl is 5.38.

use v5.30;
use strict;
use warnings;
use Test::More;
use File::Temp qw(tempfile tempdir);
use File::Path qw(make_path);
use FindBin qw($RealBin);
use lib $RealBin;
use PCLCore;

my $project_root = "$RealBin/../..";
my $pl2cl        = "$project_root/pl2cl";
my $runtime      = "$project_root/cl/pcl-runtime.lisp";
my @sbcl_rt = PCLCore::sbcl_prefix($runtime);

plan skip_all => "pl2cl not found" unless -x $pl2cl;
plan skip_all => "sbcl not found"  unless `which sbcl 2>/dev/null`;

plan tests => 10;

# A module whose import turns the feature on in its caller's scope.
my $libdir = tempdir(CLEANUP => 1);
make_path("$libdir/T2872");
open(my $mfh, '>', "$libdir/T2872/Sig.pm") or die "fixture: $!";
print $mfh <<'PM';
package T2872::Sig;
use feature ();
use warnings ();
sub import { feature->import('signatures'); warnings->unimport('experimental::signatures') }
1;
PM
close $mfh;

sub write_pl {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh "use lib '$libdir'; no warnings; my \@a = (5, 6); sub try_ (&) { my \$r = eval { \$_[0]->() }; defined \$r ? \"\$r\\n\" : \"died: \$\@\" }\n$code";
    close $fh;
    return $pl_file;
}

sub strip_at { my ($s) = @_; $s =~ s/ at \S+ line \d+\.//g; $s =~ s/ at \(eval \d+\) line \d+\.//g; $s }

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
    my ($code, $desc) = @_;
    my $file = write_pl($code);
    my $perl = strip_at(scalar `perl $file 2>/dev/null`);
    my $pcl  = strip_at(run_cl($code));
    is($pcl, $perl, "$desc (perl: " . ($perl =~ s/\n/\\n/gr) . ")");
}

# ---- the feature on the pragma's OWN line ------------------------------

both_agree(q{use feature 'signatures'; no warnings; sub f ($) { 42 } print try_ { f(@a) }; print try_ { f(1) };},
           '`($)` on the `use feature` line is ONE unnamed parameter: f(@two) dies');

both_agree(q{use feature 'signatures'; no warnings; sub g ($, $) { 43 } sub h (@) { 47 } sub e () { 45 }
print try_ { g(1) }; print try_ { g(1, 2) }; print try_ { h(1, 2, 3) }; print try_ { e() }; print try_ { e(1) };},
           '`($, $)` / `(@)` / `()` on that line: arity-checked signatures');

both_agree(q{use v5.36; no warnings; sub m1 ($) { "m1" } print try_ { m1(@a) };},
           '`use v5.36` (the bundle) on the same line');

both_agree(q{use feature 'signatures'; no warnings; my $an = sub ($) { "an" }; print try_ { $an->(@a) }; print try_ { $an->(1) };},
           'an anonymous `sub ($)` on that line');

# ---- the feature from a module's import ---------------------------------

both_agree(q{use T2872::Sig; sub f ($) { 42 } print try_ { f(@a) };},
           'a module import enabling the feature, same line');

both_agree(q{use T2872::Sig;
sub f ($) { 42 }
sub g ($, $) { 43 }
sub e () { 45 }
print try_ { f(@a) }; print try_ { g(1) }; print try_ { e(1) };},
           'a module import enabling the feature, later lines');

both_agree(q{use T2872::Sig; sub add ($x, $y) { $x + $y } print add(2, 3), "\n";},
           'named parameters through the import (unchanged)');

both_agree(q{{ use T2872::Sig; sub h ($) { "h" } print try_ { h(@a) }; }
sub k ($) { "k:@_" }
print k(@a), "\n";},
           'the import is LEXICAL: after its block `($)` is a prototype again');

# ---- the feature OFF, and :prototype under it ---------------------------

both_agree(q{sub f ($) { "f:@_" } sub g (@) { "g:" . scalar(@_) }
print f(@a), "\n"; print g(@a), "\n"; print prototype(\&f), "\n";},
           'feature off: `($)` is a prototype (scalar context), unchanged');

both_agree(q{use feature 'signatures'; no warnings; sub p :prototype($) { "p:@_" } print p(@a), "\n"; print prototype(\&p), "\n";},
           '`:prototype($)` under the feature is a prototype');
