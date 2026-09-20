#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# builtin-override-01.t — tasks #1870 + #1992 step 1: the two COMPILE-TIME
# spellings that displace a core builtin, beside the `use subs` one that
# already worked.
#
# perl overrides a WEAK keyword (Perl_keyword()'s negative half, the set
# `Pl::Environment::builtin_is_overridable` holds) when the package got a sub
# of that name at COMPILE time, by import or predeclaration:
#
#   (a) `use subs qw(time); sub time {…}`            — already worked
#   (b) `use Module qw(time)`, an IMPORT               — this file
#   (c) `BEGIN { *CORE::GLOBAL::time = sub {…} }`      — this file, every package
#   (c') `BEGIN { *Other::time = sub {…} }`            — this file, that package
#   (d) a sub merely DEFINED in the package            — perl keeps the BUILTIN
#   (e) `CORE::time()`                                 — always the builtin
#
# (d) and (e) are the cases a careless fix breaks, so they are rows here too.
# The registry is ONE registry (Environment::builtin_override_target answers
# the PACKAGE whose sub to call, or undef); a `*CORE::GLOBAL::` sub lives in
# another package, so its call is emitted QUALIFIED there.
#
# Corpus emission is IDENTICAL with this in (corpus-diff over the 111 files),
# so these rows and `t/op/override.t` (3/17 -> 9/13) are the guard.

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

plan tests => 12;

# A fixture module that exports subs named like builtins — the mechanism, not
# any one module's spelling.
my $libdir = tempdir(CLEANUP => 1);
make_path("$libdir/T1870");
open(my $mfh, '>', "$libdir/T1870/Clock.pm") or die "fixture: $!";
print $mfh <<'PM';
package T1870::Clock;
use strict; use warnings;
require Exporter;
our @ISA = qw(Exporter);
our @EXPORT_OK = qw(time sleep hex plainname);
sub time  () { 12345.5 }
sub sleep (;@) { "slept:" . (@_ ? $_[0] : 0) }
sub hex ($) { "H:" . $_[0] }
sub plainname { "plain" }
1;
PM
close $mfh;

sub write_pl {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh "use lib '$libdir';\n$code";
    close $fh;
    return $pl_file;
}

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
    my $perl = `perl @{[ write_pl($code) ]} 2>&1`;
    my $pcl  = run_cl($code);
    is($pcl, $perl, "$desc (perl: " . ($perl =~ s/\n/\\n/gr) . ")");
}

# ---- (b) an IMPORT displaces the builtin in the importing package --------

both_agree('use T1870::Clock qw(time); print time, "\n";',
           'an imported sub named `time` wins over the builtin');

both_agree('use T1870::Clock qw(sleep); print sleep(0.25), "\n";',
           '... and over `sleep`, with the call arguments intact');

both_agree('use T1870::Clock qw(time); print time(), "\n";',
           '... in the parenthesised spelling too');

both_agree('use T1870::Clock qw(time); print CORE::time() > 1000000000 ? "core\n" : "bad\n";',
           '(e) CORE:: still reaches the builtin inside the override package');

both_agree('use T1870::Clock qw(time); package Other; print main::time() =~ /\./ ? "frac\n" : "int\n";'
           . ' package main; print "done\n";',
           'the override is PACKAGE-scoped: another package still gets the builtin');

both_agree('use T1870::Clock qw(plainname); print plainname(), "\n";',
           'a non-builtin import is untouched by the registry');

# ---- (c) *CORE::GLOBAL::NAME displaces it for EVERY package -------------

both_agree('BEGIN { *CORE::GLOBAL::hex = sub { "G:" . $_[0] } } print hex("ff"), "\n";',
           'a CORE::GLOBAL glob assignment at BEGIN displaces the keyword');

both_agree('BEGIN { *CORE::GLOBAL::hex = sub { "G:" . $_[0] } }'
           . ' { package Deep; sub g { hex("ff") } } print Deep::g(), "\n";',
           '... in every package, not just the one that assigned it');

both_agree('BEGIN { *CORE::GLOBAL::hex = sub { "G:" . $_[0] } } print CORE::hex("ff"), "\n";',
           '... and CORE::hex is still the builtin');

# ---- (c\') *Pkg::NAME from ANOTHER package ------------------------------

both_agree('BEGIN { package Setter; *Target::getppid = sub { 4242 } }'
           . ' { package Target; sub g { getppid() } } print Target::g(), "\n";',
           'a glob assignment from another package displaces it THERE');

# ---- (d) the case a careless fix breaks ---------------------------------

both_agree('{ package D; sub oct { "D-local" } sub g { oct("10") } } print D::g(), "\n";',
           '(d) a sub merely DEFINED does not displace the builtin');

both_agree('{ package R; *R::getppid = sub { 7777 }; sub g { getppid() } }'
           . ' print R::g() == 7777 ? "sub\n" : "builtin\n";',
           'a RUN-TIME glob assignment does not displace it either (perl-probed)');
