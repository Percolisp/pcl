#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# imported-term-01.t — task #365: an IMPORTED `()`-prototype sub is a TERM.
#
# `sub pi () {…}` makes the bareword `pi` a term in perl, so `2 * pi` is
# 2 * pi(), `print pi, "\n"` prints the number, and `pi + 1` is pi() + 1.
# PCL printed the STRING "pi" after an operator or a comma (`2 * pi` was 0)
# and read the head of an expression as a LIST OPERATOR (`pi + 1` parsed as
# pi(+1)) — the #266 classifier answering from an ABSENCE of knowledge.
#
# THE CAUSE WAS THE `use` SEAM, NOT THE TERM READING (measured s438c, and it
# is the opposite of where the task pointed).  `Parser::_merge_module_
# prototypes` imported a module's prototype only when it affected code
# generation — a block arg, or a parameter SLOT (`$`, `\X`, `@`, `%`) — or
# when `export_names` listed it.  A `()` prototype has no slots, and
# `export_names` reads literal `qw()` out of `@EXPORT`/`@EXPORT_OK`, which real
# modules build from variables:
#
#     my @trig = qw( pi tan … );                       # Math::Complex
#     our @EXPORT = (qw( i Re Im … atan2 ), @trig);
#
# so `pi` reached @EXPORT through @trig and the scan never saw it (and
# Math::Trig re-exports it from there, which is the reported case).  Following
# that would mean interpreting the module's own code; the fix keys on the
# PROTOTYPE instead — an empty one is a PARSE fact, so it crosses a `use` on
# the same footing as a block prototype.
#
# ONE PREDICATE, `Pl::Environment::proto_is_zero_arg`: PExpr::_is_zero_arg_func
# (does this bareword parse as a term?) and the merge (must this prototype
# cross a `use`?) had drifted into two copies of the record test, which is what
# let this through.
#
# Emission is IDENTICAL across the four populations with the fix in (951 files
# A/B'd, 0 DIFF, plus corpus-diff over the 111), so no corpus guards it: these
# rows are the guard.  The fixture builds its @EXPORT the way Math::Complex
# does, so the row tests the MECHANISM and not one CPAN module's spelling.

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

plan tests => 7;

# A module whose @EXPORT is built from a VARIABLE, like Math::Complex's.
my $libdir = tempdir(CLEANUP => 1);
make_path("$libdir/T438");
open(my $mfh, '>', "$libdir/T438/Konst.pm") or die "fixture: $!";
print $mfh <<'PM';
package T438::Konst;
use strict; use warnings;
require Exporter;
our @ISA = qw(Exporter);
my @consts = qw( kpi khalf );
our @EXPORT = (qw( kname ), @consts);
sub kpi   () { 3.25 }
sub khalf () { 0.5 }
sub kname { "T438" }
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
    my $perl = `perl @{[ write_pl($code) ]} 2>&1`;
    my $pcl  = run_cl($code);
    is($pcl, $perl, "$desc (perl: " . ($perl =~ s/\n/\\n/gr) . ")");
}

# ---- the bug: a term after an operator or a separator ---------------------

both_agree('use T438::Konst; print kpi, "\n";',
           'a `()` sub after a COMMA is the term, not the string "kpi"');

both_agree('use T438::Konst; my $w = 2 * kpi; print "$w\n";',
           '... and after a binary operator (was 0: "kpi" numified)');

both_agree('use T438::Konst; my $z = kpi + 1; print "$z\n";',
           '... and at the HEAD of one: kpi() + 1, not kpi(+1)');

both_agree('use T438::Konst; my @l = (kpi, 1); print "@l\n";',
           '... and inside a list');

# ---- the inverses, which were already right and must stay right ----------

both_agree('use T438::Konst; my $y = kpi; print "$y\n";',
           'a lone term is unchanged');

both_agree('use T438::Konst; print kname(), "\n";',
           'a plain (non-prototyped) exported sub still calls');

# ---- the negative: an unknown bareword is still a STRING -----------------

both_agree('use T438::Konst; print "x=", nosuchword, "\n";',
           'an UNKNOWN bareword stays the string (no strict subs)');
