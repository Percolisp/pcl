#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# sig-param-shadow-01.t — task #454: a SIGNATURE PARAMETER is the sub's own
# declaration, so it shadows a same-named file lexical.
#
# perl desugars `sub f ($x) {…}` to `my $x = $_[0]` INSIDE the sub, so every
# `$x` in the body is the parameter and the file's `my $x` is not captured at
# all.  PCL refused the whole file — "Parser2 TODO: file lexical 'x' captured
# by sub f" — because the two scope questions were blind to signatures:
# `_check_sub_captures` scans the sub's BLOCK, and the signature is not in the
# block; `_ref_shadowed` climbs to the Statement::Sub and asks
# `_stmt_declares_canon` about the preceding siblings, of which the signature
# is one.  Both ask `_signature_param_canons` now — the detector and the
# rewriter share the resolver, which is the project's rule for this pair.
#
# ONLY THE PARAMETER NAMES: a DEFAULT is an expression evaluated in the sub,
# so `my $y = 5; sub f ($x = $y)` really does reference the outer `$y` — hence
# the top-level-comma split, and hence the last row here.
#
# PPI PRODUCES BOTH SHAPES FOR THE SAME SOURCE, and a guard that saw one would
# be half a guard: with `use feature "signatures"` on an EARLIER line `($x)`
# lexes as a PPI::Structure::Signature, on the pragma's own line as a
# PPI::Token::Prototype (that line dependence is task #455).  Row 5 is the
# one-line spelling on purpose.
#
# The shape emits IDENTICALLY across all four populations (951 files A/B'd, 0
# DIFF, plus corpus-diff over the 111), so no corpus guards it: these rows do.

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

plan tests => 7;

sub write_pl {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
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

# ---- the refusal is gone, and the answers are perl's ----------------------

both_agree(<<'PL', 'a signature param shadows a LATER file `my` of the same name');
use feature "signatures";
sub f ($x) { "f($x)" }
my $x = f 5;
print "$x\n";
PL

both_agree(<<'PL', '... with a DEFAULT parameter');
use feature "signatures";
sub f ($x = 1) { "f($x)" }
my $x = f 5;
print "$x\n";
PL

both_agree(<<'PL', '... with a slurpy tail');
use feature "signatures";
sub f ($x, @r) { "f($x)[@r]" }
my $x = f 5;
print "$x\n";
PL

both_agree(<<'PL', '... and through a nested anon sub that closes over the param');
use feature "signatures";
sub m1 ($x) { my $inner = sub { "in($x)" }; $inner->() }
my $x = 4;
print m1(2), " x=$x\n";
PL

both_agree('use feature "signatures"; sub f ($x) { "f($x)" } my $x = f 5; print "$x\n";',
           'the ONE-LINE spelling, where PPI gives a Token::Prototype instead');

# ---- the file lexical declared BEFORE the sub still belongs to the file ---

both_agree(<<'PL', 'a file `my $x` BEFORE the sub keeps its own value');
use feature "signatures";
my $x = 9;
sub k ($x) { "k($x)" }
print k(1), " x=$x\n";
PL

# ---- the narrowing must not go further: a REAL capture still refuses ------
#
# `($x)` declares `$x` and nothing else, so the body's `@x` is the file's
# `my @x` — a genuine capture, and the refusal is still the honest answer.
{
    my $pl = write_pl(<<'PL');
use feature "signatures";
sub g ($x) { "g($x)[@x]" }
my @x = (7, 8);
print g(1), "\n";
PL
    my $err = `$pl2cl $pl 2>&1 >/dev/null`;
    like($err, qr/file lexical 'x' captured by sub g/,
         'a DIFFERENT-sigil use of the same bare name is still a capture');
}
