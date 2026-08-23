#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# user-unary-01.t — task #453: a USER sub whose prototype makes it one of
# perl's NAMED UNARY OPERATORS takes the named-unary operand site.
#
# perl decides from the prototype alone (toke.c, just_a_word): after its
# leading `;`s, a prototype that is exactly one of `$` `_` `*` `+` or one
# `\X` / `\[…]` group makes a named unary operator — an operator whose operand
# binds LOOSER than `.` `+` `*` … and TIGHTER than comparison and the comma.
# So `f "a" . "b"` is f("ab"), and `g + 1, "\n"` is g(1), "\n".
#
# PCL had TWO operand sites in Pl::PExpr::handle_subcalls: the named-unary one
# (Config's BUILTIN table) ran `_extend_high_prec`, the strictly-single one did
# not — and a user `($)`/`(;$)`/`(*)`/`(_)` sub took the second.  `is_named_unary`
# now answers for a declared sub too, so both shapes reach ONE mechanism.
#
# WHY IT IS WORTH A FILE (the s371 rule, and minus-word-01.t's precedent): the
# shape emits IDENTICALLY across every measured population — 951 files of
# lib/**.pm + cpan-tests + perl's own t/, 0 DIFF, plus corpus-diff identical
# over the 111 — so no corpus can guard it and these rows are the guard.  Every
# expectation is the live `perl` answer, taken by running the same source.
#
# THE NEGATIVES ARE AS LOAD-BEARING AS THE POSITIVES: the routing must NOT
# capture a list operator (`($;$)`, `(@)`, the trailing-`;` `($;)`), and it must
# leave Config's non-named-unary 1-arg builtins (close, fileno, eof) at the
# strictly-single site, whose bareword-filehandle branch is why it still exists.

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

plan tests => 12;

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

# Both sides of every row: PCL's answer must be PERL's answer, so a future
# change to either can only agree or fail.
sub both_agree {
    my ($code, $desc) = @_;
    my $perl = `perl @{[ write_pl($code) ]} 2>&1`;
    my $pcl  = run_cl($code);
    is($pcl, $perl, "$desc (perl: " . ($perl =~ s/\n/\\n/gr) . ")");
}

# ---- the four unary-class prototypes, operand through a high-prec op -------

both_agree('sub f ($) { "f($_[0])" } print f "a" . "b", "\n";',
           '($): the operand extends through `.`');

both_agree('sub o (;$) { "o($_[0])" } print o "a" . "b", "\n";',
           '(;$): a leading `;` is still a named unary');

both_agree('sub s1 (*) { "s1($_[0])" } print s1 "a" . "b", "\n";',
           '(*): a filehandle-slot prototype is a named unary too');

both_agree('sub u (_) { "u($_[0])" } print u "a" . "b", "\n";',
           '(_): the $_-defaulting prototype is a named unary');

both_agree('sub f ($) { "f($_[0])" } my $x = 5; print f $x + 1, "\n";',
           '($): the operand extends through `+`, so f($x + 1)');

both_agree('sub f ($) { "f($_[0])" } my $x = 5; print f $x * 2, "\n";',
           '($): ... and through `*`');

# ---- the comma is the boundary, which is the other half of #453 -----------

both_agree('sub g (*) { "g($_[0])" } print "R=", g + 1, "\n";',
           '(*): the operand STOPS at the comma — g(1), not g(1, "\n")');

both_agree('sub f ($) { "f($_[0])" } print f -1, "\n";',
           '($): a negative literal is the operand, not a subtraction');

# ---- negatives: a LIST operator must not be narrowed ----------------------

both_agree('sub opt ($;$) { "opt(" . join(",", @_) . ")" }'
         . ' my @r = (3, 4); my $s = opt @r[0,1]; print "$s\n";',
           '($;$) is a LIST operator: two args still reach it');

both_agree('sub lst (@) { "lst(" . join(",", @_) . ")" }'
         . ' my $s = lst 3, 4; print "$s\n";',
           '(@) is a list operator, untouched');

both_agree('sub trail ($;) { "trail($_[0])" } my $s = trail "a" . "b";'
         . ' print "$s\n";',
           '($;): a TRAILING `;` keeps it a list operator (perl, toke.c)');

# ---- negatives: Config's own strictly-single builtins do not move ---------

both_agree('open(F, "<", "/etc/hostname") or die; my $ok = close F;'
         . ' print "ok=$ok\n"; print length "ab" . "cd", "\n";',
           'close FILEHANDLE keeps the strictly-single site; length is unchanged');
