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

plan tests => 27;

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

# ---- task #495: what a BAREWORD means in an operand position --------------
#
# (a) A bareword in a `(*)` slot is the handle/class NAME as a plain string —
#     probed against perl 5.40.3 with `sub fh (*) { ref(\$_[0]) }`: FOO, an
#     OPEN handle G and STDOUT all arrive as SCALAR "FOO"/"G"/"STDOUT", never
#     a glob.  PCL emitted the bareword NODE, which a user sub's argument list
#     has nothing to quote, so it reached SBCL as an unbound variable and
#     killed the run.
both_agree('sub fh (*) { "fh($_[0])" } print fh FOO; print "\n";',
           '(*) slot: a bareword is its NAME as a string');

both_agree('sub fh (*) { "fh($_[0])" } open(G, "<", "/etc/hostname") or die;'
         . ' print fh G; print "\n";',
           '(*) slot: an OPEN handle is still the name, not a glob');

both_agree('sub fh (*) { "fh($_[0])" } print fh(FOO); print "\n";',
           '(*) slot: the PAREN form gives the same answer as the paren-less one');

# The inverse, and it is the reason the rule reads the classifier and not the
# word: perl CALLS a name that is callable here, `(*)` slot or not.
both_agree('sub FOO { "FOO-called" } sub fh (*) { "fh($_[0])" }'
         . ' print fh FOO; print "\n";',
           '(*) inverse: a DECLARED name in the slot is CALLED, not stringified');

# The OTHER inverse, which is the half the two `*` families do NOT share: for
# a BUILTIN handle slot the bareword is always the handle, even when a sub of
# that name exists — `sub FILE1 () {42}; tell FILE1` is -1, not 42 (that is
# t/comp/parser.t:540's shape).
both_agree('sub FILE1 () { 42 } print "t=", (tell FILE1), "\n";',
           'builtin (*) inverse: `tell FILE1` is the HANDLE even with sub FILE1');

# (c) The strictly-single bareword operand ends where perl's precedence says,
#     and `close`/`eof`/`fileno` are named unary operators: their operand runs
#     through everything tighter than named unary and stops at everything
#     looser.  `?` is looser, so `close G ? "a" : "b"` is `close(G) ? …`; PCL
#     read it as `close(G ? …)`, passed the ternary's value to close and died
#     on an unbound `G`.
both_agree('open(G, "<", "/etc/hostname") or die; print close G ? "a" : "b";'
         . ' print "\n";',
           'the bareword operand of `close` ENDS at `?` (#495 shape (c))');

both_agree('open(H, "<", "/etc/hostname") or die; print eof H ? "y" : "n";'
         . ' print "\n";',
           '... and of `eof`, from the same walk');

both_agree('open(F, "<", "/etc/hostname") or die;'
         . ' sub note2 { "note2(" . join("|", @_) . ")" }'
         . ' print note2(close F, "desc"), "\n";',
           '... and still at the comma, which is the case that branch existed for');

# The inverse of (c): `.` binds TIGHTER than a named unary, so perl really does
# read `close G . "x"` as `close("Gx")` — it returns "" for the handle that is
# not open (probed).  A "stop at any operator" rule would have broken it, so
# the operand extent is asserted on the emitted SHAPE (PCL cannot RUN this one
# yet: a registered handle Word inside an expression still emits a bare CL
# symbol — task #530, measured s449r: the fix needs the "which argument slots
# are HANDLE slots" question answered once, which no site can answer from a
# token run alone).
{
    my $cl = PCLCore::transpile("$pl2cl "
        . write_pl('open(G, "<", "/etc/hostname") or die; my $r = close G . "x";'));
    like($cl, qr/\(p-close \(p-\. /,
         '#495 inverse: `close G . "x"` keeps the `.` INSIDE the operand (perl: close("Gx"))');
}

# ---- task #532: a name that is BOTH a declared sub and an open handle ------
#
# perl's open-handle registry says nothing about what a bareword MEANS.  In a
# USER `(*)` slot a DECLARED SUB is CALLED and it WINS over the handle —
# `sub FILE2 {43} tell FILE2; sub fh(*){…} fh FILE2` passes 43 — which is the
# opposite of the BUILTIN handle slot, where the handle wins even against a
# declared sub (`sub FILE1 () {42}; tell FILE1` is -1, t/comp/parser.t:540).
# PCL took the "already a registered handle, leave the Word alone" exit in
# handle_subcalls without asking whether the name is CALLABLE here, so
# `_read_star_slot_bareword` was handed a plain Word it then declined, and the
# leaf emitted the handle's CL SYMBOL: an UNBOUND VARIABLE that killed the run
# — here, and in an `open`'s dup SOURCE slot (#594).  One condition, both.
#
# Emission is IDENTICAL over every measured population (corpus-diff's 111 plus
# 1036 A/B pairs of lib/**.pm + cpan-tests + perl's own t/), so — the s371 rule
# again — these rows ARE the guard.  Expectations are the live perl answers.

both_agree('sub FILE2 {43} tell FILE2; sub fh (*) { "fh(@_)" } print fh FILE2, "\n";',
           '#532: a declared sub in a USER (*) slot is CALLED, handle or not');

# The dup SOURCE slot, #594's site, with the same collision.  The `open` comes
# BEFORE the `sub` on purpose: perl compiles top-down, so the handle slot of
# that `open` really is a handle slot there, and only the LATER dup source sees
# a declared name.
both_agree('open(SRC, ">", "/dev/null") or die; sub SRC { "STDOUT" }'
         . ' my $r = open(my $d, ">&", SRC);'
         . ' print $d "dup-both\n" if $r; close $d if $r; close SRC;',
           '... and in an open\'s dup SOURCE slot too (#594\'s site)');

# THE NEGATIVES: the three readings that must not change.
both_agree('open(G, "<", "/etc/hostname") or die;'
         . ' sub fh (*) { "fh(@_)" } print fh G, "\n"; close G;',
           '#532 inverse: a handle that is NOT a declared sub stays its NAME');

both_agree('sub fh (*) { "fh(@_)" } print fh HH, "\n";',
           '#532 inverse: a bareword that is neither is its NAME');

both_agree('sub FILE1 () {42} print "tell=", tell(FILE1), "\n";',
           '#532 inverse: a BUILTIN handle slot ignores the declared sub');

both_agree('sub G {99} open(G, "<", "/etc/hostname") or die;'
         . ' print "c=", (close(G) ? 1 : 0), " t=", tell(G), "\n";',
           '#532 inverse: close/tell keep the HANDLE when a sub G is declared');
