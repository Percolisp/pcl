#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# proto-parse-class-01.t — task #259 (Option B phase 2, Track B3.2): how a
# call to a DECLARED sub parses is decided by its PROTOTYPE's shape, never by
# its minimum arity.  Design: docs/b3-operand-collapse-s428.md §B3.2.
#
# perl (toke.c, just_a_word): an empty prototype makes a TERM; a prototype that
# is, after its leading `;`s, exactly one of `$ _ * +` or one `\X`/`\[…]` group
# makes a NAMED UNARY operator (operand optional when a `;` led); everything
# else — `($$)`, `(@)`, `(%)`, and `($;)` / `(;$;)` whose TRAILING `;` keeps
# them list operators — is a LIST operator.  A signature is not a prototype and
# does not affect parsing.
#
# PCL's Pl::PExpr::no_params_of_sub returned a declared sub's min_params — an
# ARITY fact — and its callers read 0 as "takes no arguments", so every
# `min 0` prototype (`(;$)`, `(;$;)`, `(@)`, `(%)`, `(;@)`) and every
# all-defaulted signature was called with ZERO arguments, its real arguments
# left dangling, and the whole statement DROPPED ("Bug. Fell through. Missing
# case: [" — t/comp/proto.t ×3, t/op/utftaint.t, t/op/taint.t).  `(*)` was read
# as a list operator (perl: named unary) and a 1-param / slurpy signature as a
# named unary (perl: list operator).  The fix is ONE reading of the prototype's
# shape, Pl::PExpr::_proto_parse_spec, in known_no_of_params' convention.
#
# Every expectation is the live `perl` answer (probed s429, the 16×10 matrix in
# the session log).  NOT asserted here (separate findings filed s429): a user
# named unary's operand through `.`/`+` (`f "a" . "b"` — task #453), the `(_)`
# `$_` default (#260).

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

plan tests => 5;

sub write_pl {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;
    return $pl_file;
}

# Transpile (a DROPPED statement fails the row, via PCLCore) and run.
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

# ── 1. The census shapes: list-operator prototypes with min arity 0 ──────────
# t/comp/proto.t:387/390/847 and t/op/utftaint.t:17 — every one dropped whole.
is(run_cl(<<'PL'), "A:1\nB:1\nC:6\nD:6\nE:any(7)\n",
sub a_hash (%) { scalar(@_) }
print "A:", (1 == a_hash 'a'), "\n";            # proto.t:387  (% = list op)
print "B:", (2 == a_hash 'a','b'), "\n";        # proto.t:390
sub unilist3 (;$;) { $_[0] + 1 }
print "C:", (unilist3 0 || 5), "\n";            # proto.t:847  trailing `;` = list op
sub u3 (;$;) { $_[0] + 1 }
print "D:", (u3 0 || 5), "\n";                  # the #259 reproducer
sub any_tainted (@) { "any(@_)" }
sub tainted ($) { any_tainted @_ }              # utftaint.t:17 — `(@)` inside `($)`
print "E:", tainted(7), "\n";
PL
   'min-0 list-operator prototypes: (%) (;$;) (@) take their arguments');

# ── 2. Named unary with an OPTIONAL operand: `(;$)` is f(0) || 5, not f(0||5) ─
is(run_cl(<<'PL'), "F:f(0)\nG:f(a) b\nH:f()\nI:f\n",
sub f (;$) { "f(" . join(",", @_) . ")" }
print "F:", (f 0 || 5), "\n";                   # named unary: f(0) || 5
my @x = (f 'a', 'b'); print "G:@x\n";           # one operand, `'b'` is the list's
print "H:", (f || 5), "\n";                     # no operand: f()
print "I:", (f == 1 ? "t" : "f"), "\n";         # binary op after: zero-arg call
PL
   '(;$) is a named unary with an optional operand');

# ── 3. `(*)` is a NAMED UNARY (was read as a list operator) ──────────────────
is(run_cl(<<'PL'), "J:f(0)\nK:f(a) b\n",
sub f (*) { "f(" . join(",", @_) . ")" }
print "J:", (f 0 || 5), "\n";
my @x = (f 'a', 'b'); print "K:@x\n";
PL
   '(*) alone is a named unary');

# ── 4. A SIGNATURE does not affect parsing: the sub is a list operator ────────
is(run_cl(<<'PL'), "L:f(5)\nM:f(5)\nN:f(a;b)\n",
use feature 'signatures'; no warnings;
sub f ($v = 1) { "f($v)" }
print "L:", (f 0 || 5), "\n";                   # list op: f(0 || 5)
sub g ($v) { "f($v)" }
print "M:", (g 0 || 5), "\n";
sub h ($v, @r) { "f($v;@r)" }
my @x = (h 'a', 'b'); print "N:@x\n";           # slurpy gets 'b'
PL
   'signature subs (all-defaulted, 1-param, slurpy) parse as list operators');

# ── 5. The guards: ($) / ($;$) / () keep their readings ──────────────────────
is(run_cl(<<'PL'), "O:f(0)\nP:f(a) b\nQ:f(a,b)\nR:f(x,y)\nS:4\n",
sub f ($) { "f(" . join(",", @_) . ")" }
print "O:", (f 0 || 5), "\n";                   # named unary: f(0) || 5
my @x = (f 'a', 'b'); print "P:@x\n";
sub g ($;$) { "f(" . join(",", @_) . ")" }
print "Q:", (g 'a', 'b'), "\n";                 # s361: `($;$)` takes both
my ($p, $q) = ('x', 'y'); print "R:", g($p, $q), "\n";
sub PI () { 3 }
print "S:", PI + 1, "\n";                       # `()` is a TERM: PI() + 1
PL
   '($) named unary, ($;$) list op, () term — unchanged');
