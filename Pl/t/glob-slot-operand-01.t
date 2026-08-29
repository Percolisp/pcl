#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# glob-slot-operand-01.t — task #663: the SYMBOL under a `*` cast in perl's
# glob-slot syntax, `*$x{SCALAR}`.
#
# PPI's `->symbol` is the ONE question ~40 sites in this compiler ask a Symbol
# token — "which variable is this?".  Its rule is "a `{…}` after a `$`-symbol
# means the symbol is really `%x`, unless a CAST trumps the braces", and its
# cast set is `$ @ %`: `*` is missing (PPI 1.291,
# docs/ppi-upstream-bugs.md §24).  But `*$x{SCALAR}` IS `*{$x}{SCALAR}` — the
# glob-slot syntax, whose operand is the scalar $x — and `*$x[0]{…}` is not
# even valid perl, so braces after a `*` cast are never an element access.
#
# TWO independent failures came out of that one wrong answer:
#
#   (a) `_rename_decl_within` skips a token whose ->symbol is a DIFFERENT
#       variable, so the `my $a` / `my $b` exception rename ($a__excl__0) left
#       the `$a` inside `*$a{SCALAR}` pointing at the never-assigned PACKAGE
#       global — "Can't use an undefined value as a symbol reference", and
#       because it is one top-level form the WHOLE FILE produced no output.
#       t/uni/parser.t lost 35 rows of coverage to it (18/5, one aborted form;
#       28/30 with the fix, 58 rows, no abort).
#
#   (b) with a MAGIC symbol the mis-canonicalisation reached the expression
#       compiler itself: `*$_{HASH}` emitted `(p-dynamic-typeglob (p-gethash
#       %_ "HASH"))` — a typeglob of the hash element `$_{HASH}` of a phantom
#       `%_` — where perl reads the glob's HASH slot.  SILENT WRONG, and live
#       in core Carp.pm (five sites), which is why nothing had ever caught it.
#
# Fixed where all ~40 consumers pass through, by normalising the odd spelling
# into the one the generic machinery already consumes (CLAUDE.md rule 11):
# `Pl::Parser::_brace_glob_slot_symbol` wraps the Symbol's text in braces, so
# the reparse yields `*{$x}{SLOT}` — identical emission, and a Symbol PPI can
# no longer mis-canonicalise because the subscript is no longer its sibling.
#
# THE NEGATIVE IS THE POINT: `8 *$Config{sizesize}` is MULTIPLICATION, and PPI
# lexes that `*` as an Operator, not a Cast — measured across seven preceding
# terms (call, subscript, hash element, string, number, paren) before the pass
# was written.  Rows 5 and 6 hold that line.

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

plan tests => 6;

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

sub both_agree {
    my ($code, $desc) = @_;
    my $perl = `perl @{[ write_pl($code) ]} 2>&1`;
    my $pcl  = run_cl($code);
    is($pcl, $perl, "$desc (perl: " . ($perl =~ s/\n/\\n/gr) . ")");
}

# ---- (a) the renamed lexical: t/uni/parser.t's own shape -------------------

both_agree(<<'PL', '#663 *$a{SCALAR} reads the RENAMED my $a (t/uni/parser.t:75)');
no strict 'refs';
my $a = "gslot_pn1";
*$a = sub { 5 };
print "p1\n";
*$a = \10;
print "p2=", ${*$a{SCALAR}} // "u", "\n";
my $b = "gslot_pn2";
*$b = \11;
print "p3=", ${*$b{SCALAR}} // "u", "\n";
PL

# ---- (b) the magic symbol: core Carp.pm's shape ---------------------------

both_agree(<<'PL', '#663 *$_{HASH}/{SCALAR}/{NAME} are GLOB SLOTS, not %_ elements (Carp.pm)');
no strict 'refs';
our %h = (k => 1);
our $s = "gslot_pn3";
*{$s} = \%h;
*{$s} = \51;
for ( \*{$s} ) {
  print "H=", (*$_{HASH} ? "hash" : "nohash"), "\n";
  print "S=", ${*$_{SCALAR}} // "u", "\n";
  print "N=", *$_{NAME} // "u", "\n";
}
PL

# ---- every other spelling of the same operand -----------------------------

both_agree(<<'PL', '#663 the operand may be a sub lexical, an our, or a package global');
no strict 'refs';
our $g = "gslot_pn4"; *$g = \61;
print "our=", ${*$g{SCALAR}} // "u", "\n";
sub f { my $q = shift; *$q = \62; return ${*$q{SCALAR}} }
print "sub=", f("gslot_pn5") // "u", "\n";
$main::pk = "gslot_pn6"; *$main::pk = \63;
print "pkg=", ${*$main::pk{SCALAR}} // "u", "\n";
PL

# ---- the braced spelling must be UNCHANGED (it already worked) ------------

both_agree(<<'PL', '#663 inverse: *{$x}{SLOT} and a bare *NAME{SLOT} still work');
no strict 'refs';
my $x = "gslot_pn7";
*{$x} = \71;
print "braced=", ${*{$x}{SCALAR}} // "u", "\n";
*gslot_pn8 = \72;
print "bare=", ${*gslot_pn8{SCALAR}} // "u", "\n";
*{$x} = sub { 9 };
print "code=", (*{$x}{CODE} ? "code" : "nocode"), " ", (*$x{CODE} ? "code" : "nocode"), "\n";
PL

# ---- THE NEGATIVE: `*` after a term is MULTIPLICATION ---------------------

both_agree(<<'PL', '#663 negative: EXPR *$h{k} is multiplication, not a glob slot');
my %h = (k => 3, j => 5);
my @a = (2, 4);
my $n = 7;
sub g { 6 }
print "n=",   $n     *$h{k}, "\n";
print "num=", 8      *$h{k}, "\n";
print "sub=", (1 << (2 *$h{k})), "\n";
print "arr=", $a[1]  *$h{k}, "\n";
print "hsh=", $h{j}  *$h{k}, "\n";
print "cal=", g()    *$h{k}, "\n";
print "par=", ($n+1) *$h{k}, "\n";
PL

# ---- and a REAL hash element after a real glob-slot read in one statement --

both_agree(<<'PL', '#663 negative: a glob slot and a %h element in the same expression');
no strict 'refs';
our %h = (k => 4);
my $x = "gslot_pn9";
*$x = \10;
print "mix=", ${*$x{SCALAR}} * $h{k}, "\n";
print "mix2=", $h{k} * ${*$x{SCALAR}}, "\n";
PL
