#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ref-to-ref-01.t — ref() / reftype() distinguish a scalar reference from a
# reference-to-a-reference (session 217).
#
# The bug: p-ref classified a ref-to-ref ("REF") as "SCALAR" because box-nesting
# depth does not separate the two — a `my`-bound ref ($r = \$x) round-trips to
# the same depth as a plain scalar ref. The fix uses the box `is-ref` flag (set
# only by p-backslash on scalar-ref wrappers) to find the referent, and reports
# "REF" iff that referent is itself a wrapper or *holds* a reference. The
# held-a-ref test is non-recursive, so a self-referential scalar ($x = \$x) does
# not loop and plain scalars (incl. undef, '' array elements) stay "SCALAR".

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

plan tests => 30;

sub run_cl {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;
    my $cl_code = `$pl2cl $pl_file 2>/dev/null`;
    my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cl_fh $cl_code;
    close $cl_fh;
    my $output = `sbcl @sbcl_rt --load $cl_file 2>&1`;
    $output =~ s/^;.*\n//gm;
    $output =~ s/^PCL Runtime loaded\n//gm;
    $output =~ s/^\s*\n//gm;
    return $output;
}

sub test_cl {
    my ($name, $code, $expected) = @_;
    is(run_cl($code), $expected, $name);
}

# ── plain scalar ref vs ref-to-ref, through `my` variables ───────────────────
# NB run_cl strips blank lines, so an empty ref() prints nothing observable.
test_cl('ref(non-ref) is empty',
    'my $x = 1; print "[", ref($x), "]\n";', "[]\n");

test_cl('ref($r) where $r = \\$x is SCALAR',
    'my $x = 1; my $r = \$x; print ref($r), "\n";', "SCALAR\n");

test_cl('ref($rr) where $rr = \\$r is REF',
    'my $x = 1; my $r = \$x; my $rr = \$r; print ref($rr), "\n";', "REF\n");

# ── direct (no intermediate variable) ────────────────────────────────────────
test_cl('ref(\\$x) direct is SCALAR',
    'my $x = 5; print ref(\$x), "\n";', "SCALAR\n");

test_cl('ref(\\\\1) direct is REF',
    'print ref(\\\\1), "\n";', "REF\n");

test_cl('ref(\\$r) direct is REF',
    'my $x = 1; my $r = \$x; print ref(\$r), "\n";', "REF\n");

# ── aggregate refs unaffected ────────────────────────────────────────────────
test_cl('ref(\\@a) is ARRAY',
    'my @a = (1,2); print ref(\@a), "\n";', "ARRAY\n");

test_cl('ref of scalar holding an arrayref, through a var, is REF',
    'my @a = (1,2); my $ar = \@a; my $rar = \$ar; print ref($rar), "\n";', "REF\n");

# ── undef referent stays SCALAR (regression guard: *p-undef* must not look ref) ─
test_cl('ref(\\$undef) is SCALAR',
    'my $u; print ref(\$u), "\n";', "SCALAR\n");

# ── self-referential scalar must not loop, and is REF ────────────────────────
test_cl('self-referential $x = \\$x is REF (no hang)',
    'my $x; $x = \$x; print ref($x), "\n";', "REF\n");

# ── reftype of a ref-to-ref is SCALAR (the referent scalar) ──────────────────
test_cl('reftype($rr) is SCALAR',
    'use Scalar::Util qw(reftype);'
  . 'my $x=1; my $r=\$x; my $rr=\$r; print reftype($rr), "\n";', "SCALAR\n");

test_cl('reftype($r) of a plain scalar ref is SCALAR',
    'use Scalar::Util qw(reftype);'
  . 'my $x=1; my $r=\$x; print reftype($r), "\n";', "SCALAR\n");

# ── glob ref numifies to a (non-zero) address like other refs; a *bare* glob
#    numifies to 0.  Both share box-value=typeglob — the is-ref flag set by
#    p-backslash (and preserved by box-set) is the only discriminator.  (Exact
#    address round-trip is GC-fragile, so we only assert non-zero-ness here.)
test_cl('glob ref numifies to a non-zero address',
    'our $g_t = 5; my $r = \*g_t; print +($r + 0 != 0 ? "nonzero" : "zero"), "\n";',
    "nonzero\n");

test_cl('bare glob in scalar numifies to 0',
    'our $g_t = 5; my $g = *g_t; print 0 + $g, "\n";', "0\n");

# ── ${ARRAY-, HASH- or CODE ref} is perl's fatal (#1249(1) s473h, #1592 s483a)
#    The REFERENT rule is the discriminator: `\@a`'s referent is the array
#    itself, while a `\$aref` read back out of a container has the same UNBOXED
#    shape but a scalar BOX for a referent (#154's ambiguity).  CODE was
#    excluded until s483a on a measurement of a DIFFERENT check (a type sniff on
#    the unboxed value, which Sub::Quote's `${$_[1]->{'$t'}}` does take down —
#    see the #1592 rows below and Pl/t/moo-01.t).
test_cl('${$aryref} dies "Not a SCALAR reference"',
    'my @a=(1,2); my $r=\@a; my $v = eval { "".${$r} };'
  . 'print +($@ =~ /^Not a SCALAR reference/ ? "died" : "no:[$@]"), "\n";',
    "died\n");

test_cl('${$hashref} dies "Not a SCALAR reference"',
    'my %h=(k=>1); my $r=\%h; my $v = eval { "".${$r} };'
  . 'print +($@ =~ /^Not a SCALAR reference/ ? "died" : "no:[$@]"), "\n";',
    "died\n");

test_cl('${\&named} dies "Not a SCALAR reference" (#1592)',
    'sub cc { 1 } my $r=\&cc; my $v = eval { "".${$r} };'
  . 'print +($@ =~ /^Not a SCALAR reference/ ? "died" : "no:[$@]"), "\n";',
    "died\n");

test_cl('${ \\$aryref } is the array ref, not a fatal',
    'my @a=(1,2); my $ar=\@a; my $rr=\$ar; print ref(${$rr}), "\n";', "ARRAY\n");

test_cl('${ \\$hashref } is the hash ref, not a fatal',
    'my %h=(k=>1); my $hr=\%h; my $rr=\$hr; print ref(${$rr}), "\n";', "HASH\n");

test_cl('\\(@b, 9) is (ARRAY, SCALAR) and ${$r[0]} is the fatal',
    'my @b=(7,8); my @r = \(@b, 9);'
  . 'print scalar(@r), ref($r[0]), ref($r[1]), "\n";'
  . 'eval { my $v = "".${$r[0]} };'
  . 'print +($@ =~ /^Not a SCALAR reference/ ? "died" : "no"), "\n";',
    "2ARRAYSCALAR\ndied\n");

# ── #1592 (s483a): a CODE referent is the same fatal, and the shapes that must
#    keep working are Sub::Quote's.  `${$coderef}` used to hand the code ref
#    back silently, and `${$coderef} = 5` CLOBBERED the variable holding it, so
#    the next call through that variable died "Undefined subroutine &main::5".
test_cl('${$coderef} dies and the sub stays callable',
    'my $cr = sub { 7 }; my $v = eval { "".${$cr} };'
  . 'print +($@ =~ /^Not a SCALAR reference/ ? "died" : "no:[$@]"), "\n";'
  . 'print $cr->(), "\n";',
    "died\n7\n");

test_cl('${CODE} dies through a container element and as a literal',
    'my $cr = sub { 7 }; my %h = (c => $cr); my @a = ($cr); my $o = "";'
  . 'for my $t (sub { ${$h{c}} }, sub { ${$a[0]} }, sub { ${sub {7}} }) {'
  . '  eval { my $v = "".$t->() };'
  . '  $o .= ($@ =~ /^Not a SCALAR reference/ ? "d" : "n");'
  . '} print $o, "\n";',
    "ddd\n");

test_cl('the CALL spellings still call; @{$cr} and %{$cr} keep their own fatals',
    'my $cr = sub { 7 }; print $cr->(), &{$cr}(), &$cr(), "\n";'
  . 'eval { my @x = @{$cr} };'
  . 'print +($@ =~ /^Not an ARRAY reference/ ? "ary-died" : "no:[$@]"), "\n";'
  . 'eval { my %x = %{$cr} };'
  . 'print +($@ =~ /^Not a HASH reference/ ? "hash-died" : "no:[$@]"), "\n";',
    "777\nary-died\nhash-died\n");

test_cl('a ref TO a scalar holding a code ref still derefs (Sub::Quote)',
    'my $cr = sub { 7 }; my $rr = \$cr; print ref(${$rr}), "\n";'
  . 'my $caps = { q{$t} => \$cr };'
  . 'my $s = eval q{ sub { my $t = ${$_[1]->{q{$t}}}; ref($t) } };'
  . 'print $s->(undef, $caps), "\n";',
    "CODE\nCODE\n");

test_cl('${$ref} = 5 is the same fatal for ARRAY, HASH and CODE referents',
    'my @a=(1,2); my %h=(k=>1); my $cr = sub { 7 }; my $o = "";'
  . 'for my $t (sub { ${\@a} = 5 }, sub { ${\%h} = 5 }, sub { ${$cr} = 5 }) {'
  . '  eval { $t->() };'
  . '  $o .= ($@ =~ /^Not a SCALAR reference/ ? "d" : "n");'
  . '} print $o, "\n";'
  . 'print scalar(@a), scalar(keys %h), $cr->(), "\n";',
    "ddd\n217\n");

test_cl('the scalar-ref and symbolic WRITE paths are untouched',
    'my $x = 1; my $r = \$x; ${$r} = 5; print $x, "\n";'
  . 'my $cr = sub { 7 }; my $rc = \$cr; ${$rc} = 9; print $cr, "\n";'
  . 'our $nm; my $n = "nm"; ${$n} = 3; print $main::nm, "\n";',
    "5\n9\n3\n");

test_cl('"$$coderef" interpolation is the fatal too',
    'my $cr = sub { 7 }; my $s = eval { "$$cr" };'
  . 'print +($@ =~ /^Not a SCALAR reference/ ? "died" : "no:[$s]"), "\n";',
    "died\n");

# ── #1592, the other half: `\` on a CODE VALUE gains the anonymous scalar perl
#    gives it, so a reference TO a code ref is one level deeper than the code
#    ref.  It used to be idempotent (box(FN) either way), which is what made
#    the fatal above undecidable — and it answered ref(\\&f) = CODE.
test_cl('ref(\\\\&f) is REF and ref(\\&f) is CODE',
    'sub f { 1 } print ref(\&f), " ", ref(\\\\&f), " ", ref(\ sub { 7 }), "\n";',
    "CODE REF REF\n");

test_cl('a ref to a code ref derefs to the code ref, and once more is the fatal',
    'sub mysub2 { lc shift } our $subrefref = \\\\&mysub2;'
  . 'print $$subrefref->("GOOD"), "\n";'
  . 'my $rr = \\\\&mysub2; print ref(${$rr}), " ", ${$rr}->("X"), "\n";'
  . 'eval { my $v = "".${${$rr}} };'
  . 'print +($@ =~ /^Not a SCALAR reference/ ? "died" : "no:[$@]"), "\n";',
    "good\nCODE x\ndied\n");

test_cl('\\&$coderef is that same code ref, not a reference to it',
    'my $cr = sub { 7 }; my $r = \&$cr; print ref($r), " ", $r->(), "\n";'
  . 'sub g { 5 } my $n = "g"; my $r2 = \&$n; print ref($r2), " ", $r2->(), "\n";',
    "CODE 7\nCODE 5\n");
