#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# foreach-raw-01.t — task #862 ARM A, the boxed-aggregates design's §4.4
# "proven arm" (docs/boxed-aggregates-design-s455.md).
#
# WHAT THE ARM IS.  `for my $v (@a)` binds the loop variable as an ALIAS of
# each element, and under raw element storage that PROMOTES every slot to a
# box (%p-elem-cell).  Promotion is MONOTONE, so an array walked once by a
# read-only foreach pays box indirection on every later read of it, forever.
# When VarAnnotator can prove the loop variable is READ ONLY the identity is
# never used, so the loop lowers to p-foreach-raw, which binds the slot AS IT
# STANDS — no promotion, no allocation.  Measured (tools/bench-exec.pl
# `feread`, s459am): 0.71x perl -> 0.47x.
#
# Named Kind-A gate `foreach-raw` (Pl/Passes.pm), so it bisects on its own and
# PCL_OPT=none turns it off.
#
# TWO KINDS OF ROW, answering different questions:
#
#   SHAPE rows assert the arm fires (or does NOT fire) for a given source.
#   These legitimately differ under PCL_OPT=none — the registry's contract.
#
#   BEHAVIOUR rows assert the program's OUTPUT, and every expectation is the
#   live `perl` answer (probed s459am, perl 5.40.3).  They must hold under
#   PCL_OPT=none and under PCL_RAW_ELEMS=0 too: a verdict may never change
#   what a program prints.  They are the real bar, because the hazard of this
#   arm is exactly a loop variable that stops being a box while something
#   still needs to write through it.
#
# THE TRAP THIS FILE EXISTS TO HOLD DOWN (found by the s459am probe battery,
# before the arm shipped): the annotator's REASON list is not the whole write
# story.  A statement-root `$v = RHS`, a root coercing compound `$v *= 2` and
# a root `$v++` are deliberately NOT boxing events — a raw slot stores them
# fine — so they leave no reason at all and are recorded as write FACTS.  For
# a foreach ALIAS every one of them is a write that must reach the container.
# `for my $o (@a) { for my $i (@$o) { $i *= 2 } }` was the catch: the arm
# fired on `$i`, `(p-*= $i 2)` box-set a raw value, and the doubling vanished
# silently.  Rows B1/B2/B8/B11 below are that family.

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

plan tests => 28;

sub write_pl {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;
    return $pl_file;
}

sub emitted { return PCLCore::transpile("$pl2cl " . write_pl($_[0])) }

sub run_cl {
    my ($code) = @_;
    my $cl_code = emitted($code);
    my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cl_fh $cl_code;
    close $cl_fh;
    my $output = `sbcl @sbcl_rt --load $cl_file 2>&1`;
    $output =~ s/^;.*\n//gm;
    $output =~ s/^PCL Runtime loaded\n//gm;
    $output =~ s/^\s*\n//gm;
    return $output;
}

# Each shape lives in its OWN sub, because the verdict is region-wide and
# name-keyed: a write to `$x` anywhere in a region revokes the licence for
# every `$x` loop in it (correct, and the same conservatism every other
# verdict has — but it would make a one-region test file test nothing).

# ══ SHAPE — the arm FIRES on a proven read-only loop ════════════════════════

like(emitted('sub h { my @a=(1,2,3); my $s=0; for my $x (@a) { $s += $x } $s } print h(), "\n";'),
     qr/\(p-foreach-raw \(\$x \@a\)/,
     'control: a read-only foreach over an array takes the arm');

like(emitted('sub h { my $s=0; for my $x (1,2,3) { $s += $x } $s } print h(), "\n";'),
     qr/\(p-foreach-raw \(\$x \(vector 1 2 3\)\)/,
     'a LITERAL list read-only loop takes the arm (#810 read-only half)');

like(emitted('sub h { my %g=(a=>1); my $s=0; for my $x (values %g) { $s += $x } $s } print h(), "\n";'),
     qr/\(p-foreach-raw \(\$x/,
     'values %h read-only takes the arm');

like(emitted('sub h { my @a=(1,2); my $s=0; L: for my $x (@a) { next L if $x==1; $s+=$x } $s } print h(), "\n";'),
     qr/\(p-foreach-raw \(\$x \@a\)\s+:label\s+L\b/s,
     'a LABELLED read-only loop takes the arm and keeps its label');

# ══ SHAPE — the arm is REFUSED ══════════════════════════════════════════════
# Each of these is the control plus exactly one thing.

for my $case (
    ['$x = 5'          => 'a root write to the loop var'],
    ['$x *= 2'         => 'a root COERCING compound (leaves no reason — write FACT only)'],
    ['$x++'            => 'a root ++ (leaves no reason — write FACT only)'],
    ['my $r = \\$x'    => 'a reference to the loop var'],
    ['$x =~ s/1/2/'    => 'a regex target'],
    ['chomp $x'        => 'a mutating builtin argument'],
    ['local $main::z = $x; $x = $main::z' => 'a root write reached through local'],
) {
    my ($stmt, $why) = @$case;
    unlike(emitted("sub h { my \@a=(1,2); for my \$x (\@a) { $stmt } 1 } print h(), \"\\n\";"),
           qr/p-foreach-raw/, "refused: $why");
}

# A PLAIN (non-my) loop variable is never eligible: it is a dynamically-scoped
# global a callee can read AND write, and the package-cell arm would store a
# raw value where the callee's write needs a box.
unlike(emitted('our $x; sub h { my @a=(1,2); my $s=0; for $x (@a) { $s += $x } $s } print h(), "\n";'),
       qr/p-foreach-raw/, 'refused: a plain (non-my) loop variable');

# #189: a KNOWN user sub that writes through @_ aliases its arguments.
unlike(emitted('sub wr { $_[0] = 9 } sub h { my @a=(1,2); for my $x (@a) { wr($x) } "@a" } print h(), "\n";'),
       qr/p-foreach-raw/, 'refused: the loop var is passed to an @_-writing sub');

# ══ BEHAVIOUR — every expectation is the live perl answer ═══════════════════

is(run_cl(<<'PL'), "6|1 2 3\n", 'read-only loop: sum right, array untouched (perl: 6|1 2 3)');
sub h { my @a=(1,2,3); my $s=0; for my $x (@a) { $s += $x } return "$s|@a" }
print h(), "\n";
PL

is(run_cl(<<'PL'), "ARRAY,HASH,SCALAR,CODE,\n", 'a REF in a slot keeps its ref-ness through the raw binding (perl)');
sub h { my @r=([1],{k=>1},\my $z,sub{1}); my $o=""; for my $x (@r) { $o .= ref($x)."," } return $o }
print h(), "\n";
PL

is(run_cl(<<'PL'), "6\n", 'deref through the raw-bound loop var (perl: 6)');
sub h { my @r=([1,2],[3,4]); my $s=0; for my $x (@r) { $s += $x->[1] } return $s }
print h(), "\n";
PL

is(run_cl(<<'PL'), "9|Obj\n", 'a BLESSED object keeps its class and dispatches (perl: 9|Obj)');
package Obj; sub new { bless {v=>$_[1]}, $_[0] } sub v { $_[0]{v} }
package main;
sub h { my @o=(Obj->new(4),Obj->new(5)); my $s=0; for my $x (@o) { $s += $x->v }
        return "$s|".ref($o[0]) }
print h(), "\n";
PL

is(run_cl(<<'PL'), "d1,u,u,d4,\n", 'a HOLE reads as undef through the raw binding (perl)');
sub h { my @a; $a[0]=1; $a[3]=4; my $o="";
        for my $x (@a) { $o .= defined($x) ? "d$x," : "u," } return $o }
print h(), "\n";
PL

is(run_cl(<<'PL'), "d1,u,d3,\n", 'a stored undef reads as undef through the raw binding (perl)');
sub h { my @a=(1,undef,3); my $o="";
        for my $x (@a) { $o .= defined($x) ? "d$x," : "u," } return $o }
print h(), "\n";
PL

is(run_cl(<<'PL'), "10,20,30\n", 'a read-only CLOSURE capture still gets a fresh binding per iteration (perl)');
sub h { my @a=(10,20,30); my @f; for my $x (@a) { push @f, sub { $x } }
        return join(',', map { $_->() } @f) }
print h(), "\n";
PL

# THE FAMILY THE ARM MUST REFUSE — writes that leave no annotator REASON.
is(run_cl(<<'PL'), "2 4 6\n", 'a root COERCING compound through the loop var writes the array (perl: 2 4 6)');
sub h { my @a=(1,2,3); for my $x (@a) { $x *= 2 } return "@a" }
print h(), "\n";
PL

is(run_cl(<<'PL'), "2 3 4\n", 'a root ++ through the loop var writes the array (perl: 2 3 4)');
sub h { my @a=(1,2,3); for my $x (@a) { $x++ } return "@a" }
print h(), "\n";
PL

is(run_cl(<<'PL'), "11 12\n", 'a root = through the loop var writes the array (perl: 11 12)');
sub h { my @a=(1,2); for my $x (@a) { $x = $x + 10 } return "@a" }
print h(), "\n";
PL

is(run_cl(<<'PL'), "20\n", 'nested: outer read-only, INNER writes — the inner writes reach (perl: 20)');
sub h { my @a=([1,2],[3,4]); my $s=0;
        for my $o (@a) { for my $i (@$o) { $i *= 2 } }
        for my $o (@a) { $s += $o->[0] + $o->[1] } return $s }
print h(), "\n";
PL

is(run_cl(<<'PL'), "W W\n", 'an @_-writing callee still writes through the loop var (perl: W W)');
sub wr { $_[0] = "W" }
sub h { my @a=("p","q"); for my $x (@a) { wr($x) } return "@a" }
print h(), "\n";
PL

is(run_cl(<<'PL'), "9 2\n", 'a ref taken to the loop var still aliases the element (perl: 9 2)');
sub h { my @a=(1,2); my @r; for my $x (@a) { push @r, \$x } ${$r[0]} = 9; return "@a" }
print h(), "\n";
PL

# A read-only loop must not stop a LATER alias of the same array from working:
# the arm skips promotion, it does not forbid it.
is(run_cl(<<'PL'), "99 2|3\n", 'promotion still available after a read-only walk (perl: 99 2|3)');
sub h { my @a=(1,2); my $s=0; for my $x (@a) { $s += $x } my $r=\$a[0]; $$r=99;
        return "@a|$s" }
print h(), "\n";
PL

# A write in ONE loop must revoke the licence for that loop only by REGION —
# both loops here share the region, so both stay boxed; the point of the row
# is that the WRITING one is correct, which is what a per-loop licence could
# get wrong.
is(run_cl(<<'PL'), "3|105 106\n", 'two loops of the same name in one region: the writer still writes (perl)');
sub h { my @a=(1,2); my @b=(5,6); my $s=0;
        for my $x (@a) { $s += $x } for my $x (@b) { $x += 100 }
        return "$s|@b" }
print h(), "\n";
PL
