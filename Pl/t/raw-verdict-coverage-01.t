#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# raw-verdict-coverage-01.t — tasks #758 / #759 / #760 / #761, the four
# VERDICT-COVERAGE narrowings of the s453 box-elision review
# (docs/faster-codegen-suggestions.md §13).  None of them is a new fast shape:
# each widens WHERE the already-shipped raw machinery (raw let slots,
# p-foreach-range-raw, p-incf-raw) may fire.  Each is a named Kind-A gate in
# Pl/Passes.pm, so a coverage widening can be bisected on its own.
#
#   #758 raw-block-eval       `eval {}` no longer boxes its whole region
#   #759 raw-op-family        an arith OPERATOR proves its result family
#   #760 raw-closure-capture  CAPTURE alone is not a boxing event
#   #761 raw-topic            `for (A..B)` may bind $_ to the RAW counter
#
# The file has two kinds of row and they answer different questions:
#
#   SHAPE rows assert that the raw path fires (or does NOT fire) for a given
#   source.  These are the ones that legitimately differ under PCL_OPT=none —
#   that is the registry's own contract (CLAUDE.md, Pl/t/passes-01.t).
#
#   BEHAVIOUR rows assert the program's OUTPUT.  Every expectation below is
#   the live `perl` answer, probed s456af, and every one of them must hold
#   under PCL_OPT=none as well: a verdict may never change what a program
#   prints.  They are the real bar, because the whole hazard of these four
#   changes is a value that stops being a box while something still needs one.
#
# NOT asserted here, deliberately: `for ("cat","dog") { s/o/0/ }` dies in perl
# ("Modification of a read-only value") and substitutes in PCL.  That is a
# LIST-foreach aliasing divergence, identical under PCL_OPT=none, so it is
# nothing to do with #761's range loops — filed as task #810 with its
# reproducer rather than written into an expectation here.

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

sub emitted { return PCLCore::transpile("$pl2cl " . write_pl($_[0])) }

# Transpile (a DROPPED statement fails the row, via PCLCore) and run.
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

# The control from §13: a sub whose accumulator loop emits FULLY raw.  Every
# "does the veto still fire" row below is this shape plus one thing.
my $CTL = 'sub hot { my $s = 0; for my $i (1..1000) { $s += $i } return $s }';

# ══ #758 — a BLOCK eval is not a boxing event ═══════════════════════════════

like(emitted("$CTL\nprint hot(), \"\\n\";"),
     qr/\(p-foreach-range-raw \(\$i 1 1000\).*\(p-incf-raw \$s \$i\)/s,
     '#758 control: the accumulator loop is fully raw to begin with');

like(emitted('sub hot { my $s = 0; my $r = eval { 1 }; for my $i (1..9) { $s += $i } $s }'),
     qr/\(p-foreach-range-raw \(\$i 1 9\).*\(p-incf-raw \$s \$i\)/s,
     '#758 a BLOCK eval in the region no longer boxes the accumulator');

# The capture ALIST is a string-eval mechanism, so a STRING eval must keep
# boxing — including one NESTED inside an eval block, which find() reaches.
unlike(emitted('sub hot { my $s = 0; my $v = eval "1"; for my $i (1..9) { $s += $i } $s }'),
       qr/p-foreach-range-raw/,
       '#758 a STRING eval still boxes the region');

unlike(emitted('sub hot { my $s = 0; eval { eval "\$s = 1" }; for my $i (1..9) { $s += $i } $s }'),
       qr/p-foreach-range-raw/,
       '#758 a STRING eval NESTED in a block eval still boxes the region');

# perl keeps a `my` written before a die inside eval {}; so must a raw slot.
is(run_cl(<<'PL'), "5|boom\n", '#758 a die inside eval {} leaves the slot written (perl: 5|boom)');
sub hot { my $s = 0; eval { $s = 5; die "boom\n"; $s = 99 }; my $e = $@; chomp $e; return "$s|$e" }
print hot(), "\n";
PL

is(run_cl(<<'PL'), "42\n", '#758 a string eval inside an eval block still writes through');
sub hot { my $s = 7; eval { eval "\$s = 42" }; return $s }
print hot(), "\n";
PL

# ══ #759 — the arith OPERATOR proves the family, not its operands ════════════

# The three spellings of one sum that used to give three verdicts.
like(emitted('sub hot { my $s = 0; for my $i (1..9) { $s = $s + $i } $s }'),
     qr/p-foreach-range-raw/, '#759 `$s = $s + $i` is raw (was raw before too)');

like(emitted('sub hot { my $s = 0; for (1..9) { $s = $s + $_ } $s }'),
     qr/\(setf \$s \(p-\+ \$s \$_\)\)/,
     '#759 `$s = $s + $_` is a RAW slot write — a Magic operand no longer vetoes');

like(emitted('sub hot { my @a=(1,2); my $s = 0; while (@a) { $s = $s + shift(@a) } $s }'),
     qr/\(setf \$s \(p-\+ \$s/,
     '#759 an unknown-sub operand under an arith root no longer vetoes');

# The str/num split is what guards perl's MAGICAL string increment, so it must
# survive: a `.`-family write plus a root ++ still boxes.
is(run_cl(<<'PL'), "ab\n", '#759 a str-family write keeps the root-++ veto (perl: ab)');
sub hot { my $s = "aa"; $s = $s . ""; $s++; return $s }
print hot(), "\n";
PL

# An operand may hold an overloaded object; the handler's return must survive
# in the slot, class and all.  This was already true for `$a + $b` — the point
# is that widening to `$a + f()` adds no new class.
is(run_cl(<<'PL'), "V(6)/V\n", '#759 an overloaded `+` keeps its object in the raw slot');
package V;
use overload q{+} => sub { V->new($_[0]{v} + (ref $_[1] ? $_[1]{v} : $_[1])) },
             q{""} => sub { "V(" . $_[0]{v} . ")" };
sub new { bless { v => $_[1] }, $_[0] }
package main;
sub hot { my $x = V->new(5); my $s = $x + 1; return "$s/" . ref($s) }
print hot(), "\n";
PL

# A bare `$y` RHS has no operator, so it keeps the old answer: it may alias $y's
# box, and that is exactly what the operand walk was written for.
is(run_cl(<<'PL'), "5/9\n", '#759 a bare `$y` RHS is still an unproven shape (no aliasing)');
sub hot { my $y = 5; my $s = $y; $y = 9; return "$s/$y" }
print hot(), "\n";
PL

# ══ #760 — CAPTURE is not a boxing event; the EVENT in the closure is ════════

like(emitted('sub hot { my $s = 0; my $c = sub { $s + 1 }; for my $i (1..9) { $s += $i } $s . $c->() }'),
     qr/\(let \(\(\$s 0\)\)/,
     '#760 a READ-ONLY closure capture leaves the slot raw');

unlike(emitted('sub hot { my $s = 0; my $c = sub { $s = 42 }; for my $i (1..9) { $s += $i } $s }'),
       qr/\(let \(\(\$s 0\)\)/,
       '#760 a closure that WRITES the name still boxes it');

unlike(emitted('sub hot { my $s = 0; my $c = sub { my $r = \$s; $$r }; for my $i (1..9) { $s += $i } $s }'),
       qr/\(let \(\(\$s 0\)\)/,
       '#760 a closure that takes a REF to the name still boxes it');

# THE acceptance probe named by task #760 care item (b): a closure made inside
# a RAW range loop must capture a FRESH binding per iteration, not one slot.
is(run_cl(<<'PL'), "10,20,30\n", '#760 fresh binding per iteration survives the raw loop var');
my @f;
for my $i (1..3) { push @f, sub { $i * 10 } }
print join(",", map { $_->() } @f), "\n";
PL

# A CL closure captures the BINDING, so a `setf` from outside is visible.
is(run_cl(<<'PL'), "2/3\n", '#760 a raw slot mutated outside is seen inside the closure');
sub hot { my $s = 1; my $c = sub { $s }; $s = 2; my $x = $c->(); $s = 3; return "$x/" . $c->() }
print hot(), "\n";
PL

# The closure outlives its `let` scope.
is(run_cl(<<'PL'), "7/8\n", '#760 a raw slot captured by an ESCAPING closure stays alive');
sub mk { my $n = shift; return sub { $n } }
print mk(7)->(), "/", mk(8)->(), "\n";
PL

# ══ #761 — a topic range loop may bind $_ to the RAW counter ═════════════════

like(emitted('my $s = 0; for (1..10) { $s += $_ } print "$s\n";'),
     qr/\(p-foreach-range-raw \(\$_ 1 10\)/,
     '#761 an arithmetic topic loop binds $_ raw');

# Everything that can write through $_'s box, alias it, or hand it to code the
# compiler cannot see must keep the boxed binding.  One row per family.
for my $case (
    ['a bare s/// (writes $_ with no `=~` to see)',
     'for (1..3) { s/1/2/ } print "x\n";'],
    ['a call to a user sub (which may read the global $_)',
     'sub f { 1 } my $s = 0; for (1..3) { $s += f() } print "$s\n";'],
    ['a CODE-REF call `$c->()` (no Word for a name check to catch)',
     'my $c = sub { 1 }; my $s = 0; for (1..3) { $s += $c->() } print "$s\n";'],
    ['`local $_`', 'our $g = 0; for (1..3) { local $_ = 9; $g += $_ } print "$g\n";'],
) {
    my ($what, $src) = @$case;
    unlike(emitted($src), qr/p-foreach-range-raw \(\$_/,
           "#761 $what keeps \$_ boxed");
}

# A `->` used as a plain DEREF is not a call and stays raw.
like(emitted('my $r = [1,2,3]; my $s = 0; for (0..2) { $s += $r->[$_] } print "$s\n";'),
     qr/\(p-foreach-range-raw \(\$_ 0 2\)/,
     '#761 `->` as a deref (not a call) still allows the raw binding');

# BEHAVIOUR: the dynamic binding is unchanged, so $_ outside the loop survives
# it, and a callee inside a NON-qualifying loop still sees the element.
is(run_cl(<<'PL'), "6/outer\n", '#761 the raw topic binding still restores the outer $_');
$_ = "outer";
my $s = 0;
for (1..3) { $s += $_ }
print "$s/$_\n";
PL

is(run_cl(<<'PL'), "[1][2][3]\n", '#761 a callee reading the global $_ sees the element (loop stays boxed)');
sub peek { return "[$_]" }
my $o = "";
for (1..3) { $o .= peek() }
print "$o\n";
PL

# Nested topic loops: the inner binding shadows and the outer is restored.
is(run_cl(<<'PL'), "18\n", '#761 nested raw topic loops shadow and restore correctly');
my $s = 0;
for (1..3) { for (1..3) { $s += $_ } }
print "$s\n";
PL
