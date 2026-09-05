#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# overload-01.t — use overload operator overloading

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

plan tests => 35;

sub run_cl {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;
    my $cl_code = `$pl2cl $pl_file 2>/dev/null`;
    my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cl_fh $cl_code;
    close $cl_fh;
    my $out = `sbcl @sbcl_rt --load $cl_file 2>&1`;
    $out =~ s/^;.*\n//gm;
    $out =~ s/^PCL Runtime loaded\n//gm;
    $out =~ s/^\s*\n//gm;
    return $out;
}

sub test_cl {
    my ($name, $code, $expected) = @_;
    my $got = run_cl($code);
    is($got, $expected, $name);
}


# ── Stringify overload '""' ───────────────────────────────────────────────

test_cl('stringify overload ""',
    'package MyStr;
     use overload q("") => \&str;
     sub new { bless { s => $_[1] }, $_[0] }
     sub str { "<<" . $_[0]->{s} . ">>" }
     package main;
     my $o = MyStr->new("hello");
     print $o, "\n";',
    "<<hello>>\n");

# ── Single-line use overload (regression: was parsing OK) ────────────────

test_cl('single-line use overload',
    'package MyS;
     use overload q("") => sub { "X" };
     sub new { bless {}, $_[0] }
     package main;
     my $o = MyS->new;
     print $o, "\n";',
    "X\n");

# ── Multi-line use overload (was crashing with "=> is unbound") ──────────

test_cl('multi-line use overload',
    'package MyNum;
     use overload
       "+"  => \&add,
       "0+" => \&numify,
       q("") => \&stringify,
       "<=>" => \&spaceship;
     sub new { bless { val => $_[1] }, $_[0] }
     sub add { MyNum->new($_[0]->{val} + $_[1]->{val}) }
     sub numify { $_[0]->{val} }
     sub stringify { "MyNum(" . $_[0]->{val} . ")" }
     sub spaceship { $_[0]->{val} <=> $_[1]->{val} }
     package main;
     my $a = MyNum->new(3);
     my $b = MyNum->new(5);
     print $a + $b, "\n";',
    "MyNum(8)\n");

# ── Numify overload '0+' ─────────────────────────────────────────────────

test_cl('numify overload 0+',
    'package MyN;
     use overload "0+" => \&num, q("") => \&str;
     sub new { bless { n => $_[1] }, $_[0] }
     sub num { $_[0]->{n} * 2 }
     sub str { "MyN=" . $_[0]->{n} }
     package main;
     my $o = MyN->new(7);
     print $o + 1, "\n";',
    "15\n");

# ── Bool overload ─────────────────────────────────────────────────────────

test_cl('bool overload',
    'package MyBool;
     use overload "bool" => \&as_bool, q("") => \&str;
     sub new { bless { v => $_[1] }, $_[0] }
     sub as_bool { $_[0]->{v} > 0 }
     sub str { "MB(" . $_[0]->{v} . ")" }
     package main;
     my $t = MyBool->new(5);
     my $f = MyBool->new(-1);
     print $t ? "yes" : "no", "\n";
     print $f ? "yes" : "no", "\n";',
    "yes\nno\n");

# ── Arithmetic operators ──────────────────────────────────────────────────

test_cl('subtraction overload -',
    'package MyInt;
     use overload "-" => \&sub_, q("") => \&str;
     sub new { bless { v => $_[1] }, $_[0] }
     sub sub_ {
       my ($a, $b, $r) = @_;
       $r ? MyInt->new($b - $a->{v}) : MyInt->new($a->{v} - $b)
     }
     sub str { $_[0]->{v} }
     package main;
     my $a = MyInt->new(10);
     print $a - 3, "\n";',
    "7\n");

test_cl('multiplication overload *',
    'package MyMul;
     use overload "*" => sub { MyMul->new($_[0]->{v} * $_[1]) }, q("") => sub { $_[0]->{v} };
     sub new { bless { v => $_[1] }, $_[0] }
     package main;
     my $o = MyMul->new(6);
     print $o * 7, "\n";',
    "42\n");

test_cl('negation overload neg',
    'package MyNeg;
     use overload "neg" => sub { MyNeg->new(-$_[0]->{v}) }, q("") => sub { $_[0]->{v} };
     sub new { bless { v => $_[1] }, $_[0] }
     package main;
     my $o = MyNeg->new(5);
     print -$o, "\n";',
    "-5\n");

# ── Comparison overload ───────────────────────────────────────────────────

test_cl('spaceship overload <=>',
    'package MyCmp;
     use overload "<=>" => \&cmp_, q("") => sub { $_[0]->{v} };
     sub new { bless { v => $_[1] }, $_[0] }
     sub cmp_ { $_[2] ? $_[1] <=> $_[0]->{v} : $_[0]->{v} <=> $_[1] }
     package main;
     my @objs = map { MyCmp->new($_) } (3, 1, 4, 1, 5, 9);
     my @sorted = sort { $a <=> $b } @objs;
     print join(" ", @sorted), "\n";',
    "1 1 3 4 5 9\n");

test_cl('numeric eq overload ==',
    'package MyEq;
     use overload "0+" => sub { $_[0]->{v} }, q("") => sub { $_[0]->{v} };
     sub new { bless { v => $_[1] }, $_[0] }
     package main;
     my $a = MyEq->new(5);
     print(($a == 5 ? "eq" : "ne"), "\n");
     print(($a == 6 ? "eq" : "ne"), "\n");',
    "eq\nne\n");

# ── String comparison overload ────────────────────────────────────────────

test_cl('cmp overload for string sort',
    'package MyWord;
     use overload "cmp" => \&mycmp, q("") => \&str;
     sub new { bless { w => $_[1] }, $_[0] }
     sub str { $_[0]->{w} }
     sub mycmp {
       my ($self, $other, $rev) = @_;
       my $sw = $self->{w};
       my $ow = ref($other) eq "MyWord" ? $other->{w} : $other;
       $rev ? $ow cmp $sw : $sw cmp $ow
     }
     package main;
     my @objs = map { MyWord->new($_) } qw(banana apple cherry);
     my @sorted = sort { $a cmp $b } @objs;
     print join(" ", @sorted), "\n";',
    "apple banana cherry\n");

# ── Concatenation overload '.' ────────────────────────────────────────────

test_cl('concatenation overload .',
    'package MyCat;
     use overload "." => \&concat, q("") => sub { $_[0]->{s} };
     sub new { bless { s => $_[1] }, $_[0] }
     sub concat {
       my ($a, $b, $r) = @_;
       $r ? MyCat->new($b . $a->{s}) : MyCat->new($a->{s} . $b)
     }
     package main;
     my $o = MyCat->new("world");
     print "hello " . $o, "\n";',
    "hello world\n");

# ── Fallback via 0+ / <=> ─────────────────────────────────────────────────

test_cl('fallback numeric comparison via 0+',
    'package MyFB;
     use overload "0+" => sub { $_[0]->{n} }, q("") => sub { $_[0]->{n} };
     sub new { bless { n => $_[1] }, $_[0] }
     package main;
     my $a = MyFB->new(3);
     my $b = MyFB->new(7);
     print(($a < $b ? "less" : "not less"), "\n");
     print(($b > $a ? "more" : "not more"), "\n");',
    "less\nmore\n");

# ── overload::StrVal bypasses stringify ───────────────────────────────────

test_cl('overload::StrVal bypasses stringify overload',
    'package MyV;
     use overload q("") => sub { "OVERLOADED" };
     sub new { bless {}, $_[0] }
     package main;
     my $o = MyV->new;
     my $s = overload::StrVal($o);
     print(($s =~ /^MyV=HASH/ ? "ok" : "bad: $s"), "\n");',
    "ok\n");

# ── overload::Overloaded query ────────────────────────────────────────────

test_cl('overload::Overloaded returns true for overloaded object',
    'package MyOL;
     use overload q("") => sub { "X" };
     sub new { bless {}, $_[0] }
     package main;
     my $o = MyOL->new;
     print(overload::Overloaded($o) ? "yes" : "no", "\n");
     my $plain = bless {}, "PlainClass";
     print(overload::Overloaded($plain) ? "yes" : "no", "\n");',
    "yes\nno\n");

# ── Inheritance: subclass inherits parent overloads ───────────────────────

test_cl('subclass inherits stringify overload',
    'package Base;
     use overload q("") => sub { "Base:" . $_[0]->{x} };
     sub new { bless { x => $_[1] }, $_[0] }
     package Child;
     our @ISA = ("Base");
     sub new { bless { x => $_[1] }, $_[0] }
     package main;
     my $c = Child->new(42);
     print $c, "\n";',
    "Base:42\n");

# ── Anonymous sub handler (inline) ───────────────────────────────────────

test_cl('anonymous sub as overload handler',
    'package MyAnon;
     use overload q("") => sub { "anon:" . $_[0]->{v} };
     sub new { bless { v => $_[1] }, $_[0] }
     package main;
     my $o = MyAnon->new(99);
     print $o, "\n";',
    "anon:99\n");

# ── String eq overload ────────────────────────────────────────────────────

test_cl('string eq via cmp overload',
    'package MyStr2;
     use overload "cmp" => sub { $_[0]->{s} cmp $_[1] }, q("") => sub { $_[0]->{s} };
     sub new { bless { s => $_[1] }, $_[0] }
     package main;
     my $a = MyStr2->new("foo");
     print(($a eq "foo" ? "eq" : "ne"), "\n");
     print(($a eq "bar" ? "eq" : "ne"), "\n");',
    "eq\nne\n");

# ── Division overload / ───────────────────────────────────────────────────

test_cl('division overload /',
    'package MyDiv;
     use overload "/" => sub { $_[2] ? $_[1] / $_[0]->{v} : $_[0]->{v} / $_[1] },
                  q("") => sub { $_[0]->{v} };
     sub new { bless { v => $_[1] }, $_[0] }
     package main;
     my $o = MyDiv->new(10);
     print $o / 2, "\n";',
    "5\n");

# ── "$obj" interpolation must STRINGIFY (fire the "" overload) ─────────────
# Regression: single-variable interpolation "$x" used to return the bare
# variable, dropping the stringify coercion.  For an overloaded object, $x="$x"
# then left $x an object, so a later overloaded cmp/<=> re-dispatched forever
# (BINDING-STACK-EXHAUSTED).  This mirrors the version.pm vcmp idiom.
test_cl('"$obj" interpolation fires "" overload (version-style cmp, no recursion)',
    'package Ver;
     use overload q("") => \&str, "cmp" => \&vcmp, "<=>" => \&vcmp, fallback => 1;
     sub new { bless { s => $_[1] }, $_[0] }
     sub str { $_[0]->{s} }
     sub vcmp { my ($a,$b,$sw)=@_; $a="$a"; $b=ref($b)?"$b":$b; ($a,$b)=($b,$a) if $sw; return $a cmp $b; }
     package main;
     my $a = Ver->new("1.2.3");
     my $b = Ver->new("2.0.0");
     print(($a <=> $b), "\n");
     print(($a < $b ? "less" : "ge"), "\n");',
    "-1\nless\n");

# A reference interpolated as a single part stringifies to ARRAY(0x..), not the
# bare ref.
test_cl('"$ref" interpolation stringifies a reference',
    'my $r = [1, 2];
     my $s = "$r";
     print(($s =~ /^ARRAY\(0x[0-9a-f]+\)$/ ? "ok" : "bad:$s"), "\n");',
    "ok\n");

# ── #119: s///, tr/// match against the OVERLOADED string, not the raw ─────
# print form.  do-regex-subst / do-tr used (to-string (unbox box)), which
# skipped box-sv's "" dispatch, so an overloaded object matched against
# "HASH(0x...)" and every substitution silently failed.
test_cl('s/// and tr/// on an overloaded object use the "" overload (#119)',
    'package Str;
     use overload q("") => sub { "hello world" }, fallback => 1;
     package main;
     my $o = bless {}, "Str";
     print(($o =~ s/world/perl/r), "\n");
     my $t = bless {}, "Str";
     print(($t =~ tr/lo/LO/r), "\n");
     my $u = bless {}, "Str";
     print(($u =~ tr/l//), "\n");',
    "hello perl\nheLLO wOrLd\n3\n");

# ── #402: interpolation concat consults the "." overload ([perl #124160]) ──
# perl spells "a $o b" as chained '.', so a '.' handler participates and the
# result need not be a string; a SINGLE "$o" piece is stringification only.
test_cl('interpolation dispatches "." overload; one piece stringifies (#402)',
    'package Keep;
     use overload "." => sub { $_[0] }, fallback => 1;
     package main;
     my $k = bless [], "Keep";
     my $cat = "a $k b";
     print(ref($cat), "\n");
     package Dot;
     use overload "." => sub { my ($s, $x, $r) = @_; $r ? "$x<D" : "D>$x" },
                  q("") => sub { "DSTR" }, fallback => 1;
     package main;
     my $d = bless [], "Dot";
     print("$d", "\n");
     print("a $d b", "\n");',
    "Keep\nDSTR\na <D b\n");

# ── #972 part 2: the BITWISE family dispatches like every other operator ──
# `p-bit-and/or/xor', `p-<<'/`p->>' and the string twins used to coerce with
# to-number / p-string-bit-op and never ask, so `bless([],"Baz") | "x"' gave
# the object's ADDRESS (perl-tests/bop.t 464).  perl overloads all ten keys;
# every expectation below is perl 5.40.3's own answer.
test_cl('bitwise & | ^ << >> ~ dispatch use overload (#972)',
    'package Baz;
     use overload
       q/|/  => sub { "OR" },  q/&/ => sub { "AND" }, q/^/ => sub { "XOR" },
       q/<</ => sub { "SHL" }, q(>>) => sub { "SHR" }, q/~/ => sub { "NOT" },
       q("") => sub { "BAZ" }, q(0+) => sub { 42 };
     sub new { bless [], shift }
     package main;
     my $o = Baz->new;
     print $o | "x", "\n";
     print "x" | $o, "\n";
     print $o & "x", "\n";
     print $o ^ "x", "\n";
     print $o << 2, "\n";
     print $o >> 2, "\n";
     print ~$o, "\n";
     my $c = $o; $c |= "z"; print $c, "\n";
     my $d = "z"; $d |= $o; print $d, "\n";',
    "OR\nOR\nAND\nXOR\nSHL\nSHR\nNOT\nOR\nOR\n");

# The SWAPPED flag reaches a bitwise handler like any other binary op, and
# perl's third argument is the EMPTY STRING (defined) when NOT swapped —
# `undef' is perl's third state and means "called as a mutator", which PCL
# never produces.  PCL passed undef for both.
test_cl('overload handler third argument is "" / 1, both DEFINED (#972)',
    'package Sw;
     use overload q/|/ => sub { "sw=" . (defined $_[2] ? ($_[2] eq "" ? "E" : $_[2]) : "U") },
                  q/+/ => sub { "sw=" . (defined $_[2] ? ($_[2] eq "" ? "E" : $_[2]) : "U") };
     sub new { bless [], shift }
     package main;
     my $o = Sw->new;
     print $o | 1, "\n";
     print 1 | $o, "\n";
     print $o + 1, "\n";
     print 1 + $o, "\n";',
    "sw=E\nsw=1\nsw=E\nsw=1\n");

# `<<=' / `>>=' used to spell a SECOND copy of the shift with no clamp, so
# `$z <<= 70' answered 5902958103587056517120 where perl (and `5 << 70')
# answer 0.  They delegate to p-<< / p->> now — one reading.
test_cl('<<= and >>= clamp like << and >> (#972)',
    'my $z = 5; $z <<= 70; print $z, "\n";
     my $y = 5; $y >>= 70; print $y, "\n";
     my $w = 5; $w <<= 3;  print $w, "\n";
     my $v = 40; $v >>= 3; print $v, "\n";',
    "0\n0\n40\n5\n");

# ── #972 part 1: `nomethod' ────────────────────────────────────────────────
# perl consults it at exactly the point where no handler and no autogeneration
# apply, with a FOURTH argument naming the operator.  PCL ignored the key.
test_cl('nomethod is consulted with the operator as a fourth argument (#972)',
    'package NM;
     use overload q(0+) => sub { 7 }, q("") => sub { "NM" },
       nomethod => sub { "N[" . $_[3] . "]" };
     sub new { bless [], shift }
     package main;
     my $x = NM->new;
     print $x + 2, "\n";
     print $x - 2, "\n";
     print $x | 2, "\n";
     print $x <=> 2, "\n";
     print $x == 2, "\n";
     print $x eq "q", "\n";
     print -$x, "\n";
     print ~$x, "\n";
     print 2 - $x, "\n";',
    "N[+]\nN[-]\nN[|]\nN[<=>]\nN[==]\nN[eq]\nN[neg]\nN[~]\nN[-]\n");

# NEGATIVES for the same class: `.' and `x' are AUTOGENERATED from `""', so
# nomethod must NOT intercept them, and `bool' / `""' / `int' come from the
# conversions.  A refusal that read the key would also have to stop killing
# `$x++' — #934's death fired there and perl merely calls nomethod and leaves
# the object alone (its return value is DISCARDED for ++/--).
test_cl('nomethod is NOT reached where perl autogenerates (#972)',
    'package NM2;
     use overload q(0+) => sub { 7 }, q("") => sub { "NM" },
       nomethod => sub { "N[" . $_[3] . "]" };
     # bless {} deliberately: `$aryref x 2` is list repetition in PCL today
     # and loses the reference, blessed or not — a pre-existing bug of its own
     # (filed), and this row is about nomethod, not about that.
     sub new { bless {}, shift }
     package main;
     my $x = NM2->new;
     print $x . "z", "\n";
     print $x x 2, "\n";
     print(($x ? "T" : "F"), "\n");
     print "$x", "\n";
     print int($x), "\n";
     my $y = NM2->new; $y++;
     print ref($y), " $y\n";',
    "NMz\nNMNM\nT\nNM\n7\nNM2 NM\n");

# A class whose ONLY key is nomethod gets it for the conversions too.
test_cl('nomethod answers the conversion operators (#972)',
    'package NMO;
     use overload nomethod => sub { "N[" . $_[3] . "]" };
     sub new { bless [], shift }
     package main;
     my $x = NMO->new;
     print "$x", "\n";
     print $x + 2, "\n";',
    "N[\"\"]\nN[+]\n");

# ── #972 part 3: the CONVERSION derivations ───────────────────────────────
# perl derives each of `""' / `0+' / `bool' from the other two, in a fixed
# order (Perl_amagic_call): "" <- 0+ <- bool, 0+ <- "" <- bool,
# bool <- 0+ <- "".  PCL asked p-find-overload for the key and nothing else,
# so a `0+'-only class stringified to N0=HASH(0x1).
test_cl('"" is derived from 0+, then from bool (#972)',
    'package N0; use overload q(0+) => sub { 42 }; sub new { bless {}, shift }
     package B0; use overload q(bool) => sub { 1 }; sub new { bless {}, shift }
     package NB; use overload q(0+) => sub { 0 }, q(bool) => sub { 1 };
     sub new { bless {}, shift }
     package main;
     my $n = N0->new;
     print "$n\n";
     print $n . "z", "\n";
     print $n x 2, "\n";
     print length($n), "\n";
     print lc($n), "\n";
     printf("%s|%d\n", $n, $n);
     print "", B0->new, "\n";
     print "", NB->new, "\n";',
    "42\n42z\n4242\n2\n42\n42|42\n1\n0\n");

test_cl('0+ is derived from "", then from bool (#972)',
    'package S1; use overload q("") => sub { "17abc" }, fallback => 1;
     sub new { bless {}, shift }
     package B1; use overload q(bool) => sub { 1 }, fallback => 1;
     sub new { bless {}, shift }
     package main;
     no warnings;
     print int(S1->new), "\n";
     print S1->new + 0, "\n";
     print B1->new + 0, "\n";',
    "17\n17\n1\n");

# The scary half, and it IS perl: an object is no longer unconditionally TRUE.
# `0+' wins over `""' here (a class with 0+ => 0 and "" => "yes" is FALSE).
test_cl('bool is derived from 0+, then from "" (#972)',
    'package SF; use overload q("") => sub { "0" },   fallback => 1; sub new { bless {}, shift }
     package NF; use overload q(0+) => sub { 0 },     fallback => 1; sub new { bless {}, shift }
     package NS; use overload q(0+) => sub { 0 }, q("") => sub { "yes" };
     sub new { bless {}, shift }
     package OB; use overload q(bool) => sub { 1 }, q(0+) => sub { 0 };
     sub new { bless {}, shift }
     package main;
     print((SF->new ? "T" : "F"), "\n");
     print((NF->new ? "T" : "F"), "\n");
     print((NS->new ? "T" : "F"), "\n");
     print((OB->new ? "T" : "F"), "\n");
     print(((bless {}, "Plain") ? "T" : "F"), "\n");',
    "F\nF\nF\nT\nT\n");

# `fallback => 0' forbids the derivation outright (perl dies there; PCL keeps
# its ordinary answer until the binary refusal lands with it — #960(a)), and a
# class that overloads NOTHING relevant keeps the address form.
test_cl('fallback => 0 forbids the conversion derivation (#972)',
    'package N2; use overload q(0+) => sub { 42 }, fallback => 0;
     sub new { bless {}, shift }
     package OP; use overload q(+) => sub { "P" }, fallback => 1;
     sub new { bless {}, shift }
     package main;
     print((("" . N2->new) =~ /^N2=HASH/ ? "ADDR" : "DERIVED"), "\n");
     print(((""  . OP->new) =~ /^OP=HASH/ ? "ADDR" : "DERIVED"), "\n");',
    "ADDR\nADDR\n");

# A comparison handler's RETURN VALUE is the operator's value — perl does not
# truthify it.  PCL wrapped every arm in p-true-p + p-bool and handed back 1.
# The DERIVED arm still computes a boolean, because there perl is the one
# comparing `<=>' / `cmp''s -1/0/1 against 0.
test_cl('a comparison handler returns its own value, not 1 (#972)',
    'package OE;
     use overload q(eq) => sub { "XYZ" }, q(==) => sub { "PQR" },
                  q(lt) => sub { "LLL" }, q(<)  => sub { "SSS" },
                  q("") => sub { "OE" },  q(0+) => sub { 3 };
     sub new { bless {}, shift }
     package VC;
     use overload q(cmp) => sub { 0 }, q(<=>) => sub { 0 },
                  q("") => sub { "VC" }, q(0+) => sub { 3 };
     sub new { bless {}, shift }
     package main;
     my $x = OE->new;
     print $x eq "q", "\n";
     print $x == 2, "\n";
     print $x lt "q", "\n";
     print $x < 2, "\n";
     my $y = VC->new;
     print $y eq "q", "\n";
     print(($y ne "q" ? "T" : "F"), "\n");
     print $y == 2, "\n";',
    "XYZ\nPQR\nLLL\nSSS\n1\nF\n1\n");

# ── #1021: sort's DEFAULT comparator IS the `cmp` operator ────────────────
# perldoc -f sort: "sorts in standard string comparison order".  p-sort's
# no-comparator arm compared the STRINGIFICATIONS instead, so a class whose
# `cmp` disagrees with its `""` sorted the wrong way round — and silently,
# because a `cmp` that AGREES comes out right by accident.  The explicit
# `{ $a cmp $b }` block was right all along, and the fix routes the default
# through the SAME function (p-str-cmp), so the two cannot give two answers.
#
# The rows that must NOT move are the point of the second half: plain strings
# and numbers keep the raw string compare (the overload question is asked
# ONCE per sort, not per comparison), a class with only `""` sorts by its
# string, and a handler given as a METHOD NAME is called like any other.
# Every expectation is the live perl 5.40.3 answer, call counts included.
test_cl('sort default comparator uses an overloaded cmp (#1021)',
    'package OV;
     use overload q(cmp) => sub { my ($s,$o,$sw)=@_;
                                  my $r = $o->{val} <=> $s->{val};
                                  $sw ? -$r : $r },
                  q("")  => sub { $_[0]{val} };
     sub new { bless { val => $_[1] }, $_[0] }
     package SO;
     use overload q("") => sub { "s" . $_[0]{v} }, fallback => 1;
     sub new { bless { v => $_[1] }, $_[0] }
     package MN;
     use overload q(cmp) => "mycmp", q("") => sub { $_[0]{v} };
     sub new { bless { v => $_[1] }, $_[0] }
     sub mycmp { my ($s,$o,$sw)=@_; my $r = ("" . $o) cmp ("" . $s);
                 $sw ? -$r : $r }
     package CT;
     use overload q(cmp) => sub { $main::nc++; my ($s,$o,$sw)=@_;
                                  my $r = $s->{v} cmp $o->{v}; $sw ? -$r : $r },
                  q("")  => sub { $main::ns++; $_[0]{v} };
     sub new { bless { v => $_[1] }, $_[0] }
     package main;
     our ($nc, $ns) = (0, 0);
     sub p { my ($l, @v) = @_; print $l, join(",", @v), "\n" }
     my @o = map { OV->new($_) } (1, 2, 3);
     p("default:  ", sort @o);
     p("explicit: ", sort { $a cmp $b } @o);
     my @s = map { SO->new($_) } (3, 1, 2);
     p("onlystr:  ", sort @s);
     my @m = map { MN->new($_) } ("a", "b", "c");
     p("methname: ", sort @m);
     p("plain:    ", sort qw(pear Apple banana));
     p("nums:     ", sort (10, 9, 100, 1));
     my @mixed = ("b", OV->new(5), "a");
     p("mixed:    ", sort @mixed);
     my @c = (CT->new("b"), CT->new("a"));
     my @sorted = sort @c;
     print "calls:    cmp=$nc str=$ns\n";',
    "default:  3,2,1\nexplicit: 3,2,1\nonlystr:  s1,s2,s3\n"
  . "methname: c,b,a\nplain:    Apple,banana,pear\nnums:     1,10,100,9\n"
  . "mixed:    5,a,b\ncalls:    cmp=1 str=0\n");
