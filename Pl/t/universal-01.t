#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# universal-01.t — THE UNIVERSAL BUILT-INS AS PERL VALUES, not CL booleans.
#
# `can` answers a CODE ref or FALSE, and PCL's false was CL NIL — which is not
# a perl value at all.  Crossing the return protocol in LIST context, NIL
# leaves ZERO values where perl leaves ONE:
#
#     sub r { my $x = shift; return $x->can("nope") }
#     my @a = r($obj);        # perl: 1 element (undef).  PCL: EMPTY.
#
# That is the Safe::Isa `$_can` idiom (`is_deeply [ $o->$_can("bar") ],
# [ undef ]`, safe_isa.t row 45) and the same family as #403's filetest false
# ("NIL is not a perl value").  It was invisible where the call sits in a list
# ITSELF — the emitter wraps a call site's value — and appeared only when the
# answer was a sub's RESULT.
#
# WHERE THE FIX IS NOT: `p-can` keeps answering NIL, because the RUNTIME asks
# it as a CL boolean (the die-PROPAGATE lookup, overload, DESTROY, AUTOLOAD)
# and `*p-undef*` is TRUE in CL.  The conversion belongs at the perl-visible
# boundary: every dispatch arm serving the method NAME `can`, plus
# UNIVERSAL::can — one returner, %p-can-answer, no new state.
#
# HALF THIS FILE IS THE OTHER DIRECTION.  `can` is on the hot path of every
# Moo/Role::Tiny program and its answer is read as a CONDITION everywhere, so
# the rows below check that the new false is still false in boolean, string
# and numeric context, that a HIT is still a callable code ref, and that
# overload resolution (a runtime consumer of the same lookup) is unmoved.
#
# THE ORACLE IS A LIVE PERL RUN for every row.

# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

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

plan skip_all => "pl2cl not found" if !-x $pl2cl;
plan skip_all => "sbcl not found"  if !`which sbcl 2>/dev/null`;

plan tests => 5;

sub write_pl {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    binmode($fh, ':raw');
    print $fh $code;
    close $fh;
    return $pl_file;
}

sub run_cl {
    my ($code) = @_;
    my $cl_code = PCLCore::transpile("$pl2cl " . write_pl($code));
    my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    binmode($cl_fh, ':raw');
    print $cl_fh $cl_code;
    close $cl_fh;
    my $out = `sbcl @sbcl_rt --load $cl_file 2>&1`;
    $out =~ s/^;.*\n//gm;
    $out =~ s/^PCL Runtime loaded\n//gm;
    $out =~ s/^\s*\n//gm;
    return $out;
}

sub run_perl { my $f = write_pl($_[0]); return scalar `perl $f 2>&1` }

# ── #1912: how many VALUES a returned UNIVERSAL answer carries ─────────────
{
    my $prog = <<'PL';
package Foo; sub new { bless {}, shift } sub real { 42 }
package main;
my $o = Foo->new;
sub r1  { my $x = shift; return $x->can(@_) }
sub r2  { my ($x,$m) = (shift,shift); return $x->$m(@_) }
sub r3  { my ($x,$m) = (shift,shift); $x->$m(@_) }
sub r4  { my $x = shift; return $x->isa("Nope") }
sub r5  { my $x = shift; return UNIVERSAL::can($x, "nope") }
sub r6  { my $x = shift; return UNIVERSAL::isa($x, "Nope") }
sub r7  { my $x = shift; return $x->can("real") }
sub r8  { return Foo->can("nope") }
sub r15 { my $x = shift; return $x->DOES("Nope") }
my @a = r1($o, "nope");        print "r1  n=", scalar(@a), " d=", (defined $a[0] ? "def" : "undef"), "\n";
my @b = r2($o, "can", "nope"); print "r2  n=", scalar(@b), " d=", (defined $b[0] ? "def" : "undef"), "\n";
my @c = r3($o, "can", "nope"); print "r3  n=", scalar(@c), " d=", (defined $c[0] ? "def" : "undef"), "\n";
my @d = r4($o);                print "r4  n=", scalar(@d), " d=", (defined $d[0] ? "def" : "undef"), "\n";
my @e = r5($o);                print "r5  n=", scalar(@e), " d=", (defined $e[0] ? "def" : "undef"), "\n";
my @f = r6($o);                print "r6  n=", scalar(@f), " d=", (defined $f[0] ? "def" : "undef"), "\n";
my @g = r7($o);                print "r7  n=", scalar(@g), " d=", (ref $g[0] eq 'CODE' ? "CODE" : "other"), "\n";
my @h = r8();                  print "r8  n=", scalar(@h), " d=", (defined $h[0] ? "def" : "undef"), "\n";
my @p = r15($o);               print "r15 n=", scalar(@p), " d=", (defined $p[0] ? "def" : "undef"), "\n";
my @q = ($o->can("nope"));            print "dq n=", scalar(@q), "\n";
my @s = (UNIVERSAL::can($o, "nope")); print "ds n=", scalar(@s), "\n";
PL
    is(run_cl($prog), run_perl($prog),
       '#1912: a returned `can` miss is ONE undef, not zero values (perl oracle)');
}

# The idiom that found it.
{
    my $prog = <<'PL';
package Foo; sub new { bless {}, shift } sub bar { "BAR" }
package main;
my $o = Foo->new;
my $_can = sub { my $x = shift; return unless ref $x; $x->can(@_) };
my @miss = $o->$_can("nope");
my @hit  = $o->$_can("bar");
print "miss:", scalar(@miss), ":", (defined $miss[0] ? "def" : "undef"), "\n";
print "hit:",  scalar(@hit),  ":", (ref $hit[0]), ":", $hit[0]->($o), "\n";
print "plain:", scalar(my @p = "str"->$_can("bar")), "\n";
PL
    is(run_cl($prog), run_perl($prog),
       '#1912: the Safe::Isa `$_can` idiom — miss, hit and a non-ref (perl oracle)');
}

# ── THE OTHER DIRECTION: the new false must still be FALSE ─────────────────
{
    my $prog = <<'PL';
package Base;  sub new { bless {}, shift } sub hi { "hi" }
package Child; our @ISA = ('Base');       sub bye { "bye" }
package Over;  use overload '""' => sub { "OV" }, '+' => sub { 99 };
               sub new { bless {}, shift }
package main;
my $c = Child->new;
my $o = Over->new;
print "1 ", ($c->can("nope") ? "T" : "F"), ($c->can("hi") ? "T" : "F"),
            (UNIVERSAL::can($c,"nope") ? "T" : "F"), (UNIVERSAL::can($c,"bye") ? "T" : "F"),
            (Child->can("nope") ? "T" : "F"), (Child->can("hi") ? "T" : "F"), "\n";
print "2 ", (defined($c->can("nope")) ? "def" : "undef"), " ",
            (defined($c->can("hi")) ? "def" : "undef"), "\n";
print "3 ", (!$c->can("nope") ? "not" : "yes"), " ", ($c->can("nope") // "dflt"), "\n";
my $m = $c->can("hi"); print "4 ", (ref $m), " ", $m->($c), "\n";
my $m2 = UNIVERSAL::can($c, "bye"); print "5 ", (ref $m2), " ", $m2->($c), "\n";
print "6 ", (defined(Child->can("nope")) ? "def" : "undef"), " ",
            (defined(eval { my $x = {}; $x->can("nope") }) ? "def" : "undef"), " ",
            (defined(UNIVERSAL::can("NoSuchPkg1912", "x")) ? "def" : "undef"), " ",
            (defined(UNIVERSAL::can(undef, "x")) ? "def" : "undef"), "\n";
{ no warnings;
  my $f = $c->can("nope");
  print "7 [", (defined $f ? $f : ""), "] [", ($f ? 1 : 0), "] [", (0 + ($f || 0)), "]\n"; }
print "8 $o ", ($o + 1), " ", ($o->can("nope") ? "T" : "F"), "\n";
print "9 ", ($c->can("hi") ? "inh" : "no"), ($c->can("bye") ? "own" : "no"),
            ($c->can("isa") ? "uni" : "no"), ($c->can("can") ? "can" : "no"), "\n";
my @has = grep { $c->can($_) } qw(hi bye nope isa);
print "10 ", scalar(@has), " @has\n";
my %h = (a => $c->can("nope"), b => $c->can("hi"));
print "11 ", (exists $h{a} ? "k" : "-"), (defined $h{a} ? "d" : "u"), (defined $h{b} ? "d" : "u"), "\n";
my ($x, $y) = ($c->can("nope"), 7);
print "12 ", (defined $x ? "d" : "u"), " $y\n";
PL
    is(run_cl($prog), run_perl($prog),
       '#1912 the other direction: the false is still false in every context, the hit still callable (perl oracle)');
}

# The runtime's OWN consumers read `can` as a CL boolean and must be unmoved:
# die-PROPAGATE, DESTROY and AUTOLOAD are all `can`-shaped lookups.
{
    my $prog = <<'PL';
package Ex; sub new { bless { m => $_[1] }, $_[0] }
            sub PROPAGATE { my ($s,$f,$l) = @_; return Ex->new($s->{m} . "+prop") }
package Plain; sub new { bless {}, shift }
package Auto; our $AUTOLOAD;
              sub new { bless {}, shift }
              sub AUTOLOAD { my $n = $AUTOLOAD; $n =~ s/.*:://; return "auto:$n" if $n ne 'DESTROY'; return }
package main;
eval { eval { die Ex->new("boom") }; die };
print "prop:", (ref($@) ? $@->{m} : "none"), "\n";
eval { eval { die Plain->new }; die };
print "plain:", (ref($@) ? ref($@) : "none"), "\n";
my $a = Auto->new;
print "auto:", $a->whatever, " can:", ($a->can("whatever") ? "T" : "F"), "\n";
PL
    is(run_cl($prog), run_perl($prog),
       '#1912: the runtime consumers of the same lookup (PROPAGATE, AUTOLOAD) are unmoved (perl oracle)');
}

# ── s492c, tasks #1743 + #1818: UNIVERSAL::VERSION was a STUB ──────────────
# `(defun pl-VERSION (&rest args) nil)` — so `$obj->VERSION` was undef,
# `Class->VERSION(9)` SUCCEEDED on a 1.0 module (the stub returned undef, the
# eval succeeded, and `! undef` is true, which is how t/op/universal.t row 20
# "passed"), and `use Module VERSION` had nothing to call.
#
# perl compares version OBJECTS, not numbers, and the two spellings normalise
# differently — probed 5.40.3, and these two rows are the pair a numeric
# comparison gets backwards: $VERSION "2.7.18" FAILS a VERSION(2.719) check
# (v2.7.18 < v2.719.0) while $VERSION 2.718 SATISFIES a VERSION("2.7.19") one.
{
    my $prog = <<'PL';
package Alice; our $VERSION = 2.718; sub new { bless {}, shift }
package Nover;  sub new { bless {}, shift }
package Sub1;   our @ISA = ('Alice');
package main;
sub msg { my $e = shift; $e =~ s/ at .* line \d+\.?\n?\z//s; $e =~ s/\n\z//; $e }
my $a = Alice->new;
print "1 ", (eval { $a->VERSION } // 'undef'), "\n";
print "2 ", (eval { Alice->VERSION } // 'undef'), "\n";
eval { $a->VERSION(2.719) }; print "3 [", msg($@), "]\n";
print "4 ", (eval { $a->VERSION(2.718) } ? "ok" : "BAD"), "\n";
print "5 ", (eval { $a->VERSION(2.0) } ? "ok" : "BAD"), "\n";
eval { Nover->VERSION(1) }; print "6 [", msg($@), "]\n";
print "7 ", (eval { Nover->VERSION } // 'undef'), "\n";
print "8 ", (eval { UNIVERSAL::VERSION("Alice") } // 'undef'), "\n";
{ local $Alice::VERSION = "2.7.18";
  eval { $a->VERSION(2.719) }; print "9 [", msg($@), "]\n"; }
eval { $a->VERSION("2.7.19") }; print "10 [", msg($@), "]\n";
{ local $Alice::VERSION = "not-a-version";
  eval { $a->VERSION(1) }; print "11 [", msg($@), "]\n"; }
print "12 ", (eval { $a->VERSION("1.0") } ? "ok" : "BAD"), "\n";
print "13 ", (eval { Sub1->VERSION } // 'undef'), "\n";
PL
    is(run_cl($prog), run_perl($prog),
       '#1743/#1818: ->VERSION reads $VERSION, compares as a VERSION OBJECT '
     . 'and raises perl\'s own two diagnostics (perl oracle, 13 rows)');
}
