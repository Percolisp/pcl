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

plan tests => 17;

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

# ── s501b (task #2083): UNIVERSAL::isa is the BASE implementation ──────────
# An overriding `isa` method that calls UNIVERSAL::isa (Math::BigFloat's own
# `sub isa { … UNIVERSAL::isa($self, $class) }`) recursed until the binding
# stack ran out: the function spelling delegated back to the override.  Only
# the infix `isa` operator and DOES delegate (sv_isa_sv / sv_does_sv); the
# function, `->SUPER::isa` and `->UNIVERSAL::isa` answer from @ISA.  Both
# programs are compared with the live perl answer.
{
    my $prog = <<'PL';
package Base; sub new { bless {}, shift }
package Foo; our @ISA = ('Base');
my $n = 0;
sub isa { my ($self, $class) = @_; return if $class =~ /^Nope/; die "loop" if ++$n > 50; UNIVERSAL::isa($self, $class) }
package main;
my $o = Foo->new;
print "a ", (Foo->isa("Base") ? 1 : 0), "\n";
print "b ", ($o->isa("Foo") ? 1 : 0), "\n";
print "c ", (UNIVERSAL::isa($o, "Base") ? 1 : 0), "\n";
print "d ", (UNIVERSAL::isa("Foo", "Base") ? 1 : 0), "\n";
print "e ", (Foo->isa("Nope") ? 1 : 0), "\n";
print "n=$n\n";
PL
    is(run_cl($prog), run_perl($prog),
       '#2083: an isa override calling UNIVERSAL::isa does not recurse (perl oracle)');
}
{
    my $prog = <<'PL';
package Base; sub new { bless {}, shift }
package Foo; our @ISA = ("Base"); my $n = 0;
sub isa { my ($s, $c) = @_; die "loop" if ++$n > 50; return 0 if $c eq "Nope"; $s->SUPER::isa($c) }
package main; use feature "isa"; no warnings;
my $o = Foo->new;
print(($o isa Base) ? 1 : 0, " ", ($o isa Nope) ? 1 : 0, " ", $o->DOES("Base") ? 1 : 0, " ",
      $o->DOES("Nope") ? 1 : 0, " ", $o->UNIVERSAL::isa("Nope") ? 1 : 0, " n=$n\n");
PL
    is(run_cl($prog), run_perl($prog),
       '#2083: infix isa and DOES delegate to the override, ->SUPER::isa and ->UNIVERSAL::isa do not (perl oracle)');
}
{
    # #3280: an ARRAY argument to the UNIVERSAL built-ins flattens, like any
    # perl sub's @_.  Type::Tiny's isa override is `$self->SUPER::isa(@_)` with
    # no @ISA, so SUPER:: ends at UNIVERSAL::isa -- which read the raw vector
    # as the class and answered FALSE (every Type::Tiny object "was not" one),
    # and `UNIVERSAL::isa(@two)` was a CL arity error.
    my $prog = <<'PL';
package Foo;
sub new { bless {}, shift }
sub hello { 1 }
sub i2 { my $self = shift; $self->SUPER::isa(@_) ? 1 : 0 }
sub c2 { my $self = shift; $self->SUPER::can(@_) ? 1 : 0 }
sub d2 { my $self = shift; $self->SUPER::DOES(@_) ? 1 : 0 }
package Kid; our @ISA = ('Foo');
sub isa { my $s = shift; $s->SUPER::isa(@_) }
package main;
my $f = Foo->new;
my @a = ('Foo'); my @m = ('hello'); my @two = ($f, 'Foo'); my @none = ();
print join(' ', $f->i2('Foo'), $f->i2('Nope'), $f->c2('hello'), $f->c2('nope'), $f->d2('Foo'),
  (UNIVERSAL::isa($f, @a) ? 1 : 0), (UNIVERSAL::can($f, @m) ? 1 : 0),
  (UNIVERSAL::isa(@two) ? 1 : 0), (UNIVERSAL::DOES(@two) ? 1 : 0),
  (Kid->new->isa(@a) ? 1 : 0), (Kid->new->isa('Nope') ? 1 : 0),
  (eval { UNIVERSAL::isa(@none); 1 } ? "lived" : "died: " . ($@ =~ /^(Usage: \S+)/)[0])), "\n";
PL
    is(run_cl($prog), run_perl($prog),
       '#3280: SUPER::isa/can/DOES(@_) and UNIVERSAL::isa/can/DOES(@list) flatten their arguments (perl oracle)');
}

{
    # #3392 (s518e): ->VERSION is a UNIVERSAL method like can/isa/DOES, valid on
    # ANY class name -- a package never declared answers undef.  Type-Tiny's
    # t/00-begin.t asks it of every optional module after a failed `use`, and
    # PCL died "Can't locate object method VERSION via package ...".
    my $prog = <<'PL';
package Exists; sub x { 1 }
package main;
my $m = "Return::Type";
eval "use $m ()";
my $v = $m->VERSION;
print join(' ', (defined $v ? "def" : "undef"), (defined(Exists->VERSION) ? 1 : 0),
  (defined(Nope::Deeper->VERSION) ? 1 : 0), (Nope->can("x") ? 1 : 0),
  (Nope->isa("UNIVERSAL") ? 1 : 0)), "\n";
PL
    is(run_cl($prog), run_perl($prog),
       '#3392: Class->VERSION on a package never declared is undef, not a method-not-found die (perl oracle)');
}

{
    # #3393 (s518e): `Pkg::f()->m` whose sub is installed at RUN time (Type::
    # Library's exports) is a CALL; the invocant resolver looked the name up
    # unqualified in the current package and used "Pkg::f" as a CLASS.  A
    # known package still wins, as in perl.
    my $prog = <<'PL';
package Q; sub m { "m:" . (ref($_[0]) || $_[0]) }
package Known::Cls; sub m { "class:" . $_[0] }
package main;
eval q{ *P::f = sub { bless {}, "Q" }; *Known::Cls = sub { die "called" }; 1 } or die;
print join(' ', P::f()->m, Known::Cls->m, (eval { No::Such()->m; 1 } ? "lived" : "died")), "\n";
PL
    is(run_cl($prog), run_perl($prog),
       '#3393: a qualified Pkg::f()->m whose sub exists at run time calls it; a known package stays a class (perl oracle)');
}

{
    # #3399 (s518e, the Type-Tiny census): caller() with a NEGATIVE level names
    # no frame -- perl answers the empty list / undef.  PCL took `nth` of the
    # negative index and died with a raw CL type error ("The value -1 is not of
    # type UNSIGNED-BYTE"); Error::TypeTiny::throw_cb reads `caller($level - 1)`
    # at level 0, so every Type::Tiny exception died that way and 90 census
    # files printed no TAP at all.
    my $prog = <<'PL';
sub f { my @c = caller(-1); my $s = caller(-1); my @d = caller(-2);
        print scalar(@c), " ", (defined $s ? "def" : "undef"), " ", scalar(@d), "\n" }
f();
my @t = caller(-1); print "top ", scalar(@t), "\n";
PL
    is(run_cl($prog), run_perl($prog),
       '#3399: caller(-1) inside a sub is the empty list / undef, not a CL type error (perl oracle)');
}

{
    # #3400 (s518e, the Type-Tiny census): a module written on
    # Test::Builder::Module asks its CLASS for the builder, then branches on
    # has_plan and calls skip / skip_all / is_passing -- Test::Requires's skip
    # path.  PCL answered none of them, so every file whose optional module is
    # absent DIED with no TAP instead of skipping.
    my $prog = <<'PL';
package Req; use Test::More (); our @ISA = ('Test::Builder::Module');
package main;
use Test::More;
my $b = Req->builder;
print "pre:", (defined $b->has_plan ? $b->has_plan : "undef"), "\n";
plan tests => 2;
print "has:", $b->has_plan, " same:", ($b == Test::More->builder ? 1 : 0), "\n";
$b->skip("no frob");
ok(1, "one");
print "passing:", $b->is_passing, "\n";
PL
    is(run_cl($prog), run_perl($prog),
       '#3400: Test::Builder::Module->builder, has_plan, skip and is_passing answer as Test::Builder (perl oracle)');
    my $prog2 = <<'PL';
package Req; our @ISA = ('Test::Builder::Module');
package main;
use Test::More;
my $b = Req->builder;
$b->skip_all("needs Frob") if !defined $b->has_plan;
print "not reached\n";
PL
    like(run_cl($prog2), qr/\A1\.\.0 # skip needs Frob\n\z/i,
         '#3400: ->skip_all before a plan prints 1..0 # SKIP and exits (as perl)');
}

{
    # #3346 (s519b): Test::NoWarnings' shape in plain Perl -- `use
    # Test::Builder ()` loads PCL's bridge (never the Test2 stack), the
    # builder's ok / diag / current_test are the TAP layer's, and a module's
    # END block runs its assertion BEFORE the plan-count check, as perl's
    # Test::Builder END runs after a later module's END.
    my $prog = <<'PL';
package NW;
use Test::Builder ();
my $T = Test::Builder->new;
END { $T->ok(1, "no warnings") || $T->diag("there were warnings"); }
package main;
use Test::More;
BEGIN { plan tests => 3; }
ok(1, "a");
Test::Builder->new->ok(1, "b via builder");
note "current=" . Test::Builder->new->current_test;
PL
    is(run_cl($prog), run_perl($prog),
       '#3346: Test::Builder->new->ok / current_test and a module END assertion before the plan check (perl oracle)');
}

{
    # #3373 (s519b): `package P;` switches inside a block give P no class, and
    # Child's @ISA = 'P' left P FORWARD-REFERENCED in CLOS -- a method call on
    # P itself (Class::Method::Modifiers' `$into->can`) died with a raw SBCL
    # FINALIZE-INHERITANCE backtrace.
    my $prog = <<'PL';
do { package MyParent; sub foo { "P" } package Child; our @ISA = "MyParent"; };
print Child->foo, "\n";
my $c = "MyParent";
print $c->can("foo") ? "can" : "cannot", " ", MyParent->foo, " ", (Child->isa("MyParent") ? 1 : 0), "\n";
PL
    is(run_cl($prog), run_perl($prog),
       '#3373: a parent named by @ISA before it has a class answers its own methods (perl oracle)');
}

{
    # #3401 (s518e, the Type-Tiny census): a CORE BUILTIN before `->` is a
    # CALL -- perl's keyword wins over a class name.  `caller->isa("X")`
    # (Type::Utils::extends's "Not a type library" guard) read the CLASS
    # "caller"; 19 census files died there.  A real package still wins.
    my $prog = <<'PL';
package Base0; sub hi { "hi:" . (ref($_[0]) || $_[0]) }
package MyL; BEGIN { our @ISA = ("Base0") }
package main;
sub ext { print join(" ", (caller->isa("Base0") ? 1 : 0), caller->hi, (caller(0))[0]), "\n" }
package MyL; main::ext();
package main; print((time->can("nope") ? 1 : 0), " ", Base0->hi, "\n");
PL
    is(run_cl($prog), run_perl($prog),
       '#3401: caller->isa / caller->m call caller() (scalar: the package), as perl; a package name stays a class (perl oracle)');
}

{
    # #3397 (s520a, the Type-Tiny census's largest family, 50 files / 1,531
    # rows): a QUALIFIED bareword before `->` that the unit knows as a PACKAGE
    # is a CALL when a sub of that name exists at the unit's compile (load) --
    # perl's rule (perlobj: a known sub wins over the class).  Types::Standard
    # installs `Types::Standard::ArrayRef` as a sub, then requires the file
    # that is `package Types::Standard::ArrayRef;` and says
    # `Types::Standard::ArrayRef->inline_check(...)`; PCL read the class.
    my $dir = File::Temp::tempdir(CLEANUP => 1);
    mkdir "$dir/Lib";
    open my $l, '>', "$dir/Lib.pm" or die;
    print $l 'package Lib; BEGIN { *Lib::Thing = sub { bless {}, "Obj" } } package Obj; sub hi { "obj" } 1;' . "\n";
    close $l;
    open my $t, '>', "$dir/Lib/Thing.pm" or die;
    print $t 'package Lib::Thing; sub hi { "class" } sub go { Lib::Thing->hi } 1;' . "\n";
    close $t;
    my $prog = qq{use lib "$dir"; use Lib; require Lib::Thing; print Lib::Thing::go(), "\\n";\n};
    is(run_cl($prog), "obj\n",
       '#3397: Pkg::Name->m in a unit loaded after a sub Pkg::Name was installed calls the sub (perl: obj)');
    my $prog2 = <<'PL';
package Foo::Bar; sub hi { "class" } sub new { bless {n => $_[1]}, $_[0] }
package Obj; sub hi { "obj" }
package Plain::K; sub hi { "plain" }
package main;
BEGIN { *Foo::Bar = sub { bless {}, "Obj" } }
my $o = Plain::K->hi;
print Foo::Bar->hi, " ", Foo::Bar::->hi, " ", $o, " ", Plain::K::->hi, "\n";
PL
    is(run_cl($prog2), run_perl($prog2),
       '#3397: a BEGIN-installed sub of a package name wins at the call site; Foo::Bar:: and a plain class stay classes (perl oracle)');
}

{
    # #3397 (s520a, review of its own Type-Tiny runs): `NAME()->m` with
    # PARENTHESES is a CALL in perl whatever packages exist -- the run-time
    # resolver's "a known stash is a class" rule read
    # `Types::Standard::Tuple()->of(...)` as the class once Tuple.pm had
    # loaded (CycleTuple.t 69 -> 8 rows).  Undefined, it dies as perl does.
    my $prog = <<'PL';
package Foo::Bar; sub m { "class:" . (ref($_[0]) || $_[0]) }
package Obj; sub m { "obj" }
package main;
print Foo::Bar->m, "\n";
print eval { Foo::Bar()->m; 1 } ? "lived\n" : "died: " . ($@ =~ /^(Undefined subroutine &Foo::Bar called)/)[0] . "\n";
*Foo::Bar = sub { bless {}, "Obj" };
print Foo::Bar()->m, " ", Foo::Bar->m, "\n";
sub mk { bless {}, "Obj" } print mk()->m, " ", main::mk()->m, "\n";
PL
    is(run_cl($prog), run_perl($prog),
       '#3397: NAME()->m with parentheses calls NAME even when a package NAME exists; NAME->m keeps the compile-time answer (perl oracle)');
}
