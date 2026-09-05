#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# census-bugs-01.t — the bugs the s470bm IR censuses found (tasks #1173-#1179).
#
# Each block names its task and asserts the PERL answer, probed against perl
# 5.40.3 and quoted in the block's comment.  These are RUN rows: every one of
# them is a value the program consumes, which is why the bugs were silent.

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

# ── #1179: `use parent`'s -norequire is a FLAG, never a superclass ─────────
# perl 5.40.3 on all three spellings: @ISA is (Foo), ->isa("-norequire") false.
# parent.pm strips it only in FIRST position, so the qw() spelling and the
# comma spelling must agree — they used to not.

test_cl('#1179 qw(-norequire Foo): @ISA holds only the class', <<'PL', "ISA=[Foo]\n");
package Foo; sub hi { "hi" }
package Bar; use parent qw( -norequire Foo );
package main;
print "ISA=[@Bar::ISA]\n";
PL

test_cl('#1179 qw(-norequire Foo): ->isa("-norequire") is false', <<'PL', "foo=1 nore=0\n");
package Foo; sub hi { "hi" }
package Bar; use parent qw( -norequire Foo );
package main;
printf "foo=%d nore=%d\n", (Bar->isa("Foo") ? 1 : 0), (Bar->isa("-norequire") ? 1 : 0);
PL

test_cl('#1179 comma spelling still right (the half that worked)', <<'PL', "ISA=[Foo Baz]\n");
package Foo; sub hi { "hi" }
package Baz; sub hi2 { "h2" }
package Bar; use parent -norequire, 'Foo', 'Baz';
package main;
print "ISA=[@Bar::ISA]\n";
PL

test_cl('#1179 qw() with two classes after the flag', <<'PL', "ISA=[Foo Baz]\n");
package Foo; sub hi { "hi" }
package Baz; sub hi2 { "h2" }
package Bar; use parent qw(-norequire Foo Baz);
package main;
print "ISA=[@Bar::ISA]\n";
PL

# The flag is honoured in FIRST position only — parent.pm's own rule
# (`if (@_ and $_[0] eq '-norequire') { shift }`).  perl DIES on this input
# (it goes looking for Foo.pm), so PCL is only required not to strip.
test_cl('#1179 a LATER -norequire is an ordinary list element', <<'PL', "ISA=[Foo -norequire]\n");
package Foo; sub hi { "hi" }
package Bar; use parent qw(Foo -norequire);
package main;
print "ISA=[@Bar::ISA]\n";
PL

# The parenthesised list spelling reached @ISA at all only after the flatten.
test_cl('#1179 -norequire with a parenthesised class list', <<'PL', "ISA=[Foo]\n");
package Foo; sub hi { "hi" }
package Bar; use parent -norequire, ('Foo');
package main;
print "ISA=[@Bar::ISA]\n";
PL

# ── #1178: an inline `my` inside a CALL ARGUMENT ──────────────────────────
# `f((open my $fh, ...), ...)` used to emit a call to the non-existent op
# `p-my`, and the whole emitted file died at load.  Both spellings are here:
# the plain one was always right and must stay right.

test_cl('#1178 open my $fh inside a call argument', <<'PL', "take[call-arg]=1\n");
sub take { my ($v, $d) = @_; print "take[$d]=", ($v ? 1 : 0), "\n" }
take((open my $fh, "<", "/etc/hostname"), "call-arg");
PL

test_cl('#1178 the plain spelling is unchanged', <<'PL', "plain=1\n");
my $ok = open my $fh, "<", "/etc/hostname";
print "plain=", ($ok ? 1 : 0), "\n";
PL

test_cl('#1178 the declared handle really is the opened one', <<'PL', "n=1\n");
sub take { my ($v) = @_; return $v }
my $r = take((open my $fh, "<", "/etc/hostname"));
my @l = <$fh>;
print "n=", scalar(@l), "\n";
PL

# ── #1178: `open ..., undef` is perl's ANONYMOUS TEMPORARY FILE ───────────
# perl 5.40.3: every FILE mode succeeds; `<` is read-only, and every write
# mode reads back after a seek (the temp file is O_RDWR whatever the mode).

test_cl('#1178 open +> undef opens an anonymous temp file', <<'PL', "ok=1\n");
my $ok = open(my $fh, "+>", undef);
print "ok=", ($ok ? 1 : 0), "\n";
PL

test_cl('#1178 the anonymous temp file round-trips a write', <<'PL', "read=[hello]\n");
open(my $fh, "+>", undef) or die "no";
print $fh "hello\n";
seek($fh, 0, 0);
my $l = <$fh>; chomp $l;
print "read=[$l]\n";
PL

test_cl('#1178 a > anonymous temp file reads back too (perl: O_RDWR)', <<'PL', "read=[hi]\n");
open(my $fh, ">", undef) or die "no";
print $fh "hi\n";
seek($fh, 0, 0);
my $l = <$fh>; chomp $l;
print "read=[$l]\n";
PL

test_cl('#1178 a < anonymous temp file is read-only and empty', <<'PL', "print=0 defined=0\n");
open(my $fh, "<", undef) or die "no";
my $p = print($fh "x\n") ? 1 : 0;
seek($fh, 0, 0);
my $l = <$fh>;
printf "print=%d defined=%d\n", $p, (defined $l ? 1 : 0);
PL

my $tap_banner = "# PCL Test library loaded\n";
test_cl('#1178 the task reproducer: ok((open my $fh, "+>", undef))', <<'PL', $tap_banner . "1..1\nok 1 - opened\n");
use Test::More tests => 1;
ok((open my $fh, "+>", undef), "opened");
PL

# ── #1179 (the same flatten): `use base ('A', 'B')` — the PARENTHESISED list ──
# The old per-child scan looked for Quote tokens directly under the
# Structure::List and PPI puts a Statement::Expression in between, so the whole
# `use base` was dropped: @ISA stayed empty, no defclass, and the program DIED
# at the first inherited method call.  28 files of perl's own t/mro/ are this
# spelling.  perl 5.40.3: @ISA is (Diamond_B, Diamond_C) and the method
# resolves through the diamond.

test_cl('#1179 use base with a parenthesised class list', <<'PL', "ISA_D=[Diamond_B Diamond_C]\nhello=A\nisa=1\n");
package Diamond_A; sub hello { "A" }
package Diamond_B; use base ('Diamond_A');
package Diamond_C; use base ('Diamond_A');
package Diamond_D; use base ('Diamond_B', 'Diamond_C');
package main;
print "ISA_D=[@Diamond_D::ISA]\n";
print "hello=", Diamond_D->hello, "\n";
print "isa=", (Diamond_D->isa('Diamond_A') ? 1 : 0), "\n";
PL

# ── #1178: the declarator strip must NOT swallow `local` ──────────────────
# A run that STARTS with something else — `f((1, local $g = "x"))` — reaches
# the unwrap as a Statement::Expression, where the `local` WORD is lowered on
# its own; #1178's first shape stripped every declarator on that route and the
# word simply vanished.  perl-tests/multideref.t:220 was the ONE corpus file
# that showed it, and it is the reason the strip is asymmetric.
#
# This is a SHAPE row, and it does NOT claim the emission is right: today PCL
# lowers that word to `(pl-local …)`, a call to a sub named `local` that dies
# at run time — task #1192 owns making it a real dynamic binding, and this row
# is expected to change (to a value row) when #1192 lands.  What it guards is
# only that the declarator is not SILENTLY DELETED, which is the failure #1178
# nearly introduced.

{
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh "our \$g = 'outer';\nsub wrap { my (\$v) = \@_; return \$v }\n"
            . "my \$r = wrap((1, local \$g = 'inner'));\n";
    close $fh;
    my $cl = `$pl2cl $pl_file 2>/dev/null`;
    like($cl, qr/local/,
         '#1178 a `local` in a call argument is not silently deleted (#1192)');
}

# ── #1174 + #1173: perl's two FATAL zero-operand arithmetic errors ────────
# perl 5.40.3 dies "Illegal division by zero" / "Illegal modulus zero", both
# trappable by eval.  PCL used to let SBCL's own condition print itself for
# `/` (a CL s-expression, "Operation was (/ 1 0)", in a perl program's output)
# and, for `%`, not to die at all — the zero case was folded in with NaN, so
# `$x % $n` with a computed $n that happened to be 0 gave a plausible NaN the
# program then consumed.
#
# The zero test differs between the two, and each spelling below was probed:
# `/` tests the NUMBER (5/0.5 is 10, 1/-0.0 dies), `%` tests the TRUNCATED
# number (5 % 0.5 AND 5 % 0.9 both die).

test_cl('#1174 1/0 dies with perl\'s message, trappably', <<'PL', "v=undef err=[Illegal division by zero]\n");
my ($x, $y) = (1, 0);
my $v = eval { $x / $y };
my $e = $@ || ''; $e =~ s/ at .*//s;
printf "v=%s err=[%s]\n", (defined $v ? $v : 'undef'), $e;
PL

test_cl('#1174 1/0.0 and $x/=0 and 1/"abc" die the same way', <<'PL', "float=1 compound=1 string=1 undefarg=1 half=10\n");
sub dies { my $c = shift; eval { $c->() }; return ($@ =~ /Illegal division by zero/) ? 1 : 0 }
my $u;
printf "float=%d compound=%d string=%d undefarg=%d half=%s\n",
  dies(sub { my $z = 0.0; 1 / $z }),
  dies(sub { my $n = 7; $n /= 0; $n }),
  dies(sub { my $s = "abc"; 1 / $s }),
  dies(sub { 1 / $u }),
  (5 / 0.5);
PL

test_cl('#1173 5 % 0 dies instead of answering NaN', <<'PL', "v=undef err=[Illegal modulus zero]\n");
my $v = eval { 5 % 0 };
my $e = $@ || ''; $e =~ s/ at .*//s;
printf "v=%s err=[%s]\n", (defined $v ? $v : 'undef'), $e;
PL

test_cl('#1173 the modulus zero test is on the TRUNCATED operand', <<'PL', "half=1 point9=1 compound=1 negzero=1 neg=-1\n");
sub dies { my $c = shift; eval { $c->() }; return ($@ =~ /Illegal modulus zero/) ? 1 : 0 }
printf "half=%d point9=%d compound=%d negzero=%d neg=%s\n",
  dies(sub { 5 % 0.5 }),
  dies(sub { 5 % 0.9 }),
  dies(sub { my $n = 7; $n %= 0; $n }),
  dies(sub { my $z = -0.0; 1 % $z }),
  (5 % -3);
PL

# ── #1173(a): substr() PAST THE END is undef, not "" ──────────────────────
# perl 5.40.3: substr("abc",10,2), substr("abc",10) and substr("abc",-10,2)
# are all undef; substr("abc",3) — the position one past the end, which is IN
# range — is "".  PCL answered a defined "" for every one of them, so
# `defined substr($s,$i)` was true for every $i.

#
# The `$SIG{__WARN__}` line is what isolates the VALUE claim from the separate
# diagnostic one: perl emits nothing here (the snippet has no `use warnings`)
# while PCL emits "substr outside of string" unconditionally, which is the
# #221 warnings-gated-diagnostic gap and NOT something this row should bless
# into an expected string.

test_cl('#1173(a) substr past the end is undef; one-past-the-end is ""', <<'PL', "d(10,2)=0 d(10)=0 d(-10,2)=0 d(3)=1 v(3)=[] v(1,-1)=[b]\n");
local $SIG{__WARN__} = sub { };
my $s = "abc";
printf "d(10,2)=%d d(10)=%d d(-10,2)=%d d(3)=%d v(3)=[%s] v(1,-1)=[%s]\n",
  (defined substr($s,10,2)  ? 1 : 0),
  (defined substr($s,10)    ? 1 : 0),
  (defined substr($s,-10,2) ? 1 : 0),
  (defined substr($s,3)     ? 1 : 0),
  substr($s,3), substr($s,1,-1);
PL

# ── #1173(b): a false `exists` is a DEFINED "" ────────────────────────────
# perl's `exists` is a boolean builtin, so its false is "" — the #416 rule.
# Every one of these was undef in PCL, which `defined()` can see.

test_cl('#1173(b) a false exists is defined, in all five forms', <<'PL', "hash=1 aryref=1 sub=1 env=1 true=1\n");
sub have { 1 }
my @a = (1, 2);
my $ar = [1, 2];
my %h = (a => 1);
printf "hash=%d aryref=%d sub=%d env=%d true=%d\n",
  (defined(exists $h{zz})            ? 1 : 0),
  (defined(exists $ar->[9])          ? 1 : 0),
  (defined(exists &nope)             ? 1 : 0),
  (defined(exists $ENV{NO_SUCH_XYZ}) ? 1 : 0),
  ((exists $h{a} && exists $a[0] && exists &have) ? 1 : 0);
PL

# ── #1175: four host-leak families become four named ops ──────────────────
# `Pl/t/ir-host-leak-01.t` is the SHAPE guard (each family's fixture now
# leaks the empty set).  These are the VALUE guards: the whole point of the
# rename is that nothing observable changes, so each row asserts perl's answer
# for the construct whose emission moved.

test_cl('#1175(2) a signature default still applies only when absent', <<'PL', "222 7\n");
use feature 'signatures';
no warnings;
sub f ($a = 222) { return $a }
print f(), " ", f(7), "\n";
PL

test_cl('#1175(3) a surrogate/non-character literal still has length 2', <<'PL', "2 55296 65535\n");
my $s = "\x{d800}\x{ffff}";
printf "%d %d %d\n", length($s), ord(substr($s,0,1)), ord(substr($s,1,1));
PL

test_cl('#1175(4) the __DATA__ handle still reads its section', <<'PL', "[alpha]\n[beta]\n");
while (my $l = <DATA>) { chomp $l; print "[$l]\n"; }
__DATA__
alpha
beta
PL

test_cl('#1175(5) \\(LIST) still spreads over a range plus an element', <<'PL', "3 123\n");
my (@fuu) = \(1..2,3);
print scalar(@fuu), " ", ${$fuu[0]}, ${$fuu[1]}, ${$fuu[2]}, "\n";
PL

# ── #1175(4): DATA is a PER-PACKAGE handle, and the op takes the symbol ────
# The first shape of `p-install-data-handle` interned `'DATA` inside the
# RUNTIME, so every file AND every module it loads shared one key: perl's own
# Exporter.pm has an `__END__` POD section, so loading it REPLACED the
# program's own section.  Measured on perl-tests/sprintf.t, whose 566-row
# `__END__` table is read with `while (<DATA>)`: it planned 1 test instead of
# 559, and the one record it read was Exporter's POD.  The handle name is an
# ARGUMENT now, interned by the emitter in the file's own package.
#
# Both rows load Exporter (via `use Exporter`, which every `use`-ing module
# does anyway) so the collision is IN the fixture, and both read in LIST
# context — the spelling sprintf.t does not use, and so the one this file
# would otherwise never cover.

test_cl('#1175(4) a module\'s __END__ does not replace the program\'s DATA', <<'PL', "n=10 first=[alpha] last=[omega]\n");
use Exporter;
my @all = <DATA>;
chomp @all;
print "n=", scalar(@all), " first=[$all[0]] last=[$all[-1]]\n";
__END__
alpha
beta

# comment
>%6. 6s<    >''<          >%6. 6s INVALID<
gamma
delta
epsilon
zeta
omega
PL

test_cl('#1175(4) list-context <DATA> after a while-loop read', <<'PL', "first=[one] rest=3\n");
use Exporter;
my $first = <DATA>;
chomp $first;
my @rest = <DATA>;
print "first=[$first] rest=", scalar(@rest), "\n";
__DATA__
one
two
three
four
PL

