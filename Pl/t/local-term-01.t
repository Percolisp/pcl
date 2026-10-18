#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# A `local` TERM that is not the first term of its statement (task #3370,
# RULED s518; docs/ir-spec.md "local"): a void comma list splits into one
# statement per term under the condition evaluated once (so does `COND and
# (LIST)`, ruling 3 of s519), a local that is the first thing its statement
# evaluates hoists before it; anything else in a VALUE position REFUSES at its
# statement, in a VOID position it is announced and not localized (ruling 4).  Plus #3411: `local *STDOUT` in another package is main's.
# Every row but the last compares against perl (5.38-safe: core only).

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

sub run_perl {
    my ($code) = @_;
    my ($fh, $file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;
    return `perl $file 2>&1`;
}

# Transpile and run CL, return output
sub run_cl {
    my ($code, $raw) = @_;    # $raw: a row that MEANS a refusal reads stderr itself
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;

    my ($cl_code, $err) = $raw ? PCLCore::transpile_raw(qq{$pl2cl $pl_file})
                                 : (PCLCore::transpile(qq{$pl2cl $pl_file}), q{});
    $$raw = $err if ref $raw;

    my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cl_fh $cl_code;
    close $cl_fh;

    my $output = `sbcl @sbcl_rt --load $cl_file 2>&1`;
    $output =~ s/^;.*\n//gm;
    $output =~ s/^caught .*\n//gm;
    $output =~ s/^compilation unit.*\n//gm;
    $output =~ s/^\s*Undefined.*\n//gm;
    $output =~ s/^-->.*\n//gm;
    $output =~ s/^==>.*\n//gm;
    $output =~ s/^PCL Runtime loaded\n//gm;
    $output =~ s/^WARNING:.*\n//gm;
    $output =~ s/^\s*\n//gm;
    return $output;
}

sub test_lt {
    my ($name, $code) = @_;
    my $perl_out = run_perl($code);
    my $cl_out   = run_cl($code);
    is($cl_out, $perl_out, $name) or diag("Perl: $perl_out\nCL:   $cl_out");
}

# 1. #3370 the scalar spelling: the local happens at its term, restored at
#    the block's end; the condition is evaluated once.
test_lt('comma list: local term localizes and restores', <<'PERL');
our $g = 1;
sub f { print "g=$g\n" }
{ my %l; $l{o}++, local($g), $g = 2 if 1; f(); print "o=$l{o}\n"; }
f();
PERL

# 2. a false modifier: nothing runs, nothing is localized.
test_lt('comma list: false modifier localizes nothing', <<'PERL');
our $g = 1;
{ my $n = 0; $n++, local($g), $g = 3 if 0; print "g=$g n=$n\n"; }
print "g=$g\n";
PERL

# 3. the condition is evaluated ONCE, before the local changes what it reads.
test_lt('comma list: condition evaluated once, first', <<'PERL');
our $g = 1; my $c = 0;
sub cond { $c++; $g == 1 }
{ my $n = 0; $n++, local $g = 4, $n++ if cond(); print "g=$g n=$n c=$c\n"; }
{ my $n = 0; $n++, local $g = 6, $n++ unless !cond(); print "g=$g n=$n c=$c\n"; }
print "g=$g\n";
PERL

# 4. the glob spelling (Capture::Tiny 344) in a non-main package, under a tie
#    whose OPEN dies: the local hides the tie, the reopen works (#3411: the
#    standard handles are main's from every package).
test_lt('comma list: local(*STDOUT) under a tie, in package Foo', <<'PERL');
package TE;
sub TIEHANDLE { my $x = ''; bless \$x }
sub OPEN { die "unimplemented OPEN\n" }
sub PRINT { my $s = shift; $$s .= join '', @_; 1 }
package Foo;
tie *STDOUT, 'TE';
my %l;
{
  $l{o}++, local(*STDOUT), open(STDOUT, ">&=1") if tied *STDOUT;
  print STDERR "inside=", (tied(*STDOUT) ? 1 : 0), "\n";
  open(STDOUT, ">", "/dev/null") or die "reopen: $!";
}
print STDERR "after=", (tied(*STDOUT) ? 1 : 0), " o=$l{o}\n";
PERL

# 5. #3411 alone: a leading `local *STDOUT` in package Foo is main's STDOUT.
test_lt('local *STDOUT in package Foo localizes main::STDOUT', <<'PERL');
package TE;
sub TIEHANDLE { my $x = ''; bless \$x }
sub PRINT { my $s = shift; $$s .= join '', @_; 1 }
package Foo;
tie *STDOUT, 'TE';
{ local *STDOUT; print STDERR "a=", (tied(*STDOUT) ? 1 : 0), "\n"; }
{ local(*STDOUT) if 1; print STDERR "b=", (tied(*STDOUT) ? 1 : 0), "\n"; }
print STDERR "after=", (tied(*STDOUT) ? 1 : 0), "\n";
PERL

# 6. a local that is the first thing its statement evaluates: an argument,
#    an assignment's right side, a parenthesised right side under a modifier.
test_lt('local term first evaluated: argument / my init / modifier', <<'PERL');
our @x = ([1], [2]);
sub is_ { print "is $_[0] $_[1]\n" }
{ is_(local $x[0][0] = 19, 19); print "in $x[0][0]\n"; }
print "out $x[0][0]\n";
{ my $a = local $/ = 7; print "a=$a /=$/\n"; }
print "/=", ($/ eq "\n" ? "nl" : $/), "\n";
our $z = 1;
{ my $o = (local $z = 5) if 1; print "z=$z o=$o\n"; }
print "z=$z\n";
our $r = [5];
{ is_(local +($r//0)->[0] = 9, 9); print "in $r->[0]\n"; }
print "out $r->[0]\n";
our $q = 1;
{ if (local $q = 2) { print "q=$q\n" } print "q=$q\n"; }
print "q=$q\n";
PERL

# 7. an inner local of the SAME operand under a leading one changes nothing.
test_lt('local @b = local(@b) = LIST', <<'PERL');
our @bee = (1);
{ local @bee = local(@bee) = qw(foo bar); print "@bee\n"; }
print "@bee\n";
PERL

# 7b. RULED s519 #3: `COND and (LIST);` is `(LIST) if COND;` -- Role::Tiny
#     307's exact spelling (a hash slice through a reference, localized then
#     deleted): the slots are back at the ENCLOSING block's end (the loop
#     body), and a later call inside the block sees them deleted.
test_lt('COND and (LIST): Role::Tiny 307, restored at the block end', <<'PERL');
our %role_methods = (R => { a => 1, b => 2, c => 3 }, S => { a => 4 });
my @have_conflicts = qw(a b);
sub step { my ($r) = @_; my $h = $role_methods{$r};
  print "$r: ", join(",", map { "$_=" . ($h->{$_} // "u") } sort keys %$h), "\n" }
my $role_methods;
foreach my $role (qw(R S T)) {
  $role_methods = $role_methods{$role} and (
    (local @{$role_methods}{@have_conflicts}),
    (delete @{$role_methods}{@have_conflicts}),
  );
  step($role);
}
step($_) for qw(R S);
PERL

# 7c. the other three spellings: `or` / `||` guard with unless, `&&` with if;
#     the condition is evaluated once, and the local lasts to the block end.
test_lt('COND or/||/&& (LIST): unless / if, condition once', <<'PERL');
our $g = 1; my $c = 0;
sub cnd { $c++; $_[0] }
{ cnd(0) or ((local $g = 2), print("in g=$g\n")); print "g=$g c=$c\n"; }
{ cnd(1) || ((local $g = 3)); print "g=$g c=$c\n"; }
{ cnd(1) && (local $g = 4, print("in g=$g\n")); print "g=$g c=$c\n"; }
print "g=$g\n";
PERL

# 7d. a hash slice through a reference, and an array-named key list, as a
#     LEADING local (the machinery the split hands the terms to).
test_lt('local @{$r}{@k} / local @h{@k} = LIST', <<'PERL');
our %h = (a => 1, b => 2, c => 3); my $r = \%h; my @k = qw(a b);
sub show { print join(",", map { "$_=" . ($h{$_} // "u") } sort keys %h), "\n" }
{ local @{$r}{@k}; show(); }
{ local @$r{@k} = (7, 8); show(); }
{ local @h{@k} = (5, 6); show(); }
{ local ${$r}{c} = 9; show(); }
show();
PERL

# 7e. the non-paren logical spelling `COND and local …, local(…);` (B::Deparse
#     6279, YAML::PP::Emitter 508), `or` = unless, and a void comma list under
#     a `while` modifier (a loop body's last statement is void).
test_lt('COND and local LIST / or local / comma list under while', <<'PERL');
our ($g, $h, %s) = (1, 2, (a => 1, b => 2, c => 3));
sub DEBUG () { 1 }
sub f { print "g=$g h=$h s=", join(",", map { $s{$_} // "u" } qw(a b c)), "\n" }
my $self = \%s; my $cv = 7;
{ $cv and local($g) = $cv, local($h), local(@$self{qw'a b'}); f(); }
f();
{ DEBUG and local $g = 3; f(); 0 or local $g = 4; f(); 1 or local $g = 5; f(); }
f();
{ my $n = 0; $n++, local $g = 5 while $n < 2; print "n=$n\n"; f(); }
f();
PERL

# 7f. RULED s519 #1 -- the hoist: a local that is the FIRST thing its
#     statement evaluates has nothing before it that could observe the old
#     value, so `local X;` before the statement is the same program: the
#     argument is the new value, and a call AFTER it in the statement sees it.
test_lt('hoisted local: f(local $x = 1) / (local $x = 2) + g()', <<'PERL');
our $x = 0;
sub f { print "f arg=$_[0] x=$x\n"; $_[0] }
sub g { print "g x=$x\n"; 10 }
{ f(local $x = 1); print "in x=$x\n"; }
print "out x=$x\n";
{ my $v = (local $x = 2) + g(); print "v=$v x=$x\n"; }
print "out x=$x\n";
PERL

# 7g. RULED s519 #4: a local in a VOID position no lowering reaches is
#     effect-only -- it keeps the old read (NOT localized: here the caller's
#     $_ is clobbered, where perl restores it) and the transpile ANNOUNCES it;
#     the program runs on.  (ExtUtils::CBuilder's Windows.pm 74, dumpvar.pl 71.)
{
    my $code = <<'PERL';
sub sls { (my $x, local $_) = @_; print "in x=$x _=$_\n"; }
$_ = "out"; sls(1, 2); print "ran\n";
PERL
    my $err;
    is(run_cl($code, \$err), "in x=1 _=2\nran\n", 'void unreached local runs on, unlocalized');
    like($err, qr/^PCL: local in an unsupported expression position at \S+ line 1 -- not localized$/m,
         '... and the transpile announces it');
}

# 8. a local in a VALUE position no lowering reaches is a REFUSAL at its
#    statement, not a silent read (perl: "1 9 1"; PCL refuses perl-shaped, trappable).
{
    my $code = <<'PERL';
our @x = ([1]);
my @r = eval { @{[1, local $x[0][0] = 9]} };
print "err=", ($@ =~ /PCL: local in this expression position is not supported/ ? 1 : 0), "\n";
print "x=$x[0][0]\n";
PERL
    my $err;
    is(run_cl($code, \$err), "err=1\nx=1\n", 'unreached local term refuses at its statement');
    like($err, qr/^PCL: refused statement at .* line 2: .*local in this expression position/m,
         '... and the transpile announces the refusal');
}

# 9. #3363 (R1): a `local` term in an argument list a paren-less list operator
#    OWNS is not a void comma list -- never split; the hoist owns it (the
#    operator runs AFTER its arguments).  print / return / push / die / a
#    declared `foo LIST`.
test_lt('local term in a list operator\'s arguments: print / return / push / die / foo', <<'PERL');
our $x = 0; our @l;
sub foo { print "foo @_ x=$x\n" }
sub r { return 1, local $x = 2 }
{ print "a", local $x = 5; print "\n"; print "in x=$x\n"; }
print "x=$x\n";
my @r = r(); print "r=@r x=$x\n";
{ push @l, local $x = 3; print "l=@l x=$x\n"; }
print "x=$x\n";
{ foo 1, local $x = 4; print "x=$x\n"; }
print "x=$x\n";
{ eval { die "d", local $x = 6, "\n" }; print "err=$@x=$x\n"; }
print "x=$x\n";
PERL

# 10. #3363 (R1, f1b): `COND or LIST` -- the loose `or` OWNS the comma list
#     (`COND or (h(), local $x = 9)`), so the local happens only when COND is
#     false, and lasts to the enclosing block's end.
test_lt('COND or h(), local LIST: guarded by COND, not split unconditionally', <<'PERL');
our $x = 0; my $ok = 1; sub h { print "h x=$x\n"; 2 }
{ $ok or h(), local $x = 9; print "x=$x\n"; }
{ !$ok or h(), local $x = 8; print "x=$x\n"; }
{ $ok and h(), local $x = 7, h(); print "x=$x\n"; }
print "x=$x\n";
PERL

# 11. #3363 (R8, f8): a CALL that runs before the local term (a method
#     invocant `cfg()`, `$o->get`) must still see the OLD value: the invocant
#     is evaluated first, then the local, then the owning method.
test_lt('INVOCANT->m(local X): the invocant runs before the local', <<'PERL');
our $X = 'outer';
sub cfg { print "cfg sees $X\n"; bless {}, 'C' }
sub C::apply { print "apply sees $X arg=$_[1]\n" }
sub C::get { print "get sees $X\n"; $_[0] }
{ cfg()->apply(local $X = 'new'); print "in: $X\n"; }
print "after: $X\n";
my $o = bless {}, 'C';
{ $o->get->apply(local $X = 'n2'); print "in: $X\n"; }
print "after: $X\n";
PERL

# 12. #3363 (R8): the LEFT side of an assignment runs after its right side,
#     so `(local $g) = ($g + 1)` reads the OLD $g in perl ("in g=2" then
#     "g=1") -- the hoist must not move the local above the right side
#     (fd04c048 printed "in g=1": the right side read the localized undef).
#     Not lowered: a void-position miss, announced (ruling 4), unlocalized.
{
    my $code = <<'PERL';
our $g = 1;
{ (local $g) = ($g + 1); print "in g=$g\n"; }
print "g=$g\n";
PERL
    my $err;
    is(run_cl($code, \$err), "in g=2\ng=2\n", 'local on an assignment\'s left side is not hoisted above its right side');
    like($err, qr/^PCL: local in an unsupported expression position at \S+ line 2 -- not localized$/m,
         '... and the transpile announces it');
}

# 13. #3364 (R4, f4): a rewritten statement keeps its NUMBER OF LINES, so
#     every later `at FILE line N` (die / warn / __LINE__; caller()'s line is
#     the deferred #233) is perl's
#     -- after a hoist spanning two lines, after TWO rewrites (the drift was a
#     running total) and after a split spanning three lines.
test_lt('line numbers after a hoist / two rewrites / a three-line split', <<'PERL');
our %h; our $g = 1; my $c = 1; sub foo { }
sub at { my ($m) = $_[0] =~ /line (\d+)/; print "$_[1] line $m\n" }
foo(local $h{k} = 19)
  if $c;
eval { die "x" }; at($@, "die");
print "LINE ", __LINE__, "\n";
foo(local $h{j} = 20)
  if $c;
{ my %l; $l{o}++,
    local($g),
    $g = 2 if $c;
  print "g=$g ", __LINE__, "\n"; }
eval { die "y" }; at($@, "die2");
print "LINE ", __LINE__, "\n";
PERL

# 14. #3365 (R3, f3b): a PARENTHESISED deref lvalue is a list assignment --
#     `local($$r) = (7, 8)` stores 7, `local(${"main::g"}) = @_` the FIRST
#     argument; unparenthesised `local $$r = (7, 8)` is scalar (8); the plain
#     `local($g) = (5, 6)` beside them.  (`local($$r, $y) = …` is the older
#     loud Parser2 TODO, #3470.)
test_lt('local($$r) / local(${"name"}) = LIST: list assignment', <<'PERL');
our $g = 1; my $r = 'g';
{ local($$r) = (7, 8); print "g=$g\n"; }
print "g=$g\n";
sub t { local(${"main::g"}) = @_; print "in t g=$g\n" } t(3, 4); print "g=$g\n";
{ local $$r = (7, 8); print "g=$g\n"; }
{ local($g) = (5, 6); print "g=$g\n"; }
{ local(${"main::g"}) = 5; print "g=$g\n"; }
print "g=$g\n";
PERL

# 15. #3366 (R2 + R5, f2 + f5): a side-effecting statement-modifier
#     CONDITION and a side-effecting slice BASE of a localized slice run
#     ONCE (n=1), and the right elements are localized.
test_lt('local slice: the modifier condition and the base run once', <<'PERL');
our @a = (0, 0, 0); our %h = (a => 1, b => 2); my $n = 0; my $m = 0;
sub f { $n++; 1 }
sub hb { $m++; \%h }
{ local @a[0..1] = (1, 2) if f(); print "n=$n a=@a\n"; }
print "a=@a\n";
{ local @{ hb() }{qw(a b)} = (10, 20); print "in m=$m a=$h{a} b=$h{b}\n"; }
print "out m=$m a=$h{a} b=$h{b}\n";
{ local @{ hb() }{qw(a b)} = (30, 40) if f(); print "in n=$n m=$m a=$h{a} b=$h{b}\n"; }
print "out n=$n m=$m a=$h{a} b=$h{b}\n";
PERL

# 16. #3367 (R6): NINE void comma-list locals in one file -- every one is
#     lowered (the pass used to stop after eight rewrites and leave the ninth
#     a silent plain write: out g9=9).
test_lt('nine comma-list locals in one document are all lowered', <<'PERL');
our ($g1, $g2, $g3, $g4, $g5, $g6, $g7, $g8, $g9) = (0) x 9; my $n = 0;
sub show { print "$_[0] n=$n ", join(" ", $g1, $g2, $g3, $g4, $g5, $g6, $g7, $g8, $g9), "\n" }
{
  $n++, local($g1), $g1 = 1 if 1;
  $n++, local($g2), $g2 = 2 if 1;
  $n++, local($g3), $g3 = 3 if 1;
  $n++, local($g4), $g4 = 4 if 1;
  $n++, local($g5), $g5 = 5 if 1;
  $n++, local($g6), $g6 = 6 if 1;
  $n++, local($g7), $g7 = 7 if 1;
  $n++, local($g8), $g8 = 8 if 1;
  $n++, local($g9), $g9 = 9 if 1;
  show("in");
}
show("out");
PERL

# 17. #3368 (R7, f7): a `while` / `until` condition's local is restored when
#     the LOOP ends (perl restores it per iteration and at leaveloop), not at
#     the enclosing block's end -- `while (local $_ = shift @a)`, the idiom.
test_lt('while (local X = …): restored at the loop end', <<'PERL');
our $x = 'orig'; our @a = (1, 2);
{ while (local $x = shift @a) { } print "in-block x=[", defined $x ? $x : 'undef', "]\n"; }
print "x=$x\n";
our @b = (1, 2, 3); $_ = 'top';
sub seen { print "seen $_\n" }
{ while (local $_ = shift @b) { seen(); } print "after loop [", defined $_ ? $_ : 'undef', "]\n"; }
our @c = (4, 5); our $y = 'y0';
{ L: until (!defined(local $y = shift @c)) { print "y=$y\n"; last L if $y == 5 } print "y=$y\n"; }
PERL

# 18. #3368 (R9): an anonymous sub with a PROTOTYPE or an ATTRIBUTE in a
#     while-condition `my` loop captures that iteration's variable (#3374).
test_lt('while (my $x …) { sub (&) {…} / sub :lvalue {…} } captures per iteration', <<'PERL');
my @q = (1, 2, 3); my @cbs;
while (my $x = shift @q) { push @cbs, sub (&) { "$x:" . $_[0]->() } }
print join(",", map { $_->(sub { "z" }) } @cbs), "\n";
my @r = (4, 5); my @lv;
while (my $y = shift @r) { push @lv, sub :lvalue { $y } }
print join(",", map { $_->() } @lv), "\n";
PERL

# 19. #3363: more owners and invocants -- a named unary is a term (the split
#     stays), `push @l, local …, 6` keeps its trailing argument, and a CLASS
#     invocant chain `Foo->new->set(local …)` runs `new` before the local.
test_lt('named unary term / push LIST tail / Class->new->m(local …)', <<'PERL');
our ($x, $y, $g) = (0, 1, 0); our @l;
sub show { print "$_[0] x=$x g=$g\n" }
{ my $d = 0; $d = defined $y, local $x = 3; show("defined"); }
show("out1");
{ @l = map { $_ * 2 } 1, 2; push @l, local $g = 5, 6; print "l=@l\n"; show("push"); }
show("out3");
{ local $g = 1; $g++, local $x = $g if $g; show("cond-reads"); }
show("out4");
{ Foo->new->set(local $x = 7); show("class"); }
show("out5");
package Foo; sub new { print "new x=$main::x\n"; bless {} } sub set { print "set x=$main::x\n" }
PERL

# 20. #3512: a HEREDOC among the arguments of a statement holding a `local`
#     term -- perl's t/io/open.t:199 `ok( open(local $f, '-|', <<EOC), ...)`
#     was REFUSED (#3370's value-position check), aborting the file's form.
#     The hoist keeps the here-doc's body after the rebuilt statement, and the
#     line numbers after it stay perl's; a non-interpolating here-doc BEFORE
#     the local is inert like a quote.
test_lt('a here-doc argument beside a local term (t/io/open.t:199)', <<'PERL');
our ($f, $x);
sub ok { print(($_[0] ? "ok" : "not ok"), " - $_[1]\n"); }
ok( open(local $f, '-|', <<EOC),  'open local $f, "-|", ...' );
echo hi
EOC
print "r=", scalar(<$f>), "line ", __LINE__, "\n";
sub g { return "g:$_[0]" }
print g(local $x = <<EOC);
val
EOC
print g(<<A, local $x = 5), " x=$x line ", __LINE__, "\n";
first
A
PERL

# 21. #3470 (s521d): a SCALAR deref in a parenthesised `local` list
#     (`local($$r, $y) = LIST`, `${"main::g"}`, one in the middle) localizes
#     it with the rest -- it dropped the statement ("unsupported `local`
#     target") where perl localizes both.
test_lt('local($$r, $y) = LIST / ${"name"} / a deref in the middle', <<'PERL');
our ($g, $y, $z) = (1, 0, 5); our @a = (1); my $r = "g";
{ local($$r, $y) = (7, 8); print "A g=$g y=$y\n"; }
print "B g=$g y=$y\n";
{ local($$r, @a) = (7, 8, 9); print "C g=$g a=@a\n"; }
print "D g=$g a=@a\n";
{ local(${"main::g"}, $y) = (3, 4); print "E g=$g y=$y\n"; }
print "F g=$g y=$y\n";
{ local($y, $$r, $z) = (10, 11, 12); print "G g=$g y=$y z=$z\n"; }
print "H g=$g y=$y z=$z\n";
{ local($$r) = (21, 22); print "I g=$g\n"; }
print "J g=$g\n";
PERL

done_testing();
