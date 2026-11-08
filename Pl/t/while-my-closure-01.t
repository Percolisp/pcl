#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# A `my` in a while/until CONDITION is a fresh variable each iteration
# (task #3374, docs/ir-spec.md): a closure made in the body or the continue
# block keeps its own iteration's value (Path::Tiny's openw & co. are built in
# `while (my ($k, $v) = each %opens) { *{$k} = sub { ... $v ... } }`).
# The counter-case: a C-style for's `my` stays ONE variable.
# Every row compares against perl (5.38-safe: core only).

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
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;

    my $cl_code = PCLCore::transpile(qq{$pl2cl $pl_file});

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

sub test_wm {
    my ($name, $code) = @_;
    my $perl_out = run_perl($code);
    my $cl_out   = run_cl($code);
    is($cl_out, $perl_out, $name) or diag("Perl: $perl_out\nCL:   $cl_out");
}

# 1. #3374 the reproducer: each iteration's condition `my` is its own variable.
test_wm('while-condition my: closures keep their own iteration', <<'PERL');
my %h = (a => 1, b => 2); my %s;
while (my ($k, $v) = each %h) { $s{$k} = sub { $v } }
my @l = (3, 4); my @t;
while (my $x = shift @l) { push @t, sub { $x } }
print join(",", map { "$_=" . $s{$_}->() } sort keys %s), " ", join(",", map { $_->() } @t), "\n";
PERL

# 2. next / last / continue (a closure made there too) / redo.
test_wm('while-condition my: next, last, continue and redo', <<'PERL');
my @t; my @l = (1, 2, 3, 4, 5);
while (my $x = shift @l) {
  next if $x == 2;
  last if $x == 5;
  push @t, sub { "x$x" };
} continue { push @t, sub { "c$x" }; print "cont $x\n" }
print join(",", map { $_->() } @t), "\n";
@t = (); my @z = (1, 2); my $redo = 0;
while (my $y = shift @z) { push @t, sub { "y$y" }; if (!$redo++) { redo } }
print join(",", map { $_->() } @t), "\n";
PERL

# 3. until, nested whiles, two `my`s in one condition.
test_wm('until / nested / two condition mys', <<'PERL');
my @t; my @m = (3, 2, 1);
until (!(my $u = shift @m)) { push @t, sub { "u$u" } }
my @o = ([1, 2], [3, 4]);
while (my $r = shift @o) {
  my @in = @$r;
  while (my $e = shift @in) { push @t, sub { "$r->[0]:$e" } }
}
my @n = (7, 8);
while ((my $p = shift @n) && (my $q = 1)) { push @t, sub { "$p$q" } }
print join(",", map { $_->() } @t), "\n";
PERL

# 4. the counter-cases: a C-style for's `my` IS one variable across
#    iterations in perl, and a loop with no closure keeps its last value.
test_wm('C-style for my stays one variable; no-closure while unchanged', <<'PERL');
my @t;
for (my $i = 0; $i < 3; $i++) { push @t, sub { "i$i" } }
print join(",", map { $_->() } @t), "\n";
my @w = (5, 6); my $last;
while (my $v = shift @w) { $last = $v }
print "last=$last\n";
PERL

# 5. #3431: an expression-embedded `my` in the RIGHT side of a `my`
#    declaration (`my $l = X->n(my $t)`, IO-Compress's LexFile->new(my $name))
#    is a fresh variable each iteration, as in a plain statement -- in a for,
#    in a sub, in a while, in a list declaration, and captured by a closure.
test_wm('embedded my in a declaration RHS is per-iteration (#3431)', <<'PERL');
sub X::n { shift; foreach (@_) { print defined $_ ? "D " : "U "; $_ = "y" } 1 }
for my $k (1..2) { my $l = X->n(my $t); print "a $k $t\n" }
for my $k (1..2) { my ($l, $m) = (X->n(my $t), 1); print "d $k $t\n" }
sub g { for my $k (1..2) { my $l = X->n(my $t); print "g $k $t\n" } } g();
my $c = 0; while ($c++ < 2) { my $l = X->n(my $t); print "h $c $t\n" }
my @t; for my $k (1..2) { my $l = X->n(my $u); $u .= $k; push @t, sub { $u } }
print join(",", map { $_->() } @t), "\n";
PERL

# 6. #4544: the CHAINED scalar declarator `my $c = my $d = EXPR` -- `$d` is a
#    fresh lexical per call and per iteration (it was ONE shared cell: the
#    recursion read 2/0, the closures 2,2), a chained container too, and the
#    conditional and tail forms.
test_wm('chained scalar declarator is per-call / per-iteration (#4544)', <<'PERL');
sub f { my $n = shift; my $c = my $d = $n; f($n - 1) if $n > 0; "$c/$d" }
print "1 ", f(2), "\n";
my @s; for my $i (1 .. 3) { my $a2 = my $b2 = $i; push @s, sub { "$a2$b2" } }
print "2 ", join(",", map { $_->() } @s), "\n";
my @t; for my $i (1 .. 2) { my $p = my $q = my $r = $i * 10; push @t, sub { $p + $q + $r } }
print "3 ", join(",", map { $_->() } @t), "\n";
sub g { my $c = my $d = shift } print "4 ", g(7), "\n";
my @u; for my $i (1 .. 2) { my $c = my %h = (k => $i); push @u, sub { $h{k} } }
print "6 ", join(",", map { $_->() } @u), "\n";
sub k { my $c = my $d = $_[0] if $_[1]; defined $d ? "d=$d" : "d=undef" }
print "7 ", k(3, 1), " ", k(4, 0), "\n";
my @refs; sub rr { my $c = my $d = $_[0]; push @refs, \$d } rr(1); rr(2);
print "12 ", join(",", map { $$_ } @refs), " ", ($refs[0] == $refs[1] ? "same" : "diff"), "\n";
PERL

# 7. #4544 + #3431: a chained or embedded declarator of a DECLARATION that a
#    NAMED sub nested after it captures is promoted to its cell (the capture
#    promoter reads the same declarators the lowering let-binds) -- a sub body
#    refused the file ("possibly captured by nested sub"); file level too.
test_wm('a nested named sub capturing a chained / embedded declarator (#4544)', <<'PERL');
sub id { $_[0] }
sub run { my $l = id(my $t = 1); package T; sub g { ++$t } package main; T::g(); print "a $l $t\n" } run();
sub run2 { my $S = my $F = 0; package T2; sub g2 { ++$F; ++$S } package main; T2::g2(); T2::g2(); print "b F=$F S=$S\n" } run2();
my $l3 = id(my $t3 = 5); sub g3 { ++$t3 } g3(); print "c $l3 $t3\n";
my $S4 = my $F4 = 0; sub g4 { ++$F4; ++$S4 } g4(); print "d $S4 $F4\n";
PERL

# 8. #4625: an expression-embedded `my` SHADOWING an outer lexical of the same
#    name declares a NEW variable -- in a declaration's initializer and in a
#    plain statement, in a block and in a loop.  It wrote the OUTER one
#    (IO-Compress's `LexFile->new(my $in)` after an outer `$in = ''` croaked
#    "NO!!!!"; an inner `open my $fh` re-opened the outer handle).
test_wm('an embedded my shadows an outer same-name lexical (#4625)', <<'PERL');
sub L::new { shift; foreach (@_) { print defined $_ ? "D " : "U "; $_ = "x" } bless [@_], 'L' }
sub f {
    my $in; $in = '';
    { my $lex = L->new(my $in); print "1 $in\n"; }
    print "2 [$in]\n";
    for my $k (1 .. 2) { my $l = L->new(my $in); print "3 $k $in\n" }
    { L->new(my $in); print "4 $in\n"; }
    print "5 [$in]\n";
    open my $fh, '<', '/dev/null'; my $f1 = $fh;
    { open my $fh, '<', '/dev/null'; print "6 ", ($fh == $f1 ? "same" : "diff"), "\n"; }
    print "7 ", ($fh == $f1 ? "same" : "diff"), "\n";
}
f();
my $n = 5;
{ my $l = L->new(my $n); print "8 $n\n" }
print "9 $n\n";
sub g { $_[0] }
sub h { my $s = "a"; my @c; for my $i (1 .. 2) { push @c, sub { $s }; g(my $s = $i); push @c, sub { $s } } join ",", map { $_->() } @c }
print "10 ", h(), "\n";
sub m2 { my @a = (1, 2); { my $c = g(my @a = (3)); print "11 $c @a\n"; } "@a" }
print "12 ", m2(), "\n";
PERL

done_testing();
