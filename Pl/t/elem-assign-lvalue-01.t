#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# elem-assign-lvalue-01.t -- an assignment TO AN ELEMENT is an lvalue: its
# value is the element itself (task #4303, s532c; the reduction of #3835).
#
# perl's value of an assignment is its LEFT side as an lvalue, so
# `($block->{$1} = $2) =~ s/^ *(.*?) *$/$1/` (YAML::Tiny's TestML::Tiny)
# trims the ELEMENT and `($h{a} = "x") .= "y"` appends to it.  PCL lowered an
# element assignment to a setf whose value is the stored VALUE: the s/// then
# wrote into the right-hand side (the read-only $2 -- "Modification of a
# read-only value attempted", which stopped YAML-Tiny 30_yaml_spec_tml.t and
# 31_local_tml.t) and `.=` / `+=` were lost.  The scalar spelling and
# `chomp($h{k} = ...)` were already right (rows 11-12 are those controls).
# Perl ($^X) is the oracle at test time; the row FAILS on ce665621.

use v5.30;
use strict;
use warnings;
use Test::More;
use File::Temp qw(tempdir);
use FindBin qw($RealBin);
use lib $RealBin;
use PCLCore;

my $project_root = "$RealBin/../..";
my $pl2cl        = "$project_root/pl2cl";
my $runtime      = "$project_root/cl/pcl-runtime.lisp";
my @sbcl_rt      = PCLCore::sbcl_prefix($runtime);

plan skip_all => "pl2cl not found" unless -x $pl2cl;
plan skip_all => "sbcl not found"  unless `which sbcl 2>/dev/null`;

my $workdir = tempdir(CLEANUP => 1);

sub both {
    my ($tag, $body) = @_;
    my $src = "$workdir/$tag.pl";
    open(my $s, '>', $src) or die; print {$s} $body; close $s;
    my $perl = `cd $workdir && $^X $src 2>&1`;
    my $cl_code = `cd $workdir && $pl2cl $src 2>/dev/null`;
    my $cl_file = "$workdir/$tag.lisp";
    open(my $c, '>', $cl_file) or die; print {$c} $cl_code; close $c;
    my $pcl = `cd $workdir && sbcl @sbcl_rt --load $cl_file 2>&1`;
    $pcl =~ s/^;.*\n//gm;
    $pcl =~ s/^PCL Runtime loaded\n//gm;
    $pcl =~ s/^\s*\n//gm;
    return ($perl, $pcl);
}

my ($perl, $pcl) = both('elem', <<'PL');
my %h; my @a; my $r = {}; my $ar = [];
($h{a} = "x") .= "y";             print "1 $h{a}\n";
($a[0] = 1) += 5;                 print "2 $a[0]\n";
($h{b} = "foo") =~ s/o/0/g;       print "3 $h{b}\n";
($a[1] = "abc") =~ tr/a-c/A-C/;   print "4 $a[1]\n";
($r->{c} = "p q") =~ s/ /_/;      print "5 $r->{c}\n";
($ar->[0] = "zz") =~ s/z/Z/;      print "6 $ar->[0]\n";
my $v = "src";
($h{e} = $v) =~ s/s/S/;           print "7 $h{e} $v\n";
my $i = 0;
($a[$i++] = "m") =~ s/m/M/;       print "8 $a[0] i=$i\n";
my $line = "--- warning: E_DUPKEY ";
my $block = {};
if ($line =~ /^--- +(\w+): +(.*?) *$/) {
    ($block->{$1} = $2) =~ s/^ *(.*?) *$/$1/;
}
print "9 ", join(",", map { "$_=$block->{$_}" } sort keys %$block), "\n";
($h{f} .= "q") =~ s/q/Q/;         print "10 $h{f}\n";
"ab" =~ /(b)/;
(my $m = $1) =~ s/b/B/;           print "11 $m\n";
chomp($h{d} = "line\n");          print "12 [$h{d}]\n";
PL
is($pcl, $perl, 'an element assignment used as an lvalue (=~ s///, tr///, .=, +=) edits the element');

# #4622 (s536d): the same fact in a USER-SUB ARGUMENT slot -- @_ aliases the
# assignment's left side, so a write through $_[0] in the callee lands in the
# element: named, `$`-prototype, method and coderef calls; compound and
# logical assignments; a deref element.  The scalar-variable rows (3-5) were
# already right (controls).  FAILS on 84a2e6ec.
($perl, $pcl) = both('arg', <<'PL');
sub setit { $_[0] = "via-alias" }
my %d2; setit($d2{k} = "orig"); print "1 $d2{k}\n";
my @a; sub setx { $_[0] = 9 } setx($a[0] = 1); print "2 $a[0]\n";
my $lx; setit($lx = "orig"); print "3 $lx\n";
our $g; setit($g = "orig"); print "4 $g\n";
my $n = 0; sub inc { $_[0]++ } inc($n = 5); print "5 $n\n";
my %h3 = (k => "a"); setit($h3{k} .= "b"); print "6 $h3{k}\n";
my @a4 = (1); setx($a4[0] += 5); print "7 $a4[0]\n";
my $r = {}; setit($r->{k} = "orig"); print "8 $r->{k}\n";
my %h5; setit(($h5{k} = "orig")); print "9 $h5{k}\n";
my $o = bless {}, 'O'; sub O::m { $_[1] = "meth" } $o->m($d2{m} = "x"); print "10 $d2{m}\n";
my $cr = \&setit; $cr->($d2{c} = "x"); print "11 $d2{c}\n";
sub two { $_[0] = "A"; $_[1] = "B" } my %h6; two($h6{a} = 1, $h6{b} = 2); print "12 $h6{a}$h6{b}\n";
my %h; setit($h{a} ||= "d"); setit($h{b} //= "d"); print "13 $h{a} $h{b}\n";
sub ro { my $x = shift; $x .= "!"; $x } print "14 ", ro($h{d} = "v"), " $h{d}\n";
sub rf { \$_[0] } my $rr = rf($h{e} = "e"); $$rr = "E"; print "15 $h{e}\n";
sub pr ($) { $_[0] = "P" } pr($h{f} = 1); print "16 $h{f}\n";
PL
is($pcl, $perl, 'an element assignment as a user-sub argument is aliased by @_ (#4622)');

# #4644 (s536f): the same two facts for a TIED element -- its box is a tie
# cell, whose store answered the stored COPY, so the callee's `$_[0] = ...`
# and the `=~ s///` / `++` on the assignment were lost (or died read-only).
# The value of an assignment to a box place is the BOX (p-setf's fallback, the
# compound store-back, `.=`).  Rows 8-10 are untied controls.  FAILS on c8886b61.
($perl, $pcl) = both('tied', <<'PL');
sub setit { $_[0] = "via-alias" } sub app { $_[0] .= "+" } sub inc { $_[0]++ }
tie my %t, 'TH'; $t{k} = "orig"; setit($t{k}); print "1 $t{k}\n";
setit($t{j} = "orig"); app($t{m} = "m"); app($t{m} .= "n"); print "2 $t{j} $t{m}\n";
$t{n} = 1; inc($t{n} += 2); $t{o} = 0; setit($t{o} ||= 5); setit($t{p} //= 6); print "3 $t{n} $t{o} $t{p}\n";
tie my @ta, 'TA'; setit($ta[1] = "b"); my $tr = \%t; setit($tr->{r} = "r"); print "4 $ta[1] $t{r}\n";
($t{s} = "abc") =~ s/b/B/; $t{u} = "ab"; ($t{u} .= "c") =~ s/a/A/; print "5 $t{s} $t{u}\n";
print "6 ", join(",", map { "$_=$TH::log{$_}" } sort keys %TH::log), "\n";
$t{v} = 1; ($t{v} += 1)++; print "7 $t{v}\n";   # its FETCH / STORE count is #4681's
my $x = "x"; my $y = ($x .= "y"); my $z = 1; my $w = ($z += 2); print "8 $x $y $z $w\n";
my %u = (k => 5); my $v = ($u{k} -= 1); print "9 $u{k} $v\n";
my %g; setit($g{a} = 1); setit($g{b} .= "q"); print "10 $g{a} $g{b}\n";
package TH; our %log;
sub TIEHASH { bless {}, shift } sub STORE { $log{store}++; $_[0]{$_[1]} = $_[2] } sub FETCH { $log{fetch}++; $_[0]{$_[1]} }
package TA;
sub TIEARRAY { bless [], shift } sub STORE { $_[0][$_[1]] = $_[2] } sub FETCH { $_[0][$_[1]] }
sub FETCHSIZE { scalar @{$_[0]} } sub STORESIZE { $#{$_[0]} = $_[1] - 1 } sub EXTEND {}
PL
is($pcl, $perl, 'an assignment to a TIED element is the element: @_ aliases it, =~ s/// and ++ edit it (#4644)');

# #4681 (s536i): `++` / `--` on an ASSIGNMENT evaluates the assignment ONCE and
# increments the box it answers -- the four incdec macros re-emitted the
# operand in their read and their store, so `($x += f())++` called f() three
# times (5, where perl says 3), and a tied element's FETCH / STORE ran 5 / 4
# times (perl 3 / 2).  FAILS on 0ac58419.
($perl, $pcl) = both('incdec-assign', <<'PL');
my $c = 0; sub f { $c++; 1 }
my %h = (v => 1); ($h{v} += f())++; print "1 $h{v} $c\n";
my $x = 1; ($x += f())++; print "2 $x $c\n";
my @a = (1); ++($a[0] += f()); print "3 $a[0] $c\n";
($h{w} = f())--; print "4 $h{w} $c\n";
my $y = 5; my $o = ($y .= "a")++; print "5 $y $o\n";
my $z = 2; my $p = --($z = 7); print "6 $z $p\n";
tie my %t, 'TH'; $t{v} = 1; ($TH::fe, $TH::st) = (0, 0);
($t{v} += 1)++; print "7 $t{v} $TH::fe $TH::st\n";
my $q = 3; my $r = ($q *= 2)--; print "8 $q $r\n";
package TH; our ($fe, $st) = (0, 0);
sub TIEHASH { bless {}, shift } sub STORE { $st++; $_[0]{$_[1]} = $_[2] } sub FETCH { $fe++; $_[0]{$_[1]} }
PL
is($pcl, $perl, '++ / -- on an assignment evaluates it once and increments its box (#4681)');

# #5466 (s543v): a SLICE as the operand of a modifying scalar operator (`++`
# `--` `op=` `||=` `.=` `undef`) is an LVALUE list -- every element vivified,
# the operator on the LAST one, its value the expression's -- through a
# reference, nested, in `@_` (the caller's variables) and tied (only the last
# key's FETCH / STORE); an empty slice dies read-only.  PCL acted on the
# rvalue slice's result: the `@seen{@list}++` key set stayed EMPTY.  Every row
# sorts its keys.  FAILS on 38420fae.
($perl, $pcl) = both('slice-op', <<'PL');
sub ks { my $h = shift; join(" ", map { "$_=" . ($h->{$_} // "u") } sort keys %$h) }
my %c; my $v = @c{qw(b a c)}++; print "1 ", ks(\%c), " v=$v\n";
my @a = (1, 1, 1); @a[0, 2]++; my $w = --@a[1, 0]; print "2 @a w=$w\n";
my %e; @e{qw(p q)} += 5; my %f = (q => 10); @f{qw(p q)} *= 3; print "3 ", ks(\%e), " | ", ks(\%f), "\n";
my %h; undef @h{qw(a b c)}; my %g = (a => 1, b => 2); undef @g{qw(a b)}; print "4 ", ks(\%h), " | ", ks(\%g), "\n";
my $r = [1, 2, 3]; @$r[1] += 10; @$r[2] -= 1; $r->@[0, 1] *= 2; print "5 @$r\n";
my %s; @s{qw(x y)} ||= 4; @s{qw(x z)} .= "q"; @s{qw(w y)} //= 7; print "6 ", ks(\%s), "\n";
my %n; @{$n{in}}{qw(a b)}++; my @k = qw(m n); my $calls = 0; sub kk { $calls++; @k } @n{kk()}++; print "7 ", ks($n{in}), " ", ks({ map { $_ => $n{$_} } @k }), " calls=$calls\n";
sub g { @_[0, 1] += 3 } my ($p, $q) = (1, 2); g($p, $q); print "8 $p $q\n";
my $err = eval { my @none = (); @h{@none}++; 1 } ? "no error" : ($@ =~ /^Modification of a read-only value attempted/ ? "read-only" : "other: $@");
print "9 $err\n";
tie my %t, 'TL'; @t{qw(a b)}++; @t{qw(c d)} += 2; undef @t{qw(e f)}; print "10 @TL::log\n";
my %m; my @r = (@m{qw(a b)}++); my $sum = (@m{qw(a b)} += 2); print "11 @r $sum ", ks(\%m), "\n";
package TL; our @log;
sub TIEHASH { bless {}, shift } sub FETCH { push @log, "F$_[1]"; $_[0]{$_[1]} }
sub STORE { push @log, "S$_[1]"; $_[0]{$_[1]} = $_[2] } sub EXISTS { push @log, "E$_[1]"; exists $_[0]{$_[1]} }
PL
is($pcl, $perl, 'a slice under ++ / op= / undef is an lvalue list: every element vivified, the last one modified (#5466)');

done_testing();
