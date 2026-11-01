#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later
# elem-alias-02.t — AN ELEMENT IN AN ALIASING SLOT IS THE ELEMENT, NOT A COPY
# (tasks #4086 + #3862, s532a; ir-spec §3.2d).
#
# perl aliases the loop variable of a foreach, map's / grep's $_ and each
# reference of `\(LIST)` to the list's ELEMENTS: `for ($_[3]) { s/^'// }`
# strips the caller's argument (Template::Grammar's META rule), `\($a[0],
# $h{k})` is two refs to the two slots.  PCL copied three spellings:
#   * an @_ element `$_[N]` as a foreach list -- its leading token is a
#     PPI::Token::Magic, and the single-scalar test asked ref-eq 'Symbol';
#     `for my $e ($_[0])` also escaped the callee's writes-through-@_ scan;
#   * an element in a MIXED foreach / map / grep list (`for ($_[0], "lit")`):
#     the runtime's ONE aliasing rewrite (%p-aliasing-slice-form) knew only
#     slices; an element is now a one-slot slice taken by the same box
#     accessor (p-aref-box / p-gethash-box / the -deref twins);
#   * each element of a parenthesised `\(E1, E2)` (the one-element `\(ELEM)`
#     was fixed in s526w2) -- one helper, _backslash_lvalue_form.
# Row 4 is the CONTROL the first cut broke (the everyday Archive::Tar row):
# a LIST SLICE `(lstat $p)[2,4]` uses the element head over a value form and
# a (vector ...) index -- it is a list, never one element.
# Every expected line is perl 5.40.3's output (scratch/s532a/p/g[123].pl, ls.pl).

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

plan tests => 4;

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

test_cl('a foreach over an @_ element aliases the caller (#4086)',
        <<'PL', <<'OUT');
my $s = sub { for ($_[3]) { s/^'//; s/'$//; s/\\'/'/g }; [ @_[1, 3] ] };
my @args = (0, 'author', '=', "'Andy Wardley'");
my $r = $s->(@args); print "@$r | $args[3]\n";
my $v = "pxq"; sub h { for ($_[0]) { s/x/z/ } } h($v); print "$v\n";
my $w = "pxq"; sub k { for my $e ($_[0]) { $e =~ s/x/z/ } } k($w); print "$w\n";
my $out = "o"; sub g2 { for ($_[0]) { return \$_ } } my $gr = g2($out); $$gr = "Z"; print "$out\n";
sub m2 { for ($_[0], $_[1]) { $_ = uc } } my ($p, $q) = ("p", "q"); m2($p, $q); print "$p$q\n";
my @f = (3); my %g = (z => 1); sub ar { for ($_[0]) { $_ = 4 } } ar($f[0]); ar($g{z}); print "$f[0] $g{z}\n";
sub md { for (my $i = 0; $i < 1; $i++) { } for ($_[0]) { $_ .= "!" } } my $md = "m"; md($md); print "$md\n";
PL
author Andy Wardley | Andy Wardley
pzq
pzq
Z
PQ
4 4
m!
OUT

test_cl('an element in a MIXED foreach / map / grep list is the element (#4086)',
        <<'PL', <<'OUT');
sub lit { for ($_[0], "lit") { $_ .= "+" if !ref } } my $l = "l"; eval { lit($l) }; print "$l ", ($@ ? "died" : "ok"), "\n";
my @b = (1, 2); my $y = 3; for ($b[0], "lit", $y) { $_ .= "+" if $_ ne "lit" } print "@b $y\n";
my @c = (7); for ($b[1], @c) { $_ .= "-" } print "@b @c\n";
my %h = (k => "v"); my @d = (1); for ($h{k}, @d) { $_ .= "*" } print "$h{k} @d\n";
my $rr = [1]; my $hh = {k => 1}; for ($rr->[0], $hh->{k}, @d) { $_ .= "!" } print "$rr->[0] $hh->{k} @d\n";
my $vr; for ($vr->[2], @d) { } print ref($vr), " ", scalar(@$vr), "\n";
my $vh; for ($vh->{z}, @d) { } print ref($vh), " ", join(",", keys %$vh), "\n";
my %n; for ($n{new}, @d) { } print join(",", keys %n), "\n";
my @aa = ([1, 2]); for ($aa[0][1], @d) { $_ = 9 if !ref } print "@{$aa[0]} @d\n";
my @rf = ([1,2], {a=>1}); for ($rf[0], @d) { print ref($_) || $_, "\n" }
my @e = (1, 2); $_ .= "m" for map { $_ } $e[0]; print "@e\n";
grep { $_ .= "g"; 1 } $e[1]; print "@e\n";
map { $_ .= "!" } $h{k}; print "$h{k}\n";
my %ev; my @x = map { 1 } $ev{zz}; print exists $ev{zz} ? "viv\n" : "noviv\n";
PL
l+ died
1+ 2 3+
1+ 2- 7-
v* 1*
1! 1! 1*!
ARRAY 3
HASH z
new
1 9 9
ARRAY
9
1 2
1 2g
v*!
viv
OUT

test_cl('each element of \(LIST) is referenced in place (#3862 / #4086)',
        <<'PL', <<'OUT');
my @a = (1, 2); my $ra = \($a[0]); $$ra = 9; print "@a\n";
sub f { my $x = \($_[0]); $$x = 7 } my $fv = 1; f($fv); print "$fv\n";
sub r1 { my $x = ( \( $_[0] ) )[0]; $$x .= "1" } my $rv = "a"; r1($rv); print "$rv\n";
my %h = (k => "v"); my @rs = \($a[0], $h{k}); ${$rs[0]} = "R"; ${$rs[1]} = "H"; print "@a $h{k}\n";
my $hr = {q => 1}; my $ar = [5]; my @rd = \($hr->{q}, $ar->[0]); ${$rd[0]} = 2; ${$rd[1]} = 6; print "$hr->{q} $ar->[0]\n";
my $x = "abc"; my @t = \($x, substr($x, 0, 1)); print join(",", map { ref } @t), "\n"; ${$t[1]} = "Z"; print "$x\n";
my @e = (1); my @refs = \(@e, $x); print scalar(@refs), " ", ref($refs[0]), "\n";
my @refs2 = \(@a); ${$refs2[1]} = 8; print scalar(@refs2), " @a\n";
my @mr = map { \$_ } @a; ${$mr[0]} = 0; print "@a\n";
sub t2 { my @r = \($_[0], $_[1]); ${$r[0]} .= "x"; ${$r[1]} .= "y" } my ($u, $w) = ("u", "w"); t2($u, $w); print "$u$w\n";
PL
9 2
7
a1
R 2 H
2 6
SCALAR,LVALUE
Zbc
2 ARRAY
2 R 8
0 8
uxwy
OUT

test_cl('a LIST SLICE in an aliasing consumer stays a list (control, #4086)',
        <<'PL', <<'OUT');
my @x = map { $_ } (lstat "/etc")[2,4]; print scalar(@x), "\n"; my $r = [1,2,3]; my @y = map { $_ * 2 } ($r->[0], @$r[1,2]); print "@y\n"; my @z = map { $_ } (10,20,30)[1,2]; print "@z\n"; for ((5,6,7)[0,2], 9) { print $_ } print "\n";
PL
2
2 4 6
20 30
579
OUT

