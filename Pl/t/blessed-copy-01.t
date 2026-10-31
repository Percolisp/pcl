#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later
# blessed-copy-01.t — A COPY OF A REFERENCE INTO A NEW ELEMENT IS A NEW
# CONTAINER (task #3824, s529a).
#
# perl's [$x] / @a = ($x) / push / unshift / splice copy the SCALAR: the
# element holds the same reference, in its own SV, so a later `$x = 8` does
# not reach the element and `$a[0] = 3` does not reach $x.  PCL's array
# construction arm (%p-array-store-scalar) stored the VARIABLE's box itself
# for two kinds of item -- a BLESSED box ("class must not be lost") and a
# scalar ref (box-in-box) -- so the element WAS the variable: URI's
# query_form (`$vals = [... : $vals]`) built a self-containing array, a
# Path::Tiny iterator loop (`while (my $x = $it->()) { push @d, $x }`) left
# every element holding the loop variable's last, false value (#3557).
# The fix routes every reference through ONE copy rule, %p-ref-container-copy
# (the same referent, the class slot and the is-ref flag on a fresh box),
# which the hash arm (%p-make-hash-entry) already used for globs.
#
# Every expected line is perl 5.40.3's output.  The #4000 rows (s530a): perl
# blesses the REFERENT, so a re-bless of an ARRAY / CODE object through one
# copy shows through every copy; PCL records the new class on the referent
# (*p-rebless-stash*).  NOT asserted (filed #4061): a copy taken BEFORE the
# referent's first bless, and a fresh `\@a` / `\&f` after `bless \@a`.  The
# #4001 row: a blessed qr// in an element keeps its class.  The #4060 row
# (s530c): every qr evaluation is a distinct Regexp, so a qr re-bless follows
# the #4000 referent rule (row 3 of its probe -- a copy taken before the
# FIRST bless -- is #4061's and not asserted).

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

plan tests => 8;

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

test_cl('a blessed reference copied into a new ARRAY element is a copy, every spelling',
        <<'PL', <<'OUT');
my $s = bless {}, 'K'; my $t = [$s]; $s = 8; print "anon ", ref($t->[0]), "\n";
my $o = bless {}, 'K'; my $r = $o; my @b = ($r); $r = 8; print "list-assign ", ref($b[0]), "\n";
my $v = bless {}, 'K'; $v = [$v]; print "self-wrap ", ref($v), " ", ref($v->[0]), "\n";
my @src = map { bless {n=>$_}, 'K' } 1..3; my $i = 0; my @d;
my $it = sub { $src[$i++] };
while (my $x = $it->()) { push @d, $x }
print "iterator-push ", join(",", map { ref($_) . ":" . $_->{n} } @d), "\n";
my $o2 = bless {}, 'K'; my @t3 = ($o2) x 3; $o2 = 0;
print "repeat ", join(",", map { ref } @t3), " ", ($t3[0] == $t3[1] ? "same" : "diff"), "\n";
my $o5 = bless {}, 'K'; my @c5 = ($o5);
print "identity ", (\$c5[0] == \$o5 ? "same-sv" : "own-sv"), " ", ($c5[0] == $o5 ? "same-ref" : "diff-ref"), "\n";
my $o6 = bless sub { 1 }, 'K'; my @c6 = ($o6); $o6 = 0; print "code ", ref($c6[0]), "\n";
my $o7 = bless \my $z, 'K'; my @c7 = ($o7); $o7 = 0; print "scalar-ref ", ref($c7[0]), "\n";
my $o8 = bless [], 'K'; my @c8; push @c8, $o8; $o8 = 0; print "push ", ref($c8[0]), "\n";
my $o9 = bless [], 'K'; my @c9; unshift @c9, $o9; $o9 = 0; print "unshift ", ref($c9[0]), "\n";
my $oa = bless [], 'K'; my @ca = (1,2); splice @ca, 1, 0, $oa; $oa = 0; print "splice ", ref($ca[1]), "\n";
my $oc = bless [], 'K'; my @cc = ($oc); $cc[0] = 3; print "write-element ", ref($oc), "\n";
sub f { return [@_] } my $oh = bless [], 'K'; my $rr = f($oh); $oh = 0; print "args ", ref($rr->[0]), "\n";
my $g = \*STDOUT; bless $g, 'K'; my @gg = ($g); $g = 0; print "glob ", ref($gg[0]), "\n";
my $o4 = bless {}, 'K'; my @c4 = ($o4); bless $o4, 'Other'; print "rebless-hash ", ref($c4[0]), "\n";
PL
anon K
list-assign K
self-wrap ARRAY K
iterator-push K:1,K:2,K:3
repeat K,K,K same
identity own-sv same-ref
code K
scalar-ref K
push K
unshift K
splice K
write-element K
args K
glob K
rebless-hash Other
OUT

test_cl('an UNBLESSED scalar reference copied into an element is a copy too',
        <<'PL', <<'OUT');
my $x = 1; my $r = \$x; my @a = ($r); $r = 5; print "list-assign ", ref($a[0]), "\n";
my $y = 2; my $rr = \\$y; my @b = ($rr); $rr = 0; print "ref-ref ", ref($b[0]), "\n";
my $s = 1; my $q = \$s; my @c; push @c, $q; $q = 0; print "push ", ref($c[0]), "\n";
my $q2 = \$s; my $t = [$q2]; $t->[0] = 7; print "write-element ", ref($q2), "\n";
my $z = 1; my $rz = \$z; my @rr = ($rz); ${$rr[0]} = 42; print "deref-write $z\n";
my $p = [1]; my @p = ($p); $p = 8; print "array-ref ", ref($p[0]), "\n";
PL
list-assign SCALAR
ref-ref REF
push SCALAR
write-element SCALAR
deref-write 42
array-ref ARRAY
OUT

test_cl('the hash construction arm keeps copying (the twin of the array arm)',
        <<'PL', <<'OUT');
my $h = bless [5], 'A'; my %hh = (k => $h); $h = 9; print "list ", ref($hh{k}), " $hh{k}[0]\n";
my $y = 2; my $r2 = \$y; my %h2 = (k => $r2); $r2 = 5; print "scalar-ref ", ref($h2{k}), "\n";
my $o = bless \my $z, 'K'; my %h3 = (k => $o); $o = 0; print "blessed-scalar-ref ", ref($h3{k}), "\n";
my $o3 = bless sub {1}, 'K'; my %h4 = (k => $o3); $o3 = 0; print "code ", ref($h4{k}), "\n";
my $ob = bless [], 'K'; my %hb; $hb{x} = $ob; $ob = 0; print "store ", ref($hb{x}), "\n";
my $g = \*STDOUT; bless $g, 'G'; my %hg = (k => $g); $g = 0; print "glob ", ref($hg{k}), "\n";
PL
list A 5
scalar-ref SCALAR
blessed-scalar-ref K
code K
store K
glob G
OUT

test_cl('a blessed PLAIN scalar (a bless \$x referent) copies its VALUE, unblessed',
        <<'PL', <<'OUT');
my $x = 5; bless \$x, 'K'; my @a = ($x); $a[0] = 7; print "value $x ", ref(\$a[0]), "\n";
my $y = 6; my $r = bless \$y, 'K'; my @b = ($$r); $b[0] = 9; print "deref $y\n";
my $o = bless [3], 'Q'; my @c = ($o);
print "ref ", (\$c[0] == \$o ? "same-sv" : "own-sv"), " ", ($c[0] == $o ? "same-ref" : "diff-ref"), "\n";
PL
value 5 SCALAR
deref 6
ref own-sv same-ref
OUT


test_cl('a RE-BLESS of an ARRAY / CODE referent shows through every copy (#4000)',
        <<'PL', <<'OUT');
my $o = bless [], 'P'; my @l = ($o); bless $o, 'Q'; print "1 ", ref $l[0], "\n";
my $c = bless sub {1}, 'P'; my @lc = ($c); bless $c, 'Q'; print "2 ", ref $lc[0], "\n";
my $s = bless [], 'P'; my $r = $s; bless $s, 'Q'; print "3 ", ref $r, "\n";
my $o3 = bless [], 'K'; my @c = ($o3); bless $c[0], 'Other'; print "6 ", ref($o3), "\n";
my $og = bless [], 'K'; my %hg = (a => $og); bless $og, 'O2'; print "7 ", ref($hg{a}), "\n";
my $sr = \my $sv; my $sr2 = $sr; bless $sr, 'Q'; print "9 ", ref $sr2, "\n";
my $t = [bless [], 'A']; my $e = $t->[0]; bless $t->[0], 'B'; print "10 ", ref $e, "\n";
my $ar = bless [], 'P'; my $ar2 = $ar; bless $ar2, 'Q'; print "11 ", ref $ar, " ", ("$ar" eq "$ar2" ? "same" : "diff"), "\n";
{ package Q; sub hi { "Q::hi" } } { package P; sub hi { "P::hi" } }
my $m = bless [], 'P'; my $m2 = $m; bless $m, 'Q'; print "12 ", $m2->hi, "\n";
my $m3 = bless sub {}, 'P'; my $m4 = $m3; bless $m3, 'Q'; print "13 ", $m4->hi, "\n";
my $x1 = bless [], 'P'; my $x2 = $x1; bless $x1, 'Q'; print "15 ", ("$x2" =~ /^Q=ARRAY/ ? "Q" : "$x2"), "\n";
sub mk { bless [], 'P' } my $z1 = mk(); my $z2 = $z1; bless $z1, 'Q'; print "17 ", ref $z2, "\n";
my $ua = bless [], 'P'; my $ub = $ua; bless $ua, 'P'; bless $ub, 'Q'; print "20 ", ref $ua, "\n";
PL
1 Q
2 Q
3 Q
6 Other
7 O2
9 Q
10 B
11 Q same
12 Q::hi
13 Q::hi
15 Q
17 Q
20 Q
OUT

test_cl('a re-bless reaches the other copies: dispatch, isa, overload, a local $_ copy; handles and globs keep their stash (#4000)',
        <<'PL', <<'OUT');
{ package Q; use overload '""' => sub { "Qobj" }, fallback => 1; sub hi { "Q::hi" } }
{ package P; sub hi { "P::hi" } }
my $fh; open $fh, '<', $0; my $g = $fh; bless $fh, 'P'; print "6 ", ref $g, "\n";
use Symbol; my $s = gensym; my $s2 = $s; bless $s, 'P'; print "7 ", ref $s2, "\n";
my $o = bless [], 'P'; print "8 ", ($o->isa('P') ? 1 : 0), ($o->can('hi') ? 1 : 0), "\n";
my $o2 = $o; bless $o, 'Q'; print "9 ", ($o2->isa('Q') ? 1 : 0), " $o2\n";
my $cr = bless sub { 42 }, 'P'; my $cr2 = $cr; bless $cr, 'Q'; print "10 ", $cr2->(), " ", ref $cr2, "\n";
my @st = ([1],[2]); bless $_, 'P' for @st; print "14 ", join(",", map { ref } @st), "\n";
my $q = bless [], 'P'; local $_ = $q; bless $q, 'Q'; print "15 ", ref $_, "\n";
PL
6 P
7 P
8 11
9 1 Qobj
10 42 Q
14 P,P
15 Q
OUT

test_cl('an ARRAY ELEMENT holding a BLESSED qr// reads back with its class (#4001)',
        <<'PL', <<'OUT');
my $o4 = bless qr/x/, 'K';
my @c4 = ($o4); print "1 ", ref($c4[0]), "\n";
my $t = [$o4];  print "2 ", ref($t->[0]), "\n";
my $c = $o4;    print "3 ", ref($c), "\n";
my %h = (k => $o4); print "4 ", ref($h{k}), "\n";
print "5 ", ("x" =~ $c4[0] ? "match" : "no"), " $c4[0]\n";
for my $e (@c4) { print "6 ", ref($e), "\n" }
my @m = map { ref } @c4; print "7 @m\n";
my $p = qr/y/; my @pp = ($p); print "8 ", ref($pp[0]), "\n";
{ package K; sub hi { "K::hi" } } print "10 ", $c4[0]->hi, "\n";
my $sh = shift @c4; print "11 ", ref($sh), "\n";
my @d = ($o4); my $pop = pop @d; print "12 ", ref($pop), "\n";
my ($f) = @{[ $o4 ]}; print "13 ", ref($f), "\n";
PL
1 K
2 K
3 K
4 K
5 match (?^:x)
6 K
7 K
8 Regexp
10 K::hi
11 K
12 K
13 K
OUT

test_cl('every qr// evaluation is a DISTINCT Regexp; a re-bless of one shows through its copies (#4060)',
        <<'PL', <<'OUT');
use Scalar::Util qw(refaddr);
my $r = bless qr/x/, 'K'; my $r3 = $r; bless $r, 'L'; print "1 ", ref($r3), "\n";
my @l = ($r); bless $r, 'M'; print "2 ", ref($l[0]), "\n";
my %h = (k => qr/h/); bless $h{k}, 'K'; print "4 ", ref($h{k}), "\n";
my @a; push @a, qr/z/ for 1..2; bless $a[0], 'K'; print "5 ", ref($a[0]), " ", ref($a[1]), "\n";
my $r2 = qr/$r/; print "6 ", ref($r2), "\n";
my $s = "ab"; my $q1 = qr/$s/; my $q2 = qr/$s/;
print "7 ", (refaddr($q1) == refaddr($q2) ? "same" : "distinct"), "\n";
my $q3 = qr/$q1/; print "8 ", (refaddr($q3) == refaddr($q1) ? "same" : "distinct"), "\n";
bless $q1, 'Z'; print "9 ", ref($q1), " ", ref($q2), " ", ref($q3), "\n";
print "10 ", ("xaby" =~ $q1 ? "m" : "n"), " $q1 $q3\n";
my $q4 = qr/$s/i; print "11 ", ("AB" =~ $q4 ? "m" : "n"), " $q4\n";
my @rs = map { qr/$_/ } qw(a b a); print "12 ", (refaddr($rs[0]) == refaddr($rs[2]) ? "same" : "distinct"), "\n";
my $cnt = 0; for my $i (1..3) { my $t = qr/a$i/; $cnt++ if "a2" =~ $t } print "13 $cnt\n";
my $g = 0; my $gs = "aXaXa"; my $gq = qr/$s/; $g++ while "abab" =~ /$gq/g; print "14 $g\n";
PL
1 L
2 M
4 K
5 K Regexp
6 Regexp
7 distinct
8 distinct
9 Z Regexp Regexp
10 m (?^:ab) (?^:ab)
11 m (?^i:ab)
12 distinct
13 1
14 2
OUT
