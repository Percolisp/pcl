#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# perf-levers-06.t — the round-36 levers (s499f, docs/faster-codegen-suggestions.md
# §0.2s), guarded the way perf-levers-05.t guards round 34's: every lever is
# RUNTIME-only (`pl2cl`'s output is unchanged), so a transpile grep can say
# nothing.  What can be asserted is (a) the MECHANISM — the named helpers exist
# and the macro takes the new shape, which is what FAILS on a pre-s499f tree —
# and (b) that every shape a lever can be handed still answers perl 5.40.3's
# answer (the ANSWER rows pass on the old tree too: they are the correctness
# NET for a pure speed lever, the s473v rule).
#
#   #2424 whole-hash copy `%a = %b': a table source is copied key by key
#     (%p-hash-fill-from-hash) instead of being flattened into a vector of fresh
#     key BOXES and re-read; and `my %h = %src' binds a table PRESIZED to the
#     source's count (%p-let-presize / %p-make-hash-like) instead of one that
#     regrows from empty on every copy.
#   #2425 `**': a small-integer arm (%p-pow-small) answers the (signed-byte 32)
#     base, [0,64] exponent case exactly as %p-pow-int spells it -- an NV for a
#     power-of-2 base, an exact integer otherwise -- on FIXNUM arithmetic, in
#     front of the generic path, which keeps every case outside those bounds.
#
# EVERY EXPECTED LINE BELOW IS PERL 5.40.3's OWN OUTPUT, probed by running the
# same program under perl (scratch/s499f/probes/ in the s499f worktree).
use v5.30;
use strict;
use warnings;
use Test::More;
use File::Temp qw(tempfile);
use FindBin qw($RealBin);
use lib $RealBin;
use lib "$RealBin/../..";
use PCLCore;

my $project_root = "$RealBin/../..";
my $pl2cl   = "$project_root/pl2cl";
my $runtime = "$project_root/cl/pcl-runtime.lisp";
my @sbcl_rt = PCLCore::sbcl_prefix($runtime);
plan skip_all => "pl2cl not found" if !-x $pl2cl;
plan skip_all => "sbcl not found"  if !`which sbcl 2>/dev/null`;

sub run_pl {
    my ($src) = @_;
    my ($fh, $file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $src;
    close $fh;
    my $cl = PCLCore::transpile(qq{$pl2cl $file});
    my ($cfh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cfh $cl;
    close $cfh;
    my $out = `sbcl @sbcl_rt --load $cl_file 2>/dev/null`;
    $out =~ s/^;.*\n//gm;
    $out =~ s/^(?:caught |compilation unit|-->|==>|PCL Runtime loaded).*\n//gm;
    return $out;
}

sub run_lisp {
    my ($src) = @_;
    my ($fh, $file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $fh $src;
    close $fh;
    return scalar `sbcl @sbcl_rt --load $file 2>&1`;
}

# Every line of EXPECTED must appear, whole, in OUT: one row per line.
sub lines_like {
    my ($out, $expected, $what) = @_;
    for my $line (split /\n/, $expected) {
        my $q = quotemeta $line;
        like($out, qr/^$q$/m, "$what: $line");
    }
}

# ─────────────────────────────────────────────────────────────────────────────
# THE MECHANISM — read out of the loaded runtime itself.  %p-make-hash-entry is
# the CONTROL: it predates this batch, so a probe that answered NIL for
# everything could not pass silently.
# ─────────────────────────────────────────────────────────────────────────────
my $mech = run_lisp(<<'LISP');
(in-package :pcl)
(format t "hfill ~a~%"   (and (fboundp '%p-hash-fill-from-hash) t))
(format t "hlike ~a~%"   (and (fboundp '%p-make-hash-like) t))
(format t "control ~a~%" (and (fboundp '%p-make-hash-entry) t))
(format t "presize ~a~%"
        (and (search "%p-make-hash-like"
                     (string-downcase (prin1-to-string
                      (macroexpand-1 '(p-let ((%c :hash (make-hash-table :test 'equal)))
                                        (p-hash-= %c %b))))))
             t))
(format t "nopresize-sibling ~a~%"
        (and (search "%p-make-hash-like"
                     (string-downcase (prin1-to-string
                      (macroexpand-1 '(p-let ((%a :hash (make-hash-table :test 'equal))
                                              (%b :hash (make-hash-table :test 'equal)))
                                        (p-hash-= %b %a))))))
             t))
(format t "nopresize-list ~a~%"
        (and (search "%p-make-hash-like"
                     (string-downcase (prin1-to-string
                      (macroexpand-1 '(p-let ((%c :hash (make-hash-table :test 'equal)))
                                        (p-hash-= %c (vector 1 2)))))))
             t))
(format t "size ~a~%"
        (let ((src (make-hash-table :test 'equal)))
          (dotimes (i 5000) (setf (gethash (format nil "k~d" i) src) i))
          (>= (hash-table-size (%p-make-hash-like src)) 5000)))
LISP

like($mech, qr/^hfill T$/mi,   '#2424: %p-hash-fill-from-hash exists: a table source is copied key by key');
like($mech, qr/^hlike T$/mi,   '#2424: %p-make-hash-like exists: the presized declaration table');
like($mech, qr/^control T$/mi, 'control: %p-make-hash-entry is fbound on every tree');
like($mech, qr/^presize T$/mi, '#2424: `my %c = %b` binds a table presized to %b');
like($mech, qr/^nopresize-sibling NIL$/mi,
     '#2424: a source bound by the SAME p-let is not a size hint (it is the new empty table)');
like($mech, qr/^nopresize-list NIL$/mi, '#2424: a list source keeps the fresh table');
like($mech, qr/^size T$/mi,    '#2424: the presized table holds the source count without regrowing');

# ─────────────────────────────────────────────────────────────────────────────
# THE ANSWERS — #2424: every shape a whole-hash copy can take.
# ─────────────────────────────────────────────────────────────────────────────
my $hcopy = run_pl(<<'PERL');
use strict; use warnings;
sub show { my ($n, $h) = @_; print "$n: ", join(",", map { "$_=" . (defined $h->{$_} ? $h->{$_} : "U") } sort keys %$h), " n=", scalar(keys %$h), "\n" }
my %b = (1 => "one", 2.5 => 2.5, "" => "empty", u => undef, "\x{263a}" => "smile", k => [1,2], r => \"s");
my %a = %b;
print "keys: ", scalar(keys %a), " smile=", $a{"\x{263a}"}, " empty=", $a{""}, " u=", (defined $a{u} ? "def" : "undef"), "\n";
print "same aref: ", ($a{k} == $b{k} ? "yes" : "no"), " scalar ref: ", ${$a{r}}, "\n";
print "alias: ", (\$a{1} == \$b{1} ? "yes" : "no"), "\n";
$a{1} = "changed"; print "b1 after write a1: $b{1}\n";
$b{2.5}++; print "a2.5 after b++: $a{2.5}\n";
my %h = (x => 1, y => 2);
%h = (%h, z => 3); show("h+z", \%h);
my $r1 = { p => 1, q => 2 }; my $r2 = { old => 9 };
%$r2 = %$r1; show("r2", $r2); $r2->{p} = 7; print "r1 p: $r1->{p}\n";
my %s = (m => 1, n => 2); my $sref = \$s{m};
%s = %s; show("self", \%s); $$sref = 99; print "self after old-ref write: $s{m}\n";
my %e; my %f = %e; print "empty: ", scalar(keys %f), "\n";
my %big = map { ("k$_" => $_) } 1 .. 5000;
my %c; my $cr = \%c; %c = %big; print "big: ", scalar(keys %c), " via ref ", scalar(keys %$cr), " k4999=$c{k4999}\n";
for my $i (1 .. 3) { my %t = %big; $t{"k$i"} = -1; print "loop $i: $t{\"k$i\"} $big{\"k$i\"} ", scalar(keys %t), "\n" }
my $cnt = (%a = %b); print "scalar assign count: $cnt\n";
my @l = (%a = %h); print "list assign n: ", scalar(@l), "\n";
{ package Obj; sub new { bless {v => $_[1]}, $_[0] } }
my %o = (o => Obj->new(5)); my %o2 = %o; print "obj: ", ref($o2{o}), " ", $o2{o}{v}, " same=", ($o2{o} == $o{o} ? 1 : 0), "\n";
my %src = (x => 2, y => 5); my %d = (x => 1, gone => 1); %d = %src; show("d", \%d);
$_++ foreach values %d; show("d++", \%d); show("src", \%src);
my %n = (a => 1); my %m = %n; $m{a} .= "z"; print "n a: $n{a} m a: $m{a}\n";
our %g = (g => 1); our %g2; %g2 = %g; print "our: $g2{g}\n";
my %num = (10 => 1); my %num2 = %num; $num2{10} += 0.5; print "num: $num{10} $num2{10}\n";
my %rawnum = (a => 1.5, b => "7"); my %rn2 = %rawnum; $rn2{b} .= "x"; print "rn: $rawnum{b} $rn2{b} ", $rn2{a} * 2, "\n";
my $obj = bless { f => 1 }, 'Obj'; my %plain = %$obj; print "from blessed: ", join(",", sort keys %plain), " ", (ref(\%plain)), "\n";
PERL

lines_like($hcopy, <<'EXPECTED', '#2424 hash copy');
keys: 7 smile=smile empty=empty u=undef
same aref: yes scalar ref: s
alias: no
b1 after write a1: one
a2.5 after b++: 2.5
h+z: x=1,y=2,z=3 n=3
r2: p=1,q=2 n=2
r1 p: 1
self: m=1,n=2 n=2
self after old-ref write: 1
empty: 0
big: 5000 via ref 5000 k4999=4999
loop 1: -1 1 5000
loop 2: -1 2 5000
loop 3: -1 3 5000
scalar assign count: 14
list assign n: 6
obj: Obj 5 same=1
d: x=2,y=5 n=2
d++: x=3,y=6 n=2
src: x=2,y=5 n=2
n a: 1 m a: 1z
our: 1
num: 1 1.5
rn: 7 7x 3
from blessed: f HASH
EXPECTED


# ─────────────────────────────────────────────────────────────────────────────
# #2425 `**': the small-integer arm.  MECHANISM: %p-pow-small exists and
# answers exactly %p-pow-int's spelling inside its bounds -- an NV for a
# power-of-2 base, an integer otherwise -- and NIL outside them, so those cases
# keep %p-pow-int.
# ─────────────────────────────────────────────────────────────────────────────
my $powmech = run_lisp(<<'LISP');
(in-package :pcl)
(format t "psmall ~a~%" (and (fboundp '%p-pow-small) t))
(dolist (c '((2 10) (-2 3) (0 0) (0 5) (1 64) (-1 7) (3 5) (-3 3) (7 19) (3 40) (2 53) (2 52) (1024 5) (10 18) (10 19)))
  (format t "ps[~a ~a] ~s ~s~%" (first c) (second c)
          (%p-pow-small (first c) (second c))
          (%p-pow-int (abs (first c)) (second c) (minusp (first c)))))
LISP
like($powmech, qr/^psmall T$/mi, '#2425: %p-pow-small exists: the small-integer arm of **');
for my $r (['2 10',    '1024.0 1024.0',  'a power-of-2 base answers pp_pow\'s NV'],
           ['-2 3',    '-8.0 -8.0',      'a negative power-of-2 base, odd exponent'],
           ['0 0',     '1.0 1.0',        '0**0 is the NV 1'],
           ['0 5',     '0.0 0.0',        '0**N is the NV 0'],
           ['1 64',    '1.0 1.0',        '1 is a power of 2 in pp_pow\'s test'],
           ['-1 7',    '-1.0 -1.0',      '-1 to an odd power'],
           ['3 5',     '243 243',            'any other base is an exact integer'],
           ['-3 3',    '-27 -27',            'a negative base, odd exponent'],
           ['7 19',    '11398895185373143 11398895185373143', 'an exact integer that still fits a fixnum (57 bits)'],
           ['3 40',    'NIL NIL',            'past the UV bound: NIL on both, pow() answers'],
           ['2 53',    'NIL 9.007199254740992e15', 'a power-of-2 NV at 2**53: NIL, the squaring loop answers'],
           ['2 52',    '4.503599627370496e15 4.503599627370496e15', 'the last exact power-of-2 NV'],
           ['1024 5',  '1.125899906842624e15 1.125899906842624e15', 'a large power-of-2 base'],
           ['10 18',   'NIL NIL', '10**18 is past both bounds (72 bits): pow() answers'],
           ['10 19',   'NIL NIL', '10**19 likewise']) {
    my ($args, $want, $desc) = @$r;
    my $q = quotemeta "ps[$args] $want";
    like($powmech, qr/^$q$/mi, "#2425 %p-pow-small $args: $desc");
}

my $pow = run_pl(<<'PERL');
no warnings;
my @e = (0, 1, 2, 3, 5, 19, 31, 52, 53, 62, 63, 64, 65, -1, 0.5);
for my $b (0, 1, -1, 2, -2, 3, -3, 7, -7, 10, 1024, 2147483647, -2147483648, 1.5, "3", "-2", " 4 ", "abc", undef) {
  print "b=", (defined $b ? $b : "U"), ": ", join(" ", map { $b ** $_ } @e), "\n";
}
print "2**0.5=", 2 ** 0.5, "\n";
print "(-8)**(1/3)=", (-8) ** (1/3), "\n";
print "2**64=", 2 ** 64, " 2**63=", 2 ** 63, " 2**52=", 2 ** 52, " 2**53=", 2 ** 53, "\n";
print "7**19=", 7 ** 19, " 3**40=", 3 ** 40, " 3**39=", 3 ** 39, " 5**27=", 5 ** 27, "\n";
print "\"1e3\"**2=", "1e3" ** 2, "\n";
print "0**-1=", 0 ** -1, " 0**0=", 0 ** 0, "\n";
my $x = 3; $x **= 4; print "**= $x\n"; $x **= 0.5; print "**=f $x\n";
my $s = "5"; my $t = $s ** 2; print "str operand kept: $s $t\n";
my $n = -3; print "neg var: ", $n ** 3, " ", $n ** 2, " ", -3 ** 2, "\n";
{ package OV; use overload '**' => sub { my ($a, $b, $sw) = @_; "OV(" . ($sw ? "$b**$a->[0]" : "$a->[0]**$b") . ")" }, '""' => sub { "ov" };
  sub new { bless [$_[1]], $_[0] } }
my $o = OV->new(2); print "overload: ", $o ** 3, " ", 3 ** $o, "\n";
{ package N0; use overload '0+' => sub { 4 }, fallback => 1; sub new { bless {}, $_[0] } }
my $z = N0->new; print "numify overload: ", $z ** 2, "\n";
my @d = map { $_ ** 5 } split '', 4150; my $sum = 0; $sum += $_ for @d; print "digits: @d sum=$sum\n";
print "big: ", 10 ** 15, " ", 10 ** 16, " ", 10 ** 20, " ", (-10) ** 19, "\n";
print "concat: ", (2 ** 10) . "|" . (3 ** 3) . "|" . (2 ** -1), "\n";
PERL

lines_like($pow, <<'EXPECTED', '#2425 **');
b=0: 1 0 0 0 0 0 0 0 0 0 0 0 0 Inf 0
b=1: 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
b=-1: 1 -1 1 -1 -1 -1 -1 1 -1 1 -1 1 -1 -1 NaN
b=2: 1 2 4 8 32 524288 2147483648 4.5035996273705e+15 9.00719925474099e+15 4.61168601842739e+18 9.22337203685478e+18 1.84467440737096e+19 3.68934881474191e+19 0.5 1.4142135623731
b=-2: 1 -2 4 -8 -32 -524288 -2147483648 4.5035996273705e+15 -9.00719925474099e+15 4.61168601842739e+18 -9.22337203685478e+18 1.84467440737096e+19 -3.68934881474191e+19 -0.5 NaN
b=3: 1 3 9 27 243 1162261467 617673396283947 6.46108188922667e+24 1.938324566768e+25 3.81520424476946e+29 1.14456127343084e+30 3.43368382029251e+30 1.03010514608775e+31 0.333333333333333 1.73205080756888
b=-3: 1 -3 9 -27 -243 -1162261467 -617673396283947 6.46108188922667e+24 -1.938324566768e+25 3.81520424476946e+29 -1.14456127343084e+30 3.43368382029251e+30 -1.03010514608775e+31 -0.333333333333333 NaN
b=7: 1 7 49 343 16807 11398895185373143 1.57775382034846e+26 8.81247870897232e+43 6.16873509628062e+44 2.48930711762415e+52 1.74251498233691e+53 1.21976048763584e+54 8.53832341345085e+54 0.142857142857143 2.64575131106459
b=-7: 1 -7 49 -343 -16807 -11398895185373143 -1.57775382034846e+26 8.81247870897232e+43 -6.16873509628062e+44 2.48930711762415e+52 -1.74251498233691e+53 1.21976048763584e+54 -8.53832341345085e+54 -0.142857142857143 NaN
b=10: 1 10 100 1000 100000 1e+19 1e+31 1e+52 1e+53 1e+62 1e+63 1e+64 1e+65 0.1 3.16227766016838
b=1024: 1 1024 1048576 1073741824 1.12589990684262e+15 1.56927543384667e+57 2.08592483976651e+93 3.4323988300653e+156 3.51477640198687e+159 4.35108243715496e+186 4.45550841564668e+189 4.5624406176222e+192 4.67193919244513e+195 0.0009765625 32
b=2147483647: 1 2147483647 4611686014132420609 9.90352030044798e+27 4.56719260602525e+46 2.02613063094135e+177 1.9490627741443e+289 Inf Inf Inf Inf Inf Inf 4.6566128752458e-10 46340.950001052
b=-2147483648: 1 -2147483648 4.61168601842739e+18 -9.90352031428304e+27 -4.56719261665907e+46 -2.02613064886767e+177 -1.94906280228e+289 Inf -Inf Inf -Inf Inf -Inf -4.65661287307739e-10 NaN
b=1.5: 1 1.5 2.25 3.375 7.59375 2216.8378200531 287626.588849326 1434648375.48161 2151972563.22242 82729054613.0993 124093581919.649 186140372879.473 279210559319.21 0.666666666666667 1.22474487139159
b=3: 1 3 9 27 243 1162261467 617673396283947 6.46108188922667e+24 1.938324566768e+25 3.81520424476946e+29 1.14456127343084e+30 3.43368382029251e+30 1.03010514608775e+31 0.333333333333333 1.73205080756888
b=-2: 1 -2 4 -8 -32 -524288 -2147483648 4.5035996273705e+15 -9.00719925474099e+15 4.61168601842739e+18 -9.22337203685478e+18 1.84467440737096e+19 -3.68934881474191e+19 -0.5 NaN
b= 4 : 1 4 16 64 1024 274877906944 4.61168601842739e+18 2.02824096036517e+31 8.11296384146067e+31 2.12676479325587e+37 8.50705917302346e+37 3.40282366920938e+38 1.36112946768375e+39 0.25 2
b=abc: 1 0 0 0 0 0 0 0 0 0 0 0 0 Inf 0
b=U: 1 0 0 0 0 0 0 0 0 0 0 0 0 Inf 0
2**0.5=1.4142135623731
(-8)**(1/3)=NaN
2**64=1.84467440737096e+19 2**63=9.22337203685478e+18 2**52=4.5035996273705e+15 2**53=9.00719925474099e+15
7**19=11398895185373143 3**40=1.21576654590569e+19 3**39=4.05255515301898e+18 5**27=7.45058059692383e+18
"1e3"**2=1000000
0**-1=Inf 0**0=1
**= 81
**=f 9
str operand kept: 5 25
neg var: -27 9 -9
overload: OV(2**3) OV(3**2)
numify overload: 16
digits: 1024 1 3125 0 sum=4150
big: 1000000000000000 10000000000000000 1e+20 -1e+19
concat: 1024|27|0.5
EXPECTED

done_testing();
