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

done_testing();
