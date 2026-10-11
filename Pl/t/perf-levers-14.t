#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# perf-levers-14.t — perf round 46 (s515b, docs/speed-review-s509.md §2
# "In round 46").  MECHANISM rows (the runtime's own answer) and ANSWER rows:
# programs whose expected output below IS perl's own (probed on 5.40.3;
# nothing in them needs perl >= 5.38).
#
#   #3122 a map block whose VALUE is a literal list of two or more items
#         (`map { ($_, $_ * 2) } ...') hands the list over as a CL list to
#         p-map-flat (a compiler macro on p-map, %p-map-list-block), which
#         spreads each item by the argument-list rule and copies it -- no
#         p-flatten-args vector per iteration.
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

sub src_file {
    my ($src) = @_;
    my ($fh, $file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $src;
    close $fh;
    return $file;
}

sub transpile { my ($src) = @_; return PCLCore::transpile(qq{$pl2cl } . src_file($src)) }

sub run_pl {
    my ($src) = @_;
    my $cl = transpile($src);
    my ($cfh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cfh $cl;
    close $cfh;
    my $out = `sbcl @sbcl_rt --load $cl_file 2>&1`;
    $out =~ s/^;.*\n//gm;
    $out =~ s/^(?:caught |compilation unit|-->|==>|PCL Runtime loaded).*\n//gm;
    $out =~ s/^\s*\n//gm;
    return $out;
}

sub answers {
    my ($src, $expected, $what) = @_;
    is(run_pl($src), $expected, $what);
}

# The runtime's own answers to a few forms (the MECHANISM rows).
sub lisp_out {
    my ($forms) = @_;
    my ($lfh, $lfile) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $lfh "(in-package :pcl)\n$forms";
    close $lfh;
    return scalar `sbcl @sbcl_rt --load $lfile 2>&1`;
}


# ─────────────────────────────────────────────────────────────────────────────
# #3122 MECHANISM: the compiler macro hands a block whose tail is a literal
# list of 2+ items (also under let / let* / progn) to p-map-flat; a one-item
# list, a `block' tail and a non-list tail stay p-map.
like(lisp_out(<<'END_LISP'),
(let ((*print-pretty* nil))
  (format t "~S" (mapcar (lambda (f) (car (funcall (compiler-macro-function 'p-map) f nil)))
                         (list '(p-map (lambda ($_) (p-flatten-args (list $_ (p-* $_ 2)))) (p-.. 1 3))
                               '(p-map (lambda ($_) (let ((x 1)) (p-flatten-args (list $_ x)))) @a)
                               '(p-map (lambda ($_) (p-flatten-args (list $_))) @a)
                               '(p-map (lambda ($_) (block nil (p-flatten-args (list $_ 1)))) @a)
                               '(p-map (lambda ($_) (p-* $_ 2)) @a)))))
END_LISP
     qr/\(P-MAP-FLAT P-MAP-FLAT P-MAP P-MAP P-MAP\)/i,
     '#3122: a literal-list map block (2+ items, through let) -> p-map-flat; one item, a block tail, a scalar tail stay p-map');

answers(<<'END_SRC', <<'END_EXP', '#3122 answers: list-valued map blocks -- pairs, aliasing write-through, copies per iteration, arrays / hashes / empty lists spread inside the list, refs to $_, list context inside, nested map, scalar-context count');
my @a = (1,2,3); my %h = (a=>1,b=>2); my @k = qw(x y);
sub sh { join(",", map { defined $_ ? $_ : "U" } @_) . "\n" }
print sh(map { ($_, $_*2) } @a);
print sh(map { $_ } @a);
print sh(map { $_ * 2 } @a);
print sh(map { ($_) x 3 } @a);
print "E:", sh(map { () } @a), "[empty]\n";
print sh(map { my @x = ($_); @x } @a);
print sh(map { $h{$_} } qw(a b c));
print sh(map { if ($_ > 1) { ($_, "big") } } @a);
print sh(map { my $o = $_; (map { ($o, $_) } 1..2) } @a);
my $n = map { ($_, $_) } @a; print "count $n\n";
print sh(map { ($_, 1) } (1, undef, 3));
print sh(map { ($_, $_ + 1) } grep { $_ % 2 } 1..6);
my %m = map { $_ => 1 } @k; print join(",", map { "$_=$m{$_}" } sort keys %m), "\n";
print sh(map { ${$_}[0] } map { [$_] } @a);
print sh(map { s/a/b/r } qw(abc aaa));
my @w = (1,2,3); my @r = map { $_ *= 2; ($_, 0) } @w; print sh(@w), sh(@r);
my @v = (1,2); $_++ for map { ($_, $_) } @v; print sh(@v);
my $i = 0; my @c = map { ($_, ++$i) } qw(a b c); print sh(@c);
my @y = (5,6); my @z = map { (@y, $_) } 1..2; print sh(@z);
my @hh = map { (%h, $_) } 1; print scalar(@hh), "\n";
my @rr = map { (\$_, 1) } @a; ${$rr[0]} = 99; print sh(@a);
sub ctx { wantarray ? "L" : "S" } print sh(map { (ctx(), $_) } 1..2);
my @s = map { ("$_", length $_) } qw(ab c); print sh(@s);
my @e = map { my $t = $_ * 3; ($t, -$t) } 1..2; print sh(@e);
print +(map { +{ $_ => 1 } } 1..1)[0] =~ /HASH/ ? "hashref\n" : "no\n";
my @u = map { ($_, undef) } 1..2; print scalar(@u), sh(@u);
my $j = 0; my @acc = map { ($j .= "x", $_) } 1..2; print sh(@acc);
my @ee = map { (@{[]}, $_) } 1..2; print sh(@ee);
print "Z:", sh(map { ($_, $_) } ());
END_SRC
1,2,2,4,3,6
1,2,3
2,4,6
1,1,1,2,2,2,3,3,3
E:
[empty]
1,2,3
1,2,U
,2,big,3,big
1,1,1,2,2,1,2,2,3,1,3,2
count 6
1,1,U,1,3,1
1,2,3,4,5,6
x=1,y=1
1,2,3
bbc,baa
2,4,6
2,0,4,0,6,0
1,2
a,1,b,2,c,3
5,6,1,5,6,2
5
99,2,3
L,1,L,2
ab,2,c,1
3,-3,6,-6
hashref
41,U,2,U
0x,1,0xx,2
1,2
Z:
END_EXP

# MEMBER2

done_testing();
