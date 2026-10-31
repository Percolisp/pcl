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
# Every expected line is perl 5.40.3's output.  NOT asserted here (filed):
# a RE-BLESS of an ARRAY / CODE referent through one copy is not seen by the
# others (the class of a non-hash referent lives on each container), and an
# element holding a BLESSED qr// reads back as Regexp.

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
