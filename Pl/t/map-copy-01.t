#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later
# map-copy-01.t — MAP COPIES A REFERENCE INTO A FRESH CONTAINER
# (task #4084, s532a; ir-spec §2.6).
#
# perl's pp_mapwhile COPIES every value a map block returns that is not a
# temporary, so `map { $b = $src->{$_}; ($_ => $b) } KEYS` stores each
# iteration's reference, never the variable.  PCL's %p-map-copy-scalar
# copied a number or a string but handed a REFERENCE-holding box on as it
# was, so every result was the variable and read its LAST value
# (Template::Context's BLOCKS table: every block was the last one).  The
# copy is the #3824 rule, %p-ref-container-copy: the same referent, class
# and is-ref flag in a fresh container (identity, `ref` and blessing kept).
# grep and sort return ALIASES and are the controls.  Every expected line
# is perl 5.40.3's output (scratch/s532a/p/mc1.pl).

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

plan tests => 1;

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

test_cl('a reference returned from a map block is a copy, not the variable (#4084)',
        <<'PL', <<'OUT');
my $b;
my $src = { a => sub { "A" }, b => sub { "B" }, c => [1] };
my $h = { map { $b = $src->{$_}; ($_ => $b) } sort keys %$src };
print join(",", map { "$_=" . ref($h->{$_}) } sort keys %$h), "\n";
my @l = map { $b = $src->{$_}; $b } qw(a b);
print join(",", map { $_->() } @l), "\n";
my $o1 = bless {n => 1}, 'K'; my $o2 = bless {n => 2}, 'K'; my $c;
my @bo = map { $c = $_; $c } $o1, $o2; print join(",", map { ref($_) . $_->{n} } @bo), " ", ($bo[0] == $o1 ? "same" : "diff"), "\n";
my @a = ([1], [2]); my @m = map { $_ } @a; $a[0] = "x"; print ref($m[0]), " $m[0][0]\n";
my $ar = [ map { $c = [$_]; $c } 1 .. 3 ]; print join(",", map { $_->[0] } @$ar), "\n";
my %hh = map { $c = \$_; ($_ => $c) } qw(p q); print join(",", map { ${$hh{$_}} } sort keys %hh), "\n";
sub lm { my $t; return map { $t = { v => $_ }; $t } @_ } my @lr = lm(4, 5); print join(",", map { $_->{v} } @lr), "\n";
my $cnt = () = map { $c = [$_]; $c } 1..4; print "$cnt\n";
my $sc = map { $c = [$_]; $c } 1..4; print "$sc\n";
my @g = (1, 2); $_++ for grep { 1 } @g; print "@g\n";
my @rr = ([1], [2]); my $t; my @gg = map { $t = $_; ($t, $t) } @rr; print scalar(@gg), " ", ($gg[0] == $rr[0] ? "same" : "diff"), " ", ($gg[2] == $rr[1] ? "same" : "diff"), "\n";
my $qr; my @qs = map { $qr = qr/$_/; $qr } qw(x y); print join(",", map { "z$_" =~ $_ ? 1 : 0 } @qs), " @qs\n";
PL
a=CODE,b=CODE,c=ARRAY
A,B
K1,K2 same
ARRAY 1
1,2,3
p,q
4,5
4
4
2 3
4 same same
1,1 (?^:x) (?^:y)
OUT

