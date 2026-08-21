#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# interp-chained-subscript-01.t — string interpolation of chained subscripts with
# an IMPLICIT arrow: "$h->{a}[1]" == $h->{a}->[1], "$a->[1][0]" == $a->[1]->[0],
# "$h->{a}{b}{c}" == $h->{a}->{b}->{c}.
#
# Session 218: the interpolation arrow-deref chain loop in Pl/PExpr/StringInterpolation.pm
# only continued on an EXPLICIT '->', so after the first subscript the chained
# bracket was left as literal text ("$h->{a}[1]" -> "ARRAY(0x..)[1]"). Fixed to accept
# either an explicit arrow or a bare bracket between subscripts.

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

plan tests => 10;

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

# hashref then implicit array subscript
test_cl('"$h->{a}[1]"', q{my $h={a=>[1,2,3]}; print "$h->{a}[1]\n";}, "2\n");
# arrayref then implicit array subscript
test_cl('"$a->[1][0]"', q{my $a=[[1,2],[3,4]]; print "$a->[1][0]\n";}, "3\n");
# three chained hash subscripts, implicit arrows
test_cl('"$h->{a}{b}{c}"', q{my $h={a=>{b=>{c=>9}}}; print "$h->{a}{b}{c}\n";}, "9\n");
# mixed implicit array+hash chain
test_cl('"$d->{a}[1]{n}"',
    q{my $d={a=>[{n=>"X"},{n=>"Y"}]}; print "$d->{a}[1]{n}\n";}, "Y\n");

# explicit-arrow forms still work (regression guards)
test_cl('"$h->{a}->[1]" explicit', q{my $h={a=>[1,2,3]}; print "$h->{a}->[1]\n";}, "2\n");
test_cl('"$h->{c}" single', q{my $h={c=>9}; print "$h->{c}\n";}, "9\n");

# does not over-consume following text or a spaced brace
test_cl('trailing text after chain',
    q{my $h={a=>[5]}; print "val=$h->{a}[0]!end\n";}, "val=5!end\n");
test_cl('spaced brace is literal',
    q{my $x=7; print "$x {literal}\n";}, "7 {literal}\n");

# --- task #414: a SUBSCRIPT INSIDE an interpolated subscript ------------
# "$x[$i[0]]" dropped the whole statement with "no form emitter for
# expression leaf PPI::Token::Number:?".  Cause: the fragment parser cloned
# the top-level parts but nothing held the clones, and PPI's DESTROY empties
# every descendant — so the inner tokens went HOLLOW (content undef) between
# the parse and the emit.  The clones are anchored now (_anchor).  One
# program, one SBCL launch: every shape the census carried.
test_cl('subscript inside an interpolated subscript',
    q{my @x=(10,20,30); my @i=(0,1,2); my %h=(k=>1);
      my @ops=('a','b','c'); my @cur=([1,2],[0]);
      sub f { return "1:$_[$_[2]] plus 2:$_[!$_[2]]" }
      print f(5,6,0), "\n";
      print "A:$x[$i[0]]\n";
      print "B:$x[!$i[0]]\n";
      print "C:$x[$h{k}]\n";
      print "D:$x[$i[$i[1]]]\n";
      my $first=0; my $last=2;
      print "E:@ops[$first,@{$cur[0]},$last]\n";
      my @aoa=([1,2],[3,4]);
      print "I:$aoa[$i[1]][$i[0]]\n";
      print "J:$x[ $i[1] + 1 ]\n";},
    "1:5 plus 2:6\nA:10\nB:20\nC:20\nD:20\nE:a b c c\nI:3\nJ:30\n");

# the shape as it stands in t/op/postfixderef.t: the '.' overload handler
# whose body interpolates $_[$_[2]] — the drop made the handler compile to
# nil, so the overload silently returned undef.
test_cl('interpolated subscript inside an overload handler',
    q{package O; use overload fallback=>1, '""' => sub { $_[0][0] },
        '.' => sub { bless [ "$_[$_[2]]"." plus "."$_[!$_[2]]" ] };
      package main; my $o = bless ["X"], "O";
      print "".($o . "Y"), "\n";},
    " plus X plus Y\n");
