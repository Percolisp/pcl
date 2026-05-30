#!/usr/bin/env perl
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

my $project_root = "$RealBin/../..";
my $pl2cl        = "$project_root/pl2cl";
my $runtime      = "$project_root/cl/pcl-runtime.lisp";

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
    my $output = `sbcl --noinform --non-interactive --load $runtime --load $cl_file 2>&1`;
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
