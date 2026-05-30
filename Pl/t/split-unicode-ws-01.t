#!/usr/bin/env perl
# split-unicode-ws-01.t — split ' ' (awk-mode) must treat the full Unicode
# \p{White_Space} set as separators, not just ASCII whitespace.
#
# Session 218: PCL strings are always Unicode (no per-scalar UTF8 flag, no
# `use bytes`), i.e. effectively always /u, so `split ' '` uses the full Unicode
# whitespace set via %perl-space-char-p in p-split (cl/pcl-runtime.lisp). This is
# the RT #130907 direction (\xA0/\x85/\x{2000}.. count as whitespace). The inverse
# (byte/ASCII mode where they would NOT count) is not representable — documented
# not-supported. NB: perl-tests/split.t 136-138 additionally need Unicode regex \s
# in their `grep /\s/u` setup, which is a separate CL-PPCRE gap, so they stay failing.

use v5.30;
use strict;
use warnings;
use utf8;
use Test::More;
use File::Temp qw(tempfile);
use FindBin qw($RealBin);

my $project_root = "$RealBin/../..";
my $pl2cl        = "$project_root/pl2cl";
my $runtime      = "$project_root/cl/pcl-runtime.lisp";

plan skip_all => "pl2cl not found" unless -x $pl2cl;
plan skip_all => "sbcl not found"  unless `which sbcl 2>/dev/null`;

plan tests => 6;

sub run_cl {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    binmode $fh, ':utf8';
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

# NBSP (\xA0) separates under split ' '
test_cl('NBSP \xA0 separates: count',
    q{my @g = split " ", ".\x{A0}\x{A0}/"; print scalar(@g),"\n";}, "2\n");
test_cl('NBSP \xA0 separates: fields',
    q{my @g = split " ", ".\x{A0}\x{A0}/"; print "$g[0]:$g[1]\n";}, ".:/\n");

# leading Unicode whitespace is stripped (awk mode)
test_cl('leading \xA0 stripped',
    q{my @g = split " ", "\x{A0}\x{A0}. /"; print scalar(@g),":$g[0]\n";}, "2:.\n");

# an ideographic space \x{3000} also separates
test_cl('ideographic space \x{3000} separates',
    q{my @g = split " ", "a\x{3000}b"; print scalar(@g),":$g[0]:$g[1]\n";}, "2:a:b\n");

# plain ASCII awk-mode behaviour is unchanged (leading/trailing/run collapse)
test_cl('ASCII awk-mode unchanged',
    q{my @g = split " ", "  a  b   c "; print scalar(@g),":@g\n";}, "3:a b c\n");

# limit is still honoured: 3rd field keeps the remainder verbatim
test_cl('limit keeps remainder',
    q{my @g = split " ", "a b c d", 2; print "$g[0]|$g[1]\n";}, "a|b c d\n");
