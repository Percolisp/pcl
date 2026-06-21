#!/usr/bin/env perl
# regex-gpos-01.t — the \G anchor (match at the current pos()).
#
# cl-ppcre has no \G. PCL strips \G from the pattern (%pcl-strip-gpos) and
# instead requires each /g match to BEGIN exactly at the current pos — so the
# match anchors at the previous endpoint and stops at the first gap, instead of
# skipping ahead the way a plain /g would. This is what tokenizer-style loops
# (Text::Balanced, parser combinators) rely on.
# See do-regex-match / %pcl-scan-anchored-list in cl/pcl-runtime.lisp.

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

plan tests => 7;

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

# Scalar /g loop with \G: tokenize alternating non-digits/digits.
test_cl('\G in a scalar while /g loop walks contiguous tokens',
    'my $s = "abc123def456"; my @n;'
  . 'while ($s =~ /\G(\D*)(\d+)/g) { push @n, $2; }'
  . 'print "@n\n";',
    "123 456\n");

# \G ANCHORS: it must stop at the first gap, not skip ahead like plain /g.
test_cl('\G stops at the first gap (does not skip ahead)',
    'my $s = "12 34xx56"; my @g;'
  . 'while ($s =~ /\G(\d+)\s*/g) { push @g, $1; }'
  . 'print "@g\n";',
    "12 34\n");

# Contrast: plain /g (no \G) DOES skip the gap.
test_cl('plain /g (no \G) skips the gap',
    'my $s = "12 34xx56"; my @g;'
  . 'while ($s =~ /(\d+)/g) { push @g, $1; }'
  . 'print "@g\n";',
    "12 34 56\n");

# \G in list context: all contiguous matches at once.
test_cl('\G /g in list context returns the contiguous run',
    'my $t = "1,2,3,4"; my @all = ($t =~ /\G(\d+),?/g);'
  . 'print "@all\n";',
    "1 2 3 4\n");

# List context stops at a gap too.
test_cl('\G list context stops at the first non-matching position',
    'my $t = "1,2,X,4"; my @all = ($t =~ /\G(\d+),?/g);'
  . 'print "@all\n";',
    "1 2\n");

# Non-leading \G via an interpolated qr// (Text::Balanced shape).
test_cl('\G embedded in an interpolated qr// anchors correctly',
    'my $re = qr/\G(\w)/; my $t = "abc"; my @c;'
  . 'while ($t =~ /$re/g) { push @c, $1; }'
  . 'print "@c\n";',
    "a b c\n");

# Two-field key=value tokenizer — the canonical \G use case.
test_cl('\G key=value tokenizer captures both fields per step',
    'my $code = "foo=42;bar=7;"; my @t;'
  . 'while ($code =~ /\G(\w+)=(\d+);/g) { push @t, "$1:$2"; }'
  . 'print "@t\n";',
    "foo:42 bar:7\n");
