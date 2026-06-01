#!/usr/bin/env perl
# foreach-aliasing-01.t — `for (LVALUE) { $_ = ... }` must bind $_ to the live
# container so writes propagate, exactly as `for (@a) { $_ = ... }` aliases array
# elements.  PCL binds the loop variable to the *same box object* the container
# holds; the fix is to make the foreach-list codegen surface that box (its
# box-returning form) instead of a fresh value-box.
#
# Covered here (the aliasable forms PCL supports):
#   - single hash element   $h{k}   -> p-gethash-box
#   - single array element  $a[i]   -> p-aref-box
#   - substr/pos/vec lvalues are covered in lvalue-ref-01.t
# Also pinned: forms that must NOT alias (computed temps, plain builtins) and the
# whole-array case, so the boundary doesn't silently drift.
#
# NOT yet aliased (deliberate, see docs/foreach-aliasing.md): slices @a[...]/@h{...}
# and `values %h` — they flatten through the shared list-copy machinery.

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

plan tests => 11;

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

sub transpile {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;
    return scalar `$pl2cl $pl_file 2>/dev/null`;
}

# --- codegen: element forms rewrite to their box-returning heads ---
like(transpile('my @a=(1,2,3); for ($a[1]) { $_=9 }'),
    qr/\(p-aref-box /, 'for($a[i]) compiles to p-aref-box');
like(transpile('my %h=(k=>1); for ($h{k}) { $_=9 }'),
    qr/\(p-gethash-box /, 'for($h{k}) compiles to p-gethash-box');

# --- hash element aliases (write-through) ---
test_cl('for($h{k}) write-through',
    q{my %h=(k=>1); for ($h{k}) { $_=99 } print "$h{k}\n";}, "99\n");
test_cl('for($h{k}) in-place s/// idiom',
    q{my %h=(name=>"bob"); for ($h{name}) { s/b/B/g } print "$h{name}\n";}, "BoB\n");

# --- array element aliases (write-through) ---
test_cl('for($a[i]) write-through',
    q{my @a=(1,2,3); for ($a[1]) { $_=99 } print "@a\n";}, "1 99 3\n");
test_cl('for($a[i]) increment',
    q{my @a=(1,2,3); for ($a[2]) { $_++ } print "@a\n";}, "1 2 4\n");

# --- whole-array aliasing must still work (guard against regression) ---
test_cl('for(@a) still aliases each element',
    q{my @a=(1,2,3); for (@a) { $_*=10 } print "@a\n";}, "10 20 30\n");

# --- forms that must NOT alias: a computed value is a temporary ---
test_cl('for($x+1) does not write back (computed temp)',
    q{my $x=5; for ($x+1) { $_++ } print "$x\n";}, "5\n");
test_cl('for(uc $x) does not write back (rvalue builtin)',
    q{my $x="abc"; for (uc $x) { $_="ZZ" } print "$x\n";}, "abc\n");
test_cl('for(f()) does not write back (normal sub returns a copy)',
    q{sub f { my $v=7; return $v } for (f()) { $_=99 } print "ok\n";}, "ok\n");

# --- multi-element list must NOT trigger the single-element rewrite ---
test_cl('for($a[0], $a[1]) still iterates both (no false rewrite)',
    q{my @a=(1,2,3); my @seen; for ($a[0], $a[1]) { push @seen, $_ } print "@seen\n";},
    "1 2\n");
