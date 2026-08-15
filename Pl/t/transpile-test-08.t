#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# Transpile tests part 8.  NEW TESTS GO HERE — -06 is at the ~50-test cap and
# -07 is past it, and the BIGGEST test file bounds the parallel suite's wall
# time (one SBCL spawn per test), so start a new file rather than grow one.

use v5.30;
use strict;
use warnings;
use Test::More;
use File::Temp qw(tempfile);
use FindBin qw($RealBin);
use lib $RealBin;
use PCLCore;

# Path to pl2cl and runtime
my $project_root = "$RealBin/../..";
my $pl2cl = "$project_root/pl2cl";
my $runtime = "$project_root/cl/pcl-runtime.lisp";
# Optional saved-core fast path (PCL_TEST_CORE=1); source-load otherwise.
my @sbcl_rt = PCLCore::sbcl_prefix($runtime);

# Check dependencies
plan skip_all => "pl2cl not found" unless -x $pl2cl;
plan skip_all => "sbcl not found" unless `which sbcl 2>/dev/null`;

# Run a Perl snippet and return output
sub run_perl {
    my ($code) = @_;
    my $full_code = 'use feature "state"; use Cwd; ' . $code;
    # Shell-escape embedded single quotes ('…' -> '\''), or any tick in the
    # snippet truncates the -e arg.
    (my $sh_code = $full_code) =~ s/'/'\\''/g;
    my $output = `perl -e '$sh_code' 2>&1`;
    return $output;
}

# Transpile and run CL, return output
sub run_cl {
    my ($code) = @_;

    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;

    my $cl_code = `$pl2cl $pl_file 2>&1`;

    my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cl_fh $cl_code;
    close $cl_fh;

    my $output = `sbcl @sbcl_rt --load $cl_file 2>&1`;

    $output =~ s/^;.*\n//gm;
    $output =~ s/^caught .*\n//gm;
    $output =~ s/^compilation unit.*\n//gm;
    $output =~ s/^\s*Undefined.*\n//gm;
    $output =~ s/^-->.*\n//gm;
    $output =~ s/^==>.*\n//gm;
    $output =~ s/^PCL Runtime loaded\n//gm;
    $output =~ s/^\s*\n//gm;

    return $output;
}

# Test helper: compare Perl and CL output
sub test_transpile {
    my ($name, $code) = @_;
    my $perl_out = run_perl($code);
    my $cl_out = run_cl($code);
    is($cl_out, $perl_out, $name) or diag("Perl: $perl_out\nCL: $cl_out");
}

# ---------------------------------------------------------------------------
# List assignment in LIST context returns the assigned targets as LVALUES, so
# `$_++ foreach (LIST) = (VALUES)` increments the real variables (op/hashassign.t
# t304/t307-309).  Two things used to break that, both silently — the write went
# into a temporary and the variable kept its old value:
#   * a hash's ODD trailing key stored a bare *p-undef* instead of an entry box,
#     so its padded value was not writable;
#   * a scalar target starved by a greedy array/hash collected *p-undef* rather
#     than its own box, so `$z` in ($x,$y,%h,$z) = (0) never saw the ++.
# The simple cases (duplicate keys, leading scalar) already worked, which is why
# this needs the odd/starved shapes specifically.
# ---------------------------------------------------------------------------
test_transpile('aassign list-ctx lvalues: odd hash tail and starved trailing scalar', '
sub sh { my %h = @_; join(",", map { "$_=>" . (defined $h{$_} ? $h{$_} : "u") } sort keys %h) }
my (%h, $x, $y, $z);
$_++ foreach %h = (1,2,1,4);          print "dup     : ", sh(%h), "\n";
%h = (); $_++ foreach ($x,%h) = (0,1,2,3,4); print "lead-sc : x=$x ", sh(%h), "\n";
%h = (); $_++ foreach %h = (1,2,3);   print "odd     : ", sh(%h), "\n";
%h = (); $_++ foreach ($x,$y,%h,$z) = (0);     print "starved1: $x,$y,", sh(%h), ",$z\n";
%h = (); $_++ foreach ($x,$y,%h,$z) = (0,1);   print "starved2: $x,$y,", sh(%h), ",$z\n";
%h = (); $_++ foreach ($x,$y,%h,$z) = (0,1,2); print "starved3: $x,$y,", sh(%h), ",$z\n";
%h = (); my @a; $_++ foreach ($x,@a,$z) = (7,8,9); print "ary-eat : $x,@a,$z\n";
');

# The returned values must NOT be aliased to the RHS (op/hashassign.t t305/t306):
# assigning from $x and then incrementing the returned list must leave $x alone.
test_transpile('aassign list-ctx returns are not aliased to the RHS', '
my %h; my $x = 0;
$_++ foreach %h = ($x,$x);
print "rhs-unaliased=$x\n";
my %g; my $y = 0;
$_++ foreach sub :lvalue { %g = ($y,$y) }->();
print "lvalue-sub-rhs-unaliased=$y\n";
');

# A hash written with an odd element list still reads back an undef value, and
# exists() must see the padded key — the entry box must not change that.
test_transpile('odd hash tail: padded key exists and reads undef', '
my %h = (1,2,3);
print "keys=", join(",", sort keys %h), "\n";
print "exists3=", (exists $h{3} ? 1 : 0), " defined3=", (defined $h{3} ? 1 : 0), "\n";
print "count=", scalar(keys %h), "\n";
$h{3} = 9; print "after-write=$h{3}\n";
');

# ---------------------------------------------------------------------------
# A %hash interpolated into a PARENTHESISED list literal must flatten to its
# key/value pairs (task #170).  `@a = (1,%h,2)` emits (vector 1 %h 2), and
# p-array-fill's vector/list loops had no hash-table case, so the hash fell
# through to the scalar branch and was stored as ONE element — stringifying as
# HASH(0x..) with a wrong element COUNT.  Silent and value-corrupting, and
# `my @pairs = (%defaults, %overrides)` is everyday Perl.  The BARE form
# (@a = %h), push, sub args, count context and interpolation were all already
# correct, which is why only the paren-literal shapes expose it.
#
# Output is sorted / count-based throughout: Perl hash order is undefined and
# genuinely varies run to run, so an order-sensitive assertion would be flaky.
# ---------------------------------------------------------------------------
test_transpile('hash flattens inside a parenthesised list literal', '
my %f = (k => 9);
sub srt { join("|", sort @_) }
my @a = %f;         print "bare   : n=", scalar(@a), " ", srt(@a), "\n";
my @b = (%f);       print "parens : n=", scalar(@b), " ", srt(@b), "\n";
my @c = (1, %f);    print "after  : n=", scalar(@c), " ", srt(@c), "\n";
my @d = (%f, 2);    print "before : n=", scalar(@d), " ", srt(@d), "\n";
my @e = (1, %f, 2); print "middle : n=", scalar(@e), " ", srt(@e), "\n";
my @g = (1, (2, %f), 3); print "nested : n=", scalar(@g), " ", srt(@g), "\n";
my @h; push @h, 1, %f, 2; print "push   : n=", scalar(@h), " ", srt(@h), "\n";
');

# An EMPTY hash contributes ZERO elements, not one.  This is the shape that
# broke op/hashassign.t t307-309: ($x,$y,%h,$z) with %h empty must flatten to
# three values, so [$x,$y,%h,$z] has 3 elements.
test_transpile('empty hash contributes no elements to a list literal', '
my %e; my @a = (1, %e, 2);
print "mid   : n=", scalar(@a), " [", join("|",@a), "]\n";
my @b = (%e);              print "alone : n=", scalar(@b), "\n";
my ($x,$y,$z) = (1,2,3);
my $r = [$x, $y, %e, $z];  print "aryref: n=", scalar(@$r), " [", join("|",@$r), "]\n";
my $n = () = (1, %e, 2);   print "count : $n\n";
');

# A hash REFERENCE must NOT be flattened — it is a scalar.  Guards the fix
# against over-reaching (the raw-table test must not catch boxed refs).
test_transpile('hash REFERENCE in a list literal stays one element', '
my %f = (k => 9); my $href = \%f;
my @r = (1, $href, 2);
print "n=", scalar(@r), " isref=", (ref($r[1]) ? 1 : 0), " deref=", $r[1]->{k}, "\n";
my @nested = (1, [%f], 2);
print "anon-aryref n=", scalar(@nested), " inner=", scalar(@{$nested[1]}), "\n";
');

# ---------------------------------------------------------------------------
# open() must ACCEPT a PerlIO :layer on any mode (task #171).  Both the file
# and the in-memory dispatchers compared the WHOLE mode string with string=,
# so `<:utf8` matched no arm and the open FAILED — breaking ordinary code like
# `open $fh, '<:encoding(UTF-8)', $file`.  Now one shared splitter dispatches
# on the base mode; :raw/:bytes additionally select a byte-exact external
# format, since decoding raw bytes as UTF-8 would corrupt or signal.
#
# NOT covered here (task #139 — needs the layer-model design call): :crlf,
# layer stacking, PerlIO::get_layers introspection, and byte-exact round-trips
# of MALFORMED utf8 through a :utf8 handle.
# ---------------------------------------------------------------------------
test_transpile('open accepts PerlIO layers on file and in-memory handles', '
my $tmp = "/tmp/pcl-layer-test-$$.txt";
open(my $w, ">", $tmp) or die "setup: $!"; print $w "hello\n"; close $w;
for my $mode ("<", "<:utf8", "<:raw", "<:encoding(UTF-8)", "<:bytes") {
  my $fh; my $ok = open($fh, $mode, $tmp);
  my $l = $ok ? scalar(<$fh>) : undef; chomp $l if defined $l;
  printf "file %-20s ok=%d line=[%s]\n", $mode, ($ok?1:0), (defined $l ? $l : "");
  close $fh if $ok;
}
unlink $tmp;
my $s = "abc\ndef\n";
for my $mode ("<", "<:utf8", "<:raw") {
  my $fh; my $ok = open($fh, $mode, \$s);
  my $l = $ok ? scalar(<$fh>) : undef; chomp $l if defined $l;
  printf "mem  %-20s ok=%d line=[%s]\n", $mode, ($ok?1:0), (defined $l ? $l : "");
}
my $out = ""; my $ok = open(my $mw, ">:utf8", \$out);
print $mw "written" if $ok; close $mw if $ok;
print "memwrite ok=", ($ok?1:0), " out=[$out]\n";
');

# ---------------------------------------------------------------------------
# `use Cwd` used to DIE outright (task #166) — there was no Cwd.pm, even though
# cwd() was already a builtin.  Most of CPAN reaches for Cwd, so this was a
# wall, and it only surfaced when removing a duplicate exposed File::Spec's own
# `require Cwd`.  abs_path is implemented in the shim on top of readlink/-l/-e,
# NOT aliased to cwd(): it must resolve symlinks, and a shim quietly returning
# an unresolved path would be worse than a missing one.
#
# The test builds its own symlink tree so it does not depend on the checkout.
# ---------------------------------------------------------------------------
test_transpile('Cwd: use Cwd works; abs_path resolves symlinks, .. and missing tails', '
use Cwd qw(cwd getcwd abs_path realpath);
my $base = "/tmp/pcl-cwd-test-$$";
mkdir $base; mkdir "$base/real";
open(my $fh, ">", "$base/real/f.txt") or die "setup: $!"; print $fh "x\n"; close $fh;
symlink("real", "$base/link");
symlink("/tmp", "$base/abslink");
print "cwd-eq-getcwd = ", (cwd() eq getcwd() ? 1 : 0), "\n";
for my $p ("$base/.", "$base/link", "$base/link/f.txt", "$base/abslink",
           "$base/real/../real", "$base/real/newfile", "$base/nope/x", "/") {
  my $r = abs_path($p);
  (my $show = $p) =~ s/\Q$base\E/BASE/;
  my $got = defined $r ? $r : "UNDEF";
  $got =~ s/\Q$base\E/BASE/;
  printf "abs(%-22s) = %s\n", $show, $got;
}
print "realpath-eq-abs = ", (realpath("$base/link") eq abs_path("$base/link") ? 1 : 0), "\n";
unlink "$base/real/f.txt", "$base/link", "$base/abslink";
rmdir "$base/real"; rmdir $base;
');

done_testing();
