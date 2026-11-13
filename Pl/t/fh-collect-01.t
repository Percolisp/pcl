#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# fh-collect-01.t -- the COLLECTOR closes a filehandle the program can no longer
# reach (task #2006 PART 2, s540c; docs/ir-spec.md §6.5).
#
# A handle that ESCAPED the block that opened it (returned by a sub, passed to
# a helper) used to stay open until exit, keeping its descriptor and any
# `flock' it held: Path::Tiny's `slurp' (a returned LOCK_SH handle) followed by
# a locked `append' waited on the program's own lock forever, and a loop over
# more files than the descriptor limit died.  Now a finalizer flushes and
# closes a stream nothing reaches, and the two places the program would block
# or fail without it -- a `flock' that would block on a file this process has
# locked, an opener out of descriptors -- run the collector and try once more.
#
# Each program runs under perl (the oracle) and under PCL, under `timeout' so
# that the base -- which HANGS on the blocking row -- fails instead of
# stopping the gate.

use v5.30;
use strict;
use warnings;
use Test::More;
use File::Temp qw(tempdir);
use File::Spec;
use FindBin qw($RealBin);

my $root = File::Spec->rel2abs("$RealBin/../..");
my $pcl  = "$root/pcl";

plan skip_all => "pcl not found"  unless -x $pcl;
plan skip_all => "sbcl not found" unless `which sbcl 2>/dev/null`;

plan tests => 5;

my $dir = tempdir(CLEANUP => 1);

sub write_prog {
    my ($name, $body) = @_;
    my $path = File::Spec->catfile($dir, $name);
    open my $fh, '>', $path or die "write $path: $!";
    print $fh $body;
    close $fh;
    return $path;
}

sub run_both {
    my ($prog, $pre) = @_;
    $pre //= '';
    my $perl = `$pre timeout 60 $^X '$prog' 2>&1`;
    my $pcl_out = `$pre timeout 100 '$pcl' '$prog' 2>&1`;
    $pcl_out =~ s/^PCL Runtime loaded\n//gm;
    return ($perl, $pcl_out);
}

# 1: the lock shapes.  A handle dropped (returned and let go, held by a dup that
# is then dropped, captured by a closure that is then dropped) releases its
# lock; one still held (in a hash, by a live dup, by a live closure) keeps it.
# LOCK_NB answers the question without blocking.
my $locks = write_prog('locks.pl', <<'PL');
use strict; use warnings;
use Fcntl qw(:flock);
my $f = "$ARGV[0]/locks.$$";
{ open my $w, ">", $f or die; print $w "x\n"; close $w }
sub lockfh { my ($mode, $how) = @_; open my $fh, $how // "<", $f or die; flock($fh, $mode) or die "flock: $!"; return $fh }
sub probe {
    my ($tag) = @_;
    open my $o, ">>", $f or die "open: $!";
    my $ok = flock($o, LOCK_EX | LOCK_NB);
    print "$tag: ", ($ok ? "got it" : "WOULD BLOCK"), "\n";
    close $o;
}
sub slurp { my $fh = lockfh(LOCK_SH); local $/; my $b = <$fh>; return $b }
slurp(); probe("dropped");
my %keep;
sub keepit { $keep{h} = lockfh(LOCK_SH); probe("kept-in-hash"); delete $keep{h}; 1 }
keepit(); probe("deleted-from-hash");
my $dup;
sub dupit { my $fh = lockfh(LOCK_SH); open $dup, "<&", $fh or die "dup: $!"; 1 }
dupit(); probe("dup-alive");
sub dropdup { undef $dup; 1 } dropdup(); probe("dup-dropped");
my $cl;
sub mkclos { my $fh = lockfh(LOCK_SH); $cl = sub { $fh }; 1 }
mkclos(); probe("live-closure");
sub dropcl { undef $cl; 1 } dropcl(); probe("closure-dropped");
sub mkcl { my $fh = lockfh(LOCK_SH); my $c = sub { scalar <$fh> }; $c->(); return 1 }
mkcl(); probe("dead-closure");
unlink $f;
PL
{
    my ($perl, $pcl_out) = run_both("$locks' '$dir");
    is($pcl_out, $perl, 'lock shapes: a dropped handle releases its flock, a held one keeps it');
}

# 2: a BLOCKING flock after the program dropped a handle that holds LOCK_EX on
# the same file -- perl gets the lock; PCL used to wait for itself forever.
# The dropped write handle's bytes are in the file afterwards.
my $block = write_prog('block.pl', <<'PL');
use strict; use warnings;
use Fcntl qw(:flock);
my $f = "$ARGV[0]/block.$$";
{ open my $w, ">", $f or die; print $w "x\n"; close $w }
sub exfh { open my $fh, ">>", $f or die; flock($fh, LOCK_EX) or die; return $fh }
sub usex { my $fh = exfh(); print $fh "y\n"; return 1 }
usex();
open my $r, "<", $f or die;
print "blocking flock: ", (flock($r, LOCK_SH) ? "got it" : "failed $!"), "\n";
close $r;
print "size: ", -s $f, "\n";
unlink $f;
PL
{
    my ($perl, $pcl_out) = run_both("$block' '$dir");
    is($pcl_out, $perl, 'a blocking flock after a dropped LOCK_EX handle: no self-deadlock, bytes flushed');
}

# 3-4: more opens than the descriptor limit, each handle ESCAPING its block
# (passed to a helper; returned by one).  Under `ulimit -n 256' PCL died at
# ~250 opens; the out-of-descriptors retry collects and goes on.  Every line a
# dropped write handle buffered is in the file once the program has exited.
my $fds = write_prog('fds.pl', <<'PL');
use strict; use warnings;
sub first_line { my ($h) = @_; my $l = <$h>; return length($l // '') }
my $n = 1000;
my $s = 0;
for my $i (1 .. $n) { open(my $fh, "<", $0) or die "open failed at i=$i: $!\n"; $s += first_line($fh) }
my $f = "$ARGV[0]/fds.out";
unlink $f;
sub app { open my $fh, ">>", $f or die "append failed: $!\n"; return $fh }
for my $i (1 .. $n) { my $fh = app(); print $fh "$i\n" }
print "done $n sum=$s\n";
PL
{
    my $pre = "ulimit -n 256 &&";
    my $out_f = "$dir/fds.out";
    my $perl = `bash -c "$pre timeout 60 $^X '$fds' '$dir'" 2>&1`;
    my $plines = do { open my $in, '<', $out_f or die; my @l = <$in>; scalar @l };
    unlink $out_f;
    my $pcl_out = `bash -c "$pre timeout 100 '$pcl' '$fds' '$dir'" 2>&1`;
    $pcl_out =~ s/^PCL Runtime loaded\n//gm;
    my $lines = -e $out_f ? do { open my $in, '<', $out_f or die; my @l = <$in>; scalar @l } : 0;
    is($pcl_out, $perl, 'ulimit -n 256: 1000 escaping read handles + 1000 escaping write handles');
    is($lines, $plines, 'every line a dropped write handle buffered reached the file (1000)');
}

# 5: a fork after a dropped write handle: its bytes are written ONCE.
my $fork = write_prog('fork.pl', <<'PL');
use strict; use warnings;
my $g = "$ARGV[0]/fork.out";
unlink $g;
sub gw { open my $fh, ">>", $g or die; print $fh "once\n"; return $fh }
sub gdrop { my $fh = gw(); return 1 }
gdrop();
my $pid = fork();
if ($pid == 0) { exit 0 }
waitpid($pid, 0);
open my $in, "<", $g or die; my @l = <$in>; close $in;
print "lines after fork: ", scalar(@l), "\n";
PL
{
    my ($perl, $pcl_out) = run_both("$fork' '$dir");
    is($pcl_out, $perl, 'fork after a dropped write handle: its bytes are written once');
}
