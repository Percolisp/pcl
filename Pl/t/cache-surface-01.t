#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# cache-surface-01.t — THE CACHE AS A USER MEETS IT (task #1300):
#
#   * the prune keeps what is USED, not what is young (F2, and #682 with it);
#   * the cache directory is created 0700 and an unsafe one is REFUSED (F8);
#   * `pcl --cache-info` / `--no-cache` / `--version` and `pl2cl --help`.
#
# WHY A SECOND FILE.  The directory rows of task #1303 sit in
# Pl/t/module-fasl-cache-01.t, where the module cache's own rows live; these
# would have taken that file from 33 s to well past a minute, and the gate's
# wall time is the slowest single file (CLAUDE.md rule 6).
#
# EVERY ROW THAT WRITES A CACHE RUNS IN ITS OWN $PCL_CACHE_DIR, so nothing
# here reads or writes the developer's ~/.pcl-cache — which is also the point
# being tested, since before #1303 the variable did not reach the module cache
# at all.

use v5.30;
use strict;
use warnings;
use Test::More;
use File::Temp qw(tempdir);
use File::Spec;
use FindBin qw($RealBin);

my $root  = File::Spec->rel2abs("$RealBin/../..");
my $pcl   = "$root/pcl";
my $pl2cl = "$root/pl2cl";

use lib "$RealBin/../../tools/lib";
use PCLPaths ();

plan skip_all => "pcl not found"  unless -x $pcl;
plan skip_all => "sbcl not found" unless `which sbcl 2>/dev/null`;

plan tests => 37;

my $dir = tempdir(CLEANUP => 1);       # where the fixture modules live

sub write_mod {
    my ($name, $body) = @_;
    my $path = File::Spec->catfile($dir, "$name.pm");
    open my $fh, '>', $path or die "write $path: $!";
    print $fh $body;
    close $fh;
    return $path;
}

# Run CODE under `pcl` with $dir on @INC and %env in the environment.
# PCL_COMPILE_DIRS='*' throughout: these fixtures live in a tempdir, and the
# default is to compile only modules from perl's installed library
# directories, so without it every row would take the TEXT path and the fasl
# half of an entry would never exist (the trap #1261's own rows fell into).
sub run_pcl {
    my ($code, %env) = @_;
    my %saved;
    for my $k (sort keys %env) { $saved{$k} = $ENV{$k}; $ENV{$k} = $env{$k} }
    local $ENV{PERL5LIB} = $dir;
    local $ENV{PCL_COMPILE_DIRS} = '*';
    my $out = `$pcl -I '$dir' -E '$code' 2>&1`;
    for my $k (sort keys %saved) {
        if (defined $saved{$k}) { $ENV{$k} = $saved{$k} } else { delete $ENV{$k} }
    }
    $out =~ s/^;.*\n//gm;
    $out =~ s/^PCL Runtime loaded\n//gm;
    $out =~ s/^PCL: module .*\n//gm;
    return $out;
}

# ─────────────────────────────────────────────────────────────────────────
# PRUNE BY LAST USE, not by write date (F2; task #682 folds in).
#
# Validity is the dependency manifest and nothing else (#1261, ir-spec §9.2b),
# so all that is left for the age rule is disk hygiene — and the honest
# question there is "does anything still reach this entry".  The old rule
# (7 days, WRITE date) deleted a correct, daily-used cache once a week and
# rebuilt it.  Now a USED entry is stamped and the prune drops what nothing
# has reached for 30 days, in <cache>/modules/ AND in <cache>/proto/ (#682:
# a prototype entry becomes unreachable the moment the compiler stamp
# changes, and nothing ever removed one).
{
    my $pcache = tempdir(CLEANUP => 1);
    my %env = (PCL_CACHE_DIR => $pcache);

    write_mod('PruneOld',   "package PruneOld;\nsub v { 1 }\n1;\n");
    write_mod('PruneNew',   "package PruneNew;\nsub v { 2 }\n1;\n");
    write_mod('PruneThird', "package PruneThird;\nsub v { 3 }\n1;\n");

    is(run_pcl('use PruneOld; print PruneOld::v();', %env), '1',
       'the prune rows run in a cache directory of their own');
    my @old   = glob("$pcache/modules/*");
    my @proto = glob("$pcache/proto/*");
    ok(scalar(@old) >= 2, '... which now holds a module entry');
    ok(scalar(@proto) >= 1, '... and a prototype entry');

    # Nothing has reached either for 40 days.
    my $past = time - 40 * 24 * 3600;
    utime($past, $past, @old, @proto);
    unlink "$pcache/.last-prune";

    # A cache MISS is when the prune runs.
    is(run_pcl('use PruneNew; print PruneNew::v();', %env), '2',
       'a second module misses, which is when the prune runs');
    is(scalar(grep { -e $_ } @old), 0,
       'a module entry nothing has reached for 30 days is pruned');
    is(scalar(grep { -e $_ } @proto), 0,
       '... and so is a prototype entry (task #682)');
    ok(scalar(glob("$pcache/modules/*.lisp")),
       '... while the entry that was just used survives');

    # THE STAMP.  The scan costs ~21 ms over a real cache (590 module files +
    # 1112 prototype files, measured warm) and it is called on EVERY miss, so
    # it is claimed once a day through a .last-prune marker.  With a fresh
    # marker in place, a further miss must NOT scan.
    ok(-e "$pcache/.last-prune", 'the prune leaves a .last-prune marker');
    my @fresh = glob("$pcache/modules/*");
    utime($past, $past, @fresh);
    is(run_pcl('use PruneThird; print PruneThird::v();', %env), '3',
       'a third module misses with the marker still fresh');
    is(scalar(grep { -e $_ } @fresh), scalar(@fresh),
       '... and the marker stops the scan running twice in one day');
}

# TOUCH ON HIT.  An entry that is USED is re-stamped, at most once a day, so
# that "last use" is what the prune reads.  The fixture's SOURCE is stamped 10
# days back, which is what lets a cache entry be both older than a day and
# still VALID (validity wants the entry newer than its source).
{
    my $tcache = tempdir(CLEANUP => 1);
    my %env = (PCL_CACHE_DIR => $tcache);
    my $src = write_mod('TouchMe', "package TouchMe;\nsub v { 9 }\n1;\n");
    my $ten = time - 10 * 24 * 3600;
    utime($ten, $ten, $src);

    is(run_pcl('use TouchMe; print TouchMe::v();', %env), '9',
       'the touch rows run in a cache directory of their own');
    my @e = glob("$tcache/modules/*");
    ok(scalar(@e) >= 2, '... which holds the entry to be re-stamped');

    my $five = time - 5 * 24 * 3600;
    utime($five, $five, @e);
    is(run_pcl('use TouchMe; print TouchMe::v();', %env), '9',
       'a five-day-old entry is still a HIT (validity has no age clause)');
    is(scalar(grep { (stat $_)[9] > time - 300 } @e), scalar(@e),
       '... and the HIT re-stamps EVERY file of the entry');

    # THE NEGATIVE: one utime a day per module, not one per load.
    my $hour = time - 3600;
    utime($hour, $hour, @e);
    my @before = map { (stat $_)[9] } @e;
    run_pcl('use TouchMe; print TouchMe::v();', %env);
    is(join(',', map { (stat $_)[9] } @e), join(',', @before),
       '... while an entry stamped an hour ago is left alone');
}

# ─────────────────────────────────────────────────────────────────────────
# THE CACHE DIRECTORY IS PRIVATE (F8).  A cached module is a FASL: compiled
# code this process loads and runs.  So PCL creates the directory 0700, and
# refuses one anybody else could write to.
{
    my $parent = tempdir(CLEANUP => 1);
    my $fresh  = "$parent/made-by-pcl";
    write_mod('MadeDir', "package MadeDir;\nsub v { 5 }\n1;\n");
    is(run_pcl('use MadeDir; print MadeDir::v();', PCL_CACHE_DIR => $fresh),
       '5', 'PCL creates a cache directory that does not exist yet');
    my $mode = (stat $fresh)[2] & 07777;
    is(sprintf('%04o', $mode), '0700', '... with mode 0700, because a fasl is code');
    is(sprintf('%04o', ((stat "$fresh/modules")[2] & 07777)), '0700',
       '... and so is the modules/ subdirectory it makes inside');

    # THE REFUSAL.  Rule 12: the message names the value and the fix.
    my $open = tempdir(CLEANUP => 1);
    chmod 0777, $open;
    my $out = run_pcl('use MadeDir; print MadeDir::v();', PCL_CACHE_DIR => $open);
    like($out, qr/refusing to use the cache directory \Q$open\E/,
         'a group- or world-writable cache directory is REFUSED, naming it');
    like($out, qr/group- or world-writable \(mode 0777\)/,
         '... naming the mode it found (rule 12)');
    like($out, qr/chmod 700 \Q$open\E/, '... and the fix');

    # The predicate's OWNERSHIP branch cannot be reached by making a directory
    # (a test cannot own a file as another user), so it is asked directly, of
    # a directory this box certainly has and this user certainly does not own.
    my $problem = PCLPaths::cache_dir_problem('/usr');
    like($problem // '', qr/owned by uid 0, not by you/,
         'the ownership branch answers for a root-owned directory');
    is(PCLPaths::cache_dir_problem("$parent/no-such-directory"), undef,
       '... and a directory that does not exist is not a problem (we make it)');
}

# ─────────────────────────────────────────────────────────────────────────
# THE COMMANDS (§1.2 of docs/plan-cache-and-install-s471.md).
{
    my $ncache = tempdir(CLEANUP => 1);
    write_mod('NoCacheMod', "package NoCacheMod;\nsub v { 7 }\n1;\n");

    # --no-cache forwards pl2cl's existing flag (which sets *pcl-skip-cache*),
    # so THIS run neither reads nor writes a module cache.  There is no second
    # mechanism.
    my $out = `PCL_CACHE_DIR='$ncache' PERL5LIB='$dir' $pcl --no-cache -I '$dir' -E 'use NoCacheMod; print NoCacheMod::v();' 2>&1`;
    like($out, qr/\b7\b/, 'pcl --no-cache runs the program');
    is(scalar(my @none = glob("$ncache/modules/*")), 0,
       '... and writes NO module cache entry');

    my $info = `PCL_CACHE_DIR='$ncache' $pcl --cache-info 2>&1`;
    like($info, qr/^Cache directory: \Q$ncache\E/m,
         'pcl --cache-info names the cache directory');
    like($info, qr/PCL_CACHE_DIR/,
         '... and says the directory came from the environment');
    like($info, qr/^\s*modules\//m, '... lists the module cache');
    like($info, qr/^\s*proto\//m,   '... and the prototype cache');
    like($info, qr/core for this run/i, '... names the core this run would use');
    like($info, qr/PCL_COMPILE_DIRS/, '... and the compile policy in effect');

    my $ver = `$pcl --version 2>&1`;
    like($ver, qr/^pcl \(PCL\) \S/m,     'pcl --version prints the PCL version');
    like($ver, qr/^cache generation: v/m, '... the cache generation');
    like($ver, qr/^SBCL /m,               '... the SBCL version');
    like($ver, qr/^PPI /m,                '... and the PPI version');

    my $help = `$pl2cl --help 2>&1`;
    is($? >> 8, 0, 'pl2cl --help exits 0 (it answered "Unknown option: help")');
    like($help, qr/--no-cache/, '... and documents its options');
}
