#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# std-handle-open-01.t — task #535: STDIN/STDOUT/STDERR are NAMES FOR
# DESCRIPTORS 0/1/2, and an open onto one must move the descriptor.
#
#     open(my $save, ">&", \*STDOUT);
#     open(STDOUT, ">", $file);
#     print "x";                       # perl: into $file    PCL: to the TERMINAL
#     system("echo y");                # perl: into $file    PCL: to the TERMINAL
#
# PCL merely registered the new stream under the name, so the two spellings
# disagreed: `print STDOUT "x"` reached the file and a bare `print "x"` did not,
# and an exec'd child never followed at all.  The rule now lives in one place —
# %p-std-slot / %p-rebind-std / %p-std-rebuild, which %p-install-fh consults, so
# EVERY opener follows (plain open, in-memory open, fork-pipe, socket, accept,
# pipe).  %p-open-dup's standard-handle branch, which used to hold the only copy
# of the rebuild, now shares them.
#
# Two neighbours had to close with it, both found by probing this one:
#   * `close(STDOUT)` did not free descriptor 1.  The handle is a SYNONYM stream
#     over sb-sys:*stdout* and CLOSE on a synonym stream does not close what it
#     points at, so a fresh open afterwards got fd 6 where perl gets fd 1 — and
#     `open(STDOUT,'|-',CMD); print …; close(STDOUT)` HUNG, because the child
#     read a pipe whose write end the parent still held.
#   * `open(F,'-|','')` — an EMPTY command in the THREE-argument form — was
#     treated as perl's bare fork-open, so the child went on running the whole
#     program beside its parent.  perl's bare fork-open is the TWO-argument
#     `open(F,"-|")`; the three-argument form with no command is an error
#     (undef, $! = Broken pipe).  This is what put a second process inside the
#     companion suite's recovery loader, where it raced its parent on the
#     shared file offset and truncated t/io/open.t at a random form.
#
# Every expectation below is the live `perl` answer (probed s448n, 5.40.3).

use v5.30;
use strict;
use warnings;
use Test::More;
use File::Temp qw(tempfile tempdir);
use FindBin qw($RealBin);
use lib $RealBin;
use PCLCore;

my $project_root = "$RealBin/../..";
my $pl2cl        = "$project_root/pl2cl";
my $runtime      = "$project_root/cl/pcl-runtime.lisp";
my @sbcl_rt = PCLCore::sbcl_prefix($runtime);

plan skip_all => "pl2cl not found" unless -x $pl2cl;
plan skip_all => "sbcl not found"  unless `which sbcl 2>/dev/null`;

plan tests => 5;

my $dir = tempdir(CLEANUP => 1);
my $FIX = qq{my \$O = "$dir/out.txt";\n};

sub write_pl {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;
    return $pl_file;
}

# Transpile (a DROPPED statement fails the row, via PCLCore) and run.
sub run_cl {
    my ($code) = @_;
    my $cl_code = PCLCore::transpile("$pl2cl " . write_pl($code));
    my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cl_fh $cl_code;
    close $cl_fh;
    my $output = `sbcl @sbcl_rt --load $cl_file 2>&1`;
    $output =~ s/^;.*\n//gm;
    $output =~ s/^PCL Runtime loaded\n//gm;
    $output =~ s/^\s*\n//gm;
    return $output;
}

# ── 1. the redirect reaches the DEFAULT print AND an exec'd child ────────────
# `child-echo` is the row that cannot pass by registering a stream under a
# name: it is a separate process, and only descriptor 1 can carry it.
is(run_cl($FIX . <<'PL'), "file:[default-print\nchild-echo\n]back:ok\n",
open(my $save, ">&", \*STDOUT) or die "save: $!\n";
open(STDOUT, ">", $O) or die "redirect: $!\n";
print "default-print\n";
system("/bin/echo child-echo");
close(STDOUT);
open(STDOUT, ">&", $save) or die "restore: $!\n";
close $save;
open(my $in, '<', $O) or die "reopen: $!\n";
my $got = do { local $/; <$in> }; close $in;
print "file:[$got]";
print "back:ok\n";
PL
   'open(STDOUT,">",F) moves descriptor 1: default print AND a child follow, and a dup restores it');

# ── 2. close(STDOUT) frees descriptor 1 ──────────────────────────────────────
# Measured the way perl's own behaviour is visible: the next open gets the
# lowest free descriptor, which is 1 only if the close really released it.
is(run_cl($FIX . <<'PL'), "freed:1\n",
open(my $save, ">&", \*STDOUT) or die "save: $!\n";
close(STDOUT);
open(my $probe, '>', $O) or die "probe: $!\n";
my $fd = fileno($probe);
close $probe;
open(STDOUT, ">&", $save) or die "restore: $!\n";
close $save;
print "freed:", ($fd == 1 ? 1 : 0), "\n";
PL
   'close(STDOUT) releases descriptor 1 — the next open gets it');

# ── 3. STDOUT onto a pipe: the write end goes with the close, the child reaps ─
# This is the row that HUNG before the synonym-stream close was fixed.
is(run_cl($FIX . <<'PL'), "close:1 status:0 file:[through-the-pipe\n]",
open(my $save, ">&", \*STDOUT) or die "save: $!\n";
open(STDOUT, "|-", "cat > $O") or die "pipe: $!\n";
print "through-the-pipe\n";
my $cl = close(STDOUT);
open(STDOUT, ">&", $save) or die "restore: $!\n";
close $save;
open(my $in, '<', $O) or die "reopen: $!\n";
my $got = do { local $/; <$in> }; close $in;
print "close:", ($cl ? 1 : 0), " status:", ($? >> 8), " file:[$got]";
PL
   'open(STDOUT,"|-",CMD): the default print goes down the pipe and close reaps the child');

# ── 4. a three-argument open with an EMPTY command is not the bare fork ──────
# "one-process" printed ONCE is the assertion: a fork would print it twice.
is(run_cl(<<'PL'), "ret:undef\none-process\n",
my $r = open(my $f, '-|', '');
print "ret:", (defined $r ? $r : "undef"), "\n";
print "one-process\n";
PL
   'open(F,"-|","") is an error, not perl\'s bare fork-open (which is the 2-arg spelling)');

# ── 5. STDERR follows the same rule ──────────────────────────────────────────
is(run_cl($FIX . <<'PL'), "err:[warn-to-file\n]",
open(my $esave, ">&", \*STDERR) or die "esave: $!\n";
open(STDERR, ">", $O) or die "eredirect: $!\n";
warn "warn-to-file\n";
close(STDERR);
open(STDERR, ">&", $esave) or die "erestore: $!\n";
close $esave;
open(my $in, '<', $O) or die "reopen: $!\n";
my $got = do { local $/; <$in> }; close $in;
print "err:[$got]";
PL
   'open(STDERR,">",F) moves descriptor 2 — warn lands in the file, and a dup restores it');
