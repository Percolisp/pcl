#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# per-run-init-01.t — guard for tasks #1042 and #1236: state that must be
# established PER RUN, not per IMAGE.
#
# THE FAMILY.  A `defvar` initform is evaluated when `cl/pcl-runtime.lisp` is
# LOADED.  Since s439b every runner starts from a SAVED CORE of that load, so
# an initform's value is frozen at core-BUILD time and every program run from
# that core inherits it.  Two members were wrong:
#
#   * `$^T` (#1042) was the core's build moment, so `-M`/`-A`/`-C` — which are
#     defined as ($^T - filetime)/86400 — came out NEGATIVE for a file written
#     after the core was built.  "Is this file newer than my script" is the
#     idiom, and it got the opposite answer.  Measured drift: 184 s on one
#     tree, 2831 s on a tree whose core was older.  `$^T` is also ASSIGNABLE in
#     perl, and a bare integer is not a place: `$^T = N` was a silent no-op,
#     the $0 bug of task #512 in a second variable.  Both halves are fixed by
#     making it a p-box refreshed from sb-ext:*init-hooks*, the shape `$$`
#     (the pid) already used.
#
#   * `rand` (#1236) had the opposite problem — it was never seeded at all, so
#     two runs of the same program printed the SAME "random" number.  perl
#     seeds from entropy on first use when `srand` was never called
#     (PL_srand_called in pp_rand); PCL now does the same, lazily, so an
#     explicit `srand(N)` still replays exactly.
#
# Every expectation below is the OUTPUT OF THE SAME PROGRAM under perl, run
# twice, not a hand-derivation — including the one line that must DIFFER
# between two runs, which is asserted for perl first and then for PCL.
#
# WHY THE GATE CATCHES THE FROZEN `$^T`: `tools/prove-core` builds its core
# before prove starts, and this file writes its temp program afterwards, so on
# the old runtime mtime($0) > $^T and the -M row reads "NO".

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

plan tests => 14;

my $workdir = tempdir(CLEANUP => 1);

# The program goes to a file per run so that $0 is a REAL path with a real
# mtime — that is what the -M row measures.
sub write_prog {
    my ($name, $src) = @_;
    my $path = "$workdir/$name.pl";
    open my $fh, '>', $path or die "open $path: $!";
    print $fh $src;
    close $fh;
    return $path;
}

sub run_perl { my ($p) = @_; return scalar `perl $p 2>&1` }

sub run_pcl {
    my ($path) = @_;
    my $cl_code = `$pl2cl $path 2>/dev/null`;
    my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cl_fh $cl_code;
    close $cl_fh;
    my $out = `sbcl @sbcl_rt --load $cl_file 2>&1`;
    $out =~ s/^;.*\n//gm;
    $out =~ s/^PCL Runtime loaded\n//gm;
    return $out;
}

sub lines_of {
    my ($out) = @_;
    my %got;
    $got{$1} = $2 while $out =~ /^([A-Za-z][\w-]*)=(.*)$/mg;
    return \%got;
}

my $PROG = <<'PERL';
# $^T is the moment THIS RUN started, so it is within seconds of time()
my $delta = time() - $^T;
print "T-recent=", (abs($delta) < 5 ? "yes" : "NO ($delta)"), "\n";
# -M $0 = ($^T - mtime)/86400.  The script was written before the run started,
# so it is never in the future: perl answers >= 0 always.
print "M-self=", ((-M $0) >= 0 ? "yes" : "NO"), "\n";
# ... and $^T is an ordinary assignable scalar, which -M then reads
$^T = 12345;
print "T-assign=$^T\n";
print "M-after-assign=", ((-M $0) < 0 ? "past" : "future"), "\n";
# A seeded sequence replays exactly — perl ships its own drand48
srand(1);
print "seeded=", int(rand(1000)), "\n";
srand(42); my @a = map { sprintf "%.6f", rand } 1..3;
srand(42); my @b = map { sprintf "%.6f", rand } 1..3;
print "srand-repeats=", ("@a" eq "@b" ? "yes" : "NO"), "\n";
# perl's no-argument srand picks an entropy seed and RETURNS it
print "srand-noarg-is-not-zero=", (srand() != 0 ? "yes" : "NO"), "\n";
PERL

# An unseeded first draw, in its own program: the file above calls srand, and
# the whole point of #1236 is what happens when NOTHING does.
my $UNSEEDED = "printf \"unseeded=%.9f\\n\", rand();\n";

my @KEYS = qw(T-recent M-self T-assign M-after-assign
              seeded srand-repeats srand-noarg-is-not-zero);

my $p_out = run_perl(write_prog('prog-perl', $PROG));
my $c_out = run_pcl (write_prog('prog-pcl',  $PROG));
my $perl  = lines_of($p_out);
my $pcl   = lines_of($c_out);

for my $k (@KEYS) {
    is($pcl->{$k} // "<MISSING; PCL output was:\n$c_out>",
       $perl->{$k} // "<perl produced no $k; output was:\n$p_out>",
       "$k matches perl");
}

# The perl side is the oracle for the values themselves, so say them out loud:
# a silently-empty perl run would otherwise make every row above vacuous.
is($perl->{'T-recent'}, 'yes',   'perl: $^T is this run (oracle)');
is($perl->{'M-self'},   'yes',   'perl: -M $0 >= 0 (oracle)');
is($perl->{'T-assign'}, '12345', 'perl: $^T is assignable (oracle)');
is($perl->{'seeded'},   '41',    'perl: srand(1); int rand(1000) == 41 (oracle)');

# #1236: two runs of a program that never seeds must DIFFER.  Asserted for
# perl first — that is what makes the PCL row a comparison and not a wish.
my $upath = write_prog('unseeded', $UNSEEDED);
my $pu1 = lines_of(run_perl($upath))->{unseeded};
my $pu2 = lines_of(run_perl($upath))->{unseeded};
my $cu1 = lines_of(run_pcl ($upath))->{unseeded};
my $cu2 = lines_of(run_pcl ($upath))->{unseeded};

isnt($pu1 // '', $pu2 // '', 'perl: an unseeded rand differs between runs (oracle)');
isnt($cu1 // '', $cu2 // '', 'PCL: an unseeded rand differs between runs (#1236)');
ok(defined($cu1) && $cu1 =~ /^0\.\d+$/ && $cu1 >= 0 && $cu1 < 1,
   'PCL: the unseeded draw is still a [0,1) drand48 value');
