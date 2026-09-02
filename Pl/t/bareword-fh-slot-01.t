#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# bareword-fh-slot-01.t — guard for task #1032: a BAREWORD filehandle is a
# NAME in a `stat` / `lstat` / filetest operand slot, not a variable.
#
# `open(FH,…); stat(FH)` emitted the bare CL symbol FH, which is an UNBOUND
# VARIABLE at load time: "The variable FH is unbound" killed the whole program
# — for an OPEN handle, and for `stat`, `lstat` and all 26 filetests.  It cost
# two whole companion files (t/op/stat_errors.t's 333 rows; t/op/write.t's
# abort and the 477 rows behind it).  Every other handle-taking builtin
# already routes its slot through `%p-fh-arg` — p-open, p-close, p-eof,
# p-tell, p-seek, p-binmode, p-fileno, p-readline and `p--t`, which IS a
# filetest — so this family joined the same mechanism (CLAUDE.md rule 11).
#
# THE ROWS THAT MUST *NOT* MOVE ARE HALF THE POINT.  In a READ slot perl
# CALLS a declared sub, so `use constant CPATH => …; -e CPATH` reads the
# constant and `sub SPATH {…} -e SPATH` calls the sub (probed 5.40.3,
# scratch/p13-const-handle-slot.pl).  The filetest emitter already gets that
# right — it emits `(p--e (pl-SPATH))` — which is why `%p-fh-arg`'s
# `(pl-NAME)` arm is switched OFF for this family: turning that call back
# into a handle name would be a NEW silent wrong.  Rows 21–23 are that guard.
# Row 13–16 are the OTHER exclusion: `_` is the runtime's stat-cache
# VARIABLE, not a handle named "_".
#
# Every expectation is the OUTPUT OF THE SAME PROGRAM under perl 5.40.3
# (scratch/guard-fh-prog.pl), not a hand-derivation.  Two SBCL launches, one
# per emission path — a Pl/t file's cost is its wall time, not its row count.

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

plan tests => 55;

my $workdir = tempdir(CLEANUP => 1);

sub run_pcl {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;
    my $cl_code = `$pl2cl $pl_file 2>/dev/null`;
    my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cl_fh $cl_code;
    close $cl_fh;
    my $out = `sbcl @sbcl_rt --load $cl_file 2>&1`;
    $out =~ s/^;.*\n//gm;
    $out =~ s/^PCL Runtime loaded\n//gm;
    return $out;
}

my $PROG = <<'PERL';
no warnings;
use constant CPATH => $ENV{PCL_GUARD_FILE};
sub SPATH { $ENV{PCL_GUARD_FILE} }
my $tmp = $ENV{PCL_GUARD_FILE};
open(OUT, '>', $tmp) or die "open OUT: $!";
print OUT "hello\n";
close(OUT);
open(FH, '<', $tmp) or die "open FH: $!";
my @a = stat(FH);   print "01=", scalar(@a), "\n";
my @b = lstat(FH);  print "02=", scalar(@b), "\n";
print "03=", (-e FH ? 1 : 0), "\n";
print "04=", (-f FH ? 1 : 0), "\n";
print "05=", (-d FH ? 1 : 0), "\n";
print "06=", (-s FH), "\n";
print "07=", (-z FH ? 1 : 0), "\n";
print "08=", (-r FH ? 1 : 0), "\n";
print "09=", (eof(FH) ? 1 : 0), "\n";
print "10=", (fileno(FH) >= 0 ? "ok" : "bad"), "\n";
print "11=", tell(FH), "\n";
print "12=", (binmode(FH) ? "ok" : "bad"), "\n";
my @c = stat($tmp);
print "13=", (-f _ ? 1 : 0), "\n";
print "14=", (-e _ ? 1 : 0), "\n";
print "15=", (-s _), "\n";
my @d = stat(_);  print "16=", scalar(@d), "\n";
my @e = stat(NOPE); print "17=", scalar(@e), "\n";
print "18=", (-e NOPE ? 1 : 0), "\n";
open(my $lex, '<', $tmp) or die;
my @f = stat($lex); print "19=", scalar(@f), "\n";
my @g = stat($tmp);  print "20=", scalar(@g), "\n";
close($lex);
print "21=", (-e CPATH ? 1 : 0), "\n";
print "22=", (-e SPATH ? 1 : 0), "\n";
print "23=", (-f CPATH ? 1 : 0), "\n";
print "24=", (-e main::FH ? 1 : 0), "\n";
my @h = stat(main::FH); print "25=", scalar(@h), "\n";
print "26=", (defined(-M FH) ? "ok" : "bad"), "\n";
print "27=", (fileno(STDOUT) >= 0 ? "ok" : "bad"), "\n";
close(FH);
unlink $tmp;
PERL

my %EXPECT = (
    '01' => '13',   # stat  BAREWORD on an OPEN handle — the crash
    '02' => '13',   # lstat BAREWORD
    '03' => '1',    # -e BAREWORD …
    '04' => '1',
    '05' => '0',
    '06' => '6',    # -s reads the handle's own size ("hello\n")
    '07' => '0',
    '08' => '1',
    '09' => '0',    # the already-working siblings must stay working
    '10' => 'ok',
    '11' => '0',
    '12' => 'ok',
    '13' => '1',    # `_` is the stat CACHE variable, never a handle name …
    '14' => '1',
    '15' => '6',
    '16' => '13',   # … including as `stat`'s own operand
    '17' => '0',    # a never-opened bareword IS a handle, not the string
    '18' => '0',
    '19' => '13',   # the lexical spelling was already right
    '20' => '13',   # and so was a path
    '21' => '1',    # a CONSTANT in a read slot is CALLED, not read as a name
    '22' => '1',    # and so is a declared SUB
    '23' => '1',
    '24' => '1',    # a package-QUALIFIED bareword handle (#452's spelling)
    '25' => '13',
    '26' => 'ok',   # -M reaches a value (its VALUE is #1042, pre-existing)
    '27' => 'ok',
);

my $n = 0;
for my $opt ('default', 'none') {
    if ($opt eq 'none') { $ENV{PCL_OPT} = 'none' } else { delete $ENV{PCL_OPT} }
    # a fresh path per run: the program creates and unlinks it itself
    $ENV{PCL_GUARD_FILE} = "$workdir/fh-" . $n++ . ".txt";
    my $out = run_pcl($PROG);
    my %got;
    $got{$1} = $2 while $out =~ /^(\d\d)=(.*)$/mg;
    for my $k (sort keys %EXPECT) {
        is($got{$k} // "<MISSING; output was:\n$out>", $EXPECT{$k}, "[$opt] row $k");
    }
}
delete $ENV{PCL_OPT};

# `write(BAREWORD)` is the same slot family and it broke the stub's OWN
# promise.  `p-write` is a no-op returning 1 — format/write templates are
# blessed not-supported and stripped at the source level — and its docstring
# says so "rather than crashing, so a stray write() call does not abort the
# whole program".  It aborted anyway, because the ARGUMENT was a bare CL
# symbol: `t/op/write.t` died at `The variable OUT is unbound` with 477 rows
# behind it (18/23 → 99/32 once this landed).  perl and PCL differ in the
# VALUE here by design, so the row asserts only what both must do — keep
# running — which is exactly what was broken.
{
    $ENV{PCL_GUARD_FILE} = "$workdir/write.txt";
    my $out = run_pcl(<<'PERL');
open(OUT, '>', $ENV{PCL_GUARD_FILE}) or die "open: $!";
write(OUT);
close(OUT);
unlink $ENV{PCL_GUARD_FILE};
print "write-survived\n";
PERL
    like($out, qr/write-survived/,
         'write(BAREWORD) does not abort the program (#1032)');
}
delete $ENV{PCL_GUARD_FILE};
