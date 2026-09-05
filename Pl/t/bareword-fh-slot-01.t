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

plan tests => 133;

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
{ package StrOv;  use overload '""' => sub { $ENV{PCL_GUARD_FILE} }, fallback => 1; }
{ package FtOv;   use overload '-X' => sub { $FtOv::seen = $_[1]; "-$_[1]" }, fallback => 1; }
{ package BothOv; use overload '""' => sub { "/nonexistent-zz" },
                               '-X' => sub { "-$_[1]" }, fallback => 1; }
{ package NoneOv; use overload '+' => sub { 1 }, fallback => 1; }
my $sov = bless {}, 'StrOv';
my $fov = bless {}, 'FtOv';
my $bov = bless {}, 'BothOv';
my $nov = bless {}, 'NoneOv';
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
# --- s470bs: THE ONE OPERAND RESOLVER (tasks #1031 #1048 #1049) ------------
# A STRING is a PATH even while a handle of that name is open (#1049).
print "28=", (-e "FH" ? 1 : 0), "\n";
# A glob VALUE, a glob REF and a standard stream are HANDLES (#1048).
print "29=", (-e *FH ? 1 : 0), "\n";
print "30=", (-f \*FH ? 1 : 0), "\n";
print "31=", (-e STDOUT ? 1 : 0), "\n";
my @gv = stat(*FH);   print "32=", scalar(@gv), "\n";
my @gr = stat(\*FH);  print "33=", scalar(@gr), "\n";
my @so = stat(STDOUT);print "34=", scalar(@so), "\n";
# `-l' is the ONE filetest that never takes a handle: it stringifies, so a
# glob is a (nonexistent) FILE NAME rather than the open file's inode.
print "35=", (-l \*FH ? 1 : 0), "\n";
# A blessed operand with a `""' overload STRINGIFIES to a path (#1031) — the
# Path::Tiny idiom, whose old answer was a silent "no such file".
print "36=", (-e $sov ? 1 : 0), "\n";
print "37=", (-s $sov), "\n";
my @sv = stat($sov); print "38=", scalar(@sv), "\n";
# A `-X' handler ANSWERS, and is passed the operator's letter.
print "39=", (-e $fov), "\n";
print "40=", (-f $fov), "\n";
print "41=", $FtOv::seen, "\n";
# `-X' wins over `""' when a class has both, and an overloaded filetest does
# NOT disturb the `_' cache (probed 5.40.3).
print "42=", (-e $bov), "\n";
my @cv = stat($tmp);
my $ign = -e $fov;
my @af = stat(_);  print "43=", scalar(@af), "\n";
# A class that overloads something else entirely falls back to the PLAIN
# stringification, so the answer matches the address string's own.
print "44=", ((-e $nov ? 1 : 0) == (-e "$nov" ? 1 : 0) ? "same" : "differ"), "\n";
# --- s470bs: PERL'S ERRNO (task #1033) ------------------------------------
# A HANDLE that is not open is EBADF; a missing PATH is ENOENT.  perl keeps
# those apart and code branching on $!{EBADF} vs $!{ENOENT} reads them; every
# filetest used to swallow the condition and leave $! whatever it held.
$! = 0; my $u1 = -e NOPE;              print "45=", ($!+0), "\n";
$! = 0; my @u2 = stat(NOPE);           print "46=", ($!+0), "\n";
$! = 0; my $u3 = -e "/no/such/zz-xyq"; print "47=", ($!+0), "\n";
$! = 0; my $u4 = -r "/no/such/zz-xyq"; print "48=", ($!+0), "\n";
$! = 0; my $u5 = -T NOPE;              print "49=", ($!+0), "\n";
open(CB, '<', $tmp) or die; close(CB);
$! = 0; my $u6 = -e CB;                print "50=", ($!+0), "\n";
# A pathname holding a NUL byte FAILS ([perl #131895]); the C layer truncates
# it, so this used to answer true for $tmp itself.
print "51=", (-f "$tmp\0-" ? 1 : 0), "\n";
$! = 0; my $u7 = -f "$tmp\0-";         print "52=", ($!+0), "\n";
# A DIRHANDLE is a handle (perl fstats the dirfd); a CLOSED one is EBADF.
opendir(DIRH, "/") or die;
print "53=", (-d DIRH ? 1 : 0), "\n";
my @dh = stat(DIRH); print "54=", scalar(@dh), "\n";
closedir(DIRH);
$! = 0; my $u8 = -d DIRH;              print "55=", ($!+0), "\n";
# `-t` has its own two failures: ENOTTY for an open handle that is not a tty,
# EBADF when the operand names no handle at all.
$! = 0; my $u9 = -t FH;                print "56=", ($!+0), "\n";
$! = 0; my $ua = -t NOPE;              print "57=", ($!+0), "\n";
# --- s470bs: WHAT `_` REMEMBERS (task #1047) ------------------------------
# An IO REF is a handle: `*$fh{IO}` used to stringify the STREAM into a glob
# NAME, so a stat through it stat'ed undef and never filled `_`.
open(my $lx, '<', $tmp) or die;
my $bin = "$tmp.bin";
open(my $bh, '>', $bin) or die; print $bh "\0\0\0\0binary"; close($bh);
my @ir = stat(*$lx{IO}); print "58=", scalar(@ir), "\n";
stat($bin); stat(*$lx{IO});
print "59=", (-T _ ? 1 : 0), "\n";
stat($bin); my $ig = -r *$lx{IO};
print "60=", (-T _ ? 1 : 0), "\n";
close($lx); unlink $bin;
# A FAILED stat leaves an INVALID buffer: EBADF for every filetest but -T/-B,
# which retry the remembered NAME and so answer ENOENT.
my @nz = lstat("/no/such/zz-xyq");
$! = 0; my $c1 = -e _; print "61=", ($!+0), "\n";
my @nz2 = lstat("/no/such/zz-xyq");
$! = 0; my $c2 = -T _; print "62=", ($!+0), "\n";
# The FLAVOUR: reading `_` with an lstat-flavoured op after a plain stat is
# FATAL, in perl's own two wordings.
stat($tmp);
my $d1 = eval { -l _; 1 };
print "63=", ($@ =~ /^The stat preceding -l _ wasn't an lstat/ ? "die" : "no[$@]"), "\n";
stat($tmp);
my $d2 = eval { lstat(_); 1 };
print "64=", ($@ =~ /^The stat preceding lstat\(\) wasn't an lstat/ ? "die" : "no[$@]"), "\n";
lstat($tmp);
my $d3 = eval { my $z = -l _; 1 };
print "65=", ($@ ? "die" : "no"), "\n";
lstat($tmp);
my $d4 = eval { my @z = stat(_); 1 };
print "66=", ($@ ? "die" : "no"), "\n";
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
    # --- s470bs: THE ONE OPERAND RESOLVER -------------------------------
    '28' => '0',    # a STRING is a PATH, not the open handle it names (#1049)
    '29' => '1',    # a glob VALUE is a handle (#1048)
    '30' => '1',    # a glob REF is a handle
    '31' => '1',    # a standard stream is a handle — it has no pathname, so
                    #   this is the row that forces fstat(fileno) (#1048)
    '32' => '13',
    '33' => '13',
    '34' => '13',
    '35' => '0',    # `-l' stringifies: a glob is a FILE NAME, never a handle
    '36' => '1',    # a `""'-overloaded operand STRINGIFIES to a path (#1031)
    '37' => '6',
    '38' => '13',
    '39' => '-e',   # a `-X' handler ANSWERS, and gets the operator's letter
    '40' => '-f',
    '41' => 'f',
    '42' => '-e',   # `-X' beats `""' when a class declares both
    '43' => '13',   # an overloaded filetest leaves the `_' cache alone
    '44' => 'same', # any other overloading falls back to plain stringification
    # --- s470bs: PERL'S ERRNO (task #1033) ------------------------------
    '45' => '9',    # EBADF — a never-opened HANDLE, not "no such file"
    '46' => '9',
    '47' => '2',    # ENOENT — a missing PATH
    '48' => '2',
    '49' => '9',
    '50' => '9',    # a CLOSED bareword handle is EBADF too
    '51' => '0',    # a pathname with a NUL byte FAILS ([perl #131895]) …
    '52' => '2',    #   … with ENOENT, rather than being truncated
    '53' => '1',    # a DIRHANDLE is a handle (#1048)
    '54' => '13',
    '55' => '9',    # and a CLOSED dirhandle is EBADF
    '56' => '25',   # -t on an open non-tty is ENOTTY …
    '57' => '9',    #   … and EBADF when there is no handle at all
    # --- s470bs: WHAT `_` REMEMBERS (task #1047) ------------------------
    '58' => '13',   # an IO ref (*$fh{IO}) is a handle …
    '59' => '1',    #   … and a stat through it FILLS `_`
    '60' => '1',    #   … and so does -r through it
    '61' => '9',    # a FAILED stat makes `_` EBADF …
    '62' => '2',    #   … except for -T/-B, which retry the remembered NAME
    '63' => 'die',  # `-l _` after a plain stat is FATAL, in perl's wording …
    '64' => 'die',  #   … and so is `lstat _`
    '65' => 'no',   # after an lstat both are fine …
    '66' => 'no',   #   … including plain `stat _`
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
