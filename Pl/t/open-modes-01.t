#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# open-modes-01.t — the three open shapes the #1501 round-5 census found
# missing, and the failure SHAPE they all share.  Every expectation below is
# the live `perl` answer (probed 5.40.3).
#
#   1. "+>>" — read/append — was not in %p-open-parse-2arg's mode table, so the
#      shorter "+>" arm won and left the second ">" on the FILENAME:
#      `open($fh,"+>>$path")` created, and then read-failed on, a file literally
#      named ">/tmp/…".  t/io/tell.t stopped there, 10 rows short (task #1696).
#      The same mode was missing from the anon-temporary direction map, the
#      in-memory dispatch and the input-mode predicate.
#
#   2. An APPEND anon temporary (`open $fh, "+>>", undef`) needs O_APPEND:
#      perl's writes go to the END whatever the position says.  PCL's mkstemp
#      set no flag, so `print; seek(0); print` OVERWROTE — and that was already
#      true for plain ">>" before "+>>" existed at all.
#
#   3. The LIST form `open($fh,'-|',$prog,@argv)` — perl's documented no-shell
#      pipe open — was a macroexpansion ARITY ERROR: `p-open` took three
#      parameters, so the whole top-level form failed to COMPILE and t/io/
#      closepid.t produced no TAP at all (task #1697).  A non-pipe mode with a
#      second target is perl's fatal "More than one argument to open" and says
#      so here rather than picking one (rule 12).
#
#   4. perl's `open` NEVER SIGNALS: every OS refusal is a false return with $!.
#      SBCL's `open` signals a FILE-ERROR for all of them but ENOENT, so seven
#      of nine failing shapes aborted the whole top-level form — including
#      t/run/switches.t's own `open my $fh, ">", $check` probe after a
#      chmod 0500 (task #1699).

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

plan tests => 6;

my $dir = tempdir(CLEANUP => 1);

# Transpile (a DROPPED statement fails the row, via PCLCore) and run.
sub run_cl {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;
    my $cl_code = PCLCore::transpile("$pl2cl $pl_file");
    my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cl_fh $cl_code;
    close $cl_fh;
    my $output = `sbcl @sbcl_rt --load $cl_file 2>&1`;
    $output =~ s/^;.*\n//gm;
    $output =~ s/^PCL Runtime loaded\n//gm;
    return $output;
}

# ── 1. "+>>" on a file: no truncate, positioned at the end, readable, and the
# write appends even after a seek to zero (O_APPEND) ─────────────────────────
is(run_cl(<<PL), "tell=10 first=fred size=11 endsX=1 headfred=1\n",
my \$f = "$dir/pa.txt";
open(my \$t, ">", \$f) or die "A:\$!"; print \$t "fred\\nmore\\n"; close \$t;
open(my \$u, "+>>\$f") or die "B:\$!";           # the TWO-argument spelling
my \$tell = tell(\$u);
seek(\$u, 0, 0);
my \$l = <\$u>; chomp \$l;
seek(\$u, 0, 0);                                  # a seek does NOT move the write
print \$u "X";
close \$u;
open(my \$r, "<", \$f) or die "C:\$!"; my \$all = do { local \$/; <\$r> }; close \$r;
print "tell=\$tell first=\$l size=", -s \$f,
      " endsX=", (\$all =~ /X\\z/ ? 1 : 0),
      " headfred=", (\$all =~ /\\Afred/ ? 1 : 0), "\\n";
PL
   '2-arg "+>>FILE" is read/append: no truncate, tell() = size, and the write goes to the end');

# ── 2. the three other "+>>" dispatch sites: 3-arg, a NEW file, in-memory ────
is(run_cl(<<PL), "three=10 new=0 newsize=3 mem=3 s=abcde\n",
my \$f = "$dir/pa2.txt";
open(my \$t, ">", \$f) or die "A:\$!"; print \$t "fred\\nmore\\n"; close \$t;
open(my \$v, "+>>", \$f) or die "B:\$!"; my \$three = tell(\$v); close \$v;
my \$g = "$dir/pa2-new.txt";
open(my \$w, "+>>", \$g) or die "C:\$!"; my \$new = tell(\$w); print \$w "zz\\n"; close \$w;
my \$s = "abc";
open(my \$m, "+>>", \\\$s) or die "D:\$!"; my \$mem = tell(\$m); print \$m "de"; close \$m;
print "three=\$three new=\$new newsize=", -s \$g, " mem=\$mem s=\$s\\n";
PL
   '"+>>" reaches the 3-arg, create-a-new-file and in-memory paths, each positioned at the end');

# ── 3. an APPEND anonymous temporary is O_APPEND — and ">>" is the same fact ─
is(run_cl(<<'PL'), "+>>=abcxyz >>=abcxyz +>=xyz >=xyz \n",
for my $mode ('+>>', '>>', '+>', '>') {
  open(my $fh, $mode, undef) or die "open $mode: $!";
  print $fh "abc";
  seek($fh, 0, 0);
  print $fh "xyz";
  seek($fh, 0, 0);
  my $d = <$fh>; close $fh;
  print "$mode=", (defined $d ? $d : "undef"), " ";
}
print "\n";
PL
   'an anonymous temporary opened ">>" or "+>>" is O_APPEND: a write after seek(0) still appends');

# ── 4. the LIST form of a pipe open execs directly — NO shell ───────────────
# The metacharacters are the assertion: a shell would have run `echo BOOM`.
is(run_cl(<<'PL'), "read=A B|args=x y\nmeta=[a;echo BOOM b*c]\n",
my $perl = $^X;
open(my $r, "-|", $perl, "-e", 'print "A B\n"; print "args=@ARGV\n"', "x", "y")
  or die "r: $!";
my @l = <$r>; close $r;
print "read=", join("|", map { my $c = $_; chomp $c; $c } @l), "\n";
open(my $r3, "-|", $perl, "-e", 'print "@ARGV\n"', "a;echo BOOM", "b*c") or die "r3: $!";
my $m = <$r3>; close $r3; chomp $m;
print "meta=[$m]\n";
PL
   'open($fh,"-|",PROG,ARGS) execs PROG with no shell — a metacharacter survives literally');

# ── 5. the LIST form of a WRITE pipe, and a non-pipe mode with a second
# target is perl's fatal "More than one argument to open" ───────────────────
like(run_cl(<<PL), qr/\Awrite=piped\nextra=undef err=More than one argument to open\b/,
my \$perl = \$^X;
my \$f = "$dir/lo.txt";
open(my \$w, "|-", \$perl, "-e", 'open my \$o,">",\$ARGV[0] or die; print \$o <STDIN>; close \$o', \$f)
  or die "w: \$!";
print \$w "piped\\n"; close \$w;
open(my \$i, "<", \$f) or die "i:\$!"; my \$l = <\$i>; close \$i; chomp \$l;
print "write=\$l\\n";
my \$x = eval { open(my \$h, ">", "$dir/lo2.txt", "extra") };
print "extra=", (defined \$x ? \$x : "undef"), " err=\$@";
PL
   'open($fh,"|-",PROG,ARGS) writes down the pipe; a second target on a non-pipe mode is fatal');

# ── 6. a refused open is FALSE with $!, never a signal ──────────────────────
# The row that matters is that the program REACHES the last line: before this,
# seven of these eight aborted the whole top-level form.
my $want6 = join "", map { "$_\n" }
    "ro-write=0 Permission denied",
    "noperm-read=0 Permission denied",
    "dir-write=0 Is a directory",
    "ro-append=0 Permission denied",
    "ro-plusgt=0 Permission denied",
    "ro-plusgtgt=0 Permission denied",
    "noperm-pluslt=0 Permission denied",
    "nondir-path=0 Not a directory",
    "still-running=1";
is(run_cl(<<PL), $want6,
my \$d = "$dir/fail";
mkdir \$d or die "mkdir: \$!";
mkdir "\$d/ro" or die "mkdir ro: \$!";
my \$np = "\$d/noperm";
open(my \$x, ">", \$np) or die "np: \$!"; close \$x;
chmod 0000, \$np; chmod 0500, "\$d/ro";
sub try { my (\$t, \$mode, \$p) = \@_;
          my \$ok = open(my \$fh, \$mode, \$p); close \$fh if \$ok;
          print "\$t=", (\$ok ? 1 : 0), " \$!\\n" }
try("ro-write",        ">",   "\$d/ro/new");
try("noperm-read",     "<",   \$np);
try("dir-write",       ">",   \$d);
try("ro-append",       ">>",  "\$d/ro/new2");
try("ro-plusgt",       "+>",  "\$d/ro/new3");
try("ro-plusgtgt",     "+>>", "\$d/ro/new4");
try("noperm-pluslt",   "+<",  \$np);
try("nondir-path",     ">",   "\$np/x");
print "still-running=1\\n";
chmod 0700, "\$d/ro"; chmod 0700, \$np;
PL
   'every refused open answers FALSE with perl\'s errno in $! and the program runs on');
