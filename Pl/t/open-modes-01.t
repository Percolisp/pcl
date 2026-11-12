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

plan tests => 11;

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

# 5. An in-memory open whose SCALAR REF arrives in a VARIABLE (#3202): `my $r =
#    \$s; open $fh, '<', $r` opened the ref's WRAPPER, so the handle read the
#    text "SCALAR(0x..)" and a write never reached $s -- the written-in-place
#    `\$s` was right, an array element holding the ref was right.  Text::CSV_PP
#    ::header reads its header line this way, so `csv(..., bom => 1)` keyed
#    every row by "scalar(0x1)".  The expectation is perl's own output.
{
    my $code = <<'PERL';
my $hdr = "a,b,c\n";
my $hr = \$hdr;
open my $h, "<", $hr or die "open: $!";
print "var-read: ", scalar <$h>;
close $h;
open my $h2, "<", \$hdr or die;
print "literal-read: ", scalar <$h2>;
sub f { my ($x) = @_; my $r = \$x; open my $g, "<", $r or die; return scalar <$g> }
print "sub-read: ", f("x,y\n");
my $out = "";
my $or = \$out;
open my $o, ">", $or or die; print $o "w1\n"; close $o;
print "var-write: $out";
open my $ap, ">>", $or or die; print $ap "w2\n"; close $ap;
print "var-append: $out";
my @refs = (\$hdr);
open my $p, "<", $refs[0] or die; print "elem-read: ", scalar <$p>;
my $rr = \$hr;
open my $q, "<", $rr or die; my $t = <$q>; print "ref-to-ref: ", ($t =~ /^SCALAR\(0x[0-9a-f]+\)$/ ? "the ref's text" : "[$t]"), "\n";
PERL
    my ($pfh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $pfh $code;
    close $pfh;
    my $want = `perl "$pl_file"`;
    is(run_cl($code), $want,
       'open on a scalar ref held in a VARIABLE reads and writes the referent (#3202)');
}

# #4164 (s532a): the TWO-argument open drops the white space around its mode
# and its file name (perlfunc open; doio.c skips isSPACE before and after the
# mode and strips it from the end) -- YAML-Tiny's t/lib/TestUtils.pm slurps
# with `open( FILE, " $file" )`, which died ENOENT.  The THREE-argument form
# keeps the blank (row 6, the control: perl fails it too).  Not here: an
# all-blank name (#4262) and `close` on a `>-` handle (#4263).
{
    my $code = <<'PERL';
my $d = "/tmp/pcl-open2ws-$$"; mkdir $d; my $f = "$d/f.txt";
open(W, "> $f ") or die "w: $!"; print W "line\n"; close W; print "1 ", -s $f, "\n";
open(FILE, " $f") or die "open1 failed: $!"; my $l = <FILE>; close FILE; print "2 $l";
open(my $fh, "< $f  ") or die "open2 failed: $!"; print "3 ", scalar(<$fh>); close $fh;
open(F3, "  <$f") or die "open3 failed: $!"; print "4 ", scalar(<F3>); close F3;
open(F4, ">>  $f\t") or die "open4: $!"; print F4 "more\n"; close F4; print "5 ", -s $f, "\n";
open(F5, "<", " $f") ? print "6 opened\n" : print "6 failed\n";
open(P1, " echo hi | ") or die "p1: $!"; print "7 ", scalar(<P1>); close P1;
open(P2, " | cat ") or die "p2: $!"; print P2 "8 piped\n"; close P2;
open(D, ">&STDOUT ") or die "d: $!"; print D "9 dup\n"; close D;
open(F6, "$f ") or die "f6: $!"; print "10 ", scalar(<F6>); close F6;
unlink $f; rmdir $d;
PERL
    my ($pfh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $pfh $code;
    close $pfh;
    my $want = `perl "$pl_file"`;
    is(run_cl($code), $want,
       'a 2-arg open drops the white space around its mode and name (#4164)');
}

# #4263 + #4262 (s536h): the magic name "-" of the TWO-argument open is a NEW
# handle on STDIN's / STDOUT's stream (doio.c IoTYPE_STD) -- closing it is true,
# leaves $! alone and leaves STDOUT open (PCL closed the program's STDOUT, so
# every later print was LOST); the THREE-argument form opens a FILE named "-".
# An EMPTY name fails ENOENT in every mode (PCL's read opened the current
# directory).
{
    my $code = <<'PERL';
my $d = "/tmp/pcl-opendash-$$"; mkdir $d; chdir $d or die "cd: $!";
open(O, ">-") or die "o: $!"; print O "1 via dash\n"; $! = 2; my $r = close O;
print "2 after close r=$r err=", 0+$!, "\n";
my $r2 = close O; print "3 second close=[$r2] err=", 0+$!, "\n";
open(my $a, ">>-") or die; print "4 fileno=", fileno($a), "\n"; close $a;
open(my $i, "-") or die; print "5 in fileno=", fileno($i), "\n"; close $i;
open(my $j, "+<-") or die; print "6 rw fileno=", fileno($j), "\n"; close $j;
open(my $f3, ">", "-") or die "f3: $!"; print $f3 "file\n"; close $f3;
print "7 file named dash: ", (-s "-"), "\n";
open(my $g3, "<", "-") or die "g3: $!"; print "8 read back: ", scalar(<$g3>); close $g3;
unlink "-"; chdir "/"; rmdir $d;
print "9 stdout still open\n";
PERL
    my ($pfh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $pfh $code;
    close $pfh;
    my $want = `perl "$pl_file" < /dev/null`;
    is(run_cl($code), $want,
       'close of a 2-arg "-" handle leaves STDOUT open; the 3-arg "-" is a file (#4263)');
}
{
    my $code = <<'PERL';
my $d = "/tmp/pcl-openempty-$$"; mkdir $d; chdir $d or die "cd: $!";
my @t = (sub { open(B, "") }, sub { open(C, "<") }, sub { open(D, ">") },
         sub { open(E, "   ") }, sub { open(F, ">>") }, sub { open(G, "+<") },
         sub { open(H, "<", "") }, sub { open(I, ">", "") }, sub { open(J, ">>", "") },
         sub { open(K, "+<", "") }, sub { open(L, "+>", "") }, sub { open(my $n, "<", "") });
for my $k (0 .. $#t) { $! = 0; my $r = $t[$k]->(); print "$k ", ($r ? "opened" : "failed " . (0+$!)), "\n"; }
opendir(my $dh, ".") or die; my @e = grep { !/^\./ } readdir $dh; print "files: [@e]\n";
chdir "/"; rmdir $d;
PERL
    my ($pfh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $pfh $code;
    close $pfh;
    my $want = `perl "$pl_file"`;
    is(run_cl($code), $want,
       'an empty file name fails ENOENT in every open mode (#4262)');
}

# #3618 (s538a): a handle variable holding a REFERENCE that is not a handle --
# `\$coderef`, `[1]`, `{}`, `sub {}`, `\5` -- is perl's rv2gv death "Not a
# GLOB reference", before the open (no file is created).  PCL overwrote the
# variable with a new handle and the open SUCCEEDED, so Text::CSV_PP's
# `csv(in => $f, out => \$code)` opened its INPUT through the out-handle and
# t/90_csv.t stopped at row 117 of 128.  A glob, a glob ref and an open
# handle still reopen; undef autovivifies.
{
    my $code = <<'PERL';
my $d = "/tmp/pcl-openref-$$"; mkdir $d; my $f = "$d/in.txt";
open(my $w, ">", $f) or die; print $w "x\n"; close $w;
my $c = sub { 42 };
my @h = (\$c, [1], {}, sub { 1 }, \5);
for my $k (0 .. $#h) {
    my $fh = $h[$k];
    my $r = eval { open($fh, ">", "$d/new$k") or die "failed: $!"; 1 };
    print "$k ", ($r ? "opened" : "died: " . ($@ =~ /^(.*?) at /s ? $1 : $@)),
          " created=", (-e "$d/new$k" ? 1 : 0), "\n";
}
my $g = \*FH; open($g, "<", $f) or die; print "glob-ref: ", scalar <$g>;
open(my $o, "<", $f) or die; open($o, "<", $f) or die; print "reopen: ", scalar <$o>;
my $u; open($u, "<", $f) or die; print "undef: ", scalar <$u>;
unlink glob("$d/*"); rmdir $d;
PERL
    my ($pfh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $pfh $code;
    close $pfh;
    my $want = `perl "$pl_file"`;
    is(run_cl($code), $want,
       'open on a variable holding a non-handle reference dies "Not a GLOB reference" (#3618)');
}
