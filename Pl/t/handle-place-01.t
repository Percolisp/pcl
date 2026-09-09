#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# handle-place-01.t — a filehandle's PLACE and IDENTITY (s473f).
#
# #1309 — AN INSTALLING HANDLE SLOT IS A PLACE, NOT A VALUE.  `open($a[0],…)`
# lowered its first argument with the element READ accessor, which hands back
# the shared `*p-undef*` KEYWORD for an empty slot; %p-install-fh then took
# that keyword for a handle NAME and registered the stream under it.  So every
# element open in the image shared ONE entry of *p-filehandles*: the open
# reported success, the element stayed undef, and an unrelated
# `readline($b[0])` on a never-opened element read the FIRST file's lines.
# Rows 20–23 are that cross-contamination; it is why the `or die` a careful
# program writes never fired.  The same slot in `sysopen`, `opendir`, `pipe`,
# `socket`, `socketpair` and `accept` is the same one bug (%p-fh-arg's
# :INSTALL slot is the one place it is answered).
#
# #1246 (element half) — perl AUTOVIVIFIES the handle into the place BEFORE it
# attempts the open, so a FAILED `open($a[3],…)` leaves a closed GLOB there and
# grows the array to four elements.  It falls out of the place fix: the failure
# path's %p-autoviv-failed-handle (#1271) now has a box to vivify.
#
# The dirhandle half is rule 11: readdir/closedir/rewinddir each carried their
# own two-armed resolver (symbol or box) while %p-dirhandle-path carried a
# three-armed one, so an element dirhandle opened fine and then read NOTHING.
# One %p-resolve-dh now answers for all four.
#
# THE ORACLE IS PERL ITSELF: the same program is run by perl 5.40.3 and by PCL
# and the two outputs are compared line by line, with ref ADDRESSES normalised
# (PCL's are short by design).  A hand-written expectation could encode the old
# bug; perl cannot.

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
my @sbcl_rt      = PCLCore::sbcl_prefix($runtime);

plan skip_all => "pl2cl not found" unless -x $pl2cl;
plan skip_all => "sbcl not found"  unless `which sbcl 2>/dev/null`;

plan tests => 124;

my $workdir = tempdir(CLEANUP => 1);
my $datafile = "$workdir/data.txt";
open(my $seed, '>', $datafile) or die "seed: $!";
print $seed "L1\nL2\n";
close $seed;
$ENV{PCL_GUARD_FILE} = $datafile;
$ENV{PCL_GUARD_DIR}  = $workdir;

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

sub run_perl {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;
    return `$^X $pl_file 2>&1`;
}

# A ref address is not a fact PCL promises to match (documented divergence),
# so it is normalised on both sides; everything else is compared literally.
sub norm {
    my ($s) = @_;
    $s =~ s/0x[0-9a-fA-F]+/0xADDR/g;
    return $s;
}

my $PROG = <<'PERL';
use strict; use warnings;
no warnings 'uninitialized';
my $F = $ENV{PCL_GUARD_FILE};
my $D = $ENV{PCL_GUARD_DIR};
sub g { my $v = shift; return "undef" if !defined $v; chomp $v; return $v }

# --- the four element PLACES, plus the deref pair -----------------------
{ my @a;  my $ok = open($a[0], "<", $F) ? 1 : 0;
  print "01=$ok ", g(readline($a[0])), "\n"; close($a[0]); }
{ my %h;  my $ok = open($h{k}, "<", $F) ? 1 : 0;
  print "02=$ok ", g(readline($h{k})), "\n"; close($h{k}); }
{ my $ar = []; my $ok = open($ar->[0], "<", $F) ? 1 : 0;
  print "03=$ok ", g(readline($ar->[0])), "\n"; close($ar->[0]); }
{ my $hr = {}; my $ok = open($hr->{fh}, "<", $F) ? 1 : 0;
  print "04=$ok ", g(readline($hr->{fh})), "\n"; close($hr->{fh}); }
{ my @a; my $i = 2; my $ok = open($a[$i], "<", $F) ? 1 : 0;
  print "05=$ok ", g(readline($a[$i])), "\n"; close($a[$i]); }
{ my %h; my $ok = open($h{k}{j}, "<", $F) ? 1 : 0;
  print "06=$ok ", g(readline($h{k}{j})), "\n"; close($h{k}{j}); }
{ my $s; my $r = \$s; my $ok = open($$r, "<", $F) ? 1 : 0;
  print "07=$ok ", g(readline($$r)), "\n"; close($$r); }

# --- the place is VIVIFIED, on success and on failure ------------------
{ my @a; my $ok = open($a[3], "<", "/nonexistent-pcl-xyz") ? 1 : 0;
  printf "08=%s n=%d exists=%d def=%d\n",
    $ok, scalar(@a), (exists $a[3] ? 1 : 0), (defined $a[3] ? 1 : 0); }
{ my %h; my $ok = open($h{k}, "<", "/nonexistent-pcl-xyz") ? 1 : 0;
  printf "09=%s keys=%d exists=%d def=%d\n",
    $ok, scalar(keys %h), (exists $h{k} ? 1 : 0), (defined $h{k} ? 1 : 0); }
{ my @a; open($a[3], "<", $F) or die;
  printf "10=n=%d exists=%d def=%d\n",
    scalar(@a), (exists $a[3] ? 1 : 0), (defined $a[3] ? 1 : 0); close($a[3]); }

# --- the element STRINGIFIES as the handle it holds --------------------
{ my @a; open($a[0], "<", $F) or die; my $s = "$a[0]"; $s =~ s/0x[0-9a-f]+/0xA/;
  print "11=$s\n"; close($a[0]); }
{ my %h; open($h{k}, "<", $F) or die; my $s = "$h{k}"; $s =~ s/0x[0-9a-f]+/0xA/;
  print "12=$s\n"; close($h{k}); }

# --- the SIBLING installers take the same place ------------------------
{ my @a; my $ok = opendir($a[0], $D) ? 1 : 0;
  my $e = readdir($a[0]);
  printf "13=%s got=%s def=%d\n", $ok, (defined $e ? 1 : 0), (defined $a[0] ? 1 : 0);
  closedir($a[0]); }
{ my %h; my $ok = opendir($h{d}, $D) ? 1 : 0;
  my @all = readdir($h{d});
  printf "14=%s n>=2 %d\n", $ok, (scalar(@all) >= 2 ? 1 : 0); closedir($h{d}); }
{ my @a; my $ok = pipe($a[0], $a[1]) ? 1 : 0;
  printf "15=%s def0=%d def1=%d\n", $ok, (defined $a[0]?1:0), (defined $a[1]?1:0); }
{ my $o = {}; my $ok = pipe($o->{r}, $o->{w}) ? 1 : 0;
  print { $o->{w} } "ping\n"; close($o->{w});
  my $l = readline($o->{r});
  printf "16=%s got=%s\n", $ok, g($l); close($o->{r}); }

# --- a dirhandle in a BOX and in a bareword still work -----------------
{ my $dh; opendir($dh, $D) or die; my @all = readdir($dh);
  printf "17=%d\n", (scalar(@all) >= 2 ? 1 : 0); closedir($dh);
  printf "18=%s\n", (defined readdir($dh) ? "still" : "closed"); }
{ opendir(DH, $D) or die; my @all = readdir(DH);
  printf "19=%d\n", (scalar(@all) >= 2 ? 1 : 0); closedir(DH); }

# --- NO CROSS-CONTAMINATION: an element open must not touch anything ---
{ my @a; open($a[0], "<", $F) or die;
  my @b; my $l2 = readline($b[0]);
  my $fh2;  my $l3 = readline($fh2);
  my $u;
  printf "20=%s\n", (defined $l2 ? "LEAK" : "clean");
  printf "21=%s\n", (defined $l3 ? "LEAK" : "clean");
  printf "22=%s\n", (defined $u  ? "LEAK" : "clean");
  printf "23=%s\n", g(readline($a[0]));
  close($a[0]); }

# --- reading an element handle through EVERY builtin slot --------------
{ my @a; open($a[0], "<", $F) or die;
  printf "24=eof=%d fileno>=0=%d tell=%d\n",
    (eof($a[0]) ? 1 : 0), (fileno($a[0]) >= 0 ? 1 : 0), tell($a[0]);
  my $line = readline($a[0]); chomp $line;
  printf "25=%s tell=%d\n", $line, tell($a[0]);
  seek($a[0], 0, 0);
  printf "26=%s\n", g(readline($a[0]));
  printf "27=%s\n", (close($a[0]) ? "closed" : "no"); }

# --- writing through an element handle ---------------------------------
{ my %h; my $out = "$D/w1.txt";
  open($h{out}, ">", $out) or die;
  print { $h{out} } "written\n";
  printf "28=%s\n", (close($h{out}) ? "ok" : "no");
  open(my $in, "<", $out) or die; my $l = <$in>; close $in; chomp $l;
  printf "29=%s\n", $l; }

# --- close's TWO ANSWERS (#1307, #1246's close half) --------------------
# perl's false from `close' is a DEFINED EMPTY STRING; from `closedir' it is
# UNDEF.  They are different facts and the guard says so.
sub cl { my $r = shift; return !defined $r ? "undef" : $r eq "" ? "EMPTY" : "[$r]" }
{ my $fh; open($fh, "<", "/nonexistent-pcl-xyz");
  printf "30=%s\n", cl(close($fh)); }
{ open(my $fh, "<", $F) or die; close($fh); printf "31=%s\n", cl(close($fh)); }
{ printf "32=%s\n", cl(close(NOPEHANDLE)); }
{ open(BADFH, "<", "/nonexistent-pcl-xyz"); printf "33=%s\n", cl(close(BADFH)); }
{ open(OKFH, "<", $F) or die; close(OKFH); printf "34=%s\n", cl(close(OKFH)); }
{ open(my $fh, "<", $F) or die; printf "35=%s\n", cl(close($fh)); }
{ open(my $p, "-|", "/bin/sh -c 'exit 3'") or die; my $r = close($p);
  printf "36=%s status=%d\n", cl($r), $? >> 8; }
{ open(my $p, "-|", "/bin/sh -c 'exit 0'") or die; my $r = close($p);
  printf "37=%s status=%d\n", cl($r), $? >> 8; }
{ my @a; open($a[0], "<", $F) or die;
  printf "38=%s %s\n", cl(close($a[0])), cl(close($a[0])); }
{ printf "39=%s\n", cl(closedir(NOPEDIR)); }
{ opendir(my $dh, $D) or die; closedir($dh); printf "40=%s\n", cl(closedir($dh)); }
{ my $fh; open($fh, "<", "/nonexistent-pcl-xyz"); my $r = close($fh);
  printf "41=defined=%d true=%d len=%d\n",
    (defined $r ? 1 : 0), ($r ? 1 : 0), length($r); }
# $! after a close that had no open handle is EBADF; after a close that failed
# for another reason it is left alone (probed 5.40.3).
{ $! = 0; close(NOPEHANDLE2); printf "42=errno=%d\n", $! + 0; }
{ open(my $fh, "<", $F) or die; close($fh); $! = 0; close($fh);
  printf "43=errno=%d\n", $! + 0; }
{ $! = 0; closedir(NOPEDIR3); printf "44=errno=%d\n", $! + 0; }
{ open(my $fh, "<", $F) or die; $! = 0; close($fh); printf "45=errno=%d\n", $! + 0; }
{ open(my $p, "-|", "/bin/sh -c 'exit 3'") or die; $! = 0; close($p);
  printf "46=errno=%d\n", $! + 0; }
PERL

# ── #1308 + #1233 — WHAT A HANDLE IS, OPEN AND CLOSED ────────────────────
# A box holding a STREAM is PCL's spelling of the GLOB REF perl leaves in the
# scalar, so `ref($fh)` is GLOB, `reftype` is GLOB and `\$fh` is REF; the
# string half already agreed (GLOB(0x…)), which is how the disagreement was
# visible at all.  And `close` does NOT empty the scalar: perl leaves the glob
# there, so `defined $fh` stays 1 and a stat/filetest on the closed handle is
# EBADF, not the ENOENT of the empty PATH that undef used to become.
# `p-get-stream` is what keeps every I/O caller answering "not open": a closed
# stream is not a stream you can do I/O on.
my $PROG2 = <<'PERL';
use strict; use warnings;
no warnings 'uninitialized', 'once';
use Scalar::Util qw(reftype);
my $F = $ENV{PCL_GUARD_FILE};
sub d { my $v = shift; return !defined $v ? "undef" : $v eq "" ? "EMPTY" : "$v" }

{ open(my $fh, "<", $F) or die;
  printf "01 ref=%s reftype=%s defined=%d true=%d\n",
    d(ref($fh)), d(reftype($fh)), (defined $fh?1:0), ($fh?1:0);
  my $s = "$fh"; $s =~ s/0x[0-9a-f]+/0xA/;
  printf "02 str=%s fileno>=0=%d\n", $s, (fileno($fh) >= 0 ? 1 : 0);
  printf "03 refref=%s\n", d(ref(\$fh));
  close($fh); }

{ open(my $cl, "<", $F) or die; close($cl);
  printf "04 ref=%s reftype=%s defined=%d true=%d\n",
    d(ref($cl)), d(reftype($cl)), (defined $cl?1:0), ($cl?1:0);
  my $s = "$cl"; $s =~ s/0x[0-9a-f]+/0xA/;
  printf "05 str=%s\n", $s;
  $! = 0; printf "06 fileno=%s errno=%d\n", d(fileno($cl)), $!+0;
  $! = 0; my $e = -e $cl; printf "07 -e=%s errno=%d\n", d($e), $!+0;
  $! = 0; my @st = stat($cl); printf "08 stat n=%d errno=%d\n", scalar(@st), $!+0;
  $! = 0; my $p = print {$cl} "x"; printf "09 print=%s errno=%d\n", d($p), $!+0;
  $! = 0; my $l = readline($cl); printf "10 readline=%s\n", d($l);
  printf "11 eof=%s\n", d(eof($cl) ? 1 : 0);
  printf "12 binmode=%s\n", d(binmode($cl));
  printf "13 tell=%s\n", d(tell($cl)); }

{ my $bad; open($bad, "<", "/nonexistent-pcl-xyz");
  printf "14 ref=%s defined=%d true=%d\n", d(ref($bad)), (defined $bad?1:0), ($bad?1:0);
  $! = 0; printf "15 fileno=%s\n", d(fileno($bad));
  $! = 0; my $e = -e $bad; printf "16 -e=%s errno=%d\n", d($e), $!+0; }

{ my $never;
  printf "17 ref=%s defined=%d\n", d(ref($never)), (defined $never?1:0);
  $! = 0; my $e = -e $never; printf "18 -e=%s errno=%d\n", d($e), $!+0; }

{ open(my $r, "<", $F) or die; close($r);
  my $ok = open($r, "<", $F) ? 1 : 0;
  my $l = readline($r); chomp $l;
  printf "19 reopen=%d got=%s ref=%s\n", $ok, $l, d(ref($r)); close($r); }

{ open(my $a, "<", $F) or die; my $b = $a;
  printf "20 copy-ref=%s same=%d\n", d(ref($b)), (fileno($a) == fileno($b) ? 1 : 0);
  my $l1 = readline($a); my $l2 = readline($b);
  printf "21 shared-position=%d\n", (($l1//"") ne ($l2//"") ? 1 : 0); close($a); }

# The dup source is PRE-DECLARED here on purpose: the inline-`my` spelling
# `open(my $d, "<&", $a)` loses its SOURCE argument in PCL (task #1420,
# pre-existing name-resolution bug, unrelated to the handle representation).
{ open(my $a, "<", $F) or die; my $dup;
  my $ok = open($dup, "<&", $a) ? 1 : 0;
  printf "22 dup=%d ref=%s distinct=%d\n", $ok, d(ref($dup)),
    ($ok && fileno($dup) != fileno($a) ? 1 : 0);
  close($dup) if $ok; close($a); }

{ sub shut { my $h = shift; my $r = close($h);
             return (defined $r ? ($r eq "" ? "EMPTY" : $r) : "undef") }
  open(my $h, "<", $F) or die;
  my $r = shut($h);
  printf "23 sub-close=%s caller-defined=%d caller-ref=%s\n",
    $r, (defined $h?1:0), d(ref($h));
  $! = 0; printf "24 caller-fileno=%s\n", d(fileno($h)); }

{ open(BW, "<", $F) or die;
  printf "25 bw-open fileno>=0=%d\n", (fileno(BW) >= 0 ? 1 : 0);
  close(BW);
  $! = 0; printf "26 bw-closed fileno=%s\n", d(fileno(BW));
  $! = 0; my $e = -e BW; printf "27 bw-closed -e=%s errno=%d\n", d($e), $!+0; }

{ my $buf = "abc\n"; open(my $m, "<", \$buf) or die;
  printf "28 mem ref=%s fileno=%s\n", d(ref($m)), d(fileno($m));
  close($m);
  printf "29 mem-closed fileno=%s ref=%s defined=%d\n",
    d(fileno($m)), d(ref($m)), (defined $m?1:0); }

{ open(my $g, "<", $F) or die; my $gr = \$g;
  printf "30 refref=%s deref-ref=%s\n", d(ref($gr)), d(ref($$gr)); close($g); }

{ my @a; open($a[0], "<", $F) or die; close($a[0]);
  printf "31 elem-closed ref=%s defined=%d\n", d(ref($a[0])), (defined $a[0]?1:0);
  $! = 0; printf "32 elem-closed fileno=%s\n", d(fileno($a[0]));
  $! = 0; my $e = -e $a[0]; printf "33 elem-closed -e=%s errno=%d\n", d($e), $!+0; }
PERL

# ── EVERY BUILTIN THAT CAN BE HANDED A CLOSED HANDLE ─────────────────────
# Since #1233 a closed lexical handle reaches the builtins as a CLOSED STREAM
# where it used to reach them as NIL, and the CL operation behind several of
# them SIGNALS on one (file-position, stream-external-format, read-char) — so
# those ask %p-live-stream.  The ones that do not need it are half the point:
# `output-stream-p` and `input-stream-p` are already NIL for a closed stream
# (measured), so print/printf and the readers answer perl's way for free, and
# the hot path pays nothing.  `truncate` must never fall back to a file NAMED
# after the handle.
my $PROG3 = <<'PERL';
use strict; use warnings;
no warnings 'uninitialized', 'once';
my $F = $ENV{PCL_GUARD_FILE};
my $TMP = $ENV{PCL_GUARD_DIR} . "/closed-consumers.txt";
sub d { my $v = shift; return !defined $v ? "undef" : $v eq "" ? "EMPTY" : "$v" }
sub cl_in  { open(my $h, "<", $F) or die; close($h); return $h }
sub cl_out { open(my $h, ">", $TMP) or die; close($h); return $h }

my $i = cl_in();
printf "01 readline=%s\n", d(readline($i));
printf "02 eof=%s\n", d(eof($i) ? 1 : 0);
printf "03 fileno=%s\n", d(fileno($i));
printf "04 tell=%s\n", d(tell($i));
printf "05 seek=%s\n", d(seek($i, 0, 0) ? 1 : 0);
printf "06 binmode=%s\n", d(binmode($i));
printf "07 getc=%s\n", d(getc($i));
{ my $buf = ""; printf "08 read=%s\n", d(read($i, $buf, 4)); }
{ my $buf = ""; printf "09 sysread=%s\n", d(sysread($i, $buf, 4)); }
printf "10 sysseek=%s\n", d(sysseek($i, 0, 0));
printf "11 diamond=%s\n", d(scalar <$i>);

my $o = cl_out();
$! = 0; printf "12 print=%s errno=%d\n", d(print {$o} "x"), $!+0;
$! = 0; printf "13 printf=%s errno=%d\n", d(printf {$o} "%s", "x"), $!+0;
$! = 0; printf "14 syswrite=%s errno=%d\n", d(syswrite($o, "x")), $!+0;
$! = 0; printf "15 truncate=%s errno=%d\n", d(truncate($o, 0)), $!+0;
printf "16 binmode-out=%s\n", d(binmode($o));
printf "17 tell-out=%s\n", d(tell($o));
printf "18 eof-out=%s\n", d(eof($o) ? 1 : 0);
printf "19 fileno-out=%s\n", d(fileno($o));

# truncate's failure answers, and the one shape that is NOT a handle
open(my $s, ">", $TMP) or die; print {$s} "hello"; close $s;
$! = 0; printf "20 trunc-bw-never=%s errno=%d\n", d(truncate(NOPETRUNC, 0)), $!+0;
{ open(TFH, "+<", $TMP) or die; close(TFH);
  $! = 0; printf "21 trunc-bw-closed=%s errno=%d\n", d(truncate(TFH, 0)), $!+0; }
{ open(my $h, "+<", $TMP) or die; close($h);
  $! = 0; printf "22 trunc-lex-closed=%s errno=%d\n", d(truncate($h, 0)), $!+0; }
{ my $u; $! = 0; printf "23 trunc-undef=%s errno=%d\n", d(truncate($u, 0)), $!+0; }
$! = 0; printf "24 trunc-path=%s size=%d\n", d(truncate($TMP, 2)), (-s $TMP);
{ open(my $h, "+<", $TMP) or die;
  $! = 0; printf "25 trunc-open=%s size=%d\n", d(truncate($h, 1)), (-s $TMP); close $h; }
unlink $TMP;
print "26 alive\n";
PERL

# ── #1220 — REOPENING A STANDARD HANDLE IS A dup2, WHATEVER NAMED IT ──────
# `open(*STDOUT,'>',$f)` re-points DESCRIPTOR 1 in perl, so the program's own
# prints AND ITS CHILDREN's go to the file (probed 5.40.3 — the child is the
# discriminator; without it "it went to the file" is true either way).
# %p-std-slot was blind to a typeglob, to a glob REF and to a raw name string,
# so those spellings registered a SECOND stream under the name and left
# descriptor 1 alone.  And a QUALIFIER other than main:: disqualifies: perl
# opens THAT glob (row 09 is the negative, and it was silently wrong).
#
# Each case runs in a CHILD, because the case under test redirects stdout.
sub std_case {
    my ($tag, $body) = @_;
    my $out = "$workdir/std-$tag.out";
    my $src = "$workdir/std-$tag.pl";
    unlink $out;
    open(my $s, '>', $src) or die; print {$s} "my \$F = \"$out\";\n$body"; close $s;
    my $cl_code = `$pl2cl $src 2>/dev/null`;
    my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cl_fh $cl_code; close $cl_fh;
    system("sbcl @sbcl_rt --load $cl_file >/dev/null 2>&1");
    my $pcl = "";
    if (open(my $r, '<', $out)) { local $/; $pcl = <$r> // ""; close $r }
    unlink $out;
    system($^X, $src);
    my $perl = "";
    if (open(my $r, '<', $out)) { local $/; $perl = <$r> // ""; close $r }
    unlink $out, $src;
    return ($perl, $pcl);
}

my @std_cases = (
  ['01-glob',      "open(*STDOUT, '>', \$F) or die; print \"parent\\n\"; system(\"echo child\");"],
  ['02-bareword',  "open(STDOUT, '>', \$F) or die; print \"parent\\n\"; system(\"echo child\");"],
  ['03-globref',   "open(\\*STDOUT, '>', \$F) or die; print \"parent\\n\"; system(\"echo child\");"],
  ['04-string',    "my \$n = \"STDOUT\"; open(\$n, '>', \$F) or die; print \"parent\\n\"; system(\"echo child\");"],
  ['05-mainglob',  "open(*main::STDOUT, '>', \$F) or die; print \"parent\\n\";"],
  ['06-globscalar',"my \$g = *STDOUT; open(\$g, '>', \$F) or die; print \"parent\\n\"; system(\"echo child\");"],
  ['07-grefscalar',"my \$g = \\*STDOUT; open(\$g, '>', \$F) or die; print \"parent\\n\"; system(\"echo child\");"],
  ['08-stderr',    "open(*STDERR, '>', \$F) or die; print STDERR \"pe\\n\"; system(\"echo ce 1>&2\");"],
  # THE NEGATIVE: another package's STDOUT is a different glob, and descriptor
  # 1 must be left alone — the program's own print still reaches the terminal.
  ['09-pkgstring', "my \$n = \"Foo::STDOUT\"; open(\$n, '>', \$F) or die; print \"to-real-stdout\\n\"; print {\$n} \"to-foo\\n\";"],
);

for my $c (@std_cases) {
    my ($perl, $pcl) = std_case(@$c);
    is($pcl, $perl, "std handle $c->[0]: PCL redirects exactly as perl does");
}

my $perl_out = run_perl($PROG);
my $pcl_out  = run_pcl($PROG);

my @p = grep { /^\d\d=/ } split /\n/, norm($perl_out);
my @c = grep { /^\d\d=/ } split /\n/, norm($pcl_out);

# The oracle must itself be sane before any row is claimed.
is(scalar(@p), 46, 'perl produced 46 result lines (the oracle is intact)')
  or diag("perl said:\n$perl_out");
like($p[0], qr/^01=1 L1/, 'perl row 01 reads the file (oracle sanity)');
unlike($perl_out, qr/LEAK/, 'perl leaks nothing (oracle sanity)');
is(scalar(@c), scalar(@p), 'PCL produced the same number of result lines')
  or diag("PCL said:\n$pcl_out");

for my $i (0 .. $#p) {
  my $tag = ($p[$i] =~ /^(\d\d)=/) ? "row $1" : "line " . ($i + 1);
  is($c[$i] // '(missing)', $p[$i], "$tag matches perl");
}

my $perl2 = run_perl($PROG2);
my $pcl2  = run_pcl($PROG2);
my @p2 = grep { /^\d\d / } split /\n/, norm($perl2);
my @c2 = grep { /^\d\d / } split /\n/, norm($pcl2);

is(scalar(@p2), 33, 'perl produced 33 representation lines (the oracle is intact)')
  or diag("perl said:\n$perl2");
like($p2[0], qr/^01 ref=GLOB reftype=GLOB/, 'perl calls an open lexical handle a GLOB');
is(scalar(@c2), scalar(@p2), 'PCL produced the same number of representation lines')
  or diag("PCL said:\n$pcl2");

for my $i (0 .. $#p2) {
  my $tag = ($p2[$i] =~ /^(\d\d) /) ? "repr row $1" : "repr line " . ($i + 1);
  is($c2[$i] // '(missing)', $p2[$i], "$tag matches perl");
}

my $perl3 = run_perl($PROG3);
my $pcl3  = run_pcl($PROG3);
my @p3 = grep { /^\d\d / } split /\n/, norm($perl3);
my @c3 = grep { /^\d\d / } split /\n/, norm($pcl3);

is(scalar(@p3), 26, 'perl produced 26 closed-consumer lines (the oracle is intact)')
  or diag("perl said:\n$perl3");
like($p3[-1], qr/^26 alive/, 'perl survived every closed-handle call');
is(scalar(@c3), scalar(@p3), 'PCL produced the same number of closed-consumer lines')
  or diag("PCL said:\n$pcl3");

for my $i (0 .. $#p3) {
  my $tag = ($p3[$i] =~ /^(\d\d) /) ? "closed row $1" : "closed line " . ($i + 1);
  is($c3[$i] // '(missing)', $p3[$i], "$tag matches perl");
}
