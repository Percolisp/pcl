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

plan tests => 50;

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
