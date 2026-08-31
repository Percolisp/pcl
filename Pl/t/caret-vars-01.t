#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# caret-vars-01.t — the caret / punctuation MAGIC SCALARS, read and written.
# Every expectation is the live `perl` answer.
#
#   #565  $^R was `(defvar |$^R| nil)` — a RAW value, not a p-box.  box-set
#         silently returns when its place is not a p-box (that is how it lets
#         a write to *p-undef* be a no-op), so `$^R = 7` STORED NOTHING and the
#         read that followed answered undef.  The spelling was never the
#         problem: the assignment and the read both name |$^R|.  A magic scalar
#         a program may ASSIGN to has to be a box, like $^P/$^D/$^F/$^I/$^M.
#
#   #571  $^E and $^C were absent from ExprToCL's %SPECIAL_VARS, so they fell
#         through to the ordinary global path, emitted as a bare `$^E` token,
#         and — because generated code loads under :invert, where an all-upper
#         token reads DOWN-cased — aborted the whole FILE with "The variable
#         $^e is unbound".  On POSIX perl's $^E *is* $!, probed identical in
#         both directions, so it maps onto the same ['p-errno-string'] the `$!`
#         entry uses rather than onto an inert cell of its own (a cell would
#         read "" after a failed syscall — silent wrong).  $^C is 0 at run time
#         in perl and PCL has no -c, so a box holding 0 is exact.
#
#   #573  $: (FORMAT_LINE_BREAK_CHARACTERS) read " n-" instead of " \n-".  A
#         CL string literal has no \n escape — the reader consumes a backslash
#         as "the next character literally" — so `(make-p-box " \n-")` built a
#         three-character string with a LETTER n in the middle.  It was the one
#         default-value mismatch among the 45 punctuation/caret names, and a
#         scan of every CL string literal in cl/*.lisp for a backslash before
#         an alphanumeric finds no second instance.
#
# NOT covered here, and deliberately: `local` on a caret variable is a silent
# no-op (task #600) — the `local` let binds the BARE symbol `$^P` while every
# read emits `|$^P|`, so the two are different symbols under :invert.  That is
# the local-target storage-name authority, not this.

use v5.30;
use strict;
use warnings;
use Test::More;
use File::Temp qw(tempfile);
use FindBin qw($RealBin);
use lib $RealBin;
use PCLCore;

my $project_root = "$RealBin/../..";
my $pl2cl        = "$project_root/pl2cl";
my $runtime      = "$project_root/cl/pcl-runtime.lisp";
my @sbcl_rt = PCLCore::sbcl_prefix($runtime);

plan skip_all => "pl2cl not found" unless -x $pl2cl;
plan skip_all => "sbcl not found"  unless `which sbcl 2>/dev/null`;

plan tests => 8;

sub write_pl {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;
    return $pl_file;
}

# Transpile (PCLCore::transpile FAILS the row on a dropped statement) and run.
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

sub both_agree {
    my ($code, $desc) = @_;
    my $perl = `perl @{[ write_pl($code) ]} 2>&1`;
    my $pcl  = run_cl($code);
    is($pcl, $perl, "$desc (perl: " . ($perl =~ s/\n/\\n/gr) . ")");
}

# ---- #565: $^R round-trips ------------------------------------------------

both_agree(<<'PL', '$^R: undef before any (?{}) block, then read/write round-trip');
print "pre-def=", (defined($^R) ? 1 : 0), " pre=[$^R]\n";
$^R = 7;
print "num=[$^R] plus=", $^R + 1, "\n";
$^R = "str";
print "str=[$^R]\n";
PL

both_agree(<<'PL', '$^R is a real container: a reference to it writes through');
$^R = 1;
my $r = \$^R;
$$r = 99;
print "via-ref=[$^R]\n";
PL

# ---- #571: $^E and $^C exist ----------------------------------------------

# $^E on POSIX is $! — the SAME variable, in both directions.  The row proves
# the alias, not one particular errno string: it sets $! and reads $^E, then
# sets $^E and reads $!.
both_agree(<<'PL', '$^E is $! on POSIX: read, write and numeric context');
$! = 2;
printf "a=[%s] anum=[%d]\n", $^E, $^E + 0;
print "same=", (("$!" eq "$^E") ? 1 : 0), "\n";
$^E = 13;
printf "b=[%s] bnum=[%d]\n", $!, $! + 0;
PL

both_agree(<<'PL', '$^E after a FAILED syscall is the errno string, not ""');
open(my $fh, '<', '/no/such/file/xyzzy-pcl') or 1;
print "E=[$^E]\n";
print "nonempty=", (length("$^E") ? 1 : 0), "\n";
PL

both_agree(<<'PL', '$^C is a defined 0 at run time, and writable');
print "C=[$^C] def=", (defined($^C) ? 1 : 0), "\n";
$^C = 1;
print "C2=[$^C]\n";
PL

# ---- #573: $: default value ------------------------------------------------

# The characters, not the rendering: printing " \n-" straight would compare a
# literal newline and hide a one-character slip.  ord() names each one.
both_agree(<<'PL', '$: defaults to the three chars space, NEWLINE, hyphen');
print "len=", length($:), " ords=", join(",", map { ord } split //, $:), "\n";
$: = "xy";
print "after=[$:]\n";
PL

# ---- #873: a CAPTURE VARIABLE is READ-ONLY --------------------------------
#
# The same mechanism as #565 above, one family over: $1..$20 are plain defvars
# holding a raw STRING and $21+ are read through p-high-capture, so every write
# path received a VALUE where it wanted a place, box-set returned on the
# non-box, and `$1 = 5` was a SILENT no-op where perl dies "Modification of a
# read-only value attempted".  The compiler now names the write SLOTS: an
# assignment target, and `open`'s handle.  `open` is the CONDITIONAL one and
# the condition is autovivification — `open $1` on a DEFINED capture is an
# ordinary symbolic filehandle NAME and does not die, while `open $99` on a
# group that never participated would vivify a glob into a read-only undef.
#
# The reporter must not use s/// : a substitution inside the helper sub would
# clobber the very captures under test (PCL's captures are not block-scoped —
# a separate gap, and the reason index/substr strip the " at FILE line N" tail
# here).
#
# NOT covered, and measured (task #911): `$1 =~ s///` and `$1 =~ tr///`.  perl
# decides those at RUN time — a no-match s///, any /r, and a tr that cannot
# change its target are all "no error" — so the verdict belongs at the
# runtime's two write sites, which today only warn.

both_agree(<<'PL', '#873: writing a capture is perl\'s read-only death, in every slot that dies');
sub t { my ($n,$c)=@_;
        my $r = eval { my $v = $c->(); 1 }; my $e = $@;
        my $i = index($e, " at "); $e = substr($e,0,$i) if $i >= 0;
        print "$n: ", ($r ? "no error" : $e), "\n" }
"abc" =~ /(a)(b)/;  t("1 assign low",   sub { $1 = 5 });
"abc" =~ /(a)(b)/;  t("2 assign high",  sub { $99 = 5 });
"abc" =~ /(a)(b)/;  t("3 assign undef", sub { $1 = undef });
"abc" =~ /(a)(b)/;  t("4 open defined", sub { open $1, "<", "/nope-xyz" });
"abc" =~ /(a)(b)/;  t("5 open undef",   sub { open $99, "<", "/nope-xyz" });
"abc" =~ /(a)(b)/;  print "6 unchanged: $1$2\n";
PL

# INVERSE — reading a capture is fine EVERYWHERE, and a non-capture target is
# still writable.  $0 is the program name, not a capture, and must stay
# writable; the three write slots must not fire on an ordinary place.
both_agree(<<'PL', '#873 inverse: capture READS and non-capture writes are untouched');
"abc" =~ /(a)(b)/;
my $x = $1; my @a = ($1, $2); my %h = (k => $1);
print "copies=$x,$a[0],$a[1],$h{k}\n";
print "expr=", ($1 . $2), " len=", length($1), " def99=", (defined $99 ? "D" : "U"), "\n";
my $z = "z"; $z = 5;             print "plain=$z\n";
my @b = (1,2); $b[0] = 9;        print "elem=$b[0]\n";
my $s = "abc"; $s =~ s/a/X/;     print "subst=$s\n";
my $h2 = {}; $h2->{1} = "one";   print "hashkey=$h2->{1}\n";
$0 = "argv0";                    print "dollar0=ok\n";
my $c = "miss"; if ("zz" =~ /(z)/) { $c = $1 } print "loop=$c\n";
PL
