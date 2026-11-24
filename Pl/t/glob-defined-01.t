#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# glob-defined-01.t -- a glob is looked up, never created, by `defined` and
# by a handle read (tasks #4301, #1423; s532c, the board hunts).
#
# #4301: `defined *{EXPR}` asks whether the glob EXISTS -- perl's rv2gv under
# `defined` creates nothing (OPpDONT_INIT_GV).  PCL lowered it to
# `(p-defined (p-dynamic-typeglob EXPR))`, which MAKES a glob, so the answer
# was always 1 -- Template::Plugin::Procedural installs its proxy's `new`
# only `unless defined *{"${proxy}::new"}`, so every procedural plugin died
# "Can't locate object method new" (Template-Toolkit t/proc.t, 4 rows).
# #1423 (the read half): a GLOB designator `\*DATA` resolved by NAME across
# every package's handles, so a program that loads a module with an
# `__END__` section read the MODULE's DATA (TT t/filter.t read one newline
# and planned 4 tests where perl plans 164).
# Perl ($^X) is the oracle at test time; rows 1-3 FAIL on ce665621, row 4
# (#4623) on c8886b61.

use v5.30;
use strict;
use warnings;
use Test::More;
use File::Temp qw(tempdir);
use FindBin qw($RealBin);
use lib $RealBin;
use PCLCore;

my $project_root = "$RealBin/../..";
my $pl2cl        = "$project_root/pl2cl";
my $runtime      = "$project_root/cl/pcl-runtime.lisp";
my @sbcl_rt      = PCLCore::sbcl_prefix($runtime);

plan skip_all => "pl2cl not found" unless -x $pl2cl;
plan skip_all => "sbcl not found"  unless `which sbcl 2>/dev/null`;

my $workdir = tempdir(CLEANUP => 1);

sub both {
    my ($tag, $body, $stdin) = @_;
    my $in = defined $stdin ? " < $stdin" : "";
    my $src = "$workdir/$tag.pl";
    open(my $s, '>', $src) or die; print {$s} $body; close $s;
    my $perl = `cd $workdir && $^X $src$in 2>&1`;
    my $cl_code = `cd $workdir && $pl2cl $src 2>/dev/null`;
    my $cl_file = "$workdir/$tag.lisp";
    open(my $c, '>', $cl_file) or die; print {$c} $cl_code; close $c;
    my $pcl = `cd $workdir && sbcl @sbcl_rt --load $cl_file$in 2>&1`;
    $pcl =~ s/^;.*\n//gm;
    $pcl =~ s/^PCL Runtime loaded\n//gm;
    $pcl =~ s/^\s*\n//gm;
    return ($perl, $pcl);
}

# #4301: sixteen spellings; 01-05 / 09-12 / 14-15 exist, 06 / 07 / 13 do not,
# 08 says the probe did not create the package, 16 is strict-refs.
my ($perl, $pcl) = both('defined', <<'PL');
no strict 'refs';
sub d { $_[0] ? "def" : "undef" }
our @arr = (1); our %hh = (a => 1); our $sc = 1;
sub real { 1 }
print "01 ", d(defined *{"main::STDOUT"}), "\n";
print "02 ", d(defined *{"main::arr"}), "\n";
print "03 ", d(defined *{"main::hh"}), "\n";
print "04 ", d(defined *{"main::sc"}), "\n";
print "05 ", d(defined *{"main::real"}), "\n";
print "06 ", d(defined *{"main::nothere"}), "\n";
print "07 ", d(defined *{"Nope::Pkg::x"}), "\n";
print "08 ", (%{"Nope::Pkg::"} ? "stash" : "nostash"), "\n";
my $g = \*FOO;
print "09 ", d(defined *$g), "\n";
open my $fh, '<', $0 or die;
print "10 ", d(defined *$fh), "\n";
*{"main::viaglob"} = sub { 2 };
print "11 ", d(defined *{"main::viaglob"}), "\n";
package Bar;
our $bx = 3;
print "12 ", main::d(defined *{"bx"}), "\n";
print "13 ", main::d(defined *{"by"}), "\n";
package main;
my $n = "arr";
print "14 ", d(defined(*{"main::$n"})), "\n";
print "15 ", d(defined *{"real"}), "\n";
my $ok = eval { use strict 'refs'; my $s = "main::real"; defined *{$s} } ? "ok" : "died";
print "16 $ok\n";
# Template::Plugin::Procedural's proxy install, in miniature.
my $proxy = "Proxy::X";
unless (defined(*{ $proxy . "::new" })) {
  *{ $proxy . "::new" } = sub { my $this; bless \$this, $_[0] };
}
print "17 ", ref(Proxy::X->new), "\n";
PL
is($pcl, $perl, 'defined *{EXPR}: an existing glob is defined, a missing one is not and is not created');

# #4301: the MAGICAL names main:: holds from the outset (perl's GV_ADDMG) --
# t/op/magic.t's "*X appears to be defined at the outset" list -- and the
# same names qualified with another package, which are not.
($perl, $pcl) = both('magical', <<'PL');
no strict 'refs';
my @r;
for (qw(SIG ^OPEN ^TAINT ^UNICODE ^UTF8LOCALE ^WARNING_BITS 1 2 9 42 & ` ' ? ! _ - [ ^ ~ = % . ( ) < > \ / $ | + ; ]
        ^A ^C ^E ^H ^L ^N ^S ^T ^V ^W ^UTF8CACHE ::12345 main::98732 ^LAST_FH
        # * { } ^B ^G ^J ^Q 00 01 ARGVOUT EXPORT VERSION ^RE_TRIE_MAXBUF ^FOO ENV INC ARGV STDIN ISA)) {
  my $v = $_;
  $v =~ s/^\^(?=.)// and substr($v, 0, 1) = chr(ord(substr($v, 0, 1)) - 64);
  push @r, "$_=" . (defined *{$v} ? 1 : 0);
}
print "@r\n";
package Foo;
@r = ();
push @r, "$_=" . (defined *{$_} ? 1 : 0), "Foo::$_=" . (defined *{"Foo::$_"} ? 1 : 0) for qw(1 & SIG STDOUT ISA ARGVOUT);
print "@r\n";
PL
is($pcl, $perl, 'defined *{EXPR}: the magical main:: names exist from the outset, a package-qualified twin does not');

# #1423 (read half): `\*DATA` in main is main's DATA, not a module's.
mkdir "$workdir/lib" or die;
open(my $m, '>', "$workdir/lib/FooEnd.pm") or die;
print {$m} "package FooEnd;\nsub x { 1 }\n1;\n__END__\n\n=head1 NAME\n\nFooEnd\n\n=cut\n";
close $m;
($perl, $pcl) = both('data', <<'PL');
use lib 'lib';
use FooEnd;
{ my $s = \*DATA; local $/; my $x = <$s>; print "A ", length($x // q{}), "\n"; }
package main;
print "B ", scalar(<DATA>) // 'undef', "\n";
__DATA__
hello world
second line
PL
is($pcl, $perl, '\*DATA resolves to the glob\'s own package, not a loaded module\'s DATA');

# #4623 (s532c's own regression, the round board): #1423's own-package-first
# read must NOT apply to main's standard handles.  A glob assignment
# (`*STDIN = *STDIN`, Capture::Tiny's `local *STDIN = *CT_ORIG_STDIN`) leaves a
# copy of the IO under main's same-named symbol, which `local *STDIN` neither
# saves nor restores; read first, `fileno(\*STDIN)` answered 0 inside a later
# `local *STDIN` and Capture-Tiny's four tied-STDIN files stopped with no plan.
# STDIN comes from /dev/null so the answer does not depend on the harness.
($perl, $pcl) = both('stdglob', <<'PL', '/dev/null');
sub fd { my $f = fileno($_[0]); defined $f ? $f : "undef" }
my @r;
*STDIN = *STDIN;
{ local *STDIN; push @r, "a=" . fd(\*STDIN); }
push @r, "b=" . fd(\*STDIN);
{ local *O = *STDIN; { local *STDIN = *O; push @r, "c=" . fd(\*STDIN); } }
{ local *STDIN; push @r, "d=" . fd(\*STDIN) . "/" . fd(*STDIN); }
*STDOUT = *STDOUT;
{ local *STDOUT; push @r, "e=" . fd(\*STDOUT); }
push @r, "f=" . fd(\*STDOUT) . "/" . fd(\*STDERR);
print "@r\n";
PL
is($pcl, $perl, 'a standard handle\'s glob reads its own IO inside a later `local`, never a glob-assignment copy');

# s543n -- WHICH GLOB A HANDLE NAME DENOTES, one rule for every spelling
# (%p-handle-key; ir-spec 7.5): qualified = its package, `::` = main,
# unqualified = the current package except the names forced into main.
# #1423: `open(*Foo::STDOUT, ...)` CLOSED the real STDOUT (the resolver found
# main's by the short name), so everything after it was lost, exit 0.
($perl, $pcl) = both('foostd', <<'PL', '/dev/null');
my $D = "/tmp/pcl-gd-$$"; mkdir $D;
sub rd { open my $i, '<', $_[0] or return "none"; local $/; my $c = <$i>; close $i; $c =~ s/\n/|/g; $c }
print "00 start\n";
open(*Foo::STDOUT, '>', "$D/o") or die "o: $!";
print "01 real\n";
print {*Foo::STDOUT} "a\n";
print Foo::STDOUT "b\n";
{ package Foo; print STDOUT "02 Foo's unqualified STDOUT is main's\n"; print Foo::STDOUT "c\n"; }
close *Foo::STDOUT;
print "03 fileno=", fileno(STDOUT), " file=", rd("$D/o"), "\n";
open(*Foo::STDERR, '>', "$D/e") or die; print {*Foo::STDERR} "e\n"; close *Foo::STDERR;
print "04 file=", rd("$D/e"), "\n";
open my $w, '>', "$D/i" or die; print $w "in1\n"; close $w;
open(*Foo::STDIN, '<', "$D/i") or die; my $l = readline(*Foo::STDIN); chomp $l;
print "05 $l fileno(STDIN)=", fileno(STDIN), " real=", (defined(scalar <STDIN>) ? "read" : "undef"), "\n";
my $g = \*Foo::STDOUT; open($g, '>', "$D/r") or die; print $g "r\n"; close $g;
my $s = "Foo::STDOUT"; open($s, '>', "$D/s") or die; print {"Foo::STDOUT"} "s\n"; close $s;
print "06 ", rd("$D/r"), " ", rd("$D/s"), "\n";
system("rm", "-rf", $D);
print "07 end\n";
PL
is($pcl, $perl, '#1423: *Foo::STDOUT / *Foo::STDERR / *Foo::STDIN are Foo\'s own globs; the real handles are untouched');

# #4844: a LEADING `::` is main in every slot and every package (`<::H>` globbed
# the pattern "::H", `readline(::H)` read nothing); `main::FH` written in
# another package is main's FH (it was re-homed in the current package).
($perl, $pcl) = both('colon', <<'PL');
my $D = "/tmp/pcl-gc-$$"; mkdir $D;
sub rd { open my $i, '<', $_[0] or return "none"; local $/; my $c = <$i>; close $i; $c =~ s/\n/|/g; $c }
open my $w, '>', "$D/in" or die; print $w "x\ny\nz\n"; close $w;
open(::H, '<', "$D/in") or die; my $a = <::H>; chomp $a; my @r = <::H>; print "01 $a n=", scalar(@r), "\n"; close ::H;
open(H2, '<', "$D/in") or die; my $b = readline(::H2); chomp $b; print "02 $b eof=", (eof(::H2) ? 1 : 0), "\n";
my $c = 0; while (<::H2>) { $c++ } print "03 while n=$c\n"; close ::H2;
open(MF, '>', "$D/o") or die;
{ package Foo; print main::MF "m\n"; print ::MF "n\n"; open(::G, '<', "$D/in") or die; }
close MF; print "04 ", rd("$D/o"), "\n";
my $g = <G>; chomp $g; print "05 $g\n";
{ package Foo; my $g3 = <::G>; chomp $g3; print "06 $g3 eof=", (eof(::G) ? 1 : 0), "\n"; close ::G; }
{ package Bar; open(main::BF, '<', "$D/in") or die; }
my $bf = <BF>; chomp $bf; print "07 $bf\n"; close BF;
my @gl = <Foo::*.pm>; print "08 glob n=", scalar(@gl), "\n";
system("rm", "-rf", $D);
PL
is($pcl, $perl, '#4844: `::H` and `main::H` name main\'s handle from every package and slot; `<Foo::*.pm>` still globs');

# #5081 and the symbolic half: a handle NAME string is the glob of ITS package
# (unqualified: the package in effect), created in the stash -- `defined *{$n}`
# is true after `open($n, ...)` / `select $n` -- and another package's handle of
# the same short name is never found (`\*FH` in package Q is Q's, not main's).
($perl, $pcl) = both('symname', <<'PL');
no strict 'refs';
my $D = "/tmp/pcl-gs-$$"; mkdir $D;
open my $w, '>', "$D/in" or die; print $w "x\ny\n"; close $w;
open(FH, '<', "$D/in") or die;
{ package Q; print "01 Q FH=", (defined(readline(FH)) ? "read" : "undef"), " \\*FH=", (defined(readline(\*FH)) ? "read" : "undef"), "\n"; }
my $m = <FH>; chomp $m; print "02 main FH=$m\n"; close FH;
my $n = "Foo::SYM"; open($n, '<', "$D/in") or die; my $s = <Foo::SYM>; chomp $s; print "03 $s\n"; close $n;
my $p = "PLAIN"; open($p, '<', "$D/in") or die;
{ package Q; print "04 Q PLAIN=", (defined(readline(PLAIN)) ? "read" : "undef"), "\n"; }
my $pl = <PLAIN>; chomp $pl; print "05 $pl\n"; close PLAIN;
my $g = "SYM000"; print "06 ", (defined *{$g} ? "def" : "undef");
select select $g; print " ", (defined *{$g} ? "def" : "undef");
$g++; print " ", (defined *{$g} ? "def" : "undef");
open($g, ">&STDOUT") or die; print " ", (defined *{$g} ? "def" : "undef"), "\n"; close $g;
open(Foo::OUT, '>', "$D/o") or die; my $old = select(Foo::OUT); print "selected\n"; select($old); close Foo::OUT;
print "07 $old ", (-s "$D/o"), "\n";
system("rm", "-rf", $D);
PL
is($pcl, $perl, '#5081: a symbolic handle name is its package\'s glob, vivified by open and select; no cross-package match');

done_testing();
