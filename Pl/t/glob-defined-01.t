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

done_testing();
