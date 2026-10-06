#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# first-run-01.t -- the FIRST run of a program (task #2702, s510f).
#
# The run that finds no compiled file for a program RUNS ITS TRANSPILED TEXT,
# and the compiled file (fasl) is built after the program has ended, in the
# same process, by the tail of the END phase
# (docs/first-run-from-text-design-s510.md; ir-spec §9).  Before this, that
# run compiled the text first -- which ran every `use` before an earlier BEGIN
# block, muffled a module's load-time output and ran `import` twice before the
# program -- and only then loaded the result.
#
# Every expected output below was PROBED against perl 5.40.3 (and asserts
# nothing perl 5.38 answers differently).  Each case gets a fresh directory
# and a fresh PCL_CACHE_DIR; the source is BACKDATED a minute, because the
# cache counts a compiled file that is not strictly newer than its source as
# stale, and a test must never sleep.  "built" = a fasl is in the cache after
# the run; the second run of a built case must be a FASL HIT (its output is
# compared too).  Nothing here depends on wall time.
#
# INVERSE: on main 597a2bd4 the first-run rows of print-mod, warn-mod,
# begin-before-use and the side-effect log fail (the old compile-then-load
# run), the --no-cache die rows print SBCL's "While evaluating" herald, and
# the END { $? = 7 } row is "built" only because the build came BEFORE the
# program; the guard rows (fork, BEGIN exit, import-exit, pipe) were each
# verified to fail with their guard removed (s510f STOP.md).

use v5.30;
use strict;
use warnings;
use Test::More;
use File::Temp qw(tempdir);
use File::Spec;
use FindBin qw($RealBin);

my $root = File::Spec->rel2abs("$RealBin/../..");
my $pcl  = "$root/pcl";

plan skip_all => "pcl not found"  if !-x $pcl;
plan skip_all => "sbcl not found" if !`which sbcl 2>/dev/null`;

my $base = tempdir(CLEANUP => 1);
my $n = 0;

sub slurp { my ($p) = @_; open(my $f, '<:raw', $p) or return ''; local $/; my $t = <$f>; return defined $t ? $t : '' }

# A fresh case directory holding FILES (name => text); p.pl is backdated.
sub setup {
    my (%files) = @_;
    my $d = "$base/c" . ++$n;
    mkdir $d or die "$d: $!";
    mkdir "$d/.cache", 0700 or die; chmod 0700, "$d/.cache";
    for my $f (keys %files) {
        open(my $h, '>:raw', "$d/$f") or die "$d/$f: $!";
        print $h $files{$f};
        close $h;
        my $t = time - 60;
        utime($t, $t, "$d/$f") or die "utime: $!";
    }
    return $d;
}

# Run `pcl ARGS p.pl` in D; answer "out:[..] err:[..] rc:N".
sub run_pcl {
    my ($d, @args) = @_;
    my $pid = fork() // die "fork: $!";
    if (!$pid) {
        chdir $d or die;
        $ENV{PCL_CACHE_DIR} = "$d/.cache";
        delete $ENV{PCL_FASL_DEBUG};
        open(STDIN, '<', '/dev/null'); open(STDOUT, '>', '.o'); open(STDERR, '>', '.e');
        # FIRST_RUN_ORACLE=1 runs perl instead: how the expectations were probed.
        exec($ENV{FIRST_RUN_ORACLE} ? ('perl', 'p.pl') : ($pcl, @args, 'p.pl')) or die;
    }
    waitpid($pid, 0);
    my $rc = $? & 127 ? 'sig' . ($? & 127) : $? >> 8;
    return 'out:[' . slurp("$d/.o") . '] err:[' . slurp("$d/.e") . "] rc:$rc";
}

sub fasls { my ($d) = @_; my @f = glob("$d/.cache/scripts/*.fasl"); return scalar @f }

# CASE: first run = WANT and the fasl is (or is not) built; a built case's
# second run = WANT as well (from the fasl).
sub case {
    my ($name, $want, $built, %files) = @_;
    my $d = setup(%files);
    is(run_pcl($d), $want, "$name: first run");
    is(fasls($d) ? 'built' : 'not built', $built ? 'built' : 'not built',
       "$name: fasl after the first run");
    is(run_pcl($d), $want, "$name: second run") if $built;
    return $d;
}

# --- the first-run differences the design removes (the s510 table) --------
case('print-mod', "out:[P1 body\nmain v\n] err:[] rc:0", 1,
     'p.pl' => qq{use lib '.'; use P1; print "main v\\n";\n},
     'P1.pm' => qq{package P1; print "P1 body\\n"; 1;\n});
case('warn-mod', "out:[main v\n] err:[wm warn\nwm stderr\n] rc:0", 1,
     'p.pl' => qq{use lib '.'; use WarnMod; print "main v\\n";\n},
     'WarnMod.pm' => qq{package WarnMod; warn "wm warn\\n"; print STDERR "wm stderr\\n"; 1;\n});
case('begin-before-use', "out:[b1\nLocMod body\nmain v\n] err:[] rc:0", 1,
     'p.pl' => qq{BEGIN { print "b1\\n" } use lib '.'; use LocMod; print "main v\\n";\n},
     'LocMod.pm' => qq{package LocMod; print "LocMod body\\n"; 1;\n});

# --- the exit paths: status preserved, the file built ---------------------
case('exit 3', "out:[run\n] err:[] rc:3", 1, 'p.pl' => qq{print "run\\n"; exit 3;\n});
case('uncaught die', "out:[] err:[x\n] rc:3", 1, 'p.pl' => qq{\$! = 3; die "x\\n";\n});
# The END phase leaves through a NESTED exit when an END block changed $?;
# that skips every later exit hook, so the build is the END phase's tail.
case('END { $? = 7 }', "out:[run\n] err:[] rc:7", 1,
     'p.pl' => qq{END { \$? = 7 } print "run\\n";\n});
case('END { exit 5 }', "out:[run\n] err:[] rc:5", 1,
     'p.pl' => qq{END { exit 5 } print "run\\n";\n});

# --- exits that run no exit hook: never cached -----------------------------
case('POSIX::_exit', "out:[] err:[] rc:4", 0,
     'p.pl' => qq{use POSIX (); POSIX::_exit(4);\n});
case('signal', "out:[] err:[] rc:sig15", 0,
     'p.pl' => qq{kill 'TERM', \$\$; sleep 5; print "not reached\\n";\n});

# --- the guards, each one's reason --------------------------------------------
my $side = qq{package Side;\n}
  . qq{open(my \$l, ">>", "side.log") or die; print \$l "body\\n"; close \$l;\n}
  . qq{sub import { open(my \$l, ">>", "side.log") or die; print \$l "import\\n"; close \$l }\n1;\n};

# The side-effect order of the first run: perl's, plus ONE import (the build)
# AFTER the program -- the old first run had the body and an import BEFORE the
# program's BEGIN block.
{
    my $d = case('side-effect order', "out:[] err:[] rc:0", 1,
                 'p.pl' => qq{BEGIN { open(my \$l, ">>", "side.log") or die; print \$l "BEGIN\\n"; close \$l }\n}
                         . qq{use lib '.'; use Side;\n},
                 'Side.pm' => $side);
    is(slurp("$d/side.log"), "BEGIN\nbody\nimport\nimport\nBEGIN\nbody\nimport\n",
       'side-effect order: run 1 = perl\'s BEGIN body import + the build\'s import; run 2 = perl\'s');
}

# SAME PROCESS: three fork children that `exit' run the END phase too, and
# without the pid guard each built the file again (an import per build).
{
    my $d = case('fork children', "out:[kid 1\nkid 2\nkid 3\nparent done\n] err:[] rc:0", 1,
                 'p.pl' => qq{use lib '.'; use Side; \$| = 1;\n}
                         . qq{for my \$k (1..3) { my \$pid = fork(); if (!\$pid) { print "kid \$k\\n"; exit 0 } waitpid(\$pid, 0) }\n}
                         . qq{print "parent done\\n";\n},
                 'Side.pm' => $side);
    is(slurp("$d/side.log"), "body\nimport\nimport\nbody\nimport\n",
       'fork children: one build, by the parent (run 1 = perl + one import; run 2 = perl)');
}

# THE COMPILE PHASE COMPLETED: perl never loads Side after BEGIN { exit 4 };
# a build at that exit would.
{
    my $d = case('BEGIN { exit 4 }', "out:[in begin\n] err:[] rc:4", 0,
                 'p.pl' => qq{use lib '.'; BEGIN { print "in begin\\n"; exit 4 } use Side; print "never\\n";\n},
                 'Side.pm' => $side);
    is(slurp("$d/side.log"), '', 'BEGIN { exit 4 }: Side is never loaded');
}

# THE BUILD CANNOT CHANGE THE OUTCOME: an import that exits (or dies) on its
# second call -- the build's -- ends the build only.
case('import exits during the build', "out:[ok\n] err:[] rc:0", 0,
     'p.pl' => qq{use lib '.'; use Ex; print "ok\\n";\n},
     'Ex.pm' => qq{package Ex; my \$n = 0; sub import { exit 9 if ++\$n == 2 } 1;\n});
case('import dies during the build', "out:[ok\n] err:[] rc:0", 0,
     'p.pl' => qq{use lib '.'; use Dx; print "ok\\n";\n},
     'Dx.pm' => qq{package Dx; my \$n = 0; sub import { die "build\\n" if ++\$n == 2 } 1;\n});

# STDOUT AND STDERR ARE CLOSED BEFORE THE BUILD: a reader on a pipe sees
# end-of-file when the PROGRAM is done.  A 300-line program takes far longer
# to compile than the reader takes to look, so at end-of-file there is no
# fasl yet; before the change the build came first and the fasl was always
# there.
{
    my $prog = join('', map { qq{sub f$_ { my (\$x) = \@_; return \$x * $_ + length("s$_") }\n} } 1 .. 300)
             . qq{print "done\\n";\n};
    my $d = setup('p.pl' => $prog);
    my $seen = `cd '$d' && PCL_CACHE_DIR='$d/.cache' '$pcl' p.pl 2>/dev/null </dev/null | perl -e 'my \@l = <STDIN>; my \@f = glob(".cache/scripts/*.fasl"); print scalar(\@l), " line(s), ", (\@f ? "fasl" : "no fasl"), " at EOF\\n"'`;
    is($seen, "1 line(s), no fasl at EOF\n", 'pipe reader: end-of-file before the build');
    is(fasls($d) ? 'built' : 'not built', 'built', 'pipe reader: the fasl is built after');
}

# --- member 1: a die during a TEXT run prints no SBCL lines ----------------
for my $c (["\$! = 3; \$? = 0; die \"x\\n\";\n", 'die'],
           ["use List::Util; \$! = 3; die \"x\\n\";\n", 'die after a use']) {
    my $d = setup('p.pl' => $c->[0]);
    is(run_pcl($d, '--no-cache'), "out:[] err:[x\n] rc:3", "--no-cache, $c->[1]: perl's stderr, no herald");
}

done_testing();
