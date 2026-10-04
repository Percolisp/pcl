#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# phase-units-01.t -- a REQUIRED FILE is a unit, not a program (task #2690;
# ir-spec §9.0a).  The main program's CHECK and INIT blocks run once, at the
# end of ITS compile phase; a `use`d module's CHECK/INIT blocks join those
# queues; a module's own boundary runs only its UNITCHECK blocks; a module
# required at RUN time is "too late" and its CHECK/INIT blocks never run.
# Before #2690 the first module a program used ran the program's queued
# CHECK/INIT blocks at its own boundary, so `pcl -c` (a first CHECK block
# that exits) said "syntax OK" before a later `use` could fail.
#
# Every expected value was PROBED under perl 5.40.3 and written in
# (PCL_PHASE_ORACLE=perl runs the rows under perl; the --no-cache rows then
# fail on the unknown switch, as they should).

use v5.30;
use strict;
use warnings;
use Test::More;
use FindBin qw($RealBin);
use File::Temp qw(tempdir);

my $root = "$RealBin/../..";
my $pcl  = "$root/pcl";
$pcl = 'perl' if ($ENV{PCL_PHASE_ORACLE} // '') eq 'perl';
plan skip_all => "sbcl not found" unless `which sbcl 2>/dev/null`;

my $dir = tempdir(CLEANUP => 1);
mkdir "$dir/lib" or die;

sub put {
    my ($rel, $text) = @_;
    open my $h, '>', "$dir/$rel" or die "$rel: $!";
    print $h $text;
    close $h;
}

# run($shell_args) -> (stdout, stderr, exit status)
sub run {
    my ($args) = @_;
    system("cd '$dir' && '$pcl' $args < /dev/null > .out 2> .err");
    my $st = $? >> 8;
    my ($out, $err) = map { local $/; open my $h, '<', "$dir/$_" or die; my $t = <$h>; $t } '.out', '.err';
    $err =~ s/^PCL Runtime loaded\n//m;
    return ($out, $err, $st);
}

sub row {
    my ($name, $args, $want_out, $want_err, $want_st) = @_;
    my ($out, $err, $st) = run($args);
    my $ok = 1;
    $ok &&= ref $want_out ? $out =~ $want_out : $out eq $want_out if defined $want_out;
    $ok &&= ref $want_err ? $err =~ $want_err : $err eq $want_err if defined $want_err;
    $ok &&= $st == $want_st if defined $want_st;
    ok($ok, $name) or diag "out=[$out] err=[$err] st=$st";
}

put('lib/CheckMod.pm', <<'PM');
package CheckMod;
our $ran = 0;
CHECK { $ran = 1; print "modchk\n" }
INIT { print "modinit\n" }
UNITCHECK { print "modunitchk\n" }
BEGIN { print "modbegin\n" }
print "modbody\n";
sub ran { $ran }
1;
PM

# The task's reproducer: the CHECK block runs after the later BEGIN, not at
# List::Util's boundary.  -e is the text path, where the bug lived.
row('a CHECK before a module use runs after the later BEGIN (-e)',
    q{-e 'CHECK { print "chk\n" } use List::Util; BEGIN { print "b\n" }'}, "b\nchk\n", '', 0);

# pcl -c: the three reproducers.
put('c9.pl', "use strict;\nuse warnings;\nuse File::Basename;\nuse NoSuchModuleXyz;\nprint 1;\n");
row('-c: a failing later use fails the check (no "syntax OK", status 2)',
    q{-c c9.pl}, '', qr/\A(?!.*syntax OK).*Can't locate NoSuchModuleXyz\.pm/s, 2);
put('c7.pl', "use List::Util qw(sum);\nBEGIN { die \"begin died\\n\" }\nprint sum(1,2), \"\\n\";\n");
row('-c: a dying later BEGIN fails the check',
    q{-c c7.pl}, '', qr/\A(?!.*syntax OK).*begin died/s, undef);
put('c2.pl', "use List::Util qw(sum);\nBEGIN { print STDOUT \"b\\n\" }\nprint 3, \"\\n\";\n");
row('-c: a printing later BEGIN prints, then "syntax OK"',
    q{-c c2.pl}, "b\n", "c2.pl syntax OK\n", 0);

# Three sections, two module uses: every BEGIN, then CHECK newest first,
# then INIT in source order.
put('p04.pl', <<'PL');
CHECK { print "c0\n" }
package A; BEGIN { print "bA\n" } CHECK { print "cA\n" } INIT { print "iA\n" }
use File::Basename;
package B; BEGIN { print "bB\n" } CHECK { print "cB\n" } INIT { print "iB\n" } print "runB\n";
package main; use POSIX (); BEGIN { print "bM\n" } print "runM\n";
PL
row('three sections, two module uses: the phase order holds (text path)',
    q{--no-cache p04.pl}, "bA\nbB\nbM\ncB\ncA\nc0\niA\niB\nrunB\nrunM\n", '', 0);

# A used module's CHECK/INIT join the program's queues; its UNITCHECK runs
# at its own boundary.
put('p02.pl', <<'PL');
BEGIN { print "b1\n" } CHECK { print "c1\n" } INIT { print "i1\n" } END { print "e1\n" }
use lib "lib"; use CheckMod;
BEGIN { print "b2\n" } CHECK { print "c2\n" } INIT { print "i2\n" } END { print "e2\n" }
print "run ran=", CheckMod::ran(), "\n";
PL
my $p02 = "b1\nmodbegin\nmodunitchk\nmodbody\nb2\nc2\nmodchk\nc1\ni1\nmodinit\ni2\nrun ran=1\ne2\ne1\n";
row('a used module\'s CHECK/INIT join the program\'s queues (text path)',
    q{--no-cache p02.pl}, $p02, '', 0);
run('p02.pl');    # the cache-building run (its order is #2702's)
row('... and from the cached fasl', q{p02.pl}, $p02, '', 0);

put('p08.pl', <<'PL');
UNITCHECK { print "uc main\n" } use lib "lib"; use CheckMod; CHECK { print "c\n" } BEGIN { print "b\n" }
PL
row('a module boundary runs only its own UNITCHECK, not the program\'s',
    q{--no-cache p08.pl}, "modbegin\nmodunitchk\nmodbody\nb\nuc main\nc\nmodchk\nmodinit\n", '', 0);

# Required at RUN time: too late for CHECK and INIT.
put('p03.pl', <<'PL');
use lib "lib";
print "run1\n";
require CheckMod;
print "ran=", CheckMod::ran(), "\n";
PL
row('a run-time require: the module\'s CHECK/INIT never run',
    q{--no-cache p03.pl}, "run1\nmodbegin\nmodunitchk\nmodbody\nran=0\n", '', 0);

done_testing();
