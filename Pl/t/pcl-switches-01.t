#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# pcl-switches-01.t -- perl's command-line switches in `pcl` (s506f; tasks
# #2097, #1702, #1709).  ONE parser (tools/lib/PCLSwitches.pm) reads perl's
# argv grammar and ONE expansion (in pl2cl) applies the source-changing
# switches merged with the program's own #! line.
#
# Every expected value below was PROBED under perl 5.40.3 and written in, so
# the file needs no perl at run time.  Each row runs `./pcl` with STDOUT,
# STDERR and the exit status captured separately.

use v5.30;
use strict;
use warnings;
use Test::More;
use FindBin qw($RealBin);
use File::Temp qw(tempdir);

my $root = "$RealBin/../..";
my $pcl  = "$root/pcl";
plan skip_all => "sbcl not found" unless `which sbcl 2>/dev/null`;

my $dir = tempdir(CLEANUP => 1);

sub put {
    my ($rel, $text) = @_;
    open my $h, '>', "$dir/$rel" or die "$rel: $!";
    print $h $text;
    close $h;
}

# run($shell_args, stdin => TEXT) -> (stdout, stderr, exit status)
sub run {
    my ($args, %o) = @_;
    put('.stdin', defined $o{stdin} ? $o{stdin} : '');
    system("cd '$dir' && $o{env} '$pcl' $args < .stdin > .out 2> .err") if $o{env};
    system("cd '$dir' && '$pcl' $args < .stdin > .out 2> .err") if !$o{env};
    my $st = $? >> 8;
    my ($out, $err) = map { local $/; open my $h, '<', "$dir/$_" or die; my $t = <$h>; $t } '.out', '.err';
    $err =~ s/^PCL Runtime loaded\n//m;
    return ($out, $err, $st);
}

sub row {
    my ($name, $args, $want_out, $want_err, $want_st, %o) = @_;
    my ($out, $err, $st) = run($args, %o);
    my $ok = 1;
    $ok &&= ref $want_out ? $out =~ $want_out : $out eq $want_out if defined $want_out;
    $ok &&= ref $want_err ? $err =~ $want_err : $err eq $want_err if defined $want_err;
    $ok &&= $st == $want_st if defined $want_st;
    ok($ok, $name) or diag "out=[$out] err=[$err] st=$st";
}

# ---- member 1: the argv grammar ------------------------------------------
my $UNREC = "  (-h will show valid options).\n";
row('unknown switch: perl\'s message and status, not a script name',
    q{-A -e 1}, '', "Unrecognized switch: -A$UNREC", 25);
row('unknown switch inside a cluster names the rest from the bad letter',
    q{-lAz -e 1}, '', "Unrecognized switch: -Az$UNREC", 25);
row('an unknown --word is perl\'s unknown switch',
    q{--foo -e 1}, '', "Unrecognized switch: --foo$UNREC", 25);
row('-e is repeatable; the lines join, each its own line number',
    q{-e 'print "a\n";' -e 'print __LINE__, " $0\n"'}, "a\n2 -e\n", '', 0);
row('-e attached in a cluster', q{'-eprint 5'}, '5', '', 0);
row('-v prints the version (not --verbose)', q{-v}, qr/\Apcl \(PCL\) /, '', 0);
row('-V:name answers from PCL\'s %Config in perl\'s format',
    q{-V:osname}, "osname='linux';\n", '', 0);
row('-V:name for a name %Config does not hold', q{-V:nosuch}, "nosuch='UNKNOWN';\n", '', 0);
row('-V:regex matches names', q{'-V:o.*name'}, "osname='linux';\n", '', 0);
row('-V: the summary and @INC in perl\'s layout',
    q{-V}, qr/^Summary of my perl5 \(revision 5 version 40 subversion \d+\) configuration:\n.*^  \@INC:\n/ms, '', 0);
row('-h prints the usage', q{-h}, qr/\AUsage: pcl \[switches\] \[--\] \[programfile\] \[arguments\]\n/, '', 0);
row('-? prints the usage', q{'-?'}, qr/\AUsage: pcl /, '', 0);
row('no program and no -e: the program comes from STDIN, $0 is "-"',
    q{}, "stdin prog -\n", '', 0, stdin => qq{print "stdin prog \$0\\n";\n});
row('`-` reads the program from STDIN, the rest is @ARGV',
    q{- a b}, "dash - a b\n", '', 0, stdin => qq{print "dash \$0 \@ARGV\\n";\n});
row('-M with no module: perl\'s error', q{-M- -e1}, '', "Module name required with -M option.\n", 25);
row('-M:Foo is not allowed', q{-M:Foo -e1}, '', "Invalid module name :Foo with -M option: contains single ':'.\n", 25);
row('-mFoo:Bar is not allowed', q{-mFoo:Bar -e1}, '', "Invalid module name Foo:Bar with -m option: contains single ':'.\n", 25);
row('-e with no code', q{-e}, '', "No code specified for -e.\n", 25);
row('-c: "NAME syntax OK" on STDERR, nothing run', q{-c -e 'print 1'}, '', "-e syntax OK\n", 0);
row('-mMod imports nothing', q{-mList::Util -e 'print defined(&sum) ? "imp" : "noimp", "\n"'}, "noimp\n", '', 0);
row('-mMod=a imports a', q{-mList::Util=sum -e 'print sum(1,2), "\n"'}, "3\n", '', 0);
row('-M\'Mod qw(a)\' takes the rest verbatim', q{'-MList::Util qw(sum)' -e 'print sum(1,2), "\n"'}, "3\n", '', 0);
row('after -e a word that is not a switch starts @ARGV',
    q{-e 'print "@ARGV\n"' a -b}, "a -b\n", '', 0);
row('`--` ends the switches', q{-e 'print "@ARGV\n"' -- -x y}, "-x y\n", '', 0);
row('-I takes the next word', q{-I /tmp/qq -e 'print $INC[0], "\n"'}, "/tmp/qq\n", '', 0);
row('-d is refused with one tidy line', q{-d -e 1}, '', "pcl: the perl debugger (-d) is not supported\n", 2);
row('-D warns as a non-debugging perl does and runs the program',
    q{-Dt -e 'print "D\n"'}, "D\n", "Recompile perl with -DDEBUGGING to use -D switch (did you mean -d ?)\n", 0);
put('spath.pl', qq{print "found via PATH\\n";\n});
mkdir "$dir/sbin"; rename "$dir/spath.pl", "$dir/sbin/spath.pl";
row('-S looks the program up along PATH', q{-S spath.pl}, "found via PATH\n", '', 0,
    env => "PATH=\"$dir/sbin:\$PATH\"");
row('--verbose is the long spelling of the old -v', q{--verbose -e 1}, '', qr/^pcl: exec sbcl /m, 0);

done_testing();
