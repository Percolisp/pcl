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
# PCL_SWITCHES_ORACLE=perl runs every row under perl instead: how the
# expected values were probed (the pcl-only rows -- -v, -V, -h, -d, -S's
# message, --verbose -- then fail, as they should).
$pcl = 'perl' if ($ENV{PCL_SWITCHES_ORACLE} // '') eq 'perl';
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
mkdir "$dir/sbin"; rename "$dir/spath.pl", "$dir/sbin/spath.pl"; chmod 0755, "$dir/sbin/spath.pl";
row('-S looks the program up along PATH', q{-S spath.pl}, "found via PATH\n", '', 0,
    env => "PATH=\"$dir/sbin:\$PATH\"");
put('sbin/plain.pl', qq{print 1;\n});
row('-S: found but not executable', q{-S plain.pl}, '', "Can't execute $dir/sbin/plain.pl.\n", 25,
    env => "PATH=\"$dir/sbin:\$PATH\"");
row('-S: not on PATH', q{-S nosuch-s506f.pl}, '', "Can't find nosuch-s506f.pl on PATH.\n", 25);
row('--verbose is the long spelling of the old -v', q{--verbose -e 1}, '', qr/^pcl: exec sbcl /m, 0);

# ---- member 2: the expansion (perlrun's documented equivalents) -----------
my $IN = "a b c\nd e f\n\ng:h:i\n";
row('-n', q{-ne 'print if /e/'}, "d e f\n", '', 0, stdin => $IN);
row('-p', q{-pe 's/a/A/'}, "A b c\nd e f\n\ng:h:i\n", '', 0, stdin => $IN);
row('-l chomps under -n and sets $\\', q{-lne 'print length'}, "5\n5\n0\n5\n", '', 0, stdin => $IN);
row('-lane (cluster): -a splits on whitespace', q{-lane 'print $F[1] // "u"'}, "b\ne\nu\nu\n", '', 0, stdin => $IN);
row('-F: (a plain pattern string)', q{-F: -lane 'print $F[1] // "u"'}, "u\nu\nu\nh\n", '', 0, stdin => $IN);
row('-F/:/ (slash-quoted)', q{'-F/:/' -lane 'print $F[1] // "u"'}, "u\nu\nu\nh\n", '', 0, stdin => $IN);
row('-F"X" (double-quoted)', q{'-F"X"' -lane 'print $F[1]'}, "b\n", '', 0, stdin => "aXbXc\n");
row('-F implies -a implies -n', q{-F: -le 'print $F[2]'}, "c\n", '', 0, stdin => "a:b:c\n");
row('-00 paragraph mode', q{-00 -ne 'print "<$_>"'}, "<a b c\nd e f\n\n><g:h:i\n>", '', 0, stdin => $IN);
row('-0777 slurps', q{-0777 -ne 'print length'}, "19", '', 0, stdin => $IN);
row('-g slurps', q{-g -ne 'print length'}, "19", '', 0, stdin => $IN);
row('-0xHH: a hexadecimal separator', q{-0x78 -ne 'print "<$_>"'}, "<ax><bx><c>", '', 0, stdin => "axbxc");
row('-l then -0040: $\\ is the "\\n" -l saw', q{-l -0040 -e 'BEGIN { print STDOUT unpack("H*", $\), "|" }'},
    "0a|\n", '', 0);
row('-0040 then -l: $\\ is the space -0 set', q{-0040 -l -e 'BEGIN { print STDOUT unpack("H*", $\), "|" }'},
    "20| ", '', 0);
row('$/ from -0 is set at compile time (a BEGIN sees it)',
    q{-0777 -e 'BEGIN { print defined $/ ? "rs\n" : "slurp at compile time\n" }'}, "slurp at compile time\n", '', 0);
row('-i with no file names says so as the run starts; $^I visible to BEGIN',
    q{-i -e 'BEGIN { print defined $^I ? "[$^I]\n" : "u\n" }'}, "[]\n",
    "-i used with no filenames on the command line, reading from STDIN.\n", 0);
put('ip.txt', "foo\nbar\n");
row('-pi.bak edits in place', q{-pi.bak -e 's/foo/X/' ip.txt}, '', '', 0);
{
    my $read = sub { local $/; open my $h, '<', "$dir/$_[0]" or return "(missing)"; my $t = <$h>; $t };
    is($read->('ip.txt'), "X\nbar\n", '-pi.bak: the file is rewritten');
    is($read->('ip.txt.bak'), "foo\nbar\n", '-pi.bak: the backup holds the original bytes');
}
row('line numbers do not move under -n', q{-ne 'print __LINE__, "\n" if $. == 2'}, "1\n", '', 0, stdin => $IN);
put('mfile.pl', qq{#!perl\nprint "\$0 ", __FILE__, " ", __LINE__, "\\n" if \$. == 1;\n});
row('a file under -n keeps its name and line numbers', q{-n mfile.pl}, "mfile.pl mfile.pl 2\n", '', 0, stdin => $IN);
put('nend.pl', qq{print "[\$_]";\n__END__\nignored\n});
row('-n closes the loop before __END__', q{-n nend.pl}, "[x\n][y\n]", '', 0, stdin => "x\ny\n");
put('hd.pl', qq{print <<EOT;\nx \$_\n__END__\nEOT\n__END__\nzz\n});
row('a __END__ line inside a heredoc is not the end', q{-n hd.pl}, "x a\n\n__END__\nx b\n\n__END__\n", '', 0, stdin => "a\nb\n");
put('pod.pl', qq{print "[\$_]";\n=pod\n\n__END__\n\n=cut\nprint "after pod\\n";\n__DATA__\nd1\n});
row('a __END__ line inside POD is not the end', q{-n pod.pl}, "[a\n]after pod\n", '', 0, stdin => "a\n");
put('data.pl', qq{print "[\$_]";\nprint <DATA>;\n__DATA__\nd1\n});
row('<DATA> still reads after a -n program', q{-n data.pl}, "[a\n]d1\n", '', 0, stdin => "a\n");
put('mfile2.pl', qq{#!perl\nprint "\$0 ", __FILE__, " ", __LINE__, "\\n";\n});
row('-M with a file: the script keeps its line numbers', q{-MData::Dumper mfile2.pl},
    "mfile2.pl mfile2.pl 2\n", '', 0);
row('-MList::Util=sum -lane', q{-MList::Util=sum -lane 'print sum(map { length } @F)'}, "3\n3\n\n5\n", '', 0, stdin => $IN);
row('-p with next LINE', q{-lpe 'next LINE if /d/; $_ .= "!"'}, "a b c!\nd e f\n!\ng:h:i!\n", '', 0, stdin => $IN);

done_testing();
