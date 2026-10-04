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
row('after -e, a lone - is an @ARGV word', q{-e 'print "[@ARGV]\n"' - a}, "[- a]\n", '', 0);
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
row('-F\\s+ is a PATTERN, not a literal string', q{'-F\s+' -lane 'print $F[2]'}, "c\n", '', 0, stdin => "a  b\tc\n");
row('-F\'s argument ends at whitespace (-F\' \' is the pattern \')', q{"-F' '" -lane 'print $F[1]'}, "x  d\n", '', 0,
    stdin => "c'x  d\n");
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

# ---- member 3: the program's own #! line (#1702) -------------------------
my $AB = "a b\nc d\n";
put('sb1.pl', qq{#!/usr/bin/perl -n\nprint "[\$_]";\n});
row('#!/usr/bin/perl -n runs the body in the loop', q{sb1.pl}, "[a b\n][c d\n]", '', 0, stdin => $AB);
put('shn.pl', qq{#!perl -n\nprint "[\$_]";\n});
row('command-line -l + #!perl -n: the loop is rebuilt and chomps', q{-l shn.pl}, "[a b]\n[c d]\n", '', 0, stdin => $AB);
put('shl.pl', qq{#!perl -l\nprint "[\$_]";\n});
row('command-line -n + #!perl -l: $\\ is set but the loop does not chomp', q{-n shl.pl}, "[a b\n]\n[c d\n]\n", '', 0, stdin => $AB);
put('na.pl', qq{#!./perl -na\nprint "\$F[1]\\n";\n});
row('#!./perl -na', q{na.pl}, "b\nd\n", '', 0, stdin => $AB);
put('fx.pl', qq{#!./perl -anFx+\nprint "\$F[1]\\n";\n});
row('#!./perl -anFx+', q{fx.pl}, "b\n", '', 0, stdin => "axxbxc\n");
put('pp.pl', qq{#!perl -p\ns/a/A/;\n});
row('#!perl -p', q{pp.pl}, "A b\nc d\n", '', 0, stdin => $AB);
for my $s (qw(x E S V e f)) {
    put("ref$s.pl", qq{#!perl -$s\nprint "body\\n";\n});
    row("#!perl -$s is refused as perl refuses it", "ref$s.pl", '', "Can't emulate -$s on #! line at ref$s.pl line 1.\n", 255);
}
put('tlM.pl', qq{#!perl -Mstrict\nprint "body\\n";\n});
row('#!perl -M is too late', q{tlM.pl}, '', qq{Too late for "-Mstrict" option at tlM.pl line 1.\n}, 255);
put('tlm.pl', qq{#!perl -m\nprint "body\\n";\n});
row('#!perl -m is too late', q{tlm.pl}, '', qq{Too late for "-m" option at tlm.pl line 1.\n}, 255);
put('unA.pl', qq{#!perl -l -A\nprint "body";\n});
row('an unknown switch on the #! line', q{unA.pl}, '', "Unrecognized switch: -A  (-h will show valid options) at unA.pl line 1.\n", 255);
put('pdl.pl', qq{#!/usr/bin/perl-l\nprint "dash";\n});
row('#!/usr/bin/perl-l: no switch after the perl word, nothing applies', q{pdl.pl}, "dash", '', 0);
put('p5.pl', qq{#!perl5.40 -l\nprint "p5";\n});
row('#!perl5.40 -l: the switches after the perl word apply', q{p5.pl}, "p5\n", '', 0);
put('cm.pl', qq{#!perl -l # comment\nprint "a";\n});
row('#!perl -l # comment: switch words end at the first non-switch', q{cm.pl}, "a\n", '', 0);
put('ss.pl', qq{#!perl -s\nprint "s: x=\$x [\@ARGV]\\n";\n});
row('#!perl -s parses the program\'s own switches', q{ss.pl -x a}, "s: x=1 [a]\n", '', 0);
put('si.pl', qq{#!perl -IFoo::Bar -IBla\nprint "\@INC[0,1]\\n";\n});
row('#!perl -IA -IB prepends each in turn (B first)', q{si.pl}, "Bla Foo::Bar\n", '', 0);
put('req1.pl', qq{#!perl -n\nprint "req body ran\\n";\n1;\n});
row('a require\'d file\'s #! line is NOT examined', q{-e 'require "./req1.pl"; print "after\n"'},
    "req body ran\nafter\n", '', 0, stdin => $AB);
row('an eval string\'s #! line is NOT examined',
    q{-e 'eval "#!perl -n\nprint qq{ev ran [\$_]\n};"; print "after\n"'}, "ev ran []\nafter\n", '', 0, stdin => $AB);
row('a #! line inside -e code IS examined', q{-e '#!perl -l' -e 'print 1'}, "1\n", '', 0);

# ---- member 5: -s -x -E -C -w -T and the inert ones ----------------------
row('-s: -name sets $main::name, -name=v sets "v", -- ends it',
    q{-s -e 'print "xyz=$xyz foo=$foo [@ARGV]\n"' -- -xyz -foo=bar a b}, "xyz=1 foo=bar [a b]\n", '', 0);
row('-s: a lone - stops and stays in @ARGV', q{-s -e 'print "xyz=${xyz} [@ARGV]\n"' -- -xyz - a},
    "xyz=1 [- a]\n", '', 0);
row('-s: the variables exist at compile time', q{-s -e 'BEGIN { print "begin xyz=$xyz\n" }' -- -xyz},
    "begin xyz=1\n", '', 0);
row('-s: a second -- is the program\'s', q{-s -e 'print "[@ARGV] q=$q\n"' -- -q -- -r}, "[-r] q=1\n", '', 0);
put('x.pl', qq{garbage\nmore\n#!/usr/bin/perl -l\nprint "x ran";\nwarn "w";\n__END__\nafter\n});
row('-x: leading text skipped, the #! line\'s switches apply, lines count from it',
    q{-x x.pl}, "x ran\n", "w at x.pl line 3.\n", 0);
put('x2.pl', "garbage\n");
row('-x with no #!perl line', q{-x x2.pl}, '', "No Perl script found in input\n", 255);
mkdir "$dir/xd";
put('x3.pl', qq{g\n#!perl\nuse Cwd; print getcwd() =~ m{/xd\$} ? "in xd\\n" : "not\\n";\n});
row('-xDIR changes to DIR first', q{-xxd x3.pl}, "in xd\n", '', 0);
row('-E: say, state, fc, __SUB__, the builtin bundle; strict stays off',
    q{-E 'say "hi"; say reftype([]); state $x = 1; say fc("A"); my $f = sub { __SUB__ }; say ref $f->(); $zz = 1; say $zz'},
    "hi\nARRAY\na\nCODE\n1\n", '', 0);
row('-CS: a :utf8 layer on STDOUT, ${^UNICODE} = 7', q{-CS -e 'print chr(233), " ${^UNICODE}\n"'},
    "\xc3\xa9 7\n", '', 0);
row('-C alone is SDL (95)', q{-C -e 'print "${^UNICODE}\n"'}, "95\n", '', 0, env => 'LANG=en_US.UTF-8 LC_ALL=');
row('-CSL under a non-UTF-8 locale: no layer', q{-CSL -e 'print chr(233), " ${^UNICODE}\n"'},
    "\xe9 71\n", '', 0, env => 'LANG=C LC_ALL= LC_CTYPE=');
row('-CA decodes @ARGV', qq{-CA -e 'print length(\$ARGV[0]), "\\n"' \xc3\xa9}, "1\n", '', 0);
row('-C with an unknown letter', q{-CX -e 1}, '', "Unknown Unicode option letter 'X'.\n", 25);
row('without -C, ${^UNICODE} is 0', q{-e 'print "${^UNICODE}\n"'}, "0\n", '', 0);
row('-w sets $^W at compile time', q{-w -e 'BEGIN { print "b $^W\n" } print "w $^W\n"'}, "b 1\nw 1\n", '', 0);
put('w.pl', qq{#!perl -w\nprint "w \$^W\\n";\n});
row('#!perl -w sets $^W too', q{w.pl}, "w 1\n", '', 0);
row('$^W is assignable (it was a raw 0: the assignment was lost)', q{-e '$^W = 1; print "$^W\n"'}, "1\n", '', 0);
row('-W -X -U -f are accepted and the program runs', q{-W -X -U -f -e 'print "ok\n"'}, "ok\n", '', 0);
row('-T runs the program and says taint checks are not applied',
    q{-T -e 'print "taint ${^TAINT}\n"'}, "taint 0\n",
    "pcl: taint checks (-T) are not applied: PCL does not model taint\n", 0);
row('-c runs the compile phase (BEGIN) and not the program',
    q{-c -e 'BEGIN { print "B\n" } print "run\n"; END { print "E\n" }'}, "B\n", "-e syntax OK\n", 0);
put('ck.pl', qq{BEGIN { print "b1\\n" }\nCHECK { print "c1\\n" }\nINIT { print "i1\\n" }\nprint "run\\n";\nEND { print "e1\\n" }\n});
row('-c runs CHECK blocks too, and no INIT', q{-c ck.pl}, "b1\nc1\n", "ck.pl syntax OK\n", 0);
put('sc.pl', qq{#!perl -c\nBEGIN { print "B\\n" }\nprint "run\\n";\nEND { print "E\\n" }\n});
row('#!perl -c: the same', q{sc.pl}, "B\n", "sc.pl syntax OK\n", 0);
put('sd.pl', qq{#!perl -D\nprint "body\\n";\n});
row('#!perl -D: perl\'s non-debugging message, then the program', q{sd.pl}, "body\n",
    "Recompile perl with -DDEBUGGING to use -D switch (did you mean -d ?)\n", 0);
put('sv.pl', qq{#!perl -v\nprint "body\\n";\n});
row('#!perl -v is refused in one line (perl prints its version; running would be wrong)',
    q{sv.pl}, '', "pcl: -v on the #! line is not supported (at sv.pl line 1)\n", 255);
row('PCL_TAINT_QUIET=1 silences it', q{-t -e 'print "ok\n"'}, "ok\n", '', 0, env => 'PCL_TAINT_QUIET=1');

# ---- s507 review fixes ----------------------------------------------------
# F1: `local $^W = 0` -- the pre-`no warnings` way to quiet a block -- bound a
# DOWN-cased symbol under :invert, so the program's $^W never changed.  A
# called sub sees the local value; it comes back after a die out of an eval.
row('local $^W = 0 takes effect, reaches a called sub, is restored after die',
    q{-e '$^W = 1; { local $^W = 0; print "in:$^W\n" } print "out:$^W\n"; sub p { print "sub:$^W\n" } { local $^W = 0; p() } eval { local $^W = 0; die "x\n" }; print "die:$^W\n"'},
    "in:0\nout:1\nsub:0\ndie:1\n", '', 0);
row('-w + local $^W = 0: the block is quiet', q{-w -e '{ local $^W = 0; print "q:$^W\n" } print "w:$^W\n"'},
    "q:0\nw:1\n", '', 0);
row('local on another caret variable ($^P) binds the runtime\'s symbol too',
    q{-e '{ local $^P = 1; print "P:$^P\n" } print "P:$^P\n"'}, "P:1\nP:0\n", '', 0);
# (F2, #2492 -- the SBCL load context in front of a -e program's die -- is NOT
# fixed here: routing the temp program through the script cache's fasl loader
# exposes #2686 to every -e run; measured, see #2492.)
# A switch prefix that ends in a block (`BEGIN { $^W = 1; }`) is followed by
# `;`: PPI splits a filetest right after a `}` into minus + a sub call
# (docs/ppi-upstream-bugs.md §35; op/filetest.t found it).
row('-w: a program that starts with a filetest', q{-w -e '-T _; print "w\n"'}, "w\n", '', 0);
row('-l: a program that starts with a filetest', q{-l -e '-e _; print "l"'}, "l\n", '', 0);
row('-c: a program that starts with a filetest', q{-c -e '-e _'}, '', "-e syntax OK\n", 0);
# F3: no argument at all after -M is perl's "Missing argument"; an EMPTY one
# is "Module name required" (row above).
row('-M with nothing after it', q{-M}, '', "Missing argument to -M.\n", 25);
row('-m with nothing after it', q{-m}, '', "Missing argument to -m.\n", 25);
# F4: pl2cl's STDIN path honours a #! line for a PROGRAM only -- never under
# --module / --extension (perl reads the main program's #! line only).
{
    put('M4.pm', "#!perl -l\npackage M4; sub f { 1 } 1;\n");
    for my $mode ('--module', '--extension') {
        my $cl = `'$root/pl2cl' $mode < '$dir/M4.pm' 2>&1`;
        ok($cl !~ /\(p-setf \|\$\\\\\| /, "pl2cl $mode on STDIN ignores the #! line (no -l \$\\)");
    }
    my $cl = `'$root/pl2cl' < '$dir/M4.pm' 2>&1`;
    ok($cl =~ /\(p-setf \|\$\\\\\| /, 'pl2cl on STDIN, a program: the #! -l applies (control)');
}

done_testing();
