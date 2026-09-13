#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# harness-switches.t — the two HARNESS facts the #1501 round-5 census found
# (tasks #1700, #1701).  Both are about how a `runperl` CHILD is built, so they
# live here and not in Pl/t: the gate measures the transpiler, this measures a
# measurement tool.  Run it directly:  prove tools/t/harness-switches.t
#
#   1. `tools/pclperl-for-tests -i[ext]` used to CONSUME the extension and do
#      nothing.  perl's -i IS `$^I = "ext"`, and PCL's `<>` already honours
#      that variable byte for byte, so the switch is one prelude line.  Without
#      it t/run/switches.t aborted at `Failed to open 'tmpswitches.bak'`,
#      35 rows short.
#
#   2. `perl-tests/t/test.pl`'s run_perl() quotemeta'd `args`, where the REAL
#      t/test.pl appends them RAW (its _quote_args quotes only on VMS).  That
#      is not an oversight: `args => ['>', $file]` is how t/io/inplace.t and
#      t/io/iprefix.t ask the SHELL to redirect the child's output into their
#      fixture file, and quotemeta turned the `>` into a literal argument, so
#      the fixtures were never written and every row failed on an absent file.

use strict;
use warnings;
use Test::More;
use File::Temp qw(tempdir);
use FindBin qw($RealBin);

my $root    = "$RealBin/../..";
my $pclperl = "$root/tools/pclperl-for-tests";

plan skip_all => "pclperl-for-tests not found" unless -x $pclperl;
plan skip_all => "sbcl not found" unless `which sbcl 2>/dev/null`;

plan tests => 4;

my $dir = tempdir(CLEANUP => 1);

# ── 1/2. -i.bak edits in place and leaves the backup; bare -i leaves none ────
for my $case (['-i.bak', 1], ['-i', 0]) {
    my ($sw, $want_bak) = @$case;
    my $f = "$dir/ip" . ($want_bak ? "bak" : "nobak") . ".txt";
    open my $o, '>', $f or die "$f: $!";
    print $o "foo\nfoo\n";
    close $o;
    system("$pclperl $sw -p -e 's/foo/bar/' \Q$f\E >/dev/null 2>&1");
    my $main = do { open my $i, '<', $f or die; local $/; <$i> };
    my $bak  = -e "$f.bak"
             ? do { open my $i, '<', "$f.bak" or die; local $/; <$i> }
             : undef;
    is("$main|" . (defined $bak ? $bak : "ABSENT"),
       "bar\nbar\n|" . ($want_bak ? "foo\nfoo\n" : "ABSENT"),
       "pclperl-for-tests $sw sets \$^I: the file is edited in place"
       . ($want_bak ? " and the .bak keeps the original" : " with no backup"));
}

# ── 3. test.pl's run_perl appends `args` RAW, so `['>', $file]` REDIRECTS ────
# Loaded rather than re-implemented: the assertion is about that file's code.
{
    my $out = "$dir/redirect.txt";
    unlink $out;
    local $ENV{PCLPERL} = $pclperl;
    my $rc = system($^X, '-e', <<"CODE");
        chdir "$root/perl-tests/t" or die "chdir: \$!";
        require "./test.pl";
        run_perl(prog => 'print qq(written\\n);', args => ['>', "$out"]);
CODE
    is($rc, 0, 'run_perl() with a redirection in args runs');
    my $got = -e $out ? do { open my $i, '<', $out or die; local $/; <$i> } : "(absent)";
    is($got, "written\n",
       'run_perl(args => [">", $file]) REDIRECTS the child, as the real t/test.pl does');
}
