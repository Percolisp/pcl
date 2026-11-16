#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# die-location-01.t — task #1240: a die raised BY THE RUNTIME reports the FILE
# and LINE perl reports, not the placeholder `at (eval 0) line 0.`
#
# THE MECHANISM is perl's PL_curcop: `(p-line N)` at the head of every lowered
# statement writes *p-src-line*, `(p-file "…")` at a file's run bucket and at
# every sub body writes *p-src-file-id*, and p-sub-frame saves and restores
# the pair so the CALLER's location is back when a call returns.  It is the
# Kind-A gate `line-track` (Pl/Passes.pm), ON BY DEFAULT since s543a (the
# USER, s543; its cost is in docs/caller-implementation.md) and kept by
# `PCL_OPT=none' -- so every row here runs the DEFAULT build, and the last row
# asserts the emission under the three settings: the default and `none' write
# the registers, the A/B switch `PCL_OPT=-line-track' writes none.
#
# Each program ASSERTS AGAINST ITS OWN $0, so the tempfile path never reaches
# an expectation.  Every expectation below is the live `perl` answer (probed
# s473c, perl 5.40.3).

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

sub run_cl {
    my ($code, %opt) = @_;
    # The row that requires a module runs on its own PCL_CACHE_DIR, so the
    # module is transpiled by THIS tree's compiler and not taken from an
    # entry another tree wrote.
    my $env = defined $opt{cache} ? "PCL_CACHE_DIR=$opt{cache} " : '';
    my $cl_code = PCLCore::transpile($env . "$pl2cl " . write_pl($code));
    my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cl_fh $cl_code;
    close $cl_fh;
    my $output = `$env sbcl @sbcl_rt --load $cl_file 2>&1`;
    $output =~ s/^;.*\n//gm;
    $output =~ s/^PCL Runtime loaded\n//gm;
    $output =~ s/^\s*\n//gm;
    return $output;
}

# ── 1. The five runtime-raised dies the ir-conform corpus names, each
# asserted against the program's own $0 and its own line. ───────────────────
is(run_cl(<<'PL'), "div ok\nneg ok\nmod ok\nundef ok\nsub ok\n",
my ($x, $y) = (1, 0);
eval { my $q = $x / $y };
print "div ", ($@ =~ /^Illegal division by zero at \Q$0\E line 2\.$/m ? "ok" : "BAD[$@]"), "\n";
my @a; eval { $a[-4] = 9 };
print "neg ", ($@ =~ /^Modification of non-creatable array value attempted, subscript -4 at \Q$0\E line 4\.$/m ? "ok" : "BAD[$@]"), "\n";
eval { my $q = 5 % 0 };
print "mod ", ($@ =~ /^Illegal modulus zero at \Q$0\E line 6\.$/m ? "ok" : "BAD[$@]"), "\n";
eval { nosuch_xyz() };
print "undef ", ($@ =~ /^Undefined subroutine &main::nosuch_xyz called at \Q$0\E line 8\.$/m ? "ok" : "BAD[$@]"), "\n";
sub f { last }
eval { f() };
print "sub ", ($@ =~ /^Can't "last" outside a loop block at \Q$0\E line 10\.$/m ? "ok" : "BAD[$@]"), "\n";
PL
   'the corpus five: division, modulus, negative-subscript store, undefined sub, loop control');

# ── 2. THE FRAME RESTORE.  A die in the caller's statement AFTER a call
# returned is the caller's line, not the callee's last line — that is what
# p-sub-frame's p-loc-save is for.  Both spellings of a return. ────────────
is(run_cl(<<'PL'), "after1 ok\nafter2 ok\nnested ok\n",
sub g1 { my $t = 1; my $u = 2; return $t + $u }
sub g2 { 7 }
sub g3 { my $v = g1(); return $v }
my $z = eval { g1() / 0 };
print "after1 ", ($@ =~ /line 4\.$/m ? "ok" : "BAD[$@]"), "\n";
my $w = eval { g2() % 0 };
print "after2 ", ($@ =~ /line 6\.$/m ? "ok" : "BAD[$@]"), "\n";
my $v = eval { g3() / 0 };
print "nested ", ($@ =~ /line 8\.$/m ? "ok" : "BAD[$@]"), "\n";
PL
   'p-sub-frame restores the caller location on a normal return and on `return`');

# ── 3. A CAUGHT die does not leave the location behind: everything after the
# eval reports its own line again.  (p-eval-block's p-loc-save.) ───────────
is(run_cl(<<'PL'), "one ok\ntwo ok\n",
sub boom { my $p = 0; my $q = 1 / $p }
eval { boom() };
print "one ", ($@ =~ /line 1\.$/m ? "ok" : "BAD[$@]"), "\n";
my $n = 0;
eval { my $r = 3 / $n };
print "two ", ($@ =~ /line 5\.$/m ? "ok" : "BAD[$@]"), "\n";
PL
   'the location goes back to the enclosing statement after a caught die');

# ── 4. A die raised inside a `require`d FILE names THAT file; the caller's
# file is back afterwards.  (The file half of the register.) ───────────────
{
    my $dir = File::Temp->newdir;
    my $cache = File::Temp->newdir;
    my $mod = "$dir/PclLocMod.pm";
    open my $mfh, '>', $mod or die $!;
    print $mfh "package PclLocMod;\nsub kaboom { my \$z = 0; return 1 / \$z }\n1;\n";
    close $mfh;
    is(run_cl(<<"PL", cache => "$cache"), "mod ok\nback ok\n",
push \@INC, "$dir";
require PclLocMod;
eval { PclLocMod::kaboom() };
print "mod ", (\$\@ =~ m{^Illegal division by zero at \\Q$mod\\E line 2\\.\$}m ? "ok" : "BAD[\$\@]"), "\\n";
my \$d = 0;
eval { my \$r = 1 / \$d };
print "back ", (\$\@ =~ /\\Q\$0\\E line 6\\./ ? "ok" : "BAD[\$\@]"), "\\n";
PL
       'a die inside a required module names the MODULE file, and the caller file comes back');
}

# ── 5. STRING EVAL: perl calls the file `(eval N)` and numbers the line
# within the eval'd text.  The register holds a NEGATIVE file id for that. ──
is(run_cl(<<'PL'), "ev ok\n",
my $r = eval q{ 1/0 };
print "ev ", ($@ =~ /^Illegal division by zero at \(eval \d+\) line 1\.$/m ? "ok" : "BAD[$@]"), "\n";
PL
   'a runtime die inside a string eval reports (eval N) line M');

# ── 5b. ONE MODULE, TWO SPELLINGS, ONE CACHE (s543a).  The transpile embeds
# the path it was given, so `require "DIR/./M.pm"' and an @INC search for M
# are two texts; the module cache keyed them by the true name alone, and the
# second program reported the FIRST one's spelling on a warm cache.  Each
# program asserts against its own %INC value (perl's die names exactly that).
{
    my $dir = File::Temp->newdir;
    my $cache = File::Temp->newdir;
    open my $mfh, '>', "$dir/PclSpellMod.pm" or die $!;
    print $mfh "package PclSpellMod;\nsub kaboom { my \$z = 0; return 1 / \$z }\n1;\n";
    close $mfh;
    my $check = q{eval { PclSpellMod::kaboom() };
my ($k) = grep { /PclSpellMod/ } keys %INC;
print "SPELL ", ($@ =~ /^Illegal division by zero at \Q$INC{$k}\E line 2\.$/m ? "ok" : "BAD[$INC{$k}][$@]"), "\n";
};
    my $a = run_cl(qq{require "$dir/./PclSpellMod.pm";\n} . $check, cache => "$cache");
    my $b = run_cl(qq{push \@INC, "$dir";\nrequire PclSpellMod;\n} . $check, cache => "$cache");
    is("$a$b", "SPELL ok\nSPELL ok\n",
       'a module reached by two spellings on one warm cache names each its own path');
}

# ── 5c. A file id that was never written names no file (s543a).  Id 0 is
# reserved for `-': before, the first path INTERNED took it, so a caller()
# in a BEGIN block after a module load named the MODULE.  (perl names the
# main file; PCL writes no location in a BEGIN body yet -- #5101.) ─────────
is(run_cl(<<'PL'), "begin ok\n",
BEGIN { require File::Basename }
sub w { my @c = caller; $c[1] }
BEGIN { my $f = w(); print "begin ", ($f =~ /Basename/ ? "BAD[$f]" : "ok"), "\n" }
PL
   'a caller() in a BEGIN block does not name a module loaded before it');

# ── 6. THE GATE (s543a).  The DEFAULT emission and `PCL_OPT=none' (the
# general form) write the registers; the A/B switch `PCL_OPT=-line-track'
# writes none, so it still measures what the flip costs. ───────────────────
{
    my $pl = write_pl("my \$z = 0;\nmy \$q = eval { 1 / \$z };\nprint \"E:[\$\@]\";\n");
    my $on   = PCLCore::transpile("$pl2cl $pl");
    my $none = PCLCore::transpile("PCL_OPT=none $pl2cl $pl");
    my $off  = PCLCore::transpile("PCL_OPT=-line-track $pl2cl $pl");
    ok($on =~ /\(p-line 2\)/ && $on =~ /\(p-file "/
       && $none =~ /\(p-line 2\)/ && $none =~ /\(p-file "/
       && $off !~ /\(p-line \d/ && $off !~ /\(p-file "/,
       'line-track is ON by default and under PCL_OPT=none; PCL_OPT=-line-track emits no (p-line …)/(p-file …)');
}
