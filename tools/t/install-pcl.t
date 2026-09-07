#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# End-to-end test for tools/install-pcl (task #277).
#
# NOT part of the Pl/t gate: that gate measures the transpiler, this installs
# one.  Run it directly:  prove tools/t/install-pcl.t
#
# It installs into a temp prefix with --no-core (the core build is a minute of
# SBCL and adds nothing this test can check that the smoke test does not), and
# then asks the questions an installation has to answer:
#   * is the tree there, in the RELATIVE shape the lookups depend on?
#   * do the wrappers run the installed tools, not the checkout's?
#   * does a program transpiled AND run by the installed tools print what perl
#     prints?
#   * does the installed tree carry no development material?
#
# TWO INSTALLS (task #1302, plan doc §5.2).  The `--no-core` one above is for
# the rows a core cannot change; a SECOND install WITH the core is built once
# and reused by every row below it, because half of what an installation has to
# answer only exists once the core does: which core a runner picks, whether a
# program runs from another cwd or through a symlinked bin directory, and —
# the #1303 shape — whether a core built under one HOME sends another HOME's
# modules to the builder's cache.  A core build is a few seconds here and this
# file is not in the gate.
use strict;
use warnings;
use Test::More;
use File::Temp qw(tempdir);
use File::Find;
use FindBin qw($RealBin);

my $root = "$RealBin/../..";
my $inst = "$root/tools/install-pcl";

plan skip_all => "install-pcl not executable" unless -x $inst;
plan skip_all => "sbcl not found" unless `which sbcl 2>/dev/null`;
plan tests => 57;

my $prefix = tempdir(CLEANUP => 1);

# Every file under DIR, as a sorted list — the shape of "nothing here changed".
sub tree_files {
    my ($dir) = @_;
    return () unless -d $dir;
    my @f;
    find({ no_chdir => 1, wanted => sub { push @f, $File::Find::name if -f $File::Find::name } }, $dir);
    return sort @f;
}

# --- the dry run changes nothing -------------------------------------------
my $dry = `$inst --prefix $prefix --dry-run 2>&1`;
is($?, 0, 'dry run exits 0');
like($dry, qr/checking dependencies/, 'dry run reports the dependency check');
ok(!-e "$prefix/bin", 'dry run created nothing');

# --- the dependency FLOOR refuses, loudly, before copying anything ----------
# A fake PPI that claims 1.277 (Ubuntu 24.04's package) shadows the real one
# through PERL5LIB; the installer must name the floor and the remedy and
# install nothing (the first CI run had no floor check: s440).
{
    my $fake = tempdir(CLEANUP => 1);
    open my $fh, '>', "$fake/PPI.pm" or die $!;
    print $fh "package PPI; our \$VERSION = '1.277'; 1;\n";
    close $fh;
    local $ENV{PERL5LIB} = $fake . (defined $ENV{PERL5LIB} ? ":$ENV{PERL5LIB}" : '');
    my $old = `$inst --prefix $prefix 2>&1`;
    isnt($?, 0, 'an old PPI is refused') or diag($old);
    like($old, qr/missing dependencies/, 'the refusal is the dependency report');
    like($old, qr/PPI >= 1\.291 \(this is 1\.277; `cpanm PPI`/, 'the refusal names the floor and the remedy');
    ok(!-e "$prefix/bin" && !-e "$prefix/lib", 'the refusal installed nothing');
}

# --- the real install -------------------------------------------------------
my $out = `$inst --prefix $prefix --no-core 2>&1`;
is($?, 0, 'install exits 0') or diag($out);

ok(-x "$prefix/bin/pl2cl" && -x "$prefix/bin/runpcl", 'both wrappers are executable');
ok(-f "$prefix/lib/pcl/cl/pcl-runtime.lisp" && -f "$prefix/lib/pcl/Pl/Parser2.pm"
   && -f "$prefix/lib/pcl/tools/lib/PCLSbcl.pm" && -d "$prefix/lib/pcl/lib",
   'the tree is installed in its repo-relative shape');
ok(!-e "$prefix/lib/pcl/Pl/t", 'the gate is NOT part of an installation');

# --- the installed tools actually work --------------------------------------
my $src = "$prefix/hello.pl";
open my $fh, '>', $src or die $!;
print $fh qq{my \@w = map { ucfirst } qw(installed pcl);\nprint "\@w\\n";\n};
close $fh;

my $perl_out = `perl $src 2>&1`;
is($perl_out, "Installed Pcl\n", 'the fixture is what we think it is');

my $cl = `$prefix/bin/pl2cl $src 2>&1`;
is($?, 0, 'the installed pl2cl transpiles') or diag($cl);
like($cl, qr/\(p-print/, 'and emits CL');

my $run = `$prefix/bin/runpcl $src 2>&1`;
is($run, $perl_out, 'the installed runpcl prints exactly what perl prints')
    or diag($run);

# --- (f) what is installed is the three commands and what they need ----------
# Never the development runners: they are not part of a PCL, they are part of
# working ON PCL, and shipping them would put a script that expects
# perl-tests/ and a git checkout on a user's PATH.
ok(-x "$prefix/bin/pcl", 'pcl is installed too (#1302 (a)) — it is the everyday command');
ok(!-e "$prefix/lib/pcl/tools/runt" && !-e "$prefix/lib/pcl/tools/clt"
   && !-e "$prefix/lib/pcl/tools/sweep-perl-tests.pl"
   && !-e "$prefix/lib/pcl/tools/install-pcl",
   'the development runners are NOT installed');

# ===========================================================================
# A REAL INSTALL, WITH THE CORE — built once, reused by every row below
# ===========================================================================

my $full = tempdir(CLEANUP => 1);
my $full_out = `$inst --prefix $full 2>&1`;
is($?, 0, 'the install WITH the core exits 0') or diag($full_out);

# --- 1. the core is there, and a runner picks it up -------------------------
# PCLSbcl's resolution step 3: a core sitting beside the runtime IS the
# install's product and wins over the per-user cached core.  If this row ever
# fails while the file above exists, a runner has stopped asking PCLSbcl.
ok(-f "$full/lib/pcl/pcl.core", 'the saved core is built into <prefix>/lib/pcl/pcl.core');
my $shown = `PCL_SHOW_SBCL=1 $full/bin/runpcl $src 2>&1`;
like($shown, qr{--core \Q$full/lib/pcl/pcl.core\E},
     'and the installed runpcl spawns SBCL with exactly that core') or diag($shown);

# --- 2. the installed pcl runs ----------------------------------------------
is(`$full/bin/pcl $src 2>&1`, $perl_out, 'the installed pcl runs a script');
is(`$full/bin/pcl -e 'print 1+2, "\\n"' 2>&1`, "3\n", 'and inline code');

# --- 3. from another cwd, and through a SYMLINKED bin directory -------------
# Both are ways of reaching the wrapper by a path that is not the install's:
# the wrapper execs an absolute path and the real script resolves its tree
# from its OWN real path, so neither can change the answer.
my $elsewhere = tempdir(CLEANUP => 1);
is(`cd $elsewhere && $full/bin/pcl $src 2>&1`, $perl_out, 'the same program from another cwd');
my $linkbin = "$elsewhere/linkbin";
symlink("$full/bin", $linkbin) or die "symlink: $!";
is(`$linkbin/pcl $src 2>&1`, $perl_out, 'and through a SYMLINKED bin directory');

# --- 4. the caches are per USER, the install is read-only -------------------
# The core was built by THIS user under THIS home; a run under a different HOME
# must build its cache under that home (task #1303 — before it, the cache
# directory was a defparameter initform evaluated when the core was SAVED, so a
# system-wide core would have sent every user's modules to the builder's home).
# Two synthetic homes rather than the developer's: the real one is shared with
# whatever else is running on this machine, so a count of it is not evidence.
my $homeA = tempdir(CLEANUP => 1);
my $homeB = tempdir(CLEANUP => 1);
my @prefix_before = tree_files($full);

my $ra = `HOME=$homeA $full/bin/pcl -e 'use List::Util qw(sum); print sum(1..3)' 2>&1`;
is($ra, '6', 'a run under a fresh HOME works (module transpiled and cached there)') or diag($ra);
ok(-d "$homeA/.pcl-cache", "and its cache is under that HOME, not the builder's");

my @a_before = tree_files("$homeA/.pcl-cache");
my $rb = `HOME=$homeB $full/bin/pcl -e 'use List::Util qw(sum); print sum(1..3)' 2>&1`;
is($rb, '6', 'a run under a SECOND home works too');
ok(-d "$homeB/.pcl-cache", 'and gets its own cache');
is_deeply([tree_files("$homeA/.pcl-cache")], \@a_before,
          "the first home's cache was not touched by the second home's run");
is_deeply([tree_files($full)], \@prefix_before,
          'and NOTHING under the install prefix is written at run time');

# PCL_CACHE_DIR moves the module cache, which is the half #1303 fixed: the
# .lisp and its fasl must land under the named directory, not under $HOME.
my $cdir = tempdir(CLEANUP => 1);
my $rc = `HOME=$homeB PCL_CACHE_DIR=$cdir $full/bin/pcl -e 'use Text::ParseWords; print 7' 2>&1`;
is($rc, '7', 'a run with PCL_CACHE_DIR works') or diag($rc);
my @in_cdir = grep { m{/modules/} } tree_files($cdir);
ok(scalar(grep { /\.lisp$/ } @in_cdir),
   'the cached module transpile lands under PCL_CACHE_DIR (#1303)')
    or diag("files under $cdir:\n  " . join("\n  ", tree_files($cdir)));

# --- 7. the PATH hint, both branches ----------------------------------------
# It prints the exact line to paste, and only when it is needed.  Never an rc
# file: a program that edits a shell's startup file is one the user must audit.
my $hint_p = tempdir(CLEANUP => 1);
my $h1 = `$inst --prefix $hint_p --no-core 2>&1`;
like($h1, qr/\Qexport PATH="$hint_p\/bin:\E\$PATH"/,
     'the PATH hint prints the exact export line when <bindir> is not on PATH')
    or diag($h1);
unlike($h1, qr/\.bashrc|\.profile|\.zshrc/, 'and names no rc file');

my $hint_q = tempdir(CLEANUP => 1);
my $h2 = `PATH="$hint_q/bin:\$PATH" $inst --prefix $hint_q --no-core 2>&1`;
unlike($h2, qr/is not on your PATH/,
       'and says nothing at all when <bindir> is already on PATH') or diag($h2);

# --- 8. PCL_ROOT ------------------------------------------------------------
# The override an installed tree may need (a packaged layout, a debugging
# session).  Unusable, it DIES naming both candidates — rule 12: an override
# that is silently ignored is how a run measures a tree nobody meant.
my $noroot = tempdir(CLEANUP => 1);
my $bad = `PCL_ROOT=$noroot $full/bin/pcl --version 2>&1`;
isnt($?, 0, 'an unusable PCL_ROOT is fatal');
like($bad, qr/cannot find the PCL tree/, 'and says so');
like($bad, qr/\Q$noroot\E\s+\(\$PCL_ROOT\)/, 'naming the PCL_ROOT candidate');
like($bad, qr{\Q$full/lib/pcl\E\s+\(would have been derived},
     'AND the one it would have derived — both, side by side') or diag($bad);
is(`PCL_ROOT=$full/lib/pcl $full/bin/pcl $src 2>&1`, $perl_out,
   'PCL_ROOT pointing at the install root works');

# --- (g) the installed tree knows which PCL it is ---------------------------
ok(-f "$full/lib/pcl/VERSION", 'the install records its version');
my $ver = `$full/bin/pcl --version 2>&1`;
like($ver, qr/^pcl \(PCL\) \S/, 'and `pcl --version` reports it') or diag($ver);
unlike($ver, qr/^pcl \(PCL\) unknown/, 'not "unknown" — this tree has a version');

# --- 5. --force replaces the tree; a vanished shim does not survive ----------
# The documented reason for the flag: a copy-over would leave lib/Foo.pm behind
# after the checkout deleted it, and a stale shim is not inert — it is an @INC
# entry that shadows the core module PCL now uses.
open my $ghost, '>', "$prefix/lib/pcl/lib/GhostShim.pm" or die $!;
print $ghost "package GhostShim; 1;\n";
close $ghost;
my $noforce = `$inst --prefix $prefix --no-core 2>&1`;
isnt($?, 0, 'a second install without --force refuses');
like($noforce, qr/already exists.*--force/s, 'and says which flag replaces it');
ok(-f "$prefix/lib/pcl/lib/GhostShim.pm", 'the refusal changed nothing');

my $forced = `$inst --prefix $prefix --no-core --force 2>&1`;
is($?, 0, '--force reinstalls') or diag($forced);
ok(!-e "$prefix/lib/pcl/lib/GhostShim.pm",
   'and a shim that is not in the source tree does not survive it');
ok(-x "$prefix/bin/pcl", 'the reinstalled tree is complete');

# --- 6. --uninstall ---------------------------------------------------------
# It removes what it wrote and nothing else — and a per-user cache is not part
# of an installation, so it stays.
my $cache_marker = "$homeA/.pcl-cache";
my @cache_before = tree_files($cache_marker);
printf '';   # (no output; the counts are the assertion)
my $notmine = "$prefix/bin/mine";
open my $mh, '>', $notmine or die $!;
print $mh "#!/bin/sh\necho not the installer's\n";
close $mh;

my $un = `$inst --uninstall --prefix $prefix 2>&1`;
is($?, 0, '--uninstall exits 0') or diag($un);
ok(!-e "$prefix/lib/pcl", 'the installed tree is gone');
ok(!-e "$prefix/bin/pcl" && !-e "$prefix/bin/pl2cl" && !-e "$prefix/bin/runpcl",
   'and the three wrappers with it');
ok(-e $notmine, 'a file in <bindir> that the installer did not write is left alone');
like($un, qr/cache is untouched/, 'and it says the per-user cache is untouched');
is_deeply([tree_files($cache_marker)], \@cache_before, 'which it is');

my $refuse = `$inst --uninstall --prefix $prefix 2>&1`;
isnt($?, 0, 'uninstalling again refuses');
like($refuse, qr/\Q$prefix\/lib\/pcl\E is not a PCL installation/,
     'naming the directory it will not remove (rule 12)') or diag($refuse);
