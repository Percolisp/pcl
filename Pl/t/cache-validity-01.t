#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# cache-validity-01.t — A CACHED ENTRY MUST NOTICE THAT A DEPENDENCY MOVED
# (task #1860).
#
# The module and script caches already notice "this file changed" (the
# manifest's content hashes, #1261) and "this name did not resolve and now
# does" (#1284).  They did not notice the third: the same NAME resolving to a
# DIFFERENT FILE.  Two shapes, both measured against perl:
#
#   * the search path is UNCHANGED and a file appears EARLIER on it —
#     `d1/B.pm` created on an `-I` directory that was already there, shadowing
#     the `d2/B.pm` the transpile read.  Nothing in any key moves and the
#     recorded dependency still hashes as read, so the entry stayed valid and
#     answered with the old file's parse.  For a main script this was NEW with
#     the script cache (#1841): before it, the program was re-transpiled every
#     run and therefore always right.
#   * the `-I` LIST changes, for a MODULE, whose key does not carry one.  (A
#     script's key does, so that half is closed for a script by #1841.)
#
# The fix makes the manifest SELF-DESCRIBING: per resolved `mod` dependency it
# records every directory the transpiler probed BEFORE the hit and whether the
# hit was in the HEAD of the transpiler's list (a `use lib` directory or PCL's
# shim lib/) or in the BASE (the child perl's @INC part).  The runtime checks
# R1 (no recorded directory holds the file now) always, and R2 (the first
# directory a child would search now still holds the recorded file) for a BASE
# hit of a MODULE entry.
#
# THE ORACLE IS PERL, every row: `sub zap ()`'s empty prototype makes a
# bareword a TERM, so `zap + 1` is `zap() + 1` = 8, and without it `zap(+1)`
# = 107.  A parse fact, and a sharp one.
#
# HALF THIS FILE IS THE OTHER DIRECTION — the cases a careless fix breaks.
# A shim dependency, a `use lib` dependency and a file created LATER on the
# path than the hit must all stay HITS: a rule that called them stale would
# re-transpile for ever, silently paying a transpile per run.  Those rows
# assert by INODE, not by timing: an entry is published temp-file + rename, so
# a rewrite changes the inode while the last-use stamp (utime) does not.

use v5.30;
use strict;
use warnings;
use Test::More;
use File::Temp qw(tempdir);
use File::Path qw(make_path);
use File::Spec;
use FindBin qw($RealBin);

my $root = File::Spec->rel2abs("$RealBin/../..");
my $pcl  = "$root/pcl";

use lib "$RealBin/../../tools/lib";
use PCLSbcl ();

plan skip_all => "pcl not found"  if !-x $pcl;
plan skip_all => "sbcl not found" if !`which sbcl 2>/dev/null`;

my $core = PCLSbcl::cached_core("$root/cl/pcl-runtime.lisp");
plan skip_all => "no cached core" if !($core && -f $core);

plan tests => 26;

# BACKDATED, every fixture.  Validity wants the entry STRICTLY newer than its
# source, and a fixture written in the same second as the entry it produces is
# re-transpiled on the very next run — which would make a stale entry answer
# correctly by accident.  Measured: without this, the inverse verification of
# the R1 rows on an unfixed tree passed about half the time.
sub put {
    my ($path, $body) = @_;
    open my $fh, '>', $path or die "write $path: $!";
    print $fh $body;
    close $fh;
    my $t = time - 60;
    utime($t, $t, $path) or die "utime $path: $!";
    return $path;
}

# The two B.pm bodies: one with the empty prototype, one without.
sub with_proto {
    my ($pkg) = @_;
    return "package $pkg;\nuse Exporter 'import';\nour \@EXPORT = ('zap');\n"
         . "sub zap () { return 7 + (\@_ ? 100 : 0) }\n1;\n";
}
sub no_proto {
    my ($pkg) = @_;
    my $t = with_proto($pkg);
    $t =~ s/sub zap \(\)/sub zap   /;
    return $t;
}

# One `pcl` run with its own cache dir and the ambient core.
sub run_pcl {
    my ($cache, $args) = @_;
    local $ENV{PCL_CACHE_DIR} = $cache;
    local $ENV{PCL_CORE}      = $core;
    local $ENV{PERL5LIB};
    delete $ENV{PERL5LIB};
    my $out = `$pcl $args 2>/dev/null`;
    $out =~ s/\s+\z//;
    return $out;
}

sub run_perl {
    my ($args) = @_;
    my $out = `perl $args 2>/dev/null`;
    $out =~ s/\s+\z//;
    return $out;
}

# path => inode, for every cache entry file.  A re-transpile REPLACES the
# file (temp + rename), so its inode moves; a last-use stamp does not.
sub inodes {
    my ($cache) = @_;
    my %s;
    for my $f (glob("$cache/modules/* $cache/scripts/*")) {
        next if $f =~ /-tmp\d+$/;
        my @st = stat $f or next;
        $s{$f} = $st[1];
    }
    return \%s;
}

sub rewritten {
    my ($before, $after) = @_;
    return scalar grep { !$after->{$_} || $after->{$_} != $before->{$_} }
                  keys %$before;
}

# ─────────────────────────────────────────────────────────────────────────
# R1, MODULE dependency: the search path never moves, the file appears
# EARLIER on it.  prog -> A5 -> B5, and only A5's entry can notice.
{
    my $d = tempdir(CLEANUP => 1);
    make_path("$d/d1", "$d/d2", "$d/shared");
    put("$d/d2/B5.pm", no_proto('B5'));
    put("$d/shared/A5.pm", "package A5;\nuse B5;\nsub go { return zap + 1 }\n1;\n");
    put("$d/prog.pl", "use A5;\nprint A5::go(), \"\\n\";\n");
    my $spec = "-I '$d/shared' -I '$d/d1' -I '$d/d2'";

    is(run_perl("$spec '$d/prog.pl'"), '107',
       'perl: without the prototype the bareword swallows the argument');
    is(run_pcl("$d/cache", "$spec '$d/prog.pl'"), '107', '... and PCL agrees');

    put("$d/d1/B5.pm", with_proto('B5'));   # earlier on the SAME -I list
    is(run_perl("$spec '$d/prog.pl'"), '8',
       'perl: a file created earlier on the path shadows the one read before');
    is(run_pcl("$d/cache", "$spec '$d/prog.pl'"), '8',
       'a MODULE whose dependency now resolves elsewhere is re-transpiled (R1)');
    is(run_pcl("$d/cache", "$spec '$d/prog.pl'"), '8',
       '... and the new answer sticks');
}

# ─────────────────────────────────────────────────────────────────────────
# R2, MODULE dependency: the `-I` LIST changes.  A script's key carries the
# include path (#1841); a module's does not, so this is the module's own half.
{
    my $d = tempdir(CLEANUP => 1);
    make_path("$d/d1", "$d/d2", "$d/shared");
    put("$d/d1/B2.pm", with_proto('B2'));
    put("$d/d2/B2.pm", no_proto('B2'));
    put("$d/shared/A3.pm", "package A3;\nuse B2;\nsub go { return zap + 1 }\n1;\n");
    put("$d/prog.pl", "use A3;\nprint A3::go(), \"\\n\";\n");

    for my $pass (["$d/d1", '8'], ["$d/d2", '107'], ["$d/d1", '8']) {
        my ($which, $want) = @$pass;
        my $spec = "-I '$d/shared' -I '$which'";
        is(run_perl("$spec '$d/prog.pl'"), $want, "perl answers $want under $which");
        is(run_pcl("$d/cache", "$spec '$d/prog.pl'"), $want,
           "a changed -I list re-transpiles the MODULE that read it (R2, $which)");
    }
}

# ─────────────────────────────────────────────────────────────────────────
# R1, SCRIPT dependency: the same shape one level up.  The script cache
# inherited this hole the day it landed, because nothing in its key moves.
{
    my $d = tempdir(CLEANUP => 1);
    make_path("$d/d1", "$d/d2");
    put("$d/d2/B4.pm", no_proto('B4'));
    put("$d/prog.pl", "use B4;\nprint zap + 1, \"\\n\";\n");
    my $spec = "-I '$d/d1' -I '$d/d2'";

    is(run_pcl("$d/cache", "$spec '$d/prog.pl'"), run_perl("$spec '$d/prog.pl'"),
       'a SCRIPT and perl agree before the shadowing file exists');
    put("$d/d1/B4.pm", with_proto('B4'));
    is(run_perl("$spec '$d/prog.pl'"), '8', 'perl switches to the new file');
    is(run_pcl("$d/cache", "$spec '$d/prog.pl'"), '8',
       'and so does the cached SCRIPT (R1 reaches its manifest too)');
}

# ─────────────────────────────────────────────────────────────────────────
# THE OTHER DIRECTION.  Each of these is a HIT that a careless rule breaks,
# and each would break it for ever, not once.
{
    # A SHIM dependency.  PCL's lib/ is FIRST on the transpiler's list and
    # deliberately absent from the child perl's -I, so a rule that asked the
    # runtime to re-resolve the name would call every shim dependency stale on
    # every run.  A shim hit is a HEAD hit and never runs R2.
    my $d = tempdir(CLEANUP => 1);
    make_path("$d/lib");
    put("$d/lib/UsesShim.pm",
        "package UsesShim;\nuse Carp;\nuse POSIX ();\nuse List::Util qw(first);\n"
        . "sub f { return 41 + 1 }\n1;\n");
    put("$d/prog.pl", "use Errno;\nuse UsesShim;\nprint UsesShim::f(), \"\\n\";\n");
    my $spec = "-I '$d/lib'";

    is(run_pcl("$d/cache", "$spec '$d/prog.pl'"), '42', 'a module using shims runs');
    run_pcl("$d/cache", "$spec '$d/prog.pl'");        # let every fasl settle
    my $before = inodes("$d/cache");
    is(run_pcl("$d/cache", "$spec '$d/prog.pl'"), '42', '... and runs again');
    is(rewritten($before, inodes("$d/cache")), 0,
       '... rewriting NO cache entry: a shim dependency is not a moved one');
}

{
    # A `use lib` dependency whose name ALSO exists in a base (-I) directory.
    # The transpiler's list puts the `use lib` directory first, so this is a
    # HEAD hit; the runtime's own list has them in the other order, which is
    # exactly why R2 must not run for a HEAD hit.
    my $d = tempdir(CLEANUP => 1);
    make_path("$d/ul", "$d/base");
    put("$d/ul/B8.pm",   with_proto('B8'));
    put("$d/base/B8.pm", no_proto('B8'));
    put("$d/prog.pl", "use lib '$d/ul';\nuse B8;\nprint zap + 1, \"\\n\";\n");
    my $spec = "-I '$d/base'";

    is(run_pcl("$d/cache", "$spec '$d/prog.pl'"), run_perl("$spec '$d/prog.pl'"),
       'a `use lib` hit that shadows a base twin answers as perl does');
    run_pcl("$d/cache", "$spec '$d/prog.pl'");
    my $before = inodes("$d/cache");
    is(run_pcl("$d/cache", "$spec '$d/prog.pl'"), '8', '... on every later run');
    is(rewritten($before, inodes("$d/cache")), 0, '... and re-transpiles nothing');
}

{
    # A file created LATER on the path than the hit.  perl does not care, and
    # neither may the cache: the recorded prefix stops at the hit.
    my $d = tempdir(CLEANUP => 1);
    make_path("$d/d1", "$d/d2");
    put("$d/d1/B7.pm", with_proto('B7'));
    put("$d/prog.pl", "use B7;\nprint zap + 1, \"\\n\";\n");
    my $spec = "-I '$d/d1' -I '$d/d2'";

    run_pcl("$d/cache", "$spec '$d/prog.pl'");
    run_pcl("$d/cache", "$spec '$d/prog.pl'");
    put("$d/d2/B7.pm", no_proto('B7'));       # LATER on the list than the hit
    my $before = inodes("$d/cache");
    is(run_perl("$spec '$d/prog.pl'"), '8', 'perl ignores a file later on the path');
    is(run_pcl("$d/cache", "$spec '$d/prog.pl'"), '8', '... and so does PCL');
    is(rewritten($before, inodes("$d/cache")), 0,
       '... without re-transpiling: only the prefix BEFORE the hit is watched');
}

# ─────────────────────────────────────────────────────────────────────────
# THE MANIFEST SAYS IT.  The runtime's clauses are only as good as the record
# the transpiler leaves, and a `dep mod` line without one makes the whole
# manifest invalid — so the lines themselves are the contract.
{
    my $d = tempdir(CLEANUP => 1);
    make_path("$d/d1", "$d/lib");
    put("$d/lib/MDep.pm", with_proto('MDep'));
    put("$d/lib/MMid.pm",
        "package MMid;\nuse MDep;\nuse List::Util qw(first);\n"
        . "sub go { return zap + 1 }\n1;\n");
    my $out = "$d/out.deps";
    system("perl -I '$d/d1' -I '$d/lib' '$root/pl2cl' --module --deps '$out' "
           . "'$d/lib/MMid.pm' > /dev/null 2>&1");
    my $text = do { open my $fh, '<', $out or die "$out: $!"; local $/; <$fh> };

    like($text, qr{^resolve\tmod\tMDep\tbase$}m,
         'a dependency found in the child perl\'s @INC part is recorded `base`');
    like($text, qr{^resolve\tmod\tList::Util\thead$}m,
         '... and one found in PCL\'s shim lib/ is recorded `head`');
    like($text, qr{^tried\tmod\tMDep\t\Q$d\E/d1$}m,
         '... with one `tried` line per directory probed before the hit');
}
