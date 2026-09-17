#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# script-cache-01.t — THE MAIN SCRIPT IS A CACHE ENTRY TOO (task #1841).
#
# Until this task `pcl prog.pl` transpiled the program on EVERY run and handed
# SBCL the text, which compiled every form again: measured on cl/pack-impl.pl
# (1,211 lines), 6.57 s every run, where the same code now loads from its
# cached fasl in 0.037 s.  Only `use`d MODULES were cached.
#
# A script entry IS a module entry — same stem rule, same dependency manifest,
# same ONE validity predicate, same fasl identity, same `*pcl-fasl-build*`
# discipline, same prune — so what this file tests is the three things that
# are NEW, plus the one property that makes a cache legitimate at all: a HIT
# and a MISS must be indistinguishable, and both must answer as perl does.
#
#   1. THE INCLUDE PATH IS IN THE KEY.  A script's `use` resolves under the
#      -I list, and which FILE it resolved to changes the emission (an empty
#      prototype makes a bareword a TERM).  perl re-parses every run; a cached
#      script must re-transpile when the search path moves, or it answers with
#      yesterday's parse.  Probed against perl below, both directions.
#   2. THE PATH AS GIVEN IS IN THE KEY, because `$0` is that string verbatim.
#   3. THE COMPILE-POLICY EXEMPTION: a main script is always under `.`/-I, so
#      the module rule would never compile it and the cache would buy nothing.
#      The two OFF switches must still reach it.
#
# EVERY RUN NAMES ITS OWN $PCL_CACHE_DIR *and* $PCL_CORE.  The first is
# isolation; the second is a measurement trap this file fell into while it was
# being written — a fresh PCL_CACHE_DIR has no core/ either, so the first run
# under it rebuilds the whole SBCL core and a 0.2 s miss reads as 4 s.

use v5.30;
use strict;
use warnings;
use Test::More;
use File::Temp qw(tempdir);
use File::Spec;
use FindBin qw($RealBin);

my $root = File::Spec->rel2abs("$RealBin/../..");
my $pcl  = "$root/pcl";

use lib "$RealBin/../../tools/lib";
use PCLSbcl ();

plan skip_all => "pcl not found"  unless -x $pcl;
plan skip_all => "sbcl not found" unless `which sbcl 2>/dev/null`;

my $core = PCLSbcl::cached_core("$root/cl/pcl-runtime.lisp");
plan skip_all => "no cached core" unless $core && -f $core;

plan tests => 51;

my $dir   = tempdir(CLEANUP => 1);   # fixtures
my $cache = tempdir(CLEANUP => 1);   # the cache these rows write

# BACKDATE, for a fixture whose cache HIT is asserted.  Validity wants the
# entry STRICTLY newer than the source, and both are written in the same
# second here, so without this the entry is invalid on its very next run and a
# HIT row can never pass.  (That is the module cache's rule too, and it errs
# towards a needless re-transpile; an EDIT never needs it, because an edit at
# or after the entry's own second already invalidates.)
sub write_file {
    my ($path, $body, $backdate) = @_;
    open my $fh, '>', $path or die "write $path: $!";
    print $fh $body;
    close $fh;
    if ($backdate) { my $t = time - 60; utime($t, $t, $path) or die "utime: $!" }
    return $path;
}

# `scalar(glob(PAT))` is the ITERATOR, not a count -- it answers the first
# match, or undef when there is none.  Every "wrote no entry" row below wants
# the count.
sub count_glob { my @f = glob($_[0]); return scalar @f }

# Run `pcl ARGS` with a named cache dir and the ambient core.  %opt:
#   cache => DIR   (default $cache)
#   env   => HASH  further environment
sub run_pcl {
    my ($args, %opt) = @_;
    my %env = (PCL_CACHE_DIR => ($opt{cache} // $cache), PCL_CORE => $core,
               %{ $opt{env} || {} });
    my %saved;
    for my $k (sort keys %env) { $saved{$k} = $ENV{$k}; $ENV{$k} = $env{$k} }
    local $ENV{PERL5LIB};
    delete $ENV{PERL5LIB};
    my $out = `$pcl $args 2>&1`;
    my $rc  = $?;
    for my $k (sort keys %saved) {
        if (defined $saved{$k}) { $ENV{$k} = $saved{$k} } else { delete $ENV{$k} }
    }
    $out =~ s/^;.*\n//gm;
    $out =~ s/^PCL Runtime loaded\n//gm;
    $out =~ s/^PCL: script .*\n//gm unless $opt{debug};
    return wantarray ? ($out, $rc >> 8) : $out;
}

sub entries { return glob("$cache/scripts/*") }

# ─────────────────────────────────────────────────────────────────────────
# A MISS WRITES AN ENTRY; THE NEXT RUN IS A FASL HIT THAT TRANSPILES NOTHING.
{
    my $p = write_file("$dir/hello.pl", "print \"hello\\n\";\n", 1);

    is(run_pcl("'$p'"), "hello\n", 'a script runs (the cache MISS path)');
    ok(scalar(glob("$cache/scripts/*.lisp")), '... and leaves a transpile');
    ok(scalar(glob("$cache/scripts/*.deps")), '... and its dependency manifest');
    ok(scalar(glob("$cache/scripts/*.fasl")),
       '... and a fasl: the main script is EXEMPT from the module compile policy');

    # THE MECHANISM IS ACTUALLY TAKEN.  Without this row every row below would
    # still pass with the cache silently disabled, which is how a speed change
    # dies unnoticed.  PCL_FASL_DEBUG is the ONE place this cache speaks.
    my $trace = run_pcl("'$p'", debug => 1, env => { PCL_FASL_DEBUG => 1 });
    like($trace, qr/^PCL: script \Q$p\E -> FASL HIT/m,
         'the second run loads the program from its FASL');
    unlike($trace, qr/-> (?:fasl-build|TEXT)/,
           '... and neither transpiles nor compiles again');

    # NOT BY TIMING (which would be a flaky row on a shared box): the entry's
    # own files must not have been rewritten.  Only the last-use stamp may
    # move, and that is at most once a day, so a brand-new entry is untouched.
    my @e = entries();
    my %before = map { $_ => (stat $_)[9] } @e;
    run_pcl("'$p'");
    my $moved = grep { (stat $_)[9] != $before{$_} } @e;
    is($moved, 0, '... and rewrites none of the three files');
    is(scalar(my @now = entries()), scalar(@e), '... and adds no second entry');
}

# ─────────────────────────────────────────────────────────────────────────
# STALENESS, the two kinds.  perl re-parses everything on every run, so both
# of these are simply "PCL must answer what perl answers".
{
    my $p = write_file("$dir/edit.pl", "print 1+1, \"\\n\";\n");
    is(run_pcl("'$p'"), "2\n", 'a script answers before the edit');
    write_file($p, "print 2+2, \"\\n\";\n");
    is(run_pcl("'$p'"), "4\n", 'an EDITED script is re-transpiled, not served stale');
    is(run_pcl("'$p'"), "4\n", '... and stays right on the next run');
}

# EDITING ONLY A USED MODULE'S PROTOTYPE.  The script's own mtime does not
# move; the dependency manifest is the only thing that can notice.  Every
# expectation here is perl 5.40.3's own, probed.
{
    my $mod = <<'PM';
package SCDep;
use Exporter 'import';
our @EXPORT = qw(zap);
sub zap () { return 7 + (@_ ? 100 : 0) }
1;
PM
    (my $noproto = $mod) =~ s/sub zap \(\)/sub zap/;
    write_file("$dir/SCDep.pm", $mod);
    my $p = write_file("$dir/usedep.pl", "use SCDep; print zap + 1, \"\\n\";\n");

    my $oracle = `perl -I '$dir' '$p' 2>&1`;
    is($oracle, "8\n", 'perl: an empty prototype makes the bareword a TERM');
    is(run_pcl("-I '$dir' '$p'"), $oracle, 'PCL agrees on the first run');

    write_file("$dir/SCDep.pm", $noproto);
    my $oracle2 = `perl -I '$dir' '$p' 2>&1`;
    # No newline: without the prototype the whole list is the CALL's argument
    # list, so `print zap + 1, "\n"` is `print zap(+1, "\n")` and the newline
    # never reaches print.  That is what makes this a sharp oracle.
    is($oracle2, "107", 'perl: without it the bareword swallows the whole list');
    is(run_pcl("-I '$dir' '$p'"), $oracle2,
       'editing ONLY the module re-transpiles the SCRIPT (the manifest)');
    is(run_pcl("-I '$dir' '$p'"), $oracle2, '... and the new answer sticks');
    write_file("$dir/SCDep.pm", $mod);
}

# THE INCLUDE PATH IS IN THE KEY (#1841 (c), the probe this task asked for).
# Two -I directories whose SCDep2.pm differ only in that prototype: the same
# script, the same content, the same mtime — only the search path moves.  The
# MODULE side of this has the same hole and it is NOT fixed (task #1860); the
# script side must not acquire it.
{
    my $d1 = tempdir(CLEANUP => 1);
    my $d2 = tempdir(CLEANUP => 1);
    my $with = <<'PM';
package SCDep2;
use Exporter 'import';
our @EXPORT = qw(zap2);
sub zap2 () { return 7 + (@_ ? 100 : 0) }
1;
PM
    (my $without = $with) =~ s/sub zap2 \(\)/sub zap2/;
    write_file("$d1/SCDep2.pm", $with);
    write_file("$d2/SCDep2.pm", $without);
    my $p = write_file("$dir/incmove.pl", "use SCDep2; print zap2 + 1, \"\\n\";\n");

    for my $pass (["-I '$d1'", $d1], ["-I '$d2'", $d2], ["-I '$d1'", $d1]) {
        my ($flag, $d) = @$pass;
        my $oracle = `perl -I '$d' '$p' 2>&1`;
        is(run_pcl("$flag '$p'"), $oracle,
           "a -I that resolves the same NAME to another file re-transpiles ($d)");
    }
}

# THE PATH AS GIVEN IS IN THE KEY: `$0` is that string verbatim, so the two
# spellings of one file are two entries and each answers for itself.
{
    my $p = write_file("$dir/dollar0.pl", "print \"0=\$0\\n\";\n");
    my $rel = File::Spec->abs2rel($p, '.');
    is(run_pcl("'$p'"),      "0=$p\n",    '$0 is the path as given (absolute)');
    is(run_pcl("'./$rel'"),  "0=./$rel\n",
       '... and a different spelling of the same file gets its own entry');
    is(run_pcl("'$p'"),      "0=$p\n",    '... without disturbing the first');
}

# A HIT AND A MISS ARE INDISTINGUISHABLE — the whole legitimacy of the cache.
# The script-only facts are baked in at transpile time ($0, __FILE__, the
# __END__/DATA section) and the phase blocks run at LOAD time, so both halves
# are asked, and perl is the oracle for what is not PCL-specific.
{
    my $p = write_file("$dir/facts.pl", <<'PL');
BEGIN { print "BEGIN\n" }
CHECK { print "CHECK\n" }
UNITCHECK { print "UNITCHECK\n" }
INIT { print "INIT\n" }
print "0=", ($0 =~ m{([^/]+)$})[0], "\n";
print "FILE=", (__FILE__ =~ m{([^/]+)$})[0], "\n";
print "LINE=", __LINE__, "\n";
print "ARGV=@ARGV\n";
my $d = do { local $/; <DATA> };
print "DATA=$d";
END { print "END\n" }
__END__
data-one
PL
    my $oracle = `perl '$p' a b 2>&1`;
    my $miss   = run_pcl("'$p' a b");
    my $hit    = run_pcl("'$p' a b");
    is($miss, $oracle, 'a MISS answers exactly as perl ($0, __FILE__, DATA, phases)');
    is($hit,  $miss,   '... and a HIT is byte-identical to the MISS');
    like($oracle, qr/^BEGIN\nUNITCHECK\nCHECK\nINIT\n/,
         '... and the phase order asserted is really perl\'s');
    like($hit, qr/^ARGV=a b$/m, '... with @ARGV reaching the program on a HIT');
}

# THE EXIT CODE is the program's, on both paths — `pcl` execs SBCL so that it
# is, and a cached run must not change that.
{
    my $p = write_file("$dir/exit3.pl", "print \"bye\\n\"; exit 3;\n");
    my ($o1, $rc1) = run_pcl("'$p'");
    my ($o2, $rc2) = run_pcl("'$p'");
    is($rc1, 3, 'a MISS exits with the program\'s own code');
    is($rc2, 3, '... and so does a HIT');
    is($o2, $o1, '... with the same output');
}

# ─────────────────────────────────────────────────────────────────────────
# WHAT IS NOT CACHED, and the switches that turn it off.
{
    my $ecache = tempdir(CLEANUP => 1);
    is(run_pcl(q{-E 'print "inline\n"'}, cache => $ecache), "inline\n",
       '-e runs');
    is(count_glob("$ecache/scripts/*"), 0,
       '... and leaves NO script entry: its source is a temp file with a fresh '
       . 'name each run, so a path key would leak one entry per run (#1862)');

    my $mcache = tempdir(CLEANUP => 1);
    my $p = write_file("$dir/withm.pl", "print \"m\\n\";\n");
    is(run_pcl("-MList::Util '$p'", cache => $mcache), "m\n",
       'a file run with -M prefixes runs');
    is(count_glob("$mcache/scripts/*"), 0,
       '... and is not cached either: `pcl` prepends the use-lines into a temp copy');

    my $ncache = tempdir(CLEANUP => 1);
    is(run_pcl("--no-cache '$p'", cache => $ncache), "m\n", '--no-cache runs');
    is(count_glob("$ncache/scripts/*"), 0, '... and writes no entry');

    my $vcache = tempdir(CLEANUP => 1);
    is(run_pcl("--no-cache '$p'", cache => $vcache,
               env => { PCL_NO_CACHE => 1 }), "m\n", 'PCL_NO_CACHE=1 runs');
    is(count_glob("$vcache/scripts/*"), 0, '... and writes no entry either');

    my $ccache = tempdir(CLEANUP => 1);
    my $chk = run_pcl("-c '$p'", cache => $ccache);
    like($chk, qr/syntax OK/, 'pcl -c still syntax-checks');
    is(count_glob("$ccache/scripts/*"), 0,
       '... and writes no entry: -c must transpile and NOT run');
}

# THE OFF SWITCH FOR THE FASL HALF still reaches a script (the exemption is
# from the POSITIVE list only): the entry is still cached, as readable text.
{
    my $tcache = tempdir(CLEANUP => 1);
    my $p = write_file("$dir/textonly.pl", "print \"t\\n\";\n");
    my $trace = run_pcl("'$p'", cache => $tcache, debug => 1,
                        env => { PCL_NO_FASL_CACHE => 1, PCL_FASL_DEBUG => 1 });
    like($trace, qr/-> TEXT/, 'PCL_NO_FASL_CACHE=1 takes the TEXT path');
    ok(scalar(glob("$tcache/scripts/*.lisp")), '... the transpile is still cached');
    is(count_glob("$tcache/scripts/*.fasl"), 0, '... and no fasl is written');
    is(run_pcl("'$p'", cache => $tcache, env => { PCL_NO_FASL_CACHE => 1 }),
       "t\n", '... and the next run answers from that text');
}

# A BROKEN FASL IS A MISS, NEVER A CRASH.  The cache is shared and a fasl can
# be truncated by a killed writer or left by an older key scheme; the program
# must still run, from its text.  (This is %p-load-module-fasl's recovery,
# reached here through the script path.)
{
    my $bcache = tempdir(CLEANUP => 1);
    my $p = write_file("$dir/broke.pl", "print \"b\\n\";\n");
    is(run_pcl("'$p'", cache => $bcache), "b\n", 'the broken-fasl row runs once');
    my ($fasl) = glob("$bcache/scripts/*.fasl");
    ok($fasl, '... and has a fasl to corrupt');
    write_file($fasl, "not a fasl at all\n");
    is(run_pcl("'$p'", cache => $bcache), "b\n",
       'a corrupt fasl falls back to the cached text; the program still runs');
}

# ─────────────────────────────────────────────────────────────────────────
# A CACHED-SCRIPT RUN KEYS ITS MODULE CACHE LIKE EVERY OTHER PCL RUN.
#
# The compiler fingerprint locates PPI through the module search path
# (task #1843), and it is computed at FIRST USE — which on this path is before
# any preamble has run.  With nothing to search, PPI resolves nowhere, the
# stamp records its absence, and every MODULE this program loads lands under a
# key no other run can reach: the module cache silently splits in two.  So
# `pcl` hands the runtime the search path its preamble is about to set.
#
# Measured, not asserted from the code: run one `use Carp` BOTH ways against
# one fresh cache and count the module entries.  A split shows up as the
# second run adding entries the first already had.  (It did, while this was
# being built — and with the seed in the WRONG ORDER it added perl's real
# Carp.pm beside PCL's shim.)
{
    my $ncache = tempdir(CLEANUP => 1);
    my $p = write_file("$dir/usecarp.pl", "use Carp; print \"c\\n\";\n", 1);
    is(run_pcl(q{-E 'use Carp; print "c\n";'}, cache => $ncache), "c\n",
       'a transpiled run loads Carp');
    my $after_e = count_glob("$ncache/modules/*.lisp");
    ok($after_e > 0, '... and caches it');
    is(run_pcl("'$p'", cache => $ncache), "c\n", 'a cached-script run loads Carp');
    is(count_glob("$ncache/modules/*.lisp"), $after_e,
       '... reusing the very entries the transpiled run wrote (#1843 + #1841)');
}

# ─────────────────────────────────────────────────────────────────────────
# THE RUNNERS THAT MEASURE THE TRANSPILE MUST NOT USE IT (#1841 (f)).
# They do not go through `pcl` at all, which is why they are unaffected — but
# "which is why" is an argument, and this is the check.
{
    my @runners = ("$root/runpcl", "$root/tools/sweep-perl-tests.pl",
                   "$root/tools/run-perl-suite.pl", "$root/tools/pclperl-for-tests");
    my @using = grep { my $t = do { open my $fh, '<', $_ or return ''; local $/; <$fh> };
                       defined $t && $t =~ /p-run-script-cached/ } @runners;
    is(join(',', @using), '',
       'runpcl, the sweep, the companion and pclperl-for-tests never ask for a script entry');
}
