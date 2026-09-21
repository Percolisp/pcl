#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# everyday-smoke.t — unit tests for tools/everyday-smoke.pl, the instrument
# that answers "how many ordinary Perl programs does PCL get exactly right?"
# (task #2099).
#
# NOT part of the Pl/t gate: that gate measures the transpiler, this measures
# a measurement tool.  Run it directly:  prove tools/t/everyday-smoke.t
#
# THERE IS NO SBCL AND NO PCL HERE.  The command under test is
# tools/t/everyday-fixture/fake-pcl, which runs each fixture program under the
# running perl and then misbehaves BY NAME (diff-* changes line 3, rc-* exits
# 3, slow-* sleeps).  That makes every verdict reachable in seconds, and — the
# reason it is a fake — keeps this file independent of whichever PCL bug
# happens to exist today: a test pinned to a real difference would fail the
# week that difference is fixed.
#
# The `--record` and plain-`(subset)` rows need the DEFAULT corpus, so they run
# against a TEMPORARY ROOT: a directory with a stub cl/pcl-runtime.lisp (which
# is all PCLPaths::root asks for) and the fixture copied in as everyday/.
use strict;
use warnings;
use Test::More;
use FindBin qw($RealBin);
use File::Temp qw(tempdir);
use File::Path qw(make_path);
use File::Copy qw(copy);
use File::Basename qw(basename);

my $SMOKE = "$RealBin/../everyday-smoke.pl";
my $FIX   = "$RealBin/everyday-fixture";
my $ADMIT = "$RealBin/everyday-admit";
my $FAKE  = "$FIX/fake-pcl";
plan skip_all => "$SMOKE not found" unless -f $SMOKE;

my $TMP = tempdir(CLEANUP => 1);
my $N   = 0;

# Run the instrument; returns (combined output, exit status).
sub smoke {
    my @args = @_;
    my $cmd = join ' ', map { "'$_'" } ($^X, $SMOKE, @args);
    my $out = qx{$cmd 2>&1};
    return ($out, $? >> 8);
}

# The standard fixture invocation: the fake pcl, a private outdir, no baseline.
sub fixture_run {
    my @extra = @_;
    return smoke('--corpus', $FIX, '--pcl', $FAKE, '--timeout', 2,
                 '--outdir', "$TMP/out" . ++$N, @extra);
}

sub write_file {
    my ($path, $text) = @_;
    make_path(_dir($path));
    open my $fh, '>:raw', $path or die "$path: $!";
    print $fh $text;
    close $fh;
}
sub _dir { my ($p) = @_; $p =~ s{/[^/]*$}{}; return $p }
sub slurp { my ($p) = @_; open my $h, '<:raw', $p or return ''; local $/; my $t = <$h>; return defined $t ? $t : '' }

# A copy of a corpus directory, so a test may corrupt or bless into it.
sub copy_corpus {
    my ($src, $dst) = @_;
    opendir my $dh, $src or die "$src: $!";
    for my $area (grep { -d "$src/$_" && !/^\./ } readdir $dh) {
        make_path("$dst/$area");
        opendir my $ah, "$src/$area" or next;
        for my $f (grep { -f "$src/$area/$_" } readdir $ah) { copy("$src/$area/$f", "$dst/$area/$f") }
        closedir $ah;
    }
    closedir $dh;
    return $dst;
}

# ------------------------------------------------------- every verdict once
my ($out, $rc) = fixture_run('--baseline', 'none');

like($out, qr/^DIFF\s+basic\/diff-line3\s+line 3\b/m,   'DIFF names the first differing stdout line');
like($out, qr/^RC\s+basic\/rc-status\b/m,               'stdout identical + wrong status is RC');
like($out, qr/^TIMEOUT\s+basic\/slow-sleeper\b/m,       'a program past --timeout is TIMEOUT');
unlike($out, qr/^\w+\s+basic\/ok-plain\b/m,             'an identical program gets no line at all');
unlike($out, qr/^\w+\s+basic\/ok-args\b/m,              '# args: reach the program');
unlike($out, qr/^\w+\s+basic\/ok-stdin\b/m,             'a .stdin file reaches the program');
unlike($out, qr/^\w+\s+basic\/ok-exit-two\b/m,          '# expect-rc: is honoured on both sides');
like($out, qr/^DIFF\s+basic\/diff-noisy\s+line 3\s+\S.*PCL: a real complaint/m,
     "the stderr column shows PCL's first REAL complaint, not the banner");

# ------------------------------------------------------------ the two counts
like($out, qr/^\s+basic\s+5\/9\s+same$/m,  'per-area counts: basic');
like($out, qr/^\s+more\s+1\/1\s+same$/m,   'per-area counts: the second area');
like($out, qr/^EVERYDAY \(corpus \Q$FIX\E\): 6 of 10 identical to perl \(60\.0 %\) -- \S+ gen \S+$/m,
     'the summary line is the last line, and a non-default corpus is named in it');
is((split /\n/, $out)[-1] =~ /^EVERYDAY/ ? 1 : 0, 1, 'the summary really is the LAST line');

# --------------------------------------------- the work directory is private
my $work = "$TMP/out$N/work/basic";
ok(!-d "$work/ok-workdir", 'the work directory of a matching program is removed');
ok(-d "$work/diff-line3",  'the work directory of a differing program is KEPT as evidence');

# ok-workdir appends to a file and prints the line count: a work directory
# that was not fresh would make the second run say 2.
my ($out2) = fixture_run('--baseline', 'none');
unlike($out2, qr/^\w+\s+basic\/ok-workdir\b/m, 'the work directory is FRESH on every run');

# ------------------------------------------------------------------ buckets
my @rows = (
    "basic/diff-line3\tDIFF\t3\t#1111",
    "basic/diff-noisy\tDIFF\t3\t#1111",
    "basic/rc-status\tRC\t-\t#2222",
    "basic/slow-sleeper\tTIMEOUT\t-\t#3333",
);
sub baseline_run {
    my (@lines) = @_;
    my $bl = "$TMP/baseline" . ++$N . ".tsv";
    write_file($bl, join('', map { "$_\n" } '# area/name\tverdict\tfirst-diff-line\tcause', @lines));
    return fixture_run('--baseline', $bl);
}

my ($bo, $brc) = baseline_run(@rows);
like($bo, qr/^buckets: NEW 0, FIXED 0, MOVED 0, UNEXPLAINED 0, STALE 0$/m, 'a complete baseline is quiet');
is($brc, 0, 'a complete baseline exits 0');

($bo, $brc) = baseline_run(grep { !/diff-line3/ } @rows);
like($bo, qr/^NEW \(1\)/m,                     'a differing program with no row is NEW');
like($bo, qr/^\s+basic\/diff-line3\s+DIFF line 3/m, 'NEW names the verdict and the line');
is($brc, 1, 'NEW fails the run');

($bo, $brc) = baseline_run(@rows, "basic/ok-plain\tDIFF\t1\t#4444");
like($bo, qr/^FIXED \(1\)/m,                  'a row whose program is now identical is FIXED');
like($bo, qr/edit these rows OUT/,            'FIXED says rows leave by edit');
like($bo, qr/^\s+basic\/ok-plain$/m,          'FIXED names the program');
is($brc, 0,                                   'FIXED does not fail the run');

($bo, $brc) = baseline_run(map { s/^(basic\/diff-line3\tDIFF\t)3/${1}9/r } @rows);
like($bo, qr/^MOVED \(1\)/m,                     'a changed first-diff line is MOVED');
like($bo, qr/basic\/diff-line3\s+line 9 -> 3/m,  'MOVED prints old -> new');
is($brc, 0,                                      'MOVED does not fail the run');

($bo, $brc) = baseline_run(map { s/^(basic\/rc-status\tRC\t-)\t.*/$1\t/r } @rows);
like($bo, qr/^UNEXPLAINED \(1\)/m,  'a row with no cause is UNEXPLAINED');
is($brc, 1,                         'UNEXPLAINED fails the run');

($bo, $brc) = baseline_run(@rows, "basic/no-such-program\tDIFF\t1\t#5555");
like($bo, qr/^STALE \(1\)/m,  'a row naming a program that does not exist is STALE');
is($brc, 1,                   'STALE fails the run');

# A verdict that changed class is MOVED too — the row needs re-attributing.
($bo, $brc) = baseline_run(map { s/^(basic\/rc-status\t)RC/${1}DIFF/r } @rows);
like($bo, qr/basic\/rc-status\s+DIFF -> RC/m, 'a changed verdict class is MOVED, spelled out');

# ----------------------------------------------------- a temporary PCL tree
#
# --record and the plain "(subset)" spelling only apply to the DEFAULT corpus,
# so they need a root of their own.  PCLPaths::root asks exactly one question
# of a candidate directory: is cl/pcl-runtime.lisp there.
my $ROOT = "$TMP/root";
make_path("$ROOT/cl", "$ROOT/tools");
write_file("$ROOT/cl/pcl-runtime.lisp", qq{(defparameter *pcl-cache-generation* "v2-TEST"\n  "a stub")\n});
copy_corpus($FIX, "$ROOT/everyday");
copy("$FIX/fake-pcl", "$ROOT/fake-pcl"); chmod 0755, "$ROOT/fake-pcl";

sub root_run {
    my @extra = @_;
    local $ENV{PCL_ROOT} = $ROOT;
    return smoke('--pcl', "$ROOT/fake-pcl", '--timeout', 2, '--outdir', "$TMP/rout" . ++$N,
                 '--baseline', 'none', @extra);
}

my ($ro) = root_run();
like($ro, qr/^EVERYDAY: 6 of 10 identical to perl \(60\.0 %\) -- unknown gen v2-TEST$/m,
     'the default corpus is not named, and the generation comes from cl/pcl-runtime.lisp');

($ro) = root_run('basic');
like($ro, qr/^EVERYDAY \(subset\): /m, 'a partial run says (subset), so its number cannot be quoted as THE number');

# --record
my $hist = "$ROOT/baselines/everyday-history.tsv";
($ro) = root_run('--record');
ok(-f $hist, '--record wrote the history file');
my @h = grep { !/^#/ } split /\n/, slurp($hist);
is(scalar @h, 1, '--record appended exactly one row');
like($h[0], qr/^\d{4}-\d\d-\d\dT[\d:]+Z\tunknown\tv2-TEST\t6\t10\t/, 'the row carries date, sha, generation, N and M');
like($h[0], qr/basic=5\/9 more=1\/1$/,                              'the row carries the per-area counts');

($ro) = root_run('basic', '--record');
like($ro, qr/^--record REFUSED: it measured a SUBSET/m, '--record refuses a subset');
@h = grep { !/^#/ } split /\n/, slurp($hist);
is(scalar @h, 1, 'the refused --record wrote nothing');

($ro) = smoke('--corpus', $FIX, '--pcl', $FAKE, '--timeout', 2, '--outdir', "$TMP/rout" . ++$N,
              '--baseline', 'none', '--record');
like($ro, qr/^--record REFUSED: it measured \Q$FIX\E, not the default corpus/m,
     '--record refuses a corpus that is not the default one');

# ------------------------------------------------------- admission (needs perl)
my $acopy = copy_corpus($ADMIT, "$TMP/admit");
my ($ao, $arc) = smoke('--corpus', $acopy, '--bless-expect');
like($ao, qr/^REFUSED probe\/path-dependent\s+.*\$0, __FILE__/m,
     '--bless-expect refuses a program whose answer depends on WHERE it is');
like($ao, qr/^REFUSED probe\/hash-order\s+.*hash order/m,
     '--bless-expect refuses a program whose answer varies between runs');
like($ao, qr/^REFUSED probe\/perl-rejects\s+.*perl exited 255/m,
     'a program perl itself rejects is an invalid probe, not an expectation');
like($ao, qr/^NEEDS\s+probe\/needs-missing\s+.*No::Such::Module/m,
     'a `# needs:` module this perl lacks is reported, never silently skipped');
like($ao, qr/^blessed probe\/fine\s+\d+ bytes/m, 'an admissible program IS blessed');
ok(-f "$acopy/probe/fine.expect",  'the expectation was written');
ok(!-f "$acopy/probe/hash-order.expect", 'a refused program got no expectation');
is($arc, 1, '--bless-expect exits 1 when anything was refused');

# ------------------------------------------------------ --verify-with-perl
my $vcopy = copy_corpus($FIX, "$TMP/verify");
my ($vo, $vrc) = smoke('--corpus', $vcopy, '--verify-with-perl');
like($vo, qr/^DRIFT \(0\)/m, 'a freshly checked-in corpus has no drift');
is($vrc, 0, 'no drift exits 0');

write_file("$vcopy/basic/ok-plain.expect", "one\ntwo\nSOMETHING ELSE\nfour\n");
($vo, $vrc) = smoke('--corpus', $vcopy, '--verify-with-perl');
like($vo, qr/^DRIFT \(1\)/m,                                    'a corrupted expectation is DRIFT');
like($vo, qr/basic\/ok-plain\s+stdout differs at line 3/m,      'DRIFT names the program and the line');
is($vrc, 1, 'DRIFT fails the run');

# ------------------------------------------------------------------- usage
my ($uo, $urc) = smoke('--corpus', '/no/such/corpus/anywhere');
is($urc, 2, 'a missing corpus exits 2');
like($uo, qr/no corpus directory/, 'and says so');

($uo, $urc) = smoke('--corpus', $FIX, '--pcl', '/no/such/command');
is($urc, 2, 'a command under test that is not executable exits 2');

($uo, $urc) = smoke('--corpus', $FIX, '--list');
is($urc, 0, '--list exits 0');
like($uo, qr/^basic\/ok-plain$/m, '--list prints one key per program');

done_testing();
