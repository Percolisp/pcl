#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# rosetta-corpus.t -- unit and end-to-end tests for tools/rosetta-corpus (the
# Rosetta recipe: build / verify / manifest / run / classify) and for what
# tools/everyday-smoke.pl learned for it in s543o (a NAMED corpus: --label,
# --history, a baseline with a class column).  Task #2104.
#
# NOT part of the Pl/t gate (it measures a measurement tool, like
# tools/t/everyday-smoke.t).  Run it directly:  prove tools/t/rosetta-corpus.t
#
# NO NETWORK, NO SBCL, NO ROSETTA TEXT.  Rosetta Code's text is GFDL 1.2 and
# never enters this repository, so the corpus here is SYNTHETIC: a dozen tiny
# programs written for this file, committed into a throwaway local git
# repository laid out like RosettaCodeData (Task/<task>/Perl/<file>.pl), which
# `build --repo file://... --commit SHA` fetches exactly as it fetches the
# real one.  The command under test is a fake `pcl` written below: it runs
# the program under perl and changes line 2 of the output of a program whose
# name holds `-Diff-`, so every bucket is reachable in seconds.
use strict;
use warnings;
use Test::More;
use FindBin qw($RealBin);
use File::Temp qw(tempdir);
use File::Path qw(make_path remove_tree);
use lib "$RealBin/../lib";
use PCLRosetta qw(static_refusal task_of program_id class_of @CLASSES);
use PCLPaths ();

my $TOOL  = "$RealBin/../rosetta-corpus";
my $SMOKE = "$RealBin/../everyday-smoke.pl";
plan skip_all => 'git is not installed' if system('git --version >/dev/null 2>&1') != 0;

my $TMP = tempdir(CLEANUP => 1);
$ENV{RCTEST} = "$TMP/counter";   # the run counter of 0010-Mu-Counts-Its-Runs
make_path($ENV{RCTEST});
my $N   = 0;

sub write_file {
    my ($path, $text) = @_;
    (my $d = $path) =~ s{/[^/]*$}{};
    make_path($d);
    open my $fh, '>:raw', $path or die "$path: $!";
    print $fh $text;
    close $fh;
}
sub slurp { my ($p) = @_; open my $h, '<:raw', $p or return ''; local $/; my $t = <$h>; return defined $t ? $t : '' }
sub sh_quote { my ($s) = @_; $s =~ s/'/'\\''/g; return "'$s'" }
sub run_cmd {
    my (@cmd) = @_;
    my $out = qx{@{[ join ' ', map { sh_quote($_) } @cmd ]} 2>&1};
    return ($out, $? >> 8);
}
# The same, from another working directory (the relative-path rows).
sub run_cmd_in {
    my ($dir, @cmd) = @_;
    my $out = qx{cd @{[ sh_quote($dir) ]} && @{[ join ' ', map { sh_quote($_) } @cmd ]} 2>&1};
    return ($out, $? >> 8);
}
sub tool  { return run_cmd($^X, $TOOL, @_) }
sub smoke { return run_cmd($^X, $SMOKE, @_) }
sub git_in {
    my ($dir, @args) = @_;
    local $ENV{GIT_DIR} = "$dir/.git";
    local $ENV{GIT_WORK_TREE} = $dir;
    my $out = qx{@{[ join ' ', map { sh_quote($_) } 'git', '-c', 'user.name=t', '-c', 'user.email=t@t', @args ]} 2>&1};
    die "git @args in $dir: $out" if $? != 0;
    return $out;
}

# ------------------------------------------------------------- the rules
is(static_refusal(qq{print "ok\\n";\n}), undef,            'static: a pure program is a candidate');
is(static_refusal(qq{open my \$f, '<', 'x';\n}), 'open',   'static: a program that opens a file is refused, naming the word');
is(static_refusal(qq{print time, "\\n";\n}), 'time',       'static: the clock is refused');
is(static_refusal(qq{print rand;\n}), 'rand',              'static: chance is refused');
is(static_refusal(qq{print \$ENV{HOME};\n}), undef,        'static: the s493 filter names %ENV, not $ENV{...} (kept verbatim)');
is(static_refusal(qq{use File::Spec;\n}), 'File::',        'static: a File:: module is refused');
is(task_of('Task/100-doors/Perl/100-doors.pl'), '100-doors', 'task_of: Task/<task>/Perl/<file>.pl');
is(task_of('Task/100-doors/Python/x.pl'), undef,           'task_of: another language is not a candidate');
is(program_id(7, "Kappa's (very) long name that goes past forty bytes!"), '0007-Kappa-s-very-long-name-that-goes-past-fo',
   'program_id: NNNN, non-alphanumeric runs made one -, cut to 40 bytes');

is(class_of('same', 0, 0, ''), undef,                            'class: same has no class');
is(class_of('TIMEOUT', -1, 0, ''), 'TIMEOUT',                   'class: TIMEOUT is the runner\'s verdict');
is(class_of('DIFF', 0, 0, "PCL: statement dropped at f line 3\n"), 'DROPPED', 'class: a dropped statement');
is(class_of('DIFF', 0, 0, "PCL: (?{...}) is not supported\n"), 'NOT-SUPPORTED', 'class: an announced not-supported');
is(class_of('DIFF', 2, 0, "Can't locate Foo.pm in \@INC\n"), 'MODULE-MISSING', 'class: a missing module (before LOUD)');
is(class_of('DIFF', 1, 0, "Heap exhausted, game over.\n"), 'EXHAUSTED', 'class: heap / stack exhausted');
is(class_of('DIFF', 255, 0, "Died at x line 1.\n"), 'LOUD',     'class: LOUD -- dies where perl does not');
is(class_of('DIFF', 0, 0, "PCL: a notice\n"), 'NOTICE',         'class: other output, a notice on stderr');
is(class_of('DIFF', 0, 0, ''), 'SILENT',                         'class: SILENT -- status as perl\'s, nothing on stderr');
is(scalar @CLASSES, 8, 'the classes are the plan\'s eight');

{
    local $ENV{PCL_ROSETTA_DIR} = "$TMP/elsewhere/";
    is(PCLPaths::rosetta_dir(), "$TMP/elsewhere", 'rosetta_dir: $PCL_ROSETTA_DIR wins (trailing / trimmed)');
    local $ENV{PCL_ROSETTA_DIR} = '';
    local $ENV{PCL_CACHE_DIR} = "$TMP/cache";
    is(PCLPaths::rosetta_dir(), "$TMP/cache/rosetta", 'rosetta_dir: else beside the module cache');
}

# ------------------------------------------- a synthetic RosettaCodeData
my $SRC = "$TMP/RosettaCodeData";
my %TREE = (
    'Task/Alpha-Same/Perl/alpha.pl'        => qq{print "alpha \$_\\n" for 1 .. 3;\n},
    'Task/Alpha-Same/Perl/alpha.pm'        => qq{print "not a .pl\\n";\n},
    'Task/Alpha-Same/Python/alpha.pl'      => qq{print "not under Perl/\\n";\n},
    'Task/Beta-Diff-Known/Perl/beta.pl'    => qq{print "beta \$_\\n" for 1 .. 3;\n},
    'Task/Cee-Uses-Clock/Perl/cee.pl'      => qq{print time, "\\n";\n},
    'Task/Delta-Fixed-Now/Perl/delta.pl'   => qq{print "delta\\n";\n},
    'Task/Epsilon-Hash-Order/Perl/eps.pl'  => qq{my \%h = map { \$_ => 1 } 'a' .. 'z'; print join(',', keys \%h), "\\n";\n},
    'Task/Eta-Prints-Nothing/Perl/eta.pl'  => qq{my \$quiet = 42;\n},
    'Task/Gamma-Diff-New/Perl/gamma.pl'    => qq{print "gamma \$_\\n" for 1 .. 3;\n},
    'Task/Iota-Huge/Perl/iota.pl'          => qq{print "x" x 200_001;\n},
    "Task/Kappa's (very) long name that goes past forty bytes!/Perl/kappa.pl" => qq{print "kappa\\n";\n},
    'Task/Lambda-Where-Am-I/Perl/lambda.pl' => qq{print __FILE__, "\\n";\n},
    # Counts its own runs in $RCTEST: the three admission runs print "early", the drift pass's
    # fourth run "late" -- a stand-in, deterministic, for an answer that holds the clock.
    'Task/Mu-Counts-Its-Runs/Perl/mu.pl'   => qq{my \$n = 0; \$n++ while -e "\$ENV{RCTEST}/run\$n"; link(__FILE__, "\$ENV{RCTEST}/run\$n") or die; print \$n >= 3 ? "late\\n" : "early\\n";\n},
    'Task/Theta-Dies/Perl/theta.pl'        => qq{die "theta\\n";\n},
);
write_file("$SRC/$_", $TREE{$_}) for keys %TREE;
git_in($SRC, 'init', '-q', $SRC);
git_in($SRC, 'add', '-A');
git_in($SRC, 'commit', '-q', '-m', 'synthetic');
git_in($SRC, 'config', 'uploadpack.allowAnySHA1InWant', 'true');
git_in($SRC, 'config', 'uploadpack.allowFilter', 'true');
(my $SHA = git_in($SRC, 'rev-parse', 'HEAD')) =~ s/\s+//g;

my @ADMITTED = qw(0001-Alpha-Same 0002-Beta-Diff-Known 0003-Delta-Fixed-Now 0006-Gamma-Diff-New
                  0008-Kappa-s-very-long-name-that-goes-past-fo);
my %REFUSED = ('0004-Epsilon-Hash-Order'  => qr/^run 2 differs from run 1/,
               '0005-Eta-Prints-Nothing'  => qr/^no output/,
               '0007-Iota-Huge'           => qr/^huge output: 200001 bytes/,
               '0009-Lambda-Where-Am-I'   => qr/^the COPY at another path differs/,
               '0010-Mu-Counts-Its-Runs'   => qr/^drift: a re-run minutes after the admission: stdout differs at line 1/,
               '0011-Theta-Dies'          => qr/^perl exited 255/);

# A temporary PCL ROOT (PCLPaths::root asks only for cl/pcl-runtime.lisp): its
# baselines/ hold the test's manifest, table and history, and its tools/ is
# this tree's, so `run` execs the real runner.  Every call below uses it.
my $ROOT = "$TMP/root";
make_path("$ROOT/cl", "$ROOT/baselines");
write_file("$ROOT/cl/pcl-runtime.lisp", qq{(defparameter *pcl-cache-generation* "v2-TEST"\n  "a stub")\n});
symlink("$RealBin/..", "$ROOT/tools") or die "symlink: $!";
$ENV{PCL_ROOT} = $ROOT;

# ------------------------------------------------------------- build
my $D = "$TMP/corpus-dir";
my ($bo, $brc) = tool('build', '--dir', $D, '--repo', "file://$SRC", '--commit', $SHA);
is($brc, 0, 'build: exit 0') or diag($bo);
like($bo, qr/^fetch: \S+ at $SHA \(shallow fetch of the commit\)/m, 'build: a shallow fetch of the pinned commit');
like($bo, qr/^select: 12 Perl solutions, 1 filtered statically, 11 candidates$/m,
     'build: every .pl under a Perl/ directory, the word filter, the rest numbered');
like($bo, qr/^admit: 11 candidates no manifest for this commit: the corpus is DEFINED here, at --timeout 8$/m,
     'build: with no manifest for the commit every candidate faces the 8 s rule');
my @names = sort map { m{/([^/]+)\.pl$} ? $1 : () } glob("$D/corpus/rosetta/*.pl");
is_deeply(\@names, \@ADMITTED, 'build: the admitted programs, numbered among the candidates in sorted path order');
is(slurp("$D/corpus/rosetta/0001-Alpha-Same.pl"), $TREE{'Task/Alpha-Same/Perl/alpha.pl'}, 'build: a program is copied byte for byte');
is(slurp("$D/corpus/rosetta/0001-Alpha-Same.expect"), "alpha 1\nalpha 2\nalpha 3\n", 'build: its .expect is perl\'s STDOUT');
my %why = map { /^([^#\t][^\t]*)\t(.*)$/ ? ($1 => $2) : () } split /\n/, slurp("$D/refused/REASONS.tsv");
is_deeply([sort keys %why], [sort keys %REFUSED], 'build: the refused programs are listed in refused/REASONS.tsv');
for my $k (sort keys %REFUSED) {
    like($why{$k} // '', $REFUSED{$k}, "build: $k refused for its reason");
    ok(-f "$D/refused/$k.pl" && !-e "$D/corpus/rosetta/$k.pl", "build: $k moved to refused/");
}
like(slurp("$D/BUILD-INFO"), qr/^commit\t$SHA$/m, 'build: BUILD-INFO names the commit');
like($bo, qr/^build: 12 Perl solutions: filtered-static=1, admitted=5, refused=6 \(/m, 'build: the summary line');

($bo, $brc) = tool('build', '--dir', $D, '--repo', "file://$SRC", '--commit', $SHA);
is($brc, 2, 'build: refuses to write over an existing corpus');
like($bo, qr/exists -- build writes a FRESH corpus/, 'build: and says why');

# ------------------------------------------------------ manifest / verify
my $MAN = "$ROOT/baselines/rosetta-corpus.tsv";
my ($mo, $mrc) = tool('manifest', '--dir', $D);
is($mrc, 0, 'manifest: exit 0');
write_file($MAN, $mo);
like($mo, qr/^# commit: $SHA$/m, 'manifest: the header names the commit');
like($mo, qr/^rosetta\/0001-Alpha-Same\t[0-9a-f]{64}\t[0-9a-f]{64}\t0$/m, 'manifest: name, two sha256 digests, the exit status');
unlike($mo, qr/alpha [123]/, 'manifest: names and digests only -- no program text, no output');

my ($vo, $vrc) = tool('verify', '--dir', $D, '--manifest', $MAN);
is($vrc, 0, 'verify: a fresh build matches its manifest') or diag($vo);

# A corpus made without the tool has no BUILD-INFO: --commit and --perl name the header facts.
rename("$D/BUILD-INFO", "$TMP/BUILD-INFO.aside") or die;
my ($m2, $m2rc) = tool('manifest', '--dir', $D);
is($m2rc, 2, 'manifest: no BUILD-INFO and no --commit / --perl is refused');
($m2, $m2rc) = tool('manifest', '--dir', $D, '--commit', $SHA, '--perl', 'v5.40.3');
is($m2, $mo =~ s/^# perl: \S+$/# perl: v5.40.3/mr =~ s{^# source: .*$}{# source: $PCLRosetta::UPSTREAM}mr, 'manifest: --commit / --perl stand in for BUILD-INFO');
rename("$TMP/BUILD-INFO.aside", "$D/BUILD-INFO") or die;

my $victim = "$D/corpus/rosetta/0003-Delta-Fixed-Now.pl";
my $orig = slurp($victim);
write_file($victim, $orig =~ s/delta/Delta/r);
($vo, $vrc) = tool('verify', '--dir', $D, '--manifest', $MAN);
is($vrc, 1, 'verify: a changed byte fails');
like($vo, qr/^CHANGED rosetta\/0003-Delta-Fixed-Now\.pl /m, 'verify: and names the file');
write_file($victim, $orig);

rename($victim, "$TMP/aside.pl") or die;
($vo, $vrc) = tool('verify', '--dir', $D, '--manifest', $MAN);
is($vrc, 1, 'verify: a missing program fails');
like($vo, qr/^MISSING rosetta\/0003-Delta-Fixed-Now\.pl$/m, 'verify: and names it');
rename("$TMP/aside.pl", $victim) or die;

write_file("$D/corpus/rosetta/0001-Alpha-Same.stdin", "x\n");
($vo, $vrc) = tool('verify', '--dir', $D, '--manifest', $MAN);
is($vrc, 1, 'verify: an extra file the runner would read fails');
unlink "$D/corpus/rosetta/0001-Alpha-Same.stdin";

# Rebuilding reuses the clone and reproduces the corpus byte for byte.
my $D2 = "$TMP/corpus-dir-2";
make_path("$D2/work");
rename("$D/work/RosettaCodeData", "$D2/work/RosettaCodeData") or die;
($bo, $brc) = tool('build', '--dir', $D2, '--repo', "file://$SRC", '--commit', $SHA);
like($bo, qr/\(reused\)/, 'build: a clone at the pinned commit is reused');
like($bo, qr/^admit: 5 candidates named by the manifest, at --timeout 60$/m,
     'build: the clock is decided once -- the manifest\'s programs are re-admitted at the runner\'s 60 s');
like($bo, qr/^admit: 6 candidates not in the manifest, at --timeout 8$/m,
     'build: the other candidates still face the 8 s rule');
like(slurp("$D2/refused/REASONS.tsv"), qr/^0010-Mu-Counts-Its-Runs\tadmitted here, but the manifest does not name it/m,
     'build: a program admitted here that the manifest does not name stays out, with that reason');
($vo, $vrc) = tool('verify', '--dir', $D2, '--manifest', $MAN);
is($vrc, 0, 'build: a second build verifies against the first one\'s manifest');

# ----------------------------------------------------------------- run
my $FAKE = "$TMP/fake-pcl";
write_file($FAKE, <<'FAKE');
#!/usr/bin/env perl
# a stand-in for pcl: perl's answer, with line 2 changed for a program named *-Diff-*
my ($prog, @args) = @ARGV;
open my $ph, '-|', $^X, $prog, @args or die "fake-pcl: $!\n";
my @lines = <$ph>;
close $ph;
my $status = $? >> 8;
$lines[1] = "NOT WHAT PERL SAID\n" if $prog =~ /-Diff-/ && @lines >= 2;
print @lines;
exit $status;
FAKE
chmod 0755, $FAKE;

my $TAB = "$ROOT/baselines/rosetta-causes.tsv";
my $HIS = "$ROOT/baselines/rosetta-history.tsv";
my $HEAD = "# area/name\tverdict\tfirst-diff-line\tclass\tcause\n";
my %ROW = (beta  => "rosetta/0002-Beta-Diff-Known\tDIFF\t2\tSILENT\tUNREDUCED #2104",
           gamma => "rosetta/0006-Gamma-Diff-New\tDIFF\t2\tSILENT\tUNREDUCED #2104",
           delta => "rosetta/0003-Delta-Fixed-Now\tDIFF\t1\tSILENT\t#1234 since fixed",
           gone  => "rosetta/0099-No-Such-Program\tDIFF\t1\tLOUD\t#1234");
sub run_with {
    my ($rows, @extra) = @_;
    write_file($TAB, $HEAD . join('', map { "$_\n" } @$rows));
    return tool('run', '--dir', $D, '--pcl', $FAKE, '--timeout', 10, '--outdir', "$TMP/out" . ++$N, @extra);
}

my ($ro, $rrc) = run_with([@ROW{qw(beta gamma)}]);
like($ro, qr/^ROSETTA: 3 of 5 identical to perl \(60\.0 %\) -- \S+ gen v2-TEST$/m, 'run: the ROSETTA line');
is((split /\n/, $ro)[-1] =~ /^ROSETTA: / ? 1 : 0, 1, 'run: the ROSETTA line is the LAST line');
like($ro, qr/^verify: \S+ matches /m, 'run: the corpus is verified first');
like($ro, qr/^buckets: NEW 0, FIXED 0, MOVED 0, UNEXPLAINED 0, STALE 0$/m, 'run: the table explains every differing program');
like($ro, qr/^DIFF\s+rosetta\/0002-Beta-Diff-Known\s+line 2\s+UNREDUCED #2104$/m, 'run: a five-column row\'s CAUSE is printed (not its class)');
is($rrc, 0, 'run: exit 0');

($ro, $rrc) = run_with([$ROW{beta}]);
like($ro, qr/^NEW \(1\)/m, 'run: a differing program with no row is NEW');
like($ro, qr/^\s+rosetta\/0006-Gamma-Diff-New\s+DIFF line 2/m, 'run: NEW names it');
is($rrc, 1, 'run: NEW fails');

($ro, $rrc) = run_with([@ROW{qw(beta gamma delta)}]);
like($ro, qr/^FIXED \(1\)/m, 'run: a row whose program is now identical is FIXED');
like($ro, qr/^\s+rosetta\/0003-Delta-Fixed-Now$/m, 'run: FIXED names it');
is($rrc, 0, 'run: FIXED does not fail');

($ro, $rrc) = run_with([$ROW{beta}, $ROW{gamma} =~ s/\tUNREDUCED #2104$/\t/r]);
like($ro, qr/^UNEXPLAINED \(1\)/m, 'run: a five-column row with an empty cause is UNEXPLAINED');
is($rrc, 1, 'run: UNEXPLAINED fails');

($ro, $rrc) = run_with([@ROW{qw(beta gamma gone)}]);
like($ro, qr/^STALE \(1\)/m, 'run: a row naming a program that does not exist is STALE');
is($rrc, 1, 'run: STALE fails');

($ro, $rrc) = run_with([@ROW{qw(beta gamma)}], 'rosetta/0001-Alpha-Same');
like($ro, qr/^ROSETTA \(subset\): 1 of 1 /m, 'run: a partial run says (subset)');

($ro, $rrc) = run_with([@ROW{qw(beta gamma)}], '--corpus', $D);
is($rrc, 2, 'run: --corpus is refused (run measures THE corpus)');

write_file("$D/corpus/rosetta/0003-Delta-Fixed-Now.pl", slurp($victim) . "\n");
($ro, $rrc) = run_with([@ROW{qw(beta gamma)}]);
is($rrc, 2, 'run: a corpus that does not match the manifest is not run');
like($ro, qr/not run/, 'run: and says so');
write_file($victim, $orig);

# A RELATIVE --dir / --outdir: spawn() changes into each program's work directory before it
# opens the output files and execs the program, so the runner makes them absolute (before s543o
# every program of a relative --corpus came out DIFF, and --bless-expect refused them all).
($ro, $rrc) = run_cmd_in($TMP, $^X, $TOOL, 'run', '--dir', 'corpus-dir', '--pcl', $FAKE, '--outdir', 'rel-out');
like($ro, qr/^ROSETTA: 3 of 5 identical to perl /m, 'run: a relative --dir and --outdir measure what an absolute one does');
($ro) = run_cmd_in($TMP, $^X, $SMOKE, '--corpus', 'corpus-dir/corpus', '--pcl', $FAKE, '--outdir', 'rel-out2', '--baseline', 'none');
like($ro, qr/^EVERYDAY \(corpus \S+\): 3 of 5 identical to perl /m, 'the runner: a relative --corpus and --outdir work (it makes them absolute)');
($bo, $brc) = run_cmd_in($TMP, $^X, $TOOL, 'build', '--dir', 'rel-build', '--repo', "file://$SRC", '--commit', $SHA);
is($brc, 0, 'build: a relative --dir builds') or diag($bo);
like($bo, qr/^verify: \S+ matches /m, 'build: and its corpus verifies (the admission ran the programs from the right place)');

# ---------------------------------------------------------- classify
($ro) = run_with([@ROW{qw(beta gamma)}]);
my ($co, $crc) = tool('classify', '--dir', $D, '--outdir', "$TMP/out$N");
is($crc, 0, 'classify: exit 0');
like($co, qr/^rosetta\/0002-Beta-Diff-Known\tDIFF\t2\tSILENT\t$/m, 'classify: key, verdict, line, class, complaint');
like($co, qr/^# classes: SILENT 2, LOUD 0, /m, 'classify: the class counts');
like($co, qr/^# 2 of 5 differ, 3 same; 0 not run/m, 'classify: the totals');
unlike($co, qr/^# CLASS MOVED/m, 'classify: no class moved against the table the run used');
write_file("$TMP/moved.tsv", $HEAD . ($ROW{beta} =~ s/\tSILENT\t/\tLOUD\t/r) . "\n$ROW{gamma}\n");
($co) = tool('classify', '--dir', $D, '--outdir', "$TMP/out$N", '--table', "$TMP/moved.tsv");
like($co, qr/^# CLASS MOVED .*: rosetta\/0002-Beta-Diff-Known LOUD -> SILENT$/m,
     'classify: a program whose class is not its row\'s is named (the runner\'s MOVED cannot see it)');

# ------------------------------------------------------------ --record
write_file("$ROOT/.gitignore", "/.rosetta/\n");
git_in($ROOT, 'init', '-q', $ROOT);
write_file($HIS, "# rosetta-history.tsv (test)\n# date\tsha\tgen\tsame\ttotal\tby-area\n");
write_file($TAB, $HEAD . "$ROW{beta}\n$ROW{gamma}\n");
git_in($ROOT, 'add', '-A');
git_in($ROOT, 'commit', '-q', '-m', 'test root');
($ro, $rrc) = tool('run', '--dir', $D, '--pcl', $FAKE, '--outdir', "$TMP/rec1", '--record');
like($ro, qr/^--record: appended to \Q$HIS\E$/m, '--record: a clean whole-corpus run appends');
my @h = grep { !/^#/ } split /\n/, slurp($HIS);
is(scalar @h, 1, '--record: exactly one row');
like($h[0] // '', qr/^\d{4}-\d\d-\d\dT\d\d:\d\d:\d\dZ\t[0-9a-f]{7,}\tv2-TEST\t3\t5\trosetta=3\/5$/,
     '--record: date, sha, generation, N, M, by-area -- the battery\'s columns');
($ro) = tool('run', '--dir', $D, '--pcl', $FAKE, '--outdir', "$TMP/rec2", '--record');
like($ro, qr/^--record REFUSED: the tree is dirty/m, '--record: a dirty tree is refused (the history row just written is uncommitted)');
git_in($ROOT, 'commit', '-q', '-a', '-m', 'history');
($ro) = tool('run', '--dir', $D, '--pcl', $FAKE, '--outdir', "$TMP/rec3", '--record', 'rosetta/0001-Alpha-Same');
like($ro, qr/^--record REFUSED: it measured a SUBSET/m, '--record: a subset is refused');
is(scalar(grep { !/^#/ } split /\n/, slurp($HIS)), 1, '--record: the refused runs wrote nothing');

($ro) = smoke('--corpus', "$D/corpus", '--pcl', $FAKE, '--outdir', "$TMP/rec4", '--baseline', 'none', '--record');
like($ro, qr/^--record REFUSED: it measured \S+, not the default corpus \(a named corpus records into its own history: pass --history FILE/m,
     '--record: a non-default corpus without --history is refused, and the message says how');

# -------------------------------------------- the default corpus is untouched
write_file("$ROOT/everyday/basic/hello.pl", qq{print "hello\\n";\n});
write_file("$ROOT/everyday/basic/hello.expect", "hello\n");
{
    ($ro) = smoke('--pcl', $FAKE, '--outdir', "$TMP/ev1", '--baseline', 'none');
    like($ro, qr/^EVERYDAY: 1 of 1 identical to perl \(100\.0 %\) -- [0-9a-f]+\+dirty gen v2-TEST$/m,
         'the default corpus\'s line is EVERYDAY, as before');
    ($ro, $rrc) = smoke('--pcl', $FAKE, '--outdir', "$TMP/ev2", '--baseline', 'none', '--label', 'OTHER');
    is($rrc, 2, '--label is refused for the default corpus');
}
($ro, $rrc) = smoke('--corpus', "$D/corpus", '--pcl', $FAKE, '--outdir', "$TMP/ev3", '--baseline', 'none', '--label', 'bad label');
is($rrc, 2, '--label must be a name');
($ro) = smoke('--corpus', "$D/corpus", '--pcl', $FAKE, '--outdir', "$TMP/ev4", '--baseline', 'none', '--label', 'mine');
like($ro, qr/^MINE: 3 of 5 identical to perl /m, '--label prints the name in capitals, with no "(corpus DIR)"');

done_testing();
