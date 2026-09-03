#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# bench-exec.pl — execution-speed comparison of PCL (v2) vs Perl, STARTUP
# SUBTRACTED.  Measures only the time spent running the benchmark's hot loop,
# not process startup, parsing, or fasl load.
#
#   perl tools/bench-exec.pl                 # run all benches, best-of-5
#   BENCH_K=3 perl tools/bench-exec.pl       # best-of-3 (faster, noisier)
#   perl tools/bench-exec.pl intloop fib     # only benches whose name matches
#
# METHOD (why the numbers are fair):
#   Each bench runs twice per engine — once with a BIG iteration count and once
#   with a SMALL one — where N is read at RUN TIME from $ENV{N}, so the compiled
#   code is byte-identical between the two runs.  exec = t(big) - t(small)
#   therefore cancels, for BOTH engines equally: process startup, Perl parse /
#   PCL SBCL-compile, and fasl/core load.  What remains is the loop itself.
#   Best-of-K (minimum) suppresses scheduler noise.
#
#   PCL is run against a saved SBCL core with cl/pcl-runtime.lisp baked in
#   (built fresh here, removed on exit) so runtime compilation (~1.2 s) is not
#   timed.  The per-program loop still compiles at load, but that cost is in
#   BOTH the big and small runs, so it subtracts out.
#
# EVERY ROW IS VERIFIED BEFORE IT IS TIMED (task #814, s459am).  A run that
# CRASHES is fast, and a subtraction of two crashes is 0.0000 — so without a
# correctness check the table's most attractive number is its least trustworthy
# one.  Each row therefore runs both engines once at N_big and compares stdout;
# a mismatch or a non-zero PCL exit prints BROKEN and the row is NOT timed.
# That is exactly how the regexg row was reading 0.0009 s / 0.01x: it ran on
# SBCL's 2 MB default control stack (this file was a SEVENTH SBCL runner
# hand-writing its own option string — the #324 drift, see tools/lib/PCLSbcl.pm)
# and died in `'a' x 200000` identically at N=5 and N=0.  The command line now
# comes from PCLSbcl like every other runner's, and #880 fixed the p-str-x
# stack blowup underneath it.
#
# EXTENDING: add a row to @benches: [ name, perl-source, N_big, N_small ].
#   Use the $HN prefix so the hot loop reads a lexical $n, never %ENV.  Keep the
#   source valid Perl that also transpiles under PCL — the verification pass
#   tells you at once if it does not.  N_big must make the row's own work large
#   against the ~1 s constant term both runs pay (SBCL compiles the program at
#   load): a signal below the run-to-run spread subtracts to noise.
use v5.30;
use strict;
use warnings;
use Time::HiRes qw(time);
use File::Temp qw(tempfile);
use File::Basename qw(dirname);
use Cwd qw(abs_path);

my $ROOT;
BEGIN { $ROOT = dirname(dirname(abs_path($0))) }
use lib "$ROOT/tools/lib";
use PCLSbcl qw(sbcl_prefix_str);

my $PL2CL = "$ROOT/pl2cl";
my $RT    = "$ROOT/cl/pcl-runtime.lisp";
my $K     = $ENV{BENCH_K} // 5;                 # best-of-K
my @only  = @ARGV;                              # optional name filters

# Hoist $ENV{N} into a lexical ONCE so the hot loop never touches %ENV.
my $HN = 'my $n = $ENV{N};';

# name => [perl-source, N_big, N_small]
my @benches = (
  ['intloop+=', "$HN my \$s=0; for (1..\$n) { \$s += \$_ } print \"\$s\\n\";",            5_000_000, 0],
  ['intloop=',  "$HN my \$s=0; for (1..\$n) { \$s = \$s + \$_ } print \"\$s\\n\";",        5_000_000, 0],
  ['cfor',      "$HN my \$s=0; for (my \$i=0; \$i<\$n; \$i++) { \$s = \$s + \$i } print \"\$s\\n\";", 5_000_000, 0],
  ['arrhash',   "$HN my (\@a,\%h,\$s); for (1..\$n) { \$h{x}=\$_; \$a[3]=\$_+1; \$s=\$s+\$h{x}+\$a[3] } print \"\$s\\n\";", 2_000_000, 0],
  # A COMPUTED hash key: `my $k = …; $h{$k}++` in a loop (task #995).  The
  # arrhash row above uses a LITERAL key, so it cannot see the shape this row
  # exists for — a per-iteration `my` whose only "write" is being a subscript.
  # Until s466bd VarAnnotator read `$h{$k}++` as a write to $k and boxed it:
  # one make-p-box + one box-set per iteration (15.4 % + 8.1 % of an
  # sb-sprof profile of exactly this program).  The row is the metric for
  # that class of false positive; keep the key STRING-valued, since a raw
  # string slot is the verdict the fix restores.
  ['arrhash-k',  "$HN my (\%h,\@a); for my \$i (1..\$n) { my \$k = \"k\" . (\$i % 500); \$h{\$k}++; push \@a, \$i } print scalar(keys \%h), \" \", scalar(\@a), \"\\n\";", 600_000, 0],
  ['fib(27)x',  "$HN sub fib { my \$m=shift; \$m<2 ? \$m : fib(\$m-1)+fib(\$m-2) } my \$r=0; \$r=fib(27) for 1..\$n; print \"\$r\\n\";", 30, 0],
  # Multi-statement recursive sub: exercises the sub-body :void regime
  # (task #60) — fib above coalesces to a single-statement body and skips it.
  ['gcdrec',    "$HN sub gcd { my (\$x,\$y)=\@_; return gcd(\$x-\$y,\$y) if \$x>\$y; return gcd(\$x,\$y-\$x) if \$x<\$y; \$x } my \$r=0; \$r += gcd(\$_ % 97 + 1, 89) for 1..\$n; print \"\$r\\n\";", 100_000, 0],
  ['collatz',   "$HN my \$c=0; for my \$i (1..\$n) { my \$m=\$i; while (\$m>1) { \$m = \$m%2 ? 3*\$m+1 : \$m/2; \$c++ } } print \"\$c\\n\";", 300_000, 0],
  # N_big was 100_000 until s461aq (#881): 100k appends is ~0.003 s of perl and
  # ~0.005 s of PCL, both far under the run-to-run spread of the ~1 s constant
  # term both runs pay to compile the program — so the row's own ratio was a
  # ratio of two noise samples (it printed 1.00x, 1.64x and 1.79x in three
  # consecutive runs of the SAME two trees).  20M puts perl at ~0.5 s.  This is
  # the same N_big rule regexg was fixed under (#814): the row's work must be
  # large against that constant term or the subtraction measures nothing.
  ['strcat',    "$HN my \$s=''; for (1..\$n) { \$s .= 'x' } print length(\$s), \"\\n\";", 20_000_000, 0],
  # pack/unpack: perl's is C (pp_pack.c); PCL's is the TRANSPILED pure-Perl
  # oracle (cl/pack-impl.pl → cl/pcl-pack.lisp), which re-parses the template
  # string per call — expect a big ratio; this row tracks oracle overhead,
  # not codegen quality.  Small N accordingly.
  ['pack',      "$HN my \$s=0; for (1..\$n) { \$s += length(pack('N n C a3', \$_, \$_ % 65536, \$_ % 256, 'abc')) + length(pack('V d', \$_, \$_ * 1.5)) } print \"\$s\\n\";", 20_000, 0],
  ['packunpk',  "$HN my \$s=0; for (1..\$n) { my (\$x,\$y,\$z) = unpack('N n C', pack('N n C', \$_, \$_ % 65536, \$_ % 256)); \$s += \$x + \$z } print \"\$s\\n\";", 20_000, 0],
  # The runtime paths the s413 Lisp de-duplication touched (#387 families 13,
  # 21, 23, 37, 42, 46): array fill, slices, an overloaded binop, symbolic
  # refs, slice assignment.  A/B these HEAD-vs-tree when editing those.
  ['arrfill',   "$HN my \@a; my \$s=0; for (1..\$n) { \@a = (1..20, \$_); \$s += \@a } print \"\$s\\n\";", 200_000, 0],
  ['slices',    "$HN my \@a = (1..50); my \%h = map { \$_ => \$_ } 1..50; my \@k = (1..10); my \$s=0; for (1..\$n) { my \@v = \@a[1..5]; my \@w = \@h{\@k}; \$s += \$v[0] + \$w[9] } print \"\$s\\n\";", 200_000, 0],
  ['sliceasgn', "$HN my \@a = (1..20); my \%h; my \$s=0; for (1..\$n) { \@a[1..3] = (7,8,9); \@h{'a','b'} = (\$_, 2); \$s += \$a[2] + \$h{a} } print \"\$s\\n\";", 200_000, 0],
  # `my @c = @src` — a WHOLE-ARRAY copy into a fresh lexical, the commonest
  # list assignment there is, and until s463av (#981) no row covered it: the
  # slice rows assign 5 and 10 elements, which is too few for the destination's
  # growth to show against their hash traffic.  At 50 it is 20 % of the shape
  # (measured both ways at the #981 fix).  50 elements x 1e6 puts perl at
  # ~0.74 s, comfortably above the ~1 s constant term's spread.
  ['listcopy',  "$HN my \@src = (1..50); my \$s=0; for (1..\$n) { my \@c = \@src; \$s += scalar(\@c) } print \"\$s\\n\";", 1_000_000, 0],
  # READ-ONLY foreach over an ARRAY (task #862 ARM A, the boxed-aggregates
  # design's §4.4 proven arm).  The loop variable aliases each element, which
  # under raw element storage PROMOTES every slot to a box — once, but
  # promotion is monotone, so the array then pays box indirection on every
  # later read forever.  This row is the arm's own metric; the never-promoting
  # index spelling `for my $i (0..$#a) { $s += $a[$i] }` is the floor it is
  # chasing.
  ['feread',    "$HN my \@a = (1..1000); my \$s=0; for (1..\$n) { for my \$x (\@a) { \$s += \$x } } print \"\$s\\n\";", 30_000, 0],
  # #883: the SAME read-only foreach over a MULTI-array list.  `for my $x
  # (@a, @b)` lowers to p-foreach-raw over (p-flatten-args (list @a @b)), and
  # the flattener promotes every source slot as it builds the flattened
  # vector — so ARM A's verdict fires but the allocation it exists to avoid
  # has already happened, and @a/@b pay box indirection on every later read.
  # Element count and total work are matched to `feread` (2 x 500 = 1000
  # elements, same 30k outer repeats) so the two rows subtract cleanly: if
  # this row is near feread's ratio the flattener is NOT the cost.
  ['feread2',   "$HN my \@a = (1..500); my \@b = (501..1000); my \$s=0; for (1..\$n) { for my \$x (\@a, \@b) { \$s += \$x } } print \"\$s\\n\";", 30_000, 0],
  ['ovlsub',    "$HN package V; use overload '-' => sub { V->new(\$_[2] ? \$_[1] - \$_[0]{v} : \$_[0]{v} - (ref \$_[1] ? \$_[1]{v} : \$_[1])) }, '\"\"' => sub { \$_[0]{v} }; sub new { bless { v => \$_[1] }, \$_[0] } package main; my \$x = V->new(1000); my \$s = 0; for (1..\$n) { my \$y = \$x - 3; \$s += \"\$y\" } print \"\$s\\n\";", 100_000, 0],
  ['symref',    "$HN no strict 'refs'; our \$g = 2; our \@ga = (1,2,3); my \$s=0; for (1..\$n) { \$s += \${'main::g'} + \${'g'} + scalar(\@{'main::ga'}) } print \"\$s\\n\";", 200_000, 0],
  # Scalar-context m//g per-match cost (task #680): N repeats of a 200k-char
  # /./g loop = N*200k matches.  The subject build is identical between big
  # and small runs, so it subtracts out with startup.
  # N_big was 5 until s459am (#814) — 1M matches, ~0.06 s of perl, which is
  # under the run-to-run spread of the ~1 s both runs pay to compile the
  # program, so even once the row stopped CRASHING it would have measured
  # mostly noise.  30 puts perl at ~0.36 s and PCL at ~0.8 s.
  ['regexg',    "$HN my \$x = 'a' x 200000; my \$c = 0; for (1..\$n) { \$c = 0; while (\$x =~ /./g) { \$c++ } } print \"\$c\\n\";", 30, 0],
);

# ---- build a fresh runtime core (like tools/prove-core) --------------------
my @CORES;
END { unlink $_ for @CORES }
$SIG{$_} = sub { unlink $_ for @CORES; exit 1 } for qw(INT TERM);

sub build_core {                 # save a core with $rt_source baked in
  my ($rt_source, $label) = @_;
  my ($cfh, $core) = tempfile('pcl-bench-core.XXXXXX', TMPDIR => 1, UNLINK => 0);
  close $cfh; unlink $core;
  push @CORES, $core;

  # The runtime derives *pcl-runtime-directory* from its own *load-truename*,
  # and the lazily-loaded extensions (pack/mro/warnings/xs) are found under it.
  # So a B runtime kept OUTSIDE cl/ builds a core whose `pack` cannot load —
  # which shows up as `BROKEN: pclB exit 1` on the two pack rows and is an
  # artefact of where the file sits, not of what is in it.  Stage any source
  # that is not already beside $RT into cl/ for the build.
  my $staged;
  if (dirname(abs_path($rt_source)) ne dirname(abs_path($RT))) {
    my ($sfh, $sfile) = tempfile('bench-rt-XXXXXX', SUFFIX => '.lisp',
                                 DIR => dirname($RT), UNLINK => 0);
    close $sfh;
    open my $in, '<', $rt_source or die "bench-exec: $rt_source: $!\n";
    open my $out, '>', $sfile    or die "bench-exec: $sfile: $!\n";
    print $out $_ while <$in>;
    close $in; close $out;
    $staged = $sfile;
    push @CORES, $sfile;         # same cleanup list (unlinked on exit/INT/TERM)
    $rt_source = $sfile;
  }

  print STDERR "bench-exec: building runtime core ($label) ...\n";
  system(qq{sbcl --noinform --non-interactive --load "$rt_source" }
       . qq{--eval "(sb-ext:save-lisp-and-die \\"$core\\" :executable nil)" >/dev/null 2>&1});
  my $ok = -s $core;
  unlink $staged if defined $staged;
  die "bench-exec: core build failed ($label: $rt_source)\n" if !$ok;
  return $core;
}

# RUNTIME A/B (s463av, task #950).  BENCH_RT_B=<other cl/pcl-runtime.lisp> adds
# a SECOND core built from that source and times both PCL sides INTERLEAVED
# (below).  Emission is identical for the two — only the runtime differs — so
# the one transpile is shared and the columns differ by exactly the runtime.
# Use it for any runtime-side change: a policy declaim, a hot-path rewrite, a
# base-commit worktree's runtime.  Without it the tool behaves as before.
my $RT_B = $ENV{BENCH_RT_B};
my $CORE   = build_core($RT, 'A');
my $CORE_B = defined $RT_B ? build_core($RT_B, 'B') : undef;

# The SBCL command line comes from the ONE builder every other runner uses
# (tools/lib/PCLSbcl.pm) — this file hand-wrote its own until s459am and so ran
# PCL on the 2 MB default control stack, which is #324 all over again.  Only
# the CORE is ours: it is built above from this tree's runtime on purpose.
my $SBCL   = sbcl_prefix_str(core => $CORE, runtime => $RT);
my $SBCL_B = defined $CORE_B
           ? sbcl_prefix_str(core => $CORE_B, runtime => $RT_B) : undef;

# INTERLEAVED best-of-K over several (command, N) series at once.  Every series
# gets its round-r sample before any series gets its round-(r+1) sample, so a
# drift in machine load during the row — a sibling agent starting a sweep — hits
# all series alike instead of landing wholly on whichever one ran last.  This is
# what makes an A/B verdict trustworthy on a SHARED box; on a quiet one it is
# identical in expectation to the old series-at-a-time loop.  Returns one
# minimum per series, in order.
sub best_interleaved {
  my (@series) = @_;             # each: [ $cmd, $n ]
  my @min = (undef) x @series;
  for my $round (1 .. $K) {
    for my $i (0 .. $#series) {
      my ($cmd, $n) = @{ $series[$i] };
      local $ENV{N} = $n;
      my $t = time();
      system("$cmd >/dev/null 2>&1");
      my $e = time() - $t;
      $min[$i] = $e if !defined $min[$i] || $e < $min[$i];
    }
  }
  return @min;
}

# stdout of one run, plus the exit status.  A row is timed only when the two
# engines AGREE here: a crashed run is fast, and two crashes subtract to zero.
sub run_out {
  my ($cmd, $n) = @_;
  local $ENV{N} = $n;
  my $out = `$cmd 2>/dev/null`;
  my $rc  = $?;
  $out =~ s/\s+\z//;
  return ($out, $rc);
}

printf "PCL v2 vs Perl — execution time (startup subtracted), best-of-%d\n", $K;
printf "runtime B: %s\n", $RT_B if defined $RT_B;
print "\n";
if (defined $RT_B) {
  printf "%-11s %10s %10s %10s %9s %9s\n",
         'bench', 'perl(s)', 'pclA(s)', 'pclB(s)', 'B/A', 'A/perl';
  printf "%-11s %10s %10s %10s %9s %9s\n",
         '-'x11, '-'x10, '-'x10, '-'x10, '-'x9, '-'x9;
}
else {
  printf "%-11s %10s %10s %9s\n", 'bench', 'perl(s)', 'pcl(s)', 'pcl/perl';
  printf "%-11s %10s %10s %9s\n", '-'x11, '-'x10, '-'x10, '-'x9;
}

for my $b (@benches) {
  my ($name, $src, $big, $small) = @$b;
  next if @only && !grep { index($name, $_) >= 0 } @only;

  my ($pfh, $pfile) = tempfile(SUFFIX => '.pl', UNLINK => 1);
  print $pfh $src; close $pfh;
  my $perl_cmd = "perl $pfile";

  my ($lfh, $lfile) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
  close $lfh;
  system("$PL2CL $pfile > $lfile 2>/dev/null");
  my $pcl_cmd = "$SBCL --load $lfile";
  my $pcl_cmd_b = defined $SBCL_B ? "$SBCL_B --load $lfile" : undef;

  # Verify before timing (#814): both engines must print the same thing at
  # N_big, and PCL must exit 0.  Otherwise the row is BROKEN, not fast.
  # In A/B mode BOTH runtimes are verified — a "faster" runtime that stopped
  # computing the row's answer is the failure this check exists to catch.
  my ($pout, $prc) = run_out($perl_cmd, $big);
  my @engines = ([$pcl_cmd, 'pcl']);
  push @engines, [$pcl_cmd_b, 'pclB'] if defined $pcl_cmd_b;
  my $broken;
  for my $e (@engines) {
    my ($cout, $crc) = run_out($e->[0], $big);
    next if $crc == 0 && $cout eq $pout;
    $broken = $crc != 0 ? sprintf('%s exit %d', $e->[1], $crc >> 8)
            :             sprintf('perl=%.30s %s=%.30s', $pout, $e->[1], $cout);
    last;
  }
  if (defined $broken) {
    my $cols = defined $RT_B ? 5 : 4;
    printf "%-11s%s   BROKEN: %s\n", $name, ('      -' x ($cols - 1)), $broken;
    next;
  }

  my @series = ([$perl_cmd, $big], [$perl_cmd, $small],
                [$pcl_cmd,  $big], [$pcl_cmd,  $small]);
  push @series, [$pcl_cmd_b, $big], [$pcl_cmd_b, $small] if defined $pcl_cmd_b;
  my ($pb, $ps, $cb, $cs, $bb, $bs) = best_interleaved(@series);

  my $perl_exec = $pb - $ps;
  my $pcl_exec  = $cb - $cs;
  $_ < 0 and $_ = 0 for $perl_exec, $pcl_exec;
  my $ratio = $perl_exec > 0 ? sprintf('%.2fx', $pcl_exec / $perl_exec) : 'n/a';

  if (!defined $pcl_cmd_b) {
    printf "%-11s %10.4f %10.4f %9s\n", $name, $perl_exec, $pcl_exec, $ratio;
    next;
  }
  my $pclb_exec = $bb - $bs;
  $pclb_exec = 0 if $pclb_exec < 0;
  my $ba = $pcl_exec > 0 ? sprintf('%+.1f%%', 100 * ($pclb_exec / $pcl_exec - 1))
         :                 'n/a';
  printf "%-11s %10.4f %10.4f %10.4f %9s %9s\n",
         $name, $perl_exec, $pcl_exec, $pclb_exec, $ba, $ratio;
}

print "\nratio < 1.00x = PCL faster; > 1.00x = PCL slower.\n";
