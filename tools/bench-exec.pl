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
  ['fib(27)x',  "$HN sub fib { my \$m=shift; \$m<2 ? \$m : fib(\$m-1)+fib(\$m-2) } my \$r=0; \$r=fib(27) for 1..\$n; print \"\$r\\n\";", 30, 0],
  # Multi-statement recursive sub: exercises the sub-body :void regime
  # (task #60) — fib above coalesces to a single-statement body and skips it.
  ['gcdrec',    "$HN sub gcd { my (\$x,\$y)=\@_; return gcd(\$x-\$y,\$y) if \$x>\$y; return gcd(\$x,\$y-\$x) if \$x<\$y; \$x } my \$r=0; \$r += gcd(\$_ % 97 + 1, 89) for 1..\$n; print \"\$r\\n\";", 100_000, 0],
  ['collatz',   "$HN my \$c=0; for my \$i (1..\$n) { my \$m=\$i; while (\$m>1) { \$m = \$m%2 ? 3*\$m+1 : \$m/2; \$c++ } } print \"\$c\\n\";", 300_000, 0],
  ['strcat',    "$HN my \$s=''; for (1..\$n) { \$s .= 'x' } print length(\$s), \"\\n\";", 100_000, 0],
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
  # READ-ONLY foreach over an ARRAY (task #862 ARM A, the boxed-aggregates
  # design's §4.4 proven arm).  The loop variable aliases each element, which
  # under raw element storage PROMOTES every slot to a box — once, but
  # promotion is monotone, so the array then pays box indirection on every
  # later read forever.  This row is the arm's own metric; the never-promoting
  # index spelling `for my $i (0..$#a) { $s += $a[$i] }` is the floor it is
  # chasing.
  ['feread',    "$HN my \@a = (1..1000); my \$s=0; for (1..\$n) { for my \$x (\@a) { \$s += \$x } } print \"\$s\\n\";", 30_000, 0],
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
my ($cfh, $CORE) = tempfile('pcl-bench-core.XXXXXX', TMPDIR => 1, UNLINK => 0);
close $cfh; unlink $CORE;
END { unlink $CORE if defined $CORE }
$SIG{$_} = sub { unlink $CORE if defined $CORE; exit 1 } for qw(INT TERM);
print STDERR "bench-exec: building runtime core ...\n";
system(qq{sbcl --noinform --non-interactive --load "$RT" }
     . qq{--eval "(sb-ext:save-lisp-and-die \\"$CORE\\" :executable nil)" >/dev/null 2>&1});
die "bench-exec: core build failed\n" unless -s $CORE;

# The SBCL command line comes from the ONE builder every other runner uses
# (tools/lib/PCLSbcl.pm) — this file hand-wrote its own until s459am and so ran
# PCL on the 2 MB default control stack, which is #324 all over again.  Only
# the CORE is ours: it is built above from this tree's runtime on purpose.
my $SBCL = sbcl_prefix_str(core => $CORE, runtime => $RT);

sub best {                       # min wall time over K runs of $cmd with N=$n
  my ($cmd, $n) = @_;
  my $min;
  for (1 .. $K) {
    local $ENV{N} = $n;
    my $t = time();
    system("$cmd >/dev/null 2>&1");
    my $e = time() - $t;
    $min = $e if !defined $min || $e < $min;
  }
  return $min;
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

printf "PCL v2 vs Perl — execution time (startup subtracted), best-of-%d\n\n", $K;
printf "%-11s %10s %10s %9s\n", 'bench', 'perl(s)', 'pcl(s)', 'pcl/perl';
printf "%-11s %10s %10s %9s\n", '-'x11, '-'x10, '-'x10, '-'x9;

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

  # Verify before timing (#814): both engines must print the same thing at
  # N_big, and PCL must exit 0.  Otherwise the row is BROKEN, not fast.
  my ($pout, $prc) = run_out($perl_cmd, $big);
  my ($cout, $crc) = run_out($pcl_cmd,  $big);
  if ($crc != 0 || $cout ne $pout) {
    my $why = $crc != 0 ? sprintf('pcl exit %d', $crc >> 8)
            :             sprintf('perl=%.30s pcl=%.30s', $pout, $cout);
    printf "%-11s %10s %10s %9s   BROKEN: %s\n", $name, '-', '-', '-', $why;
    next;
  }

  my $perl_exec = best($perl_cmd, $big) - best($perl_cmd, $small);
  my $pcl_exec  = best($pcl_cmd,  $big) - best($pcl_cmd,  $small);
  $_ < 0 and $_ = 0 for $perl_exec, $pcl_exec;

  my $ratio = $perl_exec > 0 ? sprintf('%.2fx', $pcl_exec / $perl_exec) : 'n/a';
  printf "%-11s %10.4f %10.4f %9s\n", $name, $perl_exec, $pcl_exec, $ratio;
}

print "\nratio < 1.00x = PCL faster; > 1.00x = PCL slower.\n";
