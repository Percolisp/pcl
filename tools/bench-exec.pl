#!/usr/bin/env perl
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
# EXTENDING: add a row to @benches: [ name, perl-source, N_big, N_small ].
#   Use the $HN prefix so the hot loop reads a lexical $n, never %ENV.  Keep the
#   source valid Perl that also transpiles under PCL (run tools once by hand and
#   eyeball that perl and pcl print the same result).
use v5.30;
use strict;
use warnings;
use Time::HiRes qw(time);
use File::Temp qw(tempfile);
use File::Basename qw(dirname);
use Cwd qw(abs_path);

my $ROOT  = dirname(dirname(abs_path($0)));
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
  my $pcl_cmd = "sbcl --core $CORE --noinform --non-interactive --load $lfile";

  my $perl_exec = best($perl_cmd, $big) - best($perl_cmd, $small);
  my $pcl_exec  = best($pcl_cmd,  $big) - best($pcl_cmd,  $small);
  $_ < 0 and $_ = 0 for $perl_exec, $pcl_exec;

  my $ratio = $perl_exec > 0 ? sprintf('%.2fx', $pcl_exec / $perl_exec) : 'n/a';
  printf "%-11s %10.4f %10.4f %9s\n", $name, $perl_exec, $pcl_exec, $ratio;
}

print "\nratio < 1.00x = PCL faster; > 1.00x = PCL slower.\n";
