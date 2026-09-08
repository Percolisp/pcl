#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# End-to-end test for `pl2cl --executable` and `pl2cl --bundle` (task #1060).
#
# NOT part of the Pl/t gate: each --executable row writes a ~49 MB SBCL image
# and each --bundle row compiles the whole runtime.  MEASURED WALL TIME on the
# dev box (s473i, warm core): ~8 s wall for the whole file -- four builds at
# ~1 s each plus one bundle at ~4.5 s, plus the binaries' own runs.  Run it
# directly:  prove tools/t/executable-01.t
#
# WHAT IT PINS.  Before #1060 the build `load`ed the emitted program, so:
#   * the PROGRAM RAN AT BUILD TIME (its output appeared in the build, and a
#     file it wrote existed afterwards), and
#   * the produced binary exited 0 having done NOTHING, and
#   * a program containing `exit` killed the builder, so no binary was
#     written at all.
# Every row below is one of those, phrased as "the binary behaves like perl".
# The oracle is real perl on the same source.

use strict;
use warnings;
use Test::More;
use File::Temp qw(tempdir);
use FindBin qw($RealBin);

my $root  = "$RealBin/../..";
my $pl2cl = "$root/pl2cl";

plan skip_all => "pl2cl not executable" unless -x $pl2cl;
plan skip_all => "sbcl not found"       unless `which sbcl 2>/dev/null`;
plan tests => 24;

my $dir = tempdir(CLEANUP => 1);

# Run CMD, returning (stdout, stderr, exit code).
sub run3 {
  my (@cmd) = @_;
  my $o = "$dir/.out";
  my $e = "$dir/.err";
  my $rc = system(join(' ', (map { quotemeta } @cmd), '>', quotemeta($o),
                       '2>', quotemeta($e)));
  return (slurp($o), slurp($e), $rc == -1 ? -1 : $rc >> 8);
}

sub slurp {
  my ($p) = @_;
  open my $fh, '<', $p or return '';
  local $/;
  my $s = <$fh>;
  close $fh;
  return defined $s ? $s : '';
}

sub write_file {
  my ($p, $text) = @_;
  open my $fh, '>', $p or die "cannot write $p: $!";
  print $fh $text;
  close $fh;
  return $p;
}

# ── 1. the program does NOT run at build, and the binary runs it ────────────
#
# The marker file is the discriminator the task was filed on: before the fix
# it existed after the BUILD and not after the RUN; now it is the other way.
my $marker = "$dir/marker";
my $src = write_file("$dir/prog.pl", <<'PERL');
use strict;
use warnings;
BEGIN { print STDERR "BEGIN-RAN\n"; }
my $marker = $ENV{PCL_EXE_MARKER} or die "no marker path\n";
print "hello\n";
open my $fh, '>', $marker or die "cannot open $marker: $!";
print $fh "written\n";
close $fh;
print "argv=@ARGV\n";
print "env=", ($ENV{PCL_EXE_ENV} // "unset"), "\n";
END { print "END-RAN\n"; }
exit 3;
PERL

my $bin = "$dir/prog";
{
  local $ENV{PCL_EXE_MARKER} = $marker;
  unlink $marker;
  my (undef, $berr, $brc) = run3($pl2cl, '--executable', '-o', $bin, $src);
  is($brc, 0, 'build succeeds even though the program contains exit');
  ok(-x $bin, 'a binary was written');
  ok(!-e $marker, 'the program did NOT run at build time (no marker)');
  like($berr, qr/BEGIN-RAN/, 'BEGIN ran at BUILD time -- that is perl\'s compile phase');
  unlike($berr, qr/^hello$/m, 'the run-phase statements did NOT run at build');
}

# ── 2. the binary matches perl: stdout, exit code, side effect, @ARGV, %ENV ──
{
  local $ENV{PCL_EXE_MARKER} = $marker;
  local $ENV{PCL_EXE_ENV}    = 'from-env';

  unlink $marker;
  my ($bout, $berr2, $brc) = run3($bin, 'a', 'b');
  ok(-e $marker, 'the binary ran the program (marker written at RUN time)');
  is($brc, 3, 'the binary propagates exit 3');
  like($bout, qr/^hello$/m,          'stdout has the program output');
  like($bout, qr/^argv=a b$/m,       '@ARGV comes from the real process');
  like($bout, qr/^env=from-env$/m,   '%ENV is the running process\'s');
  like($bout, qr/^END-RAN$/m,        'END blocks run');
  unlike($berr2, qr/BEGIN-RAN/, 'BEGIN does not run again at run time');

  unlink $marker;
  my ($pout, undef, $prc) = run3('perl', $src, 'a', 'b');
  is($brc,  $prc,  'exit code equals perl\'s');
  is($bout, $pout, 'stdout is byte-identical to perl\'s');
}

# ── 3. an uncaught die is perl-shaped: message only, exit 255 ───────────────
{
  my $dsrc = write_file("$dir/die.pl", "print \"before\\n\";\ndie \"boom\\n\";\n");
  my $dbin = "$dir/die";
  my (undef, undef, $brc0) = run3($pl2cl, '--executable', '-o', $dbin, $dsrc);
  is($brc0, 0, 'die-program builds');

  my ($bout, $berr, $brc) = run3($dbin);
  my ($pout, $perr, $prc) = run3('perl', $dsrc);
  is($brc,  $prc,  'uncaught die exits 255 like perl');
  is($bout, $pout, 'stdout before the die is perl\'s');
  is($berr, $perr, 'stderr is the die message and nothing else');
}

# ── 4. the `use` closure is preloaded at build ──────────────────────────────
{
  my $usrc = write_file("$dir/use.pl", <<'PERL');
use List::Util qw(sum);
print "sum=", sum(1, 2, 3), "\n";
PERL
  my $ubin = "$dir/use";
  my (undef, undef, $brc0) = run3($pl2cl, '--executable', '-o', $ubin, $usrc);
  is($brc0, 0, 'a program with a `use` builds');
  my ($bout, undef, undef) = run3($ubin);
  my ($pout, undef, undef) = run3('perl', $usrc);
  is($bout, $pout, 'a `use`d module works in the binary, and matches perl');
}

# ── 5. --bundle does not run the program at build either ────────────────────
{
  local $ENV{PCL_EXE_MARKER} = $marker;
  local $ENV{PCL_EXE_ENV}    = 'bundle';
  my $bsrc = write_file("$dir/bundle.pl", <<'PERL');
my $marker = $ENV{PCL_EXE_MARKER} or die "no marker path\n";
open my $fh, '>', $marker or die "cannot open $marker: $!";
print $fh "written\n";
close $fh;
print "bundled\n";
PERL
  unlink $marker;
  my (undef, undef, $brc) = run3($pl2cl, '--bundle', '-o', "$dir/b.fasl", $bsrc);
  is($brc, 0, '--bundle builds');
  ok(!-e $marker, '--bundle does NOT run the program at build time');

  my ($lout, undef, undef) =
    run3('sbcl', '--noinform', '--non-interactive', '--load', "$dir/b.fasl");
  like($lout, qr/^bundled$/m, 'loading the bundle fasl runs the program');
  ok(-e $marker, 'loading the bundle produces the program\'s side effect');
}
