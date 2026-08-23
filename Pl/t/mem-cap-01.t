#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# mem-cap-01.t — the compiler side runs under a memory cap (task #471).
#
# WHY THIS IS GUARDED: on 2026-08-23 a self-recursive helper in Pl/** took a
# single compiler-side perl to 7.76 GB, exhausted the swap and hung the
# machine into a global OOM kill.  Nothing in the tree stopped it: perl's own
# deep-recursion WARNING fired at depth 100 and changed nothing, because a
# warning does not stop an allocation.  pl2cl now re-execs itself once through
# `sh -c 'ulimit -v N; exec …'`, so the same bug dies in ~4 s naming the sub
# and the line (measured s436, with `free` showing the swap untouched).
#
# The cap is invisible when nothing is wrong — the rows below therefore assert
# the two things that can silently rot: that it is APPLIED (a future edit
# moving the BEGIN block below `use Pl::Parser` would still "work", uncapped),
# and that the emission does not depend on it.
#
# NOT asserted here: the runaway itself.  Making the compiler run away needs a
# mutated copy of Pl/, which is a scratch-tree probe (recorded in #471), not a
# gate row — the gate must never contain a test that eats 4 GB before failing.

use v5.30;
use strict;
use warnings;
use Test::More;
use FindBin qw($RealBin);

my $root  = "$RealBin/../..";
my $pl2cl = "$root/pl2cl";

plan skip_all => 'no /proc/self/limits (not Linux)' unless -r '/proc/self/limits';

# Run pl2cl over a trivial program; return (stdout, stderr, exit code).
#
# `ulimit -c 0` and the inner subshell are not decoration: the enforcement row
# below starves perl's own STARTUP, which on Linux dies by SIGSEGV rather than
# by a clean "Out of memory" (perl cannot handle a malloc failure that early).
# Without them the gate would drop a core file and print a bare "Segmentation
# fault" from the shell on every run — a row that looks like a crash while
# passing is worse than no row.
sub run_pl2cl {
  my (%opt) = @_;
  my $env  = join ' ', map { "$_=" . ($opt{env}{$_}) } sort keys %{ $opt{env} || {} };
  my $args = $opt{args} // '';
  my $ef   = "/tmp/pcl-memcap-$$.err";
  my $out  = qx{ulimit -c 0; (echo 'my \$x = 1;' | $env $pl2cl $args) 2>$ef};
  my $rc   = $?;
  my $err  = do { local $/; open my $f, '<', $ef or return ($out, '', $rc); <$f> // '' };
  unlink $ef;
  return ($out, $err, $rc);
}

# 1-2. The default cap is in force, and it is the documented 4 GB.
my (undef, $err) = run_pl2cl(env => { PCL_SHOW_MEM_CAP => 1 });
like($err, qr/^pl2cl: memory cap = \d+ bytes$/m,
     'pl2cl reports an address-space cap in force by default');
like($err, qr/memory cap = 4294967296 bytes/,
     'the default cap is 4096 MB (matches run-perl-suite.pl\'s ulimit on its children)');

# 3. PCL_MEM_CAP_MB overrides it.
(undef, $err) = run_pl2cl(env => { PCL_SHOW_MEM_CAP => 1, PCL_MEM_CAP_MB => 2048 });
like($err, qr/memory cap = 2147483648 bytes/, 'PCL_MEM_CAP_MB=2048 caps at 2 GB');

# 4. PCL_NO_MEM_CAP=1 removes it (the deliberate big compile).
(undef, $err) = run_pl2cl(env => { PCL_SHOW_MEM_CAP => 1, PCL_NO_MEM_CAP => 1 });
like($err, qr/memory cap = none/, 'PCL_NO_MEM_CAP=1 runs uncapped');

# 5. The cap is ENFORCED, not merely reported: a cap below what perl+PPI needs
#    to start must make the transpile fail.  16 MB is far under the ~140 MB
#    peak of the heaviest legitimate transpile, so this can never be flaky in
#    the other direction.
my ($out, undef, $rc) = run_pl2cl(env => { PCL_MEM_CAP_MB => 16 });
isnt($rc, 0, 'a 16 MB cap actually stops the process (the limit is real)');
is($out, '', '... and it produces no emission');

# 6. --bundle/--executable are exempt: they spawn SBCL, which RESERVES a
#    multi-gigabyte address space, and `ulimit -v` counts reservations.
#    (`--bundle` with no source file refuses AFTER the cap decision, which is
#    what makes this observable without building anything.)
(undef, $err) = run_pl2cl(env => { PCL_SHOW_MEM_CAP => 1 }, args => '--bundle');
like($err, qr/memory cap = none/, '--bundle is exempt (it spawns SBCL)');

# 7. The emission does not depend on the cap.
my ($capped)   = run_pl2cl();
my ($uncapped) = run_pl2cl(env => { PCL_NO_MEM_CAP => 1 });
ok(length $capped, 'the capped run still transpiles');
is($capped, $uncapped, 'capped and uncapped emission are byte-identical');

done_testing();
