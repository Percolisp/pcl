#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# Unit tests for PCLProc — the ONE session-isolation + reaping helper the two
# suite runners share (task #367; extracted s413, #387 family 6).
#
# NOT part of the Pl/t gate: that gate measures the transpiler, this measures
# the harness.  Run it directly:  prove tools/t/pclproc.t
use strict;
use warnings;
use Test::More tests => 7;
# The fork override for the last test must be compiled BEFORE PCLProc is.
our $FORK_FAILS = 0;
BEGIN { *CORE::GLOBAL::fork = sub { $FORK_FAILS ? do { $! = 11; undef } : CORE::fork() } }
use FindBin qw($RealBin);
use lib "$RealBin/../lib";
use PCLProc qw(run_isolated reap_session);

# --- exit status passes through -------------------------------------------
my ($rc, $reaped) = run_isolated('exit 3');
is($rc >> 8, 3, 'run_isolated returns the child\'s wait status');
is($reaped, 0,  'nothing left in the session: reaped 0');

# --- a descendant in its OWN PROCESS GROUP is still reaped by session ------
# This is the #367 shape: SBCL's run-program puts its child in a new pgroup,
# so `timeout`'s group kill misses it; the SESSION still reaches it.
my $flag = "$RealBin/.pclproc-$$";
unlink $flag;
($rc, $reaped) = run_isolated(
  "perl -e 'setpgrp(0,0); \$0=q{pclproc-orphan-$$}; sleep 30' & echo \$! > \Q$flag\E; exit 0");
is($rc, 0, 'the shell itself exited 0');
is($reaped, 1, 'the escaped grandchild (new pgroup) was found and reaped');
open my $fh, '<', $flag or die "no pid file: $!";
chomp(my $orphan = <$fh> // '');
close $fh;
unlink $flag;
like($orphan, qr/^\d+$/, 'the grandchild pid was recorded');
select undef, undef, undef, 0.3;
ok(!kill(0, $orphan), 'and it is dead afterwards');

# --- the fork-failure prefix names the runner ------------------------------
# (fork is overridden in the BEGIN block above; the default derives from $0)
{
  local $0 = q{/x/y/some-runner.pl};
  local $FORK_FAILS = 1;
  eval { PCLProc::run_isolated(q{true}) };
  my $err = $@;
  like($err, qr/^some-runner\.pl: fork failed:/, 'default runner name = basename of $0');
}
