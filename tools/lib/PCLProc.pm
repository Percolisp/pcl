# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

package PCLProc;
# The ONE place a PCL runner isolates a child in its own SESSION and reaps
# what that child leaves behind (task #387 family 6, extracted s413 from the
# byte-identical copies in sweep-perl-tests.pl and tools/run-perl-suite.pl —
# the #366/#367 fixes had to land in BOTH, and a drift here is a runner that
# kills differently).
#
# ---- Per-file SESSION isolation (task #367) --------------------------------
#
# `timeout` already kills the process GROUP it created — measured, a plain
# grandchild dies with it.  What escapes is anything SBCL starts: `run-program`
# puts its child in a NEW PROCESS GROUP (measured: the child's PID == its PGID),
# so the group signal cannot reach it, and a spinning `pclperl-for-tests` or
# `pl2cl --server` outlives the run.  In s405 one such orphan burned a core for
# 3516 s through every measurement of the session and nothing noticed.
#
# The SESSION is the handle that survives that: setpgrp does not change it, so
# every descendant of a file's run still shares the session we give it.  Run
# each file's command in its own session and, when it is over, kill whatever is
# still in there.  `timeout` stays inside the command, so the timing behaviour
# the runners were tuned to is unchanged.
#
# ---- Orphaned transpile servers (task #273 / s396–s397) --------------------
#
# An SBCL child that used string eval spawns `pl2cl --server` (the persistent
# transpiler).  When that SBCL is killed the server is REPARENTED and keeps
# running: its stdin has hit EOF, but the loop only notices between requests,
# so a server that was mid-transpile grinds on.  Measured s396: two such
# orphans (op/cond.t's 20k-nested ternary, the documented pathological-nesting
# case) sat at 4.8 GB and 4.6 GB for half an hour, competing with the run that
# had outlived them — and MemAvailable is exactly what decides whether a
# parallel sweep stays stable (task #215).  Reap them between files.
use strict;
use warnings;
use POSIX ();
use Exporter 'import';
our @EXPORT_OK = qw(run_isolated reap_session reap_orphan_transpilers);

# run_isolated($shell_cmd, %opt) -> ($wait_status, $reaped_count)
#
# Runs $shell_cmd under `/bin/sh -c` in a NEW SESSION, waits for it, then
# reaps everything still alive in that session.  Returns the child's `$?` and
# how many processes the reap found.  %opt: runner => NAME (the prefix of the
# fork-failure message; default = the script's basename).
sub run_isolated {
  my ($cmd, %opt) = @_;
  my $runner = $opt{runner} // ($0 =~ m{([^/]+)$} ? $1 : $0);
  my $pid = fork();
  die "$runner: fork failed: $!\n" if !defined $pid;
  if (!$pid) {
    POSIX::setsid();                       # our PID becomes the session id
    exec('/bin/sh', '-c', $cmd);
    POSIX::_exit(127);
  }
  waitpid($pid, 0);
  my $rc = $?;
  return ($rc, reap_session($pid));
}

# Everything still alive in session $sid: TERM, a short grace, then KILL.
# Returns how many were reaped, which the caller reports — an orphan that is
# never counted is how #367 stayed invisible for a session.
sub reap_session {
  my ($sid) = @_;
  my @doomed = _session_members($sid);
  my $found  = @doomed;          # what we REAPED — not what survived it
  return 0 if !$found;
  kill 'TERM', @doomed;
  for (1 .. 10) {
    last if !(@doomed = _session_members($sid));
    select undef, undef, undef, 0.1;
  }
  if (@doomed = _session_members($sid)) {
    kill 'KILL', @doomed;
    select undef, undef, undef, 0.2;
  }
  return $found;
}

sub _session_members {
  my ($sid) = @_;
  my @out;
  opendir my $dh, '/proc' or return ();
  for my $e (readdir $dh) {
    next if $e !~ /^[0-9]+$/ || $e == $$;
    open my $sf, '<', "/proc/$e/stat" or next;
    my $line = <$sf>;
    close $sf;
    next if !defined $line;
    # `pid (comm) state ppid pgrp session …` — comm can hold spaces and
    # parens, so parse after the LAST ')'.
    my $tail = substr($line, rindex($line, ')') + 1);
    my @f = split ' ', $tail;
    push @out, $e if defined $f[3] && $f[3] == $sid;
  }
  closedir $dh;
  return @out;
}

sub reap_orphan_transpilers {
  # ORPHAN := its parent is a REAPER, not the SBCL that spawned it.  PID 1 is
  # only one adoption target: on a systemd desktop login every orphan is
  # adopted by the session's `systemd --user` (a subreaper), so a PPID==1
  # test never fired on the machine it was written on (measured s397).  The
  # reap stays conservative — a server whose parent is anything else (an
  # sbcl, or a foreign harness) is never touched; the server's OWN
  # getppid watchdog (pl2cl --server, s397) is the fix that covers every
  # adoption target, this is the belt for a server stuck in one long op.
  my @ps = `ps -eo pid,ppid,args 2>/dev/null`;
  for my $l (@ps) {
      next unless $l =~ m{^\s*(\d+)\s+(\d+)\s+\S*perl\S*\s+\S*\bpl2cl\s+--server\s*$};
      my ($pid, $ppid) = ($1, $2);
      my $pcomm = '';
      if (open my $c, '<', "/proc/$ppid/comm") { $pcomm = <$c> // ''; chomp $pcomm }
      next unless $ppid == 1 || $pcomm =~ /^(?:systemd|init)$/;
      kill 'KILL', $pid;
  }
  return;
}

1;
