#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# tools/run-dist-t.pl under `timeout`: a t file that never ends must leave NO
# process behind (task #3064).  Before the fix `timeout` killed the perl
# wrapper and its SBCL kept spinning, orphaned, for hours (seven of them on
# the box at once, s513i) -- the SBCL ignores TERM while it is hung in Lisp
# code.  The children now run in their own SESSION (PCLProc::run_isolated),
# and a signal to the wrapper reaps that session, ending in KILL.
#
# NOT part of the Pl/t gate (it measures the harness).  Run it directly:
#   prove tools/t/run-dist-t.t
use strict;
use warnings;
use Test::More;
use File::Temp qw(tempdir);
use FindBin qw($RealBin);

my $root = "$RealBin/../..";
plan skip_all => 'sbcl not found' if !`which sbcl 2>/dev/null`;
plan tests => 6;

my $dist = tempdir(CLEANUP => 1);
mkdir "$dist/t" or die "$dist/t: $!";
open my $t, '>', "$dist/t/hang.t" or die;
# The file IGNORES TERM, as the hung SBCL does (that is what made the orphans).
print $t "\$SIG{TERM} = 'IGNORE'; print \"1..1\\n\"; print \"ok 1\\n\"; my \$i = 0; while (1) { \$i++ }\n";
close $t;

# The wrapper's pid names its SBCL's input file (/tmp/run-dist-t_PID.lisp).
my $tpid = fork() // die "fork: $!";
if (!$tpid) {
  open STDOUT, '>', '/dev/null'; open STDERR, '>', '/dev/null';
  exec 'timeout', 20, "$root/tools/run-dist-t.pl", '--rows', $dist, 't/hang.t';
  exit 127;
}
my ($wrapper, $saw_sbcl);
for (1 .. 190) {
  select undef, undef, undef, 0.1;
  $wrapper //= (child_pids($tpid))[0];
  $saw_sbcl ||= $wrapper && procs_matching("run-dist-t_$wrapper.lisp");
  last if $saw_sbcl;
}
waitpid($tpid, 0);
my $rc = $? >> 8;
select undef, undef, undef, 0.5;

is($rc, 124, 'the hung file is timed out (rc 124)');
ok($saw_sbcl, 'its SBCL was running before the timeout')
  or diag('never saw an sbcl for the wrapper -- the row below proves nothing');
my @left = procs_matching("run-dist-t_" . ($wrapper // 'none') . ".lisp");
is(scalar(@left), 0, 'and nothing that ran it is left alive');
kill 'KILL', @left;   # never leave an orphan behind, even on failure

# CWD = the dist root (#1575): a t-file that reads a RELATIVE fixture passes
# however the operator's shell stands, as it does under `prove` in the dist.
my $rdist = tempdir(CLEANUP => 1);
mkdir "$rdist/t" or die "$rdist/t: $!";
open my $fx, '>', "$rdist/t/fixture.txt" or die; print $fx "hello\n"; close $fx;
open my $rt, '>', "$rdist/t/rel.t" or die;
print $rt 'open my $f, "<", "t/fixture.txt" or die "no fixture: $!\n"; my $l = <$f>;'
        . ' print "1..1\n"; print $l eq "hello\n" ? "ok 1\n" : "not ok 1\n";' . "\n";
close $rt;
my $rout = `cd / && "$root/tools/run-dist-t.pl" --rows "$rdist" t/rel.t 2>&1`;
like($rout, qr/^pass=1 fail=0 /m, 'a relative fixture path resolves from the dist root')
  or diag($rout);

sub child_pids {
  my ($ppid) = @_;
  my @out;
  opendir my $d, '/proc' or return;
  for my $p (grep { /^\d+$/ } readdir $d) {
    open my $s, '<', "/proc/$p/stat" or next;
    my $l = <$s> // next;
    my @f = split ' ', substr($l, rindex($l, ')') + 1);
    push @out, $p if $f[1] == $ppid;
  }
  return @out;
}

sub procs_matching {
  my ($needle) = @_;
  my @n;
  opendir my $d, '/proc' or return;
  for my $p (grep { /^\d+$/ && $_ != $$ } readdir $d) {
    open my $c, '<', "/proc/$p/cmdline" or next;
    my $cmd = do { local $/; <$c> } // '';
    push @n, $p if index($cmd, $needle) >= 0;
  }
  return @n;
}

# TAP is read from STDOUT alone (#3241): a stderr line written without its
# newline must not swallow the plan line that follows it on stdout.
open my $gt, '>', "$rdist/t/glue.t" or die;
print $gt '$| = 1; print STDERR "noise without a newline"; print "ok 1\n"; print STDERR "more noise"; print "1..1\n";' . "\n";
close $gt;
my $gout = `cd / && "$root/tools/run-dist-t.pl" --rows "$rdist" t/glue.t 2>/dev/null`;
like($gout, qr/^PLAN\t1\t/m, 'a newline-less stderr line does not hide the plan')
  or diag($gout);

# A random /tmp/<name> in a row DESCRIPTION is written as /tmp/XXXX by the
# board's row writer (RULED s519 #2): the description is the join key.
open my $tt, '>', "$rdist/t/tmpname.t" or die;
print $tt 'print "1..2\n"; print "not ok 1 - Read contents of /tmp/MuB2nxVyim correctly\n";'
        . ' print "not ok 2 - kept /tmp/cpan-board.log\n";' . "\n";
close $tt;
unlink "$rdist/t/glue.t", "$rdist/t/rel.t", "$rdist/t/fixture.txt";
my $rowsf = "$rdist/rows.tsv";
system("cd / && \"$root/tools/cpan-scoreboard.pl\" --rows \"$rowsf\" \"$rdist\" >/dev/null 2>&1");
my $rows = do { open my $rf, '<', $rowsf or die "no rows file: $!"; local $/; <$rf> } // '';
like($rows, qr{\tRead contents of /tmp/XXXX correctly\t.*\n(?s:.*)\tkept /tmp/cpan-board\.log\t}m,
     'a random /tmp component is /tmp/XXXX in the row file, a named one is kept')
  or diag($rows);
