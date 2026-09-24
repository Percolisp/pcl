#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# signals-01.t — %SIG and perl's signal dispositions (task #2107).
#
# Before s494g only ALRM was wired to %SIG: a `$SIG{INT}` / TERM / HUP / USR1
# / CHLD handler never ran, ^C printed an SBCL backtrace and exited 1, SIGTERM
# exited 0 (a supervisor read SUCCESS), and SIGPIPE was ignored, so
# `prog | head -1` ran to the end.  No suite saw it: no suite sends a signal
# from OUTSIDE the process.  This file does, through a helper that waits for
# the program's "ready" line before signalling it (the only honest way).
#
# perl 5.40.3 IS the oracle: every row runs the same program both ways and
# compares stdout AND the wait status (a death BY a signal is not an exit).
# The one row that asserts PCL's own answer is the runtime-owned USR2 (the
# SBCL garbage collector's stop signal on this build): PCL announces that the
# handler is not installed, as docs/not-supported.md "%SIG" says.

use v5.30;
use strict;
use warnings;
use Test::More;
use File::Temp qw(tempdir);
use FindBin qw($RealBin);
use POSIX ":sys_wait_h";
use lib $RealBin;
use PCLCore;

my $project_root = "$RealBin/../..";
my $pl2cl        = "$project_root/pl2cl";
my $runtime      = "$project_root/cl/pcl-runtime.lisp";
my @sbcl_rt      = PCLCore::sbcl_prefix($runtime);

plan skip_all => "pl2cl not found" unless -x $pl2cl;
plan skip_all => "sbcl not found"  unless `which sbcl 2>/dev/null`;

plan tests => 16;

my $workdir = tempdir(CLEANUP => 1);

sub status_str {
    my ($st) = @_;
    return $st if $st !~ /^-?\d+$/;
    return ($st & 127) ? "signal " . ($st & 127) : "exit " . ($st >> 8);
}

# Write TAG.pl and its transpile; returns (perl argv, PCL argv).
sub prepare {
    my ($tag, $body) = @_;
    my $src = "$workdir/$tag.pl";
    open(my $s, '>', $src) or die; print {$s} $body; close $s;
    my $cl_file = "$workdir/$tag.lisp";
    system("$pl2cl $src > $cl_file 2>/dev/null") == 0 or die "transpile $tag";
    return ([$^X, $src], ['sbcl', @sbcl_rt, '--load', $cl_file]);
}

# Run ARGV to completion: (stdout, stderr, wait status).  Optionally signal it
# once it printed "ready\n" (SIG), or read only ONE line and close the pipe
# (HEAD => 1, the `| head -1` shape).
sub run_one {
    my ($argv, %o) = @_;
    pipe(my $r, my $w) or die;
    my $errf = "$workdir/err.$$." . int(rand(1e9));
    my $pid = fork // die;
    if (!$pid) {
        close $r;
        open STDOUT, '>&', $w or die; open STDERR, '>', $errf or die;
        open STDIN, '<', '/dev/null' or die;
        exec @$argv; die "exec: $!";
    }
    close $w;
    my $out = '';
    if ($o{sig}) {
        my $ready = <$r>;
        kill $o{sig}, $pid;
    }
    if ($o{head}) {
        my $l = <$r>; $out = $l // ''; close $r;
    } else {
        local $/; $out .= <$r> // ''; close $r;
    }
    my $st;
    for (1 .. 300) {
        my $k = waitpid($pid, WNOHANG);
        if ($k == $pid) { $st = $?; last }
        select undef, undef, undef, 0.1;
    }
    if (!defined $st) { kill 'KILL', $pid; waitpid($pid, 0); $st = 'still running after 30 s' }
    my $err = do { local $/; my $e; open($e, '<', $errf) ? (<$e> // '') : '' };
    unlink $errf;
    return ($out, $err, status_str($st));
}

# The same program under perl and PCL: stdout and status must agree.
sub same_as_perl {
    my ($tag, $body, $desc, %o) = @_;
    my ($perl, $pcl) = prepare($tag, $body);
    my ($po, undef, $ps) = run_one($perl, %o);
    my ($co, $ce, $cs) = run_one($pcl, %o);
    is("$cs\n$co", "$ps\n$po", $desc) or diag("PCL stderr: $ce");
}

# 1-2: self-signals, deterministic.
same_as_perl('self-basic', <<'PERL', 'INT / TERM / USR1 handlers run on a self-kill and the program continues');
$| = 1; $SIG{INT} = sub { print "caught INT\n" }; $SIG{TERM} = sub { print "caught TERM\n" };
$SIG{USR1} = sub { print "caught $_[0]\n" };
kill "INT", $$; kill "TERM", $$; kill "USR1", $$; print "end\n";
PERL

same_as_perl('self-store', <<'PERL', 'the %SIG store seam: name qualification, named sub, local restore, IGNORE, delete, DEFAULT kills');
$| = 1;
sub named { print "named got $_[0]\n" }
$SIG{USR1} = "named"; print "stored=$SIG{USR1}\n"; kill USR1 => $$;
{ package Q; $SIG{HUP} = "h"; print "in Q: $SIG{HUP}\n"; }
$SIG{HUP} = \&named; kill HUP => $$;
{ local $SIG{USR1} = sub { print "local got $_[0]\n" }; kill USR1 => $$; }
kill USR1 => $$;
$SIG{USR1} = "IGNORE"; kill USR1 => $$; print "ignored ok\n";
my $got = 0; $SIG{USR1} = sub { $got++ }; kill USR1 => $$; kill USR1 => $$; print "count=$got\n";
my $r = eval { local $SIG{USR1} = sub { die "boom\n" }; kill USR1 => $$; 1 }; print "eval: ", ($r ? "no die\n" : "died $@");
my $d = delete $SIG{HUP}; print "deleted ", ref($d), " now ", (defined $SIG{HUP} ? "def" : "undef"), "\n";
@SIG{qw(HUP WINCH)} = (sub { print "slice $_[0]\n" }) x 2; kill HUP => $$; kill WINCH => $$;
$SIG{TERM} = "DEFAULT"; print "term=$SIG{TERM}\n";
$SIG{USR1} = "DEFAULT"; kill USR1 => $$; sleep 1; print "NOT REACHED\n";
PERL

# 3-4: sleep and alarm.
same_as_perl('alarm-sleep', <<'PERL', 'a handler that returns ends sleep early; one that dies leaves an eval');
$| = 1; $SIG{ALRM} = sub { print "alrm $_[0]\n" }; alarm 1; my $s = sleep 5;
print "sleep ", ($s <= 2 ? "early" : "late"), "\n";
my $r = eval { local $SIG{ALRM} = sub { die "timeout\n" }; alarm 1; sleep 5; 1 };
print "eval: ", ($r ? "no\n" : $@);
PERL

same_as_perl('alarm-default', <<'PERL', 'alarm with no handler kills the process by SIGALRM');
$| = 1; print "before\n"; alarm 1; sleep 3; print "NOT REACHED\n";
PERL

# 5-10: EXTERNAL signals, sent after "ready".
my $loop = '$| = 1; print "ready\n"; while (1) { select(undef, undef, undef, 0.05) }';
same_as_perl('ext-int', $loop, 'INT with no handler: death by the signal, quietly', sig => 'INT');
same_as_perl('ext-term', $loop, 'TERM with no handler: death by the signal (it used to EXIT 0)', sig => 'TERM');

my $handlers = '$| = 1; $SIG{INT} = sub { print "caught INT\n"; exit 7 }; $SIG{TERM} = sub { print "caught TERM\n"; exit 8 };'
             . ' $SIG{USR1} = sub { print "caught USR1\n"; exit 9 }; $SIG{HUP} = sub { print "caught HUP\n"; exit 3 };'
             . ' print "ready\n"; while (1) { select(undef, undef, undef, 0.05) }';
same_as_perl('ext-h-int',  $handlers, 'an external INT runs $SIG{INT}',  sig => 'INT');
same_as_perl('ext-h-term', $handlers, 'an external TERM runs $SIG{TERM}', sig => 'TERM');
same_as_perl('ext-h-usr1', $handlers, 'an external USR1 runs $SIG{USR1} (the relay thread)', sig => 'USR1');
same_as_perl('ext-h-sleep',
             '$| = 1; $SIG{HUP} = sub { print "caught HUP\n"; exit 3 }; print "ready\n"; while (1) { sleep 1 }',
             'an external HUP interrupts a sleep and runs $SIG{HUP}', sig => 'HUP');

# 11-12: SIGPIPE.
same_as_perl('pipe-default', 'print "line $_\n" for 1 .. 200000; print STDERR "reached the end\n";',
             'print into a closed pipe kills the producer by SIGPIPE (`| head -1`)', head => 1);
same_as_perl('pipe-ignore', <<'PERL', q{with $SIG{PIPE} = "IGNORE" print returns false and $! is EPIPE}, head => 1);
$| = 1; $SIG{PIPE} = "IGNORE"; my $n = 0;
for (1 .. 200000) { if (!print "line $_\n") { $n++; last } }
exit($n && $!{EPIPE} ? 42 : 1);
PERL

# 13-14: children.
# qx and a command pipe open are left out on purpose: perl lets a reaping
# handler take THEIR child's status ($? = -1); PCL cannot (SBCL reaps a
# run-program child before any Perl handler runs) -- docs/not-supported.md
# "%SIG".
same_as_perl('chld', <<'PERL', "a reaping CHLD handler runs and keeps system()'s status, as in perl");
$| = 1; my $n = 0;
$SIG{CHLD} = sub { while ((my $k = waitpid(-1, 1)) > 0) { $n++ } };
my $rc = system("sh", "-c", "exit 3"); print "system=", $rc >> 8, "\n";
my $pid = fork; if (!$pid) { exit 9 } sleep 1; print "reaped by handler: ", ($n > 0 ? "yes" : "no"), "\n";
PERL

same_as_perl('fork-relay', <<'PERL', 'fork keeps a relayed handler in both processes; a system() child does not inherit the block');
$| = 1; my $got = 0;
$SIG{USR1} = sub { $got++ };
my $rc = system("perl", "-e", 'kill USR1 => $$; sleep 1; print "child SURVIVED\n"');
print "system child: ", ($rc & 127 ? "signal " . ($rc & 127) : "exit " . ($rc >> 8)), "\n";
my $pid = fork; if (!$pid) { kill USR1 => $$; print "child got=$got\n"; exit 0 }
waitpid($pid, 0); kill USR1 => $$; print "parent got=$got\n";
my $pp = $$; my $k = fork; if (!$k) { kill USR1 => $pp; exit 0 } waitpid($k, 0); sleep 2;
print "after external: $got\n";
PERL

# 15: the runtime-owned signal, PCL's own answer (docs/not-supported.md "%SIG").
{
    my (undef, $pcl) = prepare('usr2', '$SIG{USR2} = sub { print "x\n" }; $SIG{USR2} = sub { 1 }; print "ok\n";');
    my ($o, $e, $s) = run_one($pcl);
    my @lines = grep { /\$SIG\{USR2\}/ } split /\n/, $e;
    ok($o eq "ok\n" && $s eq 'exit 0' && @lines == 1 && $lines[0] =~ /^PCL: %SIG: .*SBCL runtime/,
       'a USR2 handler is announced ONCE and not installed (runtime-owned)')
        or diag("stdout=[$o] status=$s stderr=[$e]");
}

# 16: ./runpcl stands in for perl: a signal death is re-raised, not exit 128+N.
{
    my $src = "$workdir/runpcl-term.pl";
    open(my $s, '>', $src) or die; print {$s} 'kill "TERM", $$; sleep 2; print "NOT REACHED\n";'; close $s;
    my ($o, undef, $st) = run_one(["$project_root/runpcl", $src]);
    is("$st|$o", 'signal 15|', './runpcl reports a signal death as one (PCLSbcl::exit_like)');
}
