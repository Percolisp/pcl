#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# gate-cost-01.t — the gate's own per-row cost machinery (s473u, #1544).
#
# The gate is this project's throughput constraint: a batch runs it six to nine
# times, and one run is ~2900 CPU-s.  The mechanisms that keep the per-row price
# down are the kind that stop working SILENTLY — the rows keep passing, they
# just cost hundreds of times more — so they are gated here rather than trusted.
#
# THE SAVED CORE.  Every sbcl command line comes from the ONE builder,
# tools/lib/PCLSbcl.pm via PCLCore::sbcl_prefix (task #344), which supplies the
# content-keyed core with the runtime already compiled in, plus the 512 MB
# control stack.  A file that spells `sbcl … --load cl/pcl-runtime.lisp` itself
# recompiles the whole runtime on every row: 2.89 CPU-s against 0.007 s from
# the core, measured s473u.  Fifteen files did, and they were 974 of the gate's
# 2933 CPU-s for 8 % of its rows — and they ran on the default 2 MB control
# stack, which is exactly the drift #344 exists to stop (#324 spent months
# measuring PCL on a 2 MB stack for the same reason).

use v5.30;
use strict;
use warnings;
use Test::More;
use File::Temp qw(tempdir);
use Cwd ();
use POSIX ();
use FindBin;
use lib "$FindBin::Bin";
use PCLCore;

my @drift;
for my $f (sort glob("$FindBin::Bin/*.t")) {
    open my $fh, '<', $f or next;
    my @l = <$fh>;
    close $fh;
    for my $i (0 .. $#l) {
        next if $l[$i] =~ /^\s*#/;               # a comment may quote the shape
        # ANY `sbcl … --load` that is not built from the shared prefix.  The
        # first version of this row looked for the literal `pcl-runtime.lisp`
        # ON the sbcl line, and TEN files spelled the same source load through
        # a variable (`my $runtime = 'cl/pcl-runtime.lisp'`) — they sailed
        # past it and were the entire top of the table afterwards.  The rule is
        # therefore about WHERE THE COMMAND COMES FROM, not how the path is
        # spelled: a gate file names `sbcl` only with @sbcl_rt.
        next unless $l[$i] =~ /(?:^|[^-\w])sbcl\b[^\n]*--load\b/;
        next unless $l[$i] =~ /`|qx[{(]|system\s*\(/;   # a COMMAND, not a rule about one
        next if $l[$i] =~ /\@sbcl(?:_rt)?\b|\@prefix\b|sbcl_prefix/;
        push @drift, (split m{/}, $f)[-1] . ':' . ($i + 1);
    }
}
is_deeply(\@drift, [],
    'no gate file spells its own sbcl runtime-source load')
    or diag("these recompile the runtime on every row (2.89 CPU-s each) and run\n"
          . "on the default 2 MB control stack.  Use the shared builder:\n"
          . "  my \@sbcl_rt = PCLCore::sbcl_prefix(\"\$FindBin::Bin/../../cl/pcl-runtime.lisp\");\n"
          . "  my \$out = `sbcl \@sbcl_rt --load \$file 2>&1`;\n  "
          . join("\n  ", @drift));

# The row above can only fail; this one says it was actually looking at files.
my @all_t = glob("$FindBin::Bin/*.t");
ok(scalar(@all_t) > 100, 'the scan saw the gate directory (not an empty glob)');

# ---------------------------------------------------------------------------
# THE TRANSPILE SERVER (#1545)
# ---------------------------------------------------------------------------
# A `pl2cl` spawn costs 0.13 CPU-s of perl + PPI + every Pl::* module before it
# reads a byte of the snippet, and the gate makes ~3200 of them.  PCLCore names
# a socket in PCL_XSERVER; the first pl2cl to find it starts ONE already-loaded
# server there and every later call is a round trip.
#
# The bar is that the served answer IS the spawned answer — byte for byte on
# stdout, byte for byte on stderr, and the same exit status — and that it does
# not matter what the server transpiled before.  That last part is what the
# fork-per-request shape buys, and the two isolation rows below are how it is
# checked rather than asserted: the regex-gap announcement is announce-ONCE per
# process, so a server answering from one process would say it on the first
# request and go silent on the second.
#
# `PCL_XSERVER=` (empty) in a child's environment is the spawn path — the
# client block in pl2cl declines on an empty variable.  PCLCore has set the
# variable for this process, so the default here is the SERVED path.

my $root  = "$FindBin::Bin/../..";
my $pl2cl = "$root/pl2cl";

SKIP: {
    skip "pl2cl not executable", 27 unless -x $pl2cl;
    my $dir = tempdir(CLEANUP => 1);

    # PCLCore has named a socket for this test process — unless the run was
    # invoked with PCL_NO_SERVER=1, which is exactly how the two acceptance
    # gates are compared.  The MECHANISM must be testable either way, so when
    # there is nothing to inherit this file makes its own socket directory;
    # only the row below, which asserts PCLCore's WIRING, looks at which of the
    # two happened.  Without this the whole section would skip in the
    # server-off leg and the two gates' TAP could not be compared row for row.
    my $from_core = $ENV{PCL_XSERVER};
    if (!defined $from_core || !length $from_core) {
        mkdir "$dir/x" or die "mkdir $dir/x: $!";
        $ENV{PCL_XSERVER}       = "$dir/x/s";
        $ENV{PCL_XSERVER_OWNER} = $$;
    }

    my $write_pl = sub {
        my ($name, $src) = @_;
        my $p = "$dir/$name";
        open my $fh, '>', $p or die "cannot write $p: $!";
        print $fh $src;
        close $fh;
        return $p;
    };

    # run pl2cl and return (stdout, stderr, exit code); served unless spawn=>1
    # (or env=>'VAR=value ' names a different environment for one call)
    my $run = sub {
        my (%o) = @_;
        my ($out, $err) = ("$dir/o.$$", "$dir/e.$$");
        my $pre = $o{spawn} ? 'PCL_XSERVER= ' : ($o{env} // '');
        system("$pre" . ($o{cmd} // $pl2cl) . " $o{args} > $out 2> $err");
        my $rc = $?;
        my $slurp = sub {
            open my $fh, '<:raw', $_[0] or return '';
            local $/; my $s = <$fh>; close $fh; return defined $s ? $s : '';
        };
        my @r = ($slurp->($out), $slurp->($err), $rc >> 8, $rc);
        unlink $out, $err;
        return @r;
    };

    my $plain = $write_pl->('plain.pl', <<'PL');
my $x = 1;
my @a = (1, 2, 3);
my %h = (k => $x);
for my $i (@a) { $x = $x + $i }
print "$x\n";
PL

    # a regex control verb: a `print STDERR` from a SUCCESSFUL transpile, and
    # announce-once-per-process, so it is the stderr row AND the state probe.
    my $verb = $write_pl->('verb.pl', <<'PL');
my $s = "abc";
print "yes\n" if $s =~ /a(*FAIL)/;
print "done\n";
PL

    # a (&@) prototype in another package: what a server that kept
    # Pl::Environment between requests would leak into the next parse.
    my $proto = $write_pl->('proto.pl', <<'PL');
package Foo;
sub x (&@) { my ($c, @r) = @_; return $c->(), @r }
package main;
print "p\n";
PL

    my $bare = $write_pl->('bare.pl', <<'PL');
sub x { return 7 }
my $n = x;
print "$n\n";
PL

    my ($so, $se, $sc) = $run->(args => $plain);
    my ($po, $pe, $pc) = $run->(args => $plain, spawn => 1);
    is($so, $po, 'served transpile is byte-identical to the spawned one');
    is($se, $pe, '... and so is its stderr');
    is($sc, $pc, '... and so is its exit status');
    ok(length($so) > 100, 'the comparison was not two empty strings');

    # Without this row every comparison here could pass with the server never
    # used at all.
    ok($ENV{PCL_NO_SERVER} ? !defined $from_core
                           : (defined $from_core && length $from_core),
       'PCLCore names a transpile socket unless PCL_NO_SERVER=1 turns it off');
    my @sock = glob("$ENV{PCL_XSERVER}-*.sock");
    ok(scalar(@sock) && -S $sock[0],
       'a served transpile left a listening server behind')
        or diag("PCL_XSERVER=" . ($ENV{PCL_XSERVER} // '(unset)'));

    my ($vo, $ve) = $run->(args => $verb);
    my ($vpo, $vpe) = $run->(args => $verb, spawn => 1);
    like($vpe, qr/^PCL: regex control verb/m, 'the spawn path announces the regex gap');
    is($ve, $vpe, 'the served path announces it identically (stderr survives)');
    is($vo, $vpo, '... with the same CL');
    my ($v2o, $v2e) = $run->(args => $verb);
    is($v2e, $vpe, 'a SECOND served request announces it again (no state carried over)');
    is($v2o, $vo, '... and emits the same CL as the first');

    $run->(args => $proto);
    my ($bo) = $run->(args => $bare);
    my ($bpo) = $run->(args => $bare, spawn => 1);
    is($bo, $bpo, "an earlier request's (&\@) prototype does not reach the next parse");

    my (undef, $ue, $uc) = $run->(args => "--no-such-option $plain");
    my (undef, $upe, $upc) = $run->(args => "--no-such-option $plain", spawn => 1);
    isnt($upc, 0, 'an unknown option fails the spawn path');
    is($uc, $upc, 'the served path fails with the same exit status');
    is($ue, $upe, '... and the same stderr');

    # ---- WHAT HAPPENS WHEN THE SERVER IS NOT THERE -----------------------
    # The whole point of the fallback is that it can never make a run WRONG,
    # only slow — and that it cannot be slow SILENTLY, which is the drift #324
    # cost this project months of mis-measurement.  Three shapes:

    # (1) the server DIES mid-file.  The next client finds a stale socket
    # (nothing unlinks it), starts a fresh server and answers identically.
    # This is a RECOVERY, not a fallback, so it is deliberately quiet.
    my $sock = @sock ? $sock[0] : "$dir/no-socket-found";
    my @pids;
    if (@sock && open my $ps, '-|', 'ps', '-eo', 'pid=,args=') {
        while (my $l = <$ps>) {
            push @pids, $1 if $l =~ /^\s*(\d+)\s.*--xsock \Q$sock\E(?:\s|$)/;
        }
        close $ps;
    }
  SKIP: {
        skip "could not find the server process", 3 unless @pids;
        kill 'KILL', @pids;
        select(undef, undef, undef, 0.2);
        ok(-S $sock, 'a killed server leaves its socket file behind');
        my ($ko, $ke) = $run->(args => $plain);
        is($ko, $po, 'the next request starts a new server and answers identically');
        is($ke, '',  '... and a recovered server says nothing on stderr');
    }

    # (2) the server cannot be reached AT ALL.  A missing socket directory is
    # the shape that can neither start a server nor record that it failed, so
    # it must decline AT ONCE and LOUDLY -- the alternative measured before
    # this row existed was a 10 s start timeout on every one of ~3200 calls.
    my ($no, $ne, $nc) = $run->(args => $plain,
                                env => 'PCL_XSERVER=/pcl-no-such-dir-xyz/s ');
    is($no, $po, 'an unreachable socket directory still transpiles correctly');
    is($nc, $pc, '... with the spawn path\'s exit status');
    like($ne, qr/^pl2cl: PCL_XSERVER=\S+ names no existing directory/m,
         '... and says so on stderr rather than degrading in silence');

    # (3) the .down marker: once a start has failed, later calls in the same
    # test process decline immediately instead of paying the wait again.  The
    # answer is still the spawn path's, and the line is not repeated.
    open(my $dm, '>', "$sock.down") or die "cannot write $sock.down: $!";
    close $dm;
    my ($do_, $de) = $run->(args => $plain);
    unlink "$sock.down";
    is($do_, $po, 'a .down marker declines to the spawn path, same answer');
    is($de, '', '... without repeating the announcement');

    # ---- A SIGNAL DEATH MUST ARRIVE AS A SIGNAL DEATH ---------------------
    # The reply carries the child's RAW wait status and the client re-raises a
    # signal rather than exiting 128+n, so a caller's $? says the same thing on
    # both paths.  No transpile can be made to segfault on demand, so the row
    # drives the client against a STAND-IN server: the real one is started (to
    # learn the socket name, which is keyed by interpreter + script + PCL
    # environment), killed, and replaced by a listener that speaks the protocol
    # and answers SIGTERM.
    my $fake = "$dir/fake";
    mkdir $fake or die "mkdir $fake: $!";
    $run->(args => $plain, env => "PCL_XSERVER=$fake/s ");
    my ($fsock) = glob("$fake/s-*.sock");
  SKIP: {
        skip "no stand-in socket to take over", 2 unless defined $fsock;
        my @fpid;
        if (open my $ps, '-|', 'ps', '-eo', 'pid=,args=') {
            while (my $l = <$ps>) {
                push @fpid, $1 if $l =~ /^\s*(\d+)\s.*--xsock \Q$fsock\E(?:\s|$)/;
            }
            close $ps;
        }
        kill 'KILL', @fpid if @fpid;
        select(undef, undef, undef, 0.2);
        unlink $fsock;
        require Socket;
        my $kid = fork();
        die "fork: $!" unless defined $kid;
        if (!$kid) {
            alarm 30;                       # never outlive the row
            socket(my $srv, Socket::PF_UNIX(), Socket::SOCK_STREAM(), 0)
                or POSIX::_exit(1);
            bind($srv, Socket::pack_sockaddr_un($fsock)) or POSIX::_exit(1);
            listen($srv, 4) or POSIX::_exit(1);
            if (accept(my $c, $srv)) {
                binmode($c, ':raw');
                my $hdr = '';
                while (sysread($c, my $ch, 1)) { last if $ch eq "\n"; $hdr .= $ch }
                my $n = $hdr =~ /^PCLXS2 (\d+)$/ ? $1 : 0;
                while ($n > 0) {
                    my $got = sysread($c, my $b, $n) or last;
                    $n -= $got;
                }
                print {$c} "PCLXS2 15 0 0\n";   # raw wait status: killed by SIGTERM
                close $c;
            }
            POSIX::_exit(0);
        }
        select(undef, undef, undef, 0.3);
        # NOT $run: that goes through a shell (the redirections), and a shell
        # reports its child's signal death as exit 143 -- which is exactly the
        # substitution this row exists to catch, so it must exec directly.
        my $raw;
        my $c2 = fork();
        die "fork: $!" unless defined $c2;
        if (!$c2) {
            $ENV{PCL_XSERVER} = "$fake/s";
            open(STDOUT, '>', "$dir/sig.out");
            open(STDERR, '>', "$dir/sig.err");
            exec($pl2cl, $plain) or do { POSIX::_exit(127) };
        }
        waitpid($c2, 0);
        $raw = $?;
        waitpid($kid, 0);
        is($raw & 127, 15, 'a signal death arrives at the caller AS a signal');
        is($raw >> 8, 0, '... not as exit code 143');
    }

    # ---- A pl2cl FROM ANOTHER TREE MUST NOT ANSWER FROM THIS ONE ----------
    # PCL_XSERVER is inherited by everything a test file spawns, and some of
    # those are a pl2cl from somewhere else — an installed tree, a `git
    # archive` extraction of a base commit (that is how every guard in this
    # repo is inverse-verified).  If it reached this tree's server it would be
    # answering with THIS compiler's emission under that tree's name, which is
    # the quietest wrong answer available.  The socket name carries the
    # script's absolute path, so it gets its own server instead.
    #
    # (The INSTALLED tree never even reaches this question: tools/install-pcl
    # copies Pl/ but skips Pl/t, so PCLCore — the only place that sets
    # PCL_XSERVER, gated by the row below — does not exist there.)
    my $other = "$dir/other";
    mkdir $other or die "mkdir $other: $!";
    system("cp", $pl2cl, "$other/pl2cl") == 0 or die "cp pl2cl: $?";
    symlink("$root/$_", "$other/$_") or die "symlink $_: $!"
        for qw(Pl lib cl tools);
    my ($oo, $oe, $oc) = $run->(args => $plain, cmd => "$other/pl2cl");
    # The preamble names the tree it was compiled by (*pcl-pl2cl-path*, the
    # @INC pushes), so "identically" means: identical once that name is put
    # back — which is also the proof that the answer came from the OTHER tree.
    my $abs = Cwd::abs_path($root);
    (my $on = $oo) =~ s/\Q$other\E/$abs/g;
    is($on, $po, 'a pl2cl from another tree transpiles identically');
    my @sock2 = glob("$ENV{PCL_XSERVER}-*.sock");
    ok(scalar(@sock2) >= 2, 'it is answered by its OWN server, not this tree\'s')
        or diag("sockets: @sock2 (exit $oc)\nstderr: $oe");
}

# PCLCore is the ONLY place that turns the server on: everything else in the
# tree — pl2cl itself, the sweep, the suite runner, the installer — must be
# unaffected unless a gate test process is its ancestor.  A second setter would
# quietly extend the server to a population nothing here measures.
{
    my @setters;
    for my $f (glob("$root/Pl/*.pm"), glob("$root/Pl/*/*.pm"),
               glob("$root/tools/*.pl"), glob("$root/tools/lib/*.pm"),
               "$root/pl2cl", "$root/runpcl", "$root/pcl") {
        next unless -f $f;
        next if $f =~ m{/Pl/t/};          # PCLCore is THE setter
        open my $fh, '<', $f or next;
        while (my $l = <$fh>) {
            next if $l =~ /^\s*#/;
            push @setters, $f and last
                if $l =~ /\$ENV\{['"]?PCL_XSERVER['"]?\}\s*=/;
        }
        close $fh;
    }
    is_deeply(\@setters, [], 'nothing outside Pl/t switches the server on')
        or diag("these set PCL_XSERVER:\n  " . join("\n  ", @setters));
}

# The value-taking options the client understands must be exactly the ones
# pl2cl's own GetOptions declares with `=s`: the client has to know which argv
# element is an option VALUE rather than the source file, and it cannot ask
# GetOptions without loading it.  Every `=s` option is therefore either SERVED
# (the client skips its value) or DECLINED (the whole call falls through to a
# spawn) — a new one that is neither would be classified as the source file.
{
    open my $fh, '<', $pl2cl or die "cannot read $pl2cl: $!";
    my $src = do { local $/; <$fh> };
    close $fh;
    my ($opts) = $src =~ /\nsub _parse_options \{\nGetOptions\((.*?)\n\) or (?:die|do \{)/s;
    my @value_opts;
    if (defined $opts) {
        for my $l (split /\n/, $opts) {
            next unless $l =~ /^\s*['"]([\w|\-]+)=s['"]/;
            push @value_opts, split /\|/, $1;
        }
    }
    my %served  = map { $_ => 1 } qw(deps);
    my %declined = map { $_ => 1 } qw(output o eval-pkg xsock);
    my @unhandled = sort grep { !$served{$_} && !$declined{$_} } @value_opts;
    ok(scalar(@value_opts) >= 4, 'found pl2cl\'s value-taking options')
        or diag("parsed: @value_opts");
    is_deeply(\@unhandled, [],
        'every value-taking pl2cl option is either served or declined by the client')
        or diag("the client would read these options' VALUES as the source file:\n  "
              . join("\n  ", @unhandled));
}

done_testing();
