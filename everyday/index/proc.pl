# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# origin: s493 perlfunc index walk / pf-proc.pl
$| = 1; my $pid = fork(); die if !defined $pid; if (!$pid) { print "child\n"; exit 3 } waitpid($pid, 0); print "status=", $? >> 8, "\n"; pipe(my $r, my $w) or die; my $p2 = fork(); if (!$p2) { close $r; print $w "from child\n"; close $w; exit 0 } close $w; my $line = <$r>; print "got: $line"; waitpid($p2, 0); open(my $ph, "-|", "echo", "list form") or die; print scalar <$ph>; close $ph; open(my $out, "|-", "cat") or die; print $out "to cat\n"; close $out; print "close-status=$?\n"; my $rc = system("sh", "-c", "exit 2"); print "sys=", $rc >> 8, "\n"; my $o = `sh -c "echo err 1>&2; echo out" 2>/dev/null`; print $o; local $SIG{ALRM} = sub { print "alarm\n" }; alarm 1; my $sl = sleep 3; print "slept<3\n" if $sl < 3; kill 0, $$ and print "kill0\n"; local $SIG{USR1} = sub { print "usr1\n" }; kill "USR1", $$; sleep 0; print "wait=", wait(), "\n"; print exists $ENV{PATH} ? "env\n" : "noenv\n"; $ENV{S493} = "v"; print `echo \$S493`; my @t = times; print scalar(@t), "\n"; print "time-ok\n" if time - $^T < 100;
