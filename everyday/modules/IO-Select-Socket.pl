# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# origin: s492 modwork / mw-18-IO-Select-Socket.pl
use strict; use warnings; use IO::Select; use Socket; use IO::Socket::INET;
socketpair(my $a, my $b, AF_UNIX, SOCK_STREAM, PF_UNSPEC) or die "socketpair: $!"; my $sel = IO::Select->new($b); syswrite($a, "ping\n"); my @ready = $sel->can_read(2); my $line = ""; sysread($ready[0], $line, 100) if @ready; chomp $line;
my $srv = IO::Socket::INET->new(Listen => 1, LocalAddr => "127.0.0.1", LocalPort => 0, Proto => "tcp", ReuseAddr => 1) or die "listen: $@"; my $port = $srv->sockport;
my $pid = fork // die; if (!$pid) { my $c = IO::Socket::INET->new(PeerAddr => "127.0.0.1", PeerPort => $port, Proto => "tcp") or exit 1; print $c "hello from child\n"; close $c; exit 0 }
my $conn = $srv->accept or die; my $got = <$conn>; chomp $got; close $conn; waitpid($pid, 0); print scalar(@ready), " $line ", ($port > 0 ? "port" : "noport"), " $got ", inet_ntoa(inet_aton("127.0.0.1")), " rc=", $? >> 8, "\n";
