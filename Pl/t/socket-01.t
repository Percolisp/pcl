#!/usr/bin/env perl
# Regression tests for socket support (AF_INET / AF_UNIX, SOCK_STREAM / SOCK_DGRAM).
#
# The address-packing helpers (inet_aton/inet_ntoa/sockaddr_in/…) are pure Perl
# in lib/Socket.pm; the core builtins (socket/bind/connect/listen/accept/send/
# recv/shutdown/getsockname/getpeername/getprotobyname/setsockopt) are runtime
# functions backed by sb-bsd-sockets.  See docs/socket-impl-plan.md.
#
# Every case is checked DIFFERENTIALLY against real perl (which uses its own XS
# Socket).  The TCP cases use a single-process ephemeral-port loopback (server
# bind+listen, client connect, accept) so they need no fork and no fixed port —
# deterministic and safe under `prove -j8`.

use v5.30;
use strict;
use warnings;
use Test::More;
use File::Temp qw(tempfile);
use FindBin qw($RealBin);

my $project_root = "$RealBin/../..";
my $pl2cl        = "$project_root/pl2cl";
my $runtime      = "$project_root/cl/pcl-runtime.lisp";

plan skip_all => "pl2cl not found" unless -x $pl2cl;
plan skip_all => "sbcl not found"  unless `which sbcl 2>/dev/null`;

sub run_cl {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;
    my $cl_code = `$pl2cl $pl_file 2>&1`;
    my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cl_fh $cl_code;
    close $cl_fh;
    my $output = `sbcl --noinform --non-interactive --load $runtime --load $cl_file 2>&1`;
    $output =~ s/^;.*\n//gm;
    $output =~ s/^caught .*\n//gm;
    $output =~ s/^compilation unit.*\n//gm;
    $output =~ s/^\s*Undefined.*\n//gm;
    $output =~ s/^PCL Runtime loaded\n//gm;
    $output =~ s/^WARNING:.*\n//gm;
    $output =~ s/^\s*\n//gm;
    return $output;
}

# Differential: run the snippet through real perl and through PCL, compare.
sub test_sock {
    my ($name, $code) = @_;
    my ($fh, $file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;
    my $perl_out = `perl $file 2>&1`;
    my $cl_out   = run_cl($code);
    is($cl_out, $perl_out, $name) or diag("Perl: [$perl_out]\nCL:   [$cl_out]");
}

# --- Socket.pm address helpers (pure Perl, pack/unpack) ----------------------
test_sock('inet_aton/inet_ntoa round-trip and sockaddr_in pack/unpack',
    'use Socket;
     my $a = inet_aton("127.0.0.1");
     print inet_ntoa($a), "\n";
     my $sa = sockaddr_in(8080, $a);
     my ($port, $addr) = sockaddr_in($sa);
     print "$port ", inet_ntoa($addr), "\n";
     print AF_INET, " ", SOCK_STREAM, " ", SO_REUSEADDR, "\n";
     print inet_ntoa(INADDR_LOOPBACK), "\n";');

# --- full TCP loopback in one process ---------------------------------------
test_sock('TCP loopback: socket/bind/listen/connect/accept + bidirectional I/O',
    'use Socket;
     socket(my $srv, PF_INET, SOCK_STREAM, getprotobyname("tcp")) or die "socket: $!";
     setsockopt($srv, SOL_SOCKET, SO_REUSEADDR, 1);
     bind($srv, sockaddr_in(0, INADDR_LOOPBACK)) or die "bind: $!";
     listen($srv, 5) or die "listen: $!";
     my ($port, $host) = sockaddr_in(getsockname($srv));
     print "port_positive ", ($port > 0 ? 1 : 0), "\n";
     print "host ", inet_ntoa($host), "\n";
     socket(my $cli, PF_INET, SOCK_STREAM, getprotobyname("tcp")) or die "socket2: $!";
     connect($cli, sockaddr_in($port, INADDR_LOOPBACK)) or die "connect: $!";
     accept(my $acc, $srv) or die "accept: $!";
     my ($cport, $caddr) = sockaddr_in(getpeername($acc));
     print "peer ", inet_ntoa($caddr), "\n";
     syswrite($cli, "ping\n");
     my $l1 = <$acc>;
     print "server_got $l1";
     syswrite($acc, "pong\n");
     my $l2 = <$cli>;
     print "client_got $l2";
     close $cli; close $acc; close $srv;
     print "done\n";');

# --- send/recv over a connected TCP socket ----------------------------------
test_sock('send/recv truncates to the bytes actually received',
    'use Socket;
     socket(my $srv, PF_INET, SOCK_STREAM, getprotobyname("tcp")) or die;
     setsockopt($srv, SOL_SOCKET, SO_REUSEADDR, 1);
     bind($srv, sockaddr_in(0, INADDR_LOOPBACK)) or die;
     listen($srv, 5) or die;
     my ($port) = sockaddr_in(getsockname($srv));
     socket(my $cli, PF_INET, SOCK_STREAM, getprotobyname("tcp")) or die;
     connect($cli, sockaddr_in($port, INADDR_LOOPBACK)) or die;
     accept(my $acc, $srv) or die;
     send($cli, "data42\n", 0);
     my $buf;
     recv($acc, $buf, 16, 0);
     print "recv_got $buf";
     print "len ", length($buf), "\n";
     close $cli; close $acc; close $srv;
     print "ok\n";');

# --- failure modes set $! and return false ----------------------------------
test_sock('connect to a closed port fails and sets $!',
    'use Socket;
     socket(my $srv, PF_INET, SOCK_STREAM, getprotobyname("tcp")) or die;
     setsockopt($srv, SOL_SOCKET, SO_REUSEADDR, 1);
     bind($srv, sockaddr_in(0, INADDR_LOOPBACK)) or die;
     listen($srv, 1) or die;
     my ($port) = sockaddr_in(getsockname($srv));
     close $srv;
     socket(my $cli, PF_INET, SOCK_STREAM, getprotobyname("tcp")) or die;
     my $ok = connect($cli, sockaddr_in($port, INADDR_LOOPBACK));
     print "connect_failed ", ($ok ? 0 : 1), "\n";');

done_testing();
