# Copyright (c) 2025-2026
# This is free software; you can redistribute it and/or modify it
# under the same terms as the Perl 5 programming language system itself.

# PCL shim for the (XS) Socket module.
#
# Per CLAUDE.md §9a, address handling is *module behaviour*, so it lives here:
# the constants mirror the platform C values (Linux x86_64), and the address
# pack/unpack helpers (inet_aton/inet_ntoa/sockaddr_in/…) are pure Perl built on
# pack/unpack — PCL transpiles them like user code.  The runtime only implements
# the genuine core builtins (socket/bind/connect/accept/…), which take these
# already-packed sockaddr byte-strings.  See docs/socket-impl-plan.md.

package Socket;
use strict;
use Exporter q{import};

our @EXPORT = qw(
    PF_INET PF_INET6 PF_UNIX AF_INET AF_INET6 AF_UNIX AF_UNSPEC
    SOCK_STREAM SOCK_DGRAM SOCK_RAW
    SOL_SOCKET SO_REUSEADDR SO_KEEPALIVE SO_BROADCAST SO_ERROR
    SO_RCVBUF SO_SNDBUF SO_TYPE SO_LINGER
    SOMAXCONN SHUT_RD SHUT_WR SHUT_RDWR
    INADDR_ANY INADDR_BROADCAST INADDR_LOOPBACK INADDR_NONE
    IPPROTO_TCP IPPROTO_UDP IPPROTO_IP TCP_NODELAY
    inet_aton inet_ntoa
    sockaddr_in pack_sockaddr_in unpack_sockaddr_in
    sockaddr_un pack_sockaddr_un unpack_sockaddr_un
);

our @EXPORT_OK = @EXPORT;

# --- Address / protocol families --------------------------------------------
use constant PF_INET   => 2;
use constant PF_INET6  => 10;
use constant PF_UNIX   => 1;
use constant AF_INET   => 2;
use constant AF_INET6  => 10;
use constant AF_UNIX   => 1;
use constant AF_UNSPEC => 0;

# --- Socket types -----------------------------------------------------------
use constant SOCK_STREAM => 1;
use constant SOCK_DGRAM  => 2;
use constant SOCK_RAW    => 3;

# --- Option levels / names (SOL_SOCKET) -------------------------------------
use constant SOL_SOCKET   => 1;
use constant SO_REUSEADDR => 2;
use constant SO_TYPE      => 3;
use constant SO_ERROR     => 4;
use constant SO_BROADCAST => 6;
use constant SO_SNDBUF    => 7;
use constant SO_RCVBUF    => 8;
use constant SO_KEEPALIVE => 9;
use constant SO_LINGER    => 13;

# --- Misc -------------------------------------------------------------------
use constant SOMAXCONN => 4096;
use constant SHUT_RD   => 0;
use constant SHUT_WR   => 1;
use constant SHUT_RDWR => 2;

use constant IPPROTO_IP  => 0;
use constant IPPROTO_TCP => 6;
use constant IPPROTO_UDP => 17;
use constant TCP_NODELAY => 1;

# Well-known IPv4 addresses, as 4-byte packed strings.
sub INADDR_ANY       { pack 'N', 0x00000000 }
sub INADDR_LOOPBACK  { pack 'N', 0x7F000001 }
sub INADDR_BROADCAST { pack 'N', 0xFFFFFFFF }
sub INADDR_NONE      { pack 'N', 0xFFFFFFFF }

# --- Address conversion -----------------------------------------------------

# inet_aton(STRING) -> 4-byte packed address (or undef).  Handles dotted-quad
# ("127.0.0.1") and the literal "localhost"; no DNS resolution (XS-less).
sub inet_aton {
    my ($host) = @_;
    return undef unless defined $host;
    $host = '127.0.0.1' if $host eq 'localhost';
    if ($host =~ /^(\d{1,3})\.(\d{1,3})\.(\d{1,3})\.(\d{1,3})$/) {
        return undef if $1 > 255 || $2 > 255 || $3 > 255 || $4 > 255;
        return pack 'C4', $1, $2, $3, $4;
    }
    return undef;
}

# inet_ntoa(PACKED) -> "a.b.c.d"
sub inet_ntoa {
    my ($addr) = @_;
    return undef unless defined $addr && length($addr) == 4;
    return join '.', unpack 'C4', $addr;
}

# --- sockaddr_in ------------------------------------------------------------
# struct sockaddr_in: family (native short, S) . port (network short, n)
#                     . addr (4 bytes, a4) . 8 bytes pad (x8)  = 16 bytes.

sub pack_sockaddr_in {
    my ($port, $addr) = @_;
    $addr = "\0\0\0\0" unless defined $addr && length($addr) == 4;
    return pack 'S n a4 x8', AF_INET, $port, $addr;
}

sub unpack_sockaddr_in {
    my ($sockaddr) = @_;
    my ($family, $port, $addr) = unpack 'S n a4', $sockaddr;
    return ($port, $addr);
}

# sockaddr_in(SOCKADDR) in list context unpacks; sockaddr_in(PORT, ADDR) packs.
sub sockaddr_in {
    if (@_ == 1) {
        return unpack_sockaddr_in($_[0]);
    }
    return pack_sockaddr_in($_[0], $_[1]);
}

# --- sockaddr_un (AF_UNIX) --------------------------------------------------
# struct sockaddr_un: family (native short, S) . path (NUL-padded, up to ~108).

sub pack_sockaddr_un {
    my ($path) = @_;
    $path = '' unless defined $path;
    return pack 'S a108', AF_UNIX, $path;
}

sub unpack_sockaddr_un {
    my ($sockaddr) = @_;
    my ($family, $path) = unpack 'S Z108', $sockaddr;
    return $path;
}

sub sockaddr_un {
    if (@_ == 1) {
        return unpack_sockaddr_un($_[0]);
    }
    return pack_sockaddr_un($_[0]);
}

1;
