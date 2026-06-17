# Copyright (c) 2025-2026
# This is free software; you can redistribute it and/or modify it
# under the same terms as the Perl 5 programming language system itself.

package Errno;
use strict;
use Exporter q{import};

# Constants generated from the real Errno module (perl -MErrno) via
# tools/shim-gaps.pl. The real Errno.pm is pure-perl but installs its
# constants through a dynamic symbol-table loop PCL does not execute, so we
# mirror them as plain use constant. Regenerate if the platform errno set
# changes: perl -MErrno -e (...) > lib/Errno.pm

our @EXPORT_OK = qw(
    E2BIG EACCES EADDRINUSE EADDRNOTAVAIL EADV EAFNOSUPPORT EAGAIN
    EALREADY EBADE EBADF EBADFD EBADMSG EBADR EBADRQC EBADSLT EBFONT EBUSY
    ECANCELED ECHILD ECHRNG ECOMM ECONNABORTED ECONNREFUSED ECONNRESET
    EDEADLK EDEADLOCK EDESTADDRREQ EDOM EDOTDOT EDQUOT EEXIST EFAULT EFBIG
    EHOSTDOWN EHOSTUNREACH EHWPOISON EIDRM EILSEQ EINPROGRESS EINTR EINVAL
    EIO EISCONN EISDIR EISNAM EKEYEXPIRED EKEYREJECTED EKEYREVOKED EL2HLT
    EL2NSYNC EL3HLT EL3RST ELIBACC ELIBBAD ELIBEXEC ELIBMAX ELIBSCN ELNRNG
    ELOOP EMEDIUMTYPE EMFILE EMLINK EMSGSIZE EMULTIHOP ENAMETOOLONG
    ENAVAIL ENETDOWN ENETRESET ENETUNREACH ENFILE ENOANO ENOBUFS ENOCSI
    ENODATA ENODEV ENOENT ENOEXEC ENOKEY ENOLCK ENOLINK ENOMEDIUM ENOMEM
    ENOMSG ENONET ENOPKG ENOPROTOOPT ENOSPC ENOSR ENOSTR ENOSYS ENOTBLK
    ENOTCONN ENOTDIR ENOTEMPTY ENOTNAM ENOTRECOVERABLE ENOTSOCK ENOTSUP
    ENOTTY ENOTUNIQ ENXIO EOPNOTSUPP EOVERFLOW EOWNERDEAD EPERM
    EPFNOSUPPORT EPIPE EPROTO EPROTONOSUPPORT EPROTOTYPE ERANGE EREMCHG
    EREMOTE EREMOTEIO ERESTART ERFKILL EROFS ESHUTDOWN ESOCKTNOSUPPORT
    ESPIPE ESRCH ESRMNT ESTALE ESTRPIPE ETIME ETIMEDOUT ETOOMANYREFS
    ETXTBSY EUCLEAN EUNATCH EUSERS EWOULDBLOCK EXDEV EXFULL
);
our %EXPORT_TAGS = (POSIX => [@EXPORT_OK]);

use constant E2BIG           => 7;
use constant EACCES          => 13;
use constant EADDRINUSE      => 98;
use constant EADDRNOTAVAIL   => 99;
use constant EADV            => 68;
use constant EAFNOSUPPORT    => 97;
use constant EAGAIN          => 11;
use constant EALREADY        => 114;
use constant EBADE           => 52;
use constant EBADF           => 9;
use constant EBADFD          => 77;
use constant EBADMSG         => 74;
use constant EBADR           => 53;
use constant EBADRQC         => 56;
use constant EBADSLT         => 57;
use constant EBFONT          => 59;
use constant EBUSY           => 16;
use constant ECANCELED       => 125;
use constant ECHILD          => 10;
use constant ECHRNG          => 44;
use constant ECOMM           => 70;
use constant ECONNABORTED    => 103;
use constant ECONNREFUSED    => 111;
use constant ECONNRESET      => 104;
use constant EDEADLK         => 35;
use constant EDEADLOCK       => 35;
use constant EDESTADDRREQ    => 89;
use constant EDOM            => 33;
use constant EDOTDOT         => 73;
use constant EDQUOT          => 122;
use constant EEXIST          => 17;
use constant EFAULT          => 14;
use constant EFBIG           => 27;
use constant EHOSTDOWN       => 112;
use constant EHOSTUNREACH    => 113;
use constant EHWPOISON       => 133;
use constant EIDRM           => 43;
use constant EILSEQ          => 84;
use constant EINPROGRESS     => 115;
use constant EINTR           => 4;
use constant EINVAL          => 22;
use constant EIO             => 5;
use constant EISCONN         => 106;
use constant EISDIR          => 21;
use constant EISNAM          => 120;
use constant EKEYEXPIRED     => 127;
use constant EKEYREJECTED    => 129;
use constant EKEYREVOKED     => 128;
use constant EL2HLT          => 51;
use constant EL2NSYNC        => 45;
use constant EL3HLT          => 46;
use constant EL3RST          => 47;
use constant ELIBACC         => 79;
use constant ELIBBAD         => 80;
use constant ELIBEXEC        => 83;
use constant ELIBMAX         => 82;
use constant ELIBSCN         => 81;
use constant ELNRNG          => 48;
use constant ELOOP           => 40;
use constant EMEDIUMTYPE     => 124;
use constant EMFILE          => 24;
use constant EMLINK          => 31;
use constant EMSGSIZE        => 90;
use constant EMULTIHOP       => 72;
use constant ENAMETOOLONG    => 36;
use constant ENAVAIL         => 119;
use constant ENETDOWN        => 100;
use constant ENETRESET       => 102;
use constant ENETUNREACH     => 101;
use constant ENFILE          => 23;
use constant ENOANO          => 55;
use constant ENOBUFS         => 105;
use constant ENOCSI          => 50;
use constant ENODATA         => 61;
use constant ENODEV          => 19;
use constant ENOENT          => 2;
use constant ENOEXEC         => 8;
use constant ENOKEY          => 126;
use constant ENOLCK          => 37;
use constant ENOLINK         => 67;
use constant ENOMEDIUM       => 123;
use constant ENOMEM          => 12;
use constant ENOMSG          => 42;
use constant ENONET          => 64;
use constant ENOPKG          => 65;
use constant ENOPROTOOPT     => 92;
use constant ENOSPC          => 28;
use constant ENOSR           => 63;
use constant ENOSTR          => 60;
use constant ENOSYS          => 38;
use constant ENOTBLK         => 15;
use constant ENOTCONN        => 107;
use constant ENOTDIR         => 20;
use constant ENOTEMPTY       => 39;
use constant ENOTNAM         => 118;
use constant ENOTRECOVERABLE => 131;
use constant ENOTSOCK        => 88;
use constant ENOTSUP         => 95;
use constant ENOTTY          => 25;
use constant ENOTUNIQ        => 76;
use constant ENXIO           => 6;
use constant EOPNOTSUPP      => 95;
use constant EOVERFLOW       => 75;
use constant EOWNERDEAD      => 130;
use constant EPERM           => 1;
use constant EPFNOSUPPORT    => 96;
use constant EPIPE           => 32;
use constant EPROTO          => 71;
use constant EPROTONOSUPPORT => 93;
use constant EPROTOTYPE      => 91;
use constant ERANGE          => 34;
use constant EREMCHG         => 78;
use constant EREMOTE         => 66;
use constant EREMOTEIO       => 121;
use constant ERESTART        => 85;
use constant ERFKILL         => 132;
use constant EROFS           => 30;
use constant ESHUTDOWN       => 108;
use constant ESOCKTNOSUPPORT => 94;
use constant ESPIPE          => 29;
use constant ESRCH           => 3;
use constant ESRMNT          => 69;
use constant ESTALE          => 116;
use constant ESTRPIPE        => 86;
use constant ETIME           => 62;
use constant ETIMEDOUT       => 110;
use constant ETOOMANYREFS    => 109;
use constant ETXTBSY         => 26;
use constant EUCLEAN         => 117;
use constant EUNATCH         => 49;
use constant EUSERS          => 87;
use constant EWOULDBLOCK     => 11;
use constant EXDEV           => 18;
use constant EXFULL          => 54;

1;
