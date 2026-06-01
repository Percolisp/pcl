# Copyright (c) 2025-2026
# This is free software; you can redistribute it and/or modify it
# under the same terms as the Perl 5 programming language system itself.

package Errno;
use strict;
use Exporter 'import';

our @EXPORT_OK = qw(
    EPERM ENOENT EINTR EIO EAGAIN ENOMEM EACCES EBUSY EEXIST ENODEV
    ENOTDIR EISDIR EINVAL ENFILE EMFILE EFBIG ENOSPC EROFS EPIPE
    EDOM ERANGE ENAMETOOLONG ENOSYS ENOTEMPTY ELOOP EWOULDBLOCK
    ENOTSOCK ETIMEDOUT ECONNREFUSED EHOSTUNREACH EALREADY EINPROGRESS
);
our %EXPORT_TAGS = (POSIX => [@EXPORT_OK]);

use constant EPERM   => 1;
use constant ENOENT  => 2;
use constant EINTR   => 4;
use constant EIO     => 5;
use constant EAGAIN  => 11;
use constant ENOMEM  => 12;
use constant EACCES  => 13;
use constant EBUSY   => 16;
use constant EEXIST  => 17;
use constant ENODEV  => 19;
use constant ENOTDIR => 20;
use constant EISDIR  => 21;
use constant EINVAL  => 22;
use constant ENFILE  => 23;
use constant EMFILE  => 24;
use constant EFBIG   => 27;
use constant ENOSPC  => 28;
use constant EROFS   => 30;
use constant EPIPE   => 32;
use constant EDOM    => 33;
use constant ERANGE  => 34;
use constant ENAMETOOLONG => 36;
use constant ENOSYS  => 38;
use constant ENOTEMPTY => 39;
use constant ELOOP   => 40;
use constant EWOULDBLOCK => 11;
use constant ENOTSOCK => 88;
use constant ETIMEDOUT => 110;
use constant ECONNREFUSED => 111;
use constant EHOSTUNREACH => 113;
use constant EALREADY => 114;
use constant EINPROGRESS => 115;

1;
