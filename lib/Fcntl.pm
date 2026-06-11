# Copyright (c) 2025-2026
# This is free software; you can redistribute it and/or modify it
# under the same terms as the Perl 5 programming language system itself.

# PCL shim for the (XS) Fcntl module.
#
# The real Fcntl is an XS module whose constants come from the platform C
# headers; PCL cannot run XS, so we mirror the Linux values as plain
# `use constant` (right layer: module behaviour lives in lib/, not the
# parser/runtime).  The numeric values were generated from the real Fcntl
# (perl -MFcntl) on x86_64 Linux.  The S_IS*/S_IMODE/S_IFMT helpers are the
# real pure-Perl mode-bit predicates.  Platform macros that this build of
# Perl does not define (e.g. F_ALLOCSP, O_RAW) croak when *called*, exactly
# as stock Fcntl does, but can still be imported.

package Fcntl;
use strict;
use Exporter q{import};

our @EXPORT = qw(
    FD_CLOEXEC F_ALLOCSP F_ALLOCSP64 F_COMPAT F_DUP2FD F_DUPFD F_EXLCK 
    F_FREESP F_FREESP64 F_FSYNC F_FSYNC64 F_GETFD F_GETFL F_GETLK 
    F_GETLK64 F_GETOWN F_NODNY F_POSIX F_RDACC F_RDDNY F_RDLCK F_RWACC 
    F_RWDNY F_SETFD F_SETFL F_SETLK F_SETLK64 F_SETLKW F_SETLKW64 
    F_SETOWN F_SHARE F_SHLCK F_UNLCK F_UNSHARE F_WRACC F_WRDNY F_WRLCK 
    O_ACCMODE O_ALIAS O_APPEND O_ASYNC O_BINARY O_CREAT O_DEFER O_DIRECT 
    O_DIRECTORY O_DSYNC O_EXCL O_EXLOCK O_LARGEFILE O_NDELAY O_NOCTTY 
    O_NOFOLLOW O_NOINHERIT O_NONBLOCK O_RANDOM O_RAW O_RDONLY O_RDWR 
    O_RSRC O_RSYNC O_SEQUENTIAL O_SHLOCK O_SYNC O_TEMPORARY O_TEXT 
    O_TRUNC O_WRONLY 
);

our @EXPORT_OK = qw(
    DN_ACCESS DN_ATTRIB DN_CREATE DN_DELETE DN_MODIFY DN_MULTISHOT 
    DN_RENAME F_ADD_SEALS F_GETLEASE F_GETPIPE_SZ F_GET_SEALS F_GETSIG 
    F_NOTIFY F_SEAL_FUTURE_WRITE F_SEAL_GROW F_SEAL_SEAL F_SEAL_SHRINK 
    F_SEAL_WRITE F_SETLEASE F_SETPIPE_SZ F_SETSIG LOCK_MAND LOCK_READ 
    LOCK_RW LOCK_WRITE O_ALT_IO O_EVTONLY O_IGNORE_CTTY O_NOATIME 
    O_NOLINK O_NOSIGPIPE O_NOTRANS O_SYMLINK O_TMPFILE O_TTY_INIT FAPPEND 
    FASYNC FCREAT FDEFER FDSYNC FEXCL FLARGEFILE FNDELAY FNONBLOCK FRSYNC 
    FSYNC FTRUNC SEEK_SET SEEK_CUR SEEK_END S_ISUID S_ISGID S_ISVTX 
    S_ISTXT _S_IFMT S_IFREG S_IFDIR S_IFLNK S_IFSOCK S_IFBLK S_IFCHR 
    S_IFIFO S_IFWHT S_ENFMT S_IRUSR S_IWUSR S_IXUSR S_IRWXU S_IRGRP 
    S_IWGRP S_IXGRP S_IRWXG S_IROTH S_IWOTH S_IXOTH S_IRWXO S_IREAD 
    S_IWRITE S_IEXEC S_ISREG S_ISDIR S_ISLNK S_ISSOCK S_ISBLK S_ISCHR 
    S_ISFIFO S_ISWHT S_ISENFMT S_IFMT S_IMODE LOCK_SH LOCK_EX LOCK_NB 
    LOCK_UN 
);

our %EXPORT_TAGS = (
    Fcompat => [qw(
    FAPPEND FASYNC FCREAT FDEFER FDSYNC FEXCL FLARGEFILE FNDELAY 
    FNONBLOCK FRSYNC FSYNC FTRUNC 
    )],
    flock => [qw(
    LOCK_SH LOCK_EX LOCK_NB LOCK_UN 
    )],
    mode => [qw(
    S_ISUID S_ISGID S_ISVTX S_ISTXT _S_IFMT S_IFREG S_IFDIR S_IFLNK 
    S_IFSOCK S_IFBLK S_IFCHR S_IFIFO S_IFWHT S_ENFMT S_IRUSR S_IWUSR 
    S_IXUSR S_IRWXU S_IRGRP S_IWGRP S_IXGRP S_IRWXG S_IROTH S_IWOTH 
    S_IXOTH S_IRWXO S_IREAD S_IWRITE S_IEXEC S_ISREG S_ISDIR S_ISLNK 
    S_ISSOCK S_ISBLK S_ISCHR S_ISFIFO S_ISWHT S_ISENFMT S_IFMT S_IMODE 
    )],
    seek => [qw(
    SEEK_SET SEEK_CUR SEEK_END 
    )],
);

# --- constants --------------------------------------------------------
use constant FD_CLOEXEC       => 1;
use constant F_DUPFD          => 0;
use constant F_EXLCK          => 4;
use constant F_GETFD          => 1;
use constant F_GETFL          => 3;
use constant F_GETLK          => 5;
use constant F_GETLK64        => 5;
use constant F_GETOWN         => 9;
use constant F_RDLCK          => 0;
use constant F_SETFD          => 2;
use constant F_SETFL          => 4;
use constant F_SETLK          => 6;
use constant F_SETLK64        => 6;
use constant F_SETLKW         => 7;
use constant F_SETLKW64       => 7;
use constant F_SETOWN         => 8;
use constant F_SHLCK          => 8;
use constant F_UNLCK          => 2;
use constant F_WRLCK          => 1;
use constant O_ACCMODE        => 3;
use constant O_APPEND         => 1024;
use constant O_ASYNC          => 8192;
use constant O_BINARY         => 0;
use constant O_CREAT          => 64;
use constant O_DIRECT         => 16384;
use constant O_DIRECTORY      => 65536;
use constant O_DSYNC          => 4096;
use constant O_EXCL           => 128;
use constant O_LARGEFILE      => 0;
use constant O_NDELAY         => 2048;
use constant O_NOCTTY         => 256;
use constant O_NOFOLLOW       => 131072;
use constant O_NONBLOCK       => 2048;
use constant O_RDONLY         => 0;
use constant O_RDWR           => 2;
use constant O_RSYNC          => 1052672;
use constant O_SYNC           => 1052672;
use constant O_TEXT           => 0;
use constant O_TRUNC          => 512;
use constant O_WRONLY         => 1;
use constant DN_ACCESS        => 1;
use constant DN_ATTRIB        => 32;
use constant DN_CREATE        => 4;
use constant DN_DELETE        => 8;
use constant DN_MODIFY        => 2;
use constant DN_MULTISHOT     => 2147483648;
use constant DN_RENAME        => 16;
use constant F_ADD_SEALS      => 1033;
use constant F_GETLEASE       => 1025;
use constant F_GETPIPE_SZ     => 1032;
use constant F_GET_SEALS      => 1034;
use constant F_GETSIG         => 11;
use constant F_NOTIFY         => 1026;
use constant F_SEAL_FUTURE_WRITE => 16;
use constant F_SEAL_GROW      => 4;
use constant F_SEAL_SEAL      => 1;
use constant F_SEAL_SHRINK    => 2;
use constant F_SEAL_WRITE     => 8;
use constant F_SETLEASE       => 1024;
use constant F_SETPIPE_SZ     => 1031;
use constant F_SETSIG         => 10;
use constant LOCK_MAND        => 32;
use constant LOCK_READ        => 64;
use constant LOCK_RW          => 192;
use constant LOCK_WRITE       => 128;
use constant O_NOATIME        => 262144;
use constant O_TMPFILE        => 4259840;
use constant FAPPEND          => 1024;
use constant FASYNC           => 8192;
use constant FNDELAY          => 2048;
use constant FNONBLOCK        => 2048;
use constant SEEK_SET         => 0;
use constant SEEK_CUR         => 1;
use constant SEEK_END         => 2;
use constant S_ISUID          => 2048;
use constant S_ISGID          => 1024;
use constant S_ISVTX          => 512;
use constant _S_IFMT          => 61440;
use constant S_IFREG          => 32768;
use constant S_IFDIR          => 16384;
use constant S_IFLNK          => 40960;
use constant S_IFSOCK         => 49152;
use constant S_IFBLK          => 24576;
use constant S_IFCHR          => 8192;
use constant S_IFIFO          => 4096;
use constant S_IRUSR          => 256;
use constant S_IWUSR          => 128;
use constant S_IXUSR          => 64;
use constant S_IRWXU          => 448;
use constant S_IRGRP          => 32;
use constant S_IWGRP          => 16;
use constant S_IXGRP          => 8;
use constant S_IRWXG          => 56;
use constant S_IROTH          => 4;
use constant S_IWOTH          => 2;
use constant S_IXOTH          => 1;
use constant S_IRWXO          => 7;
use constant S_IREAD          => 256;
use constant S_IWRITE         => 128;
use constant S_IEXEC          => 64;
use constant LOCK_SH          => 1;
use constant LOCK_EX          => 2;
use constant LOCK_NB          => 4;
use constant LOCK_UN          => 8;

# --- mode-bit helpers (real pure-Perl bodies) -------------------------
sub S_IMODE { $_[0] & 07777 }
sub S_IFMT  { @_ ? ($_[0] & 0170000) : 0170000 }
sub S_ISREG  { ( $_[0] & 0170000 ) == 0100000 }
sub S_ISDIR  { ( $_[0] & 0170000 ) == 0040000 }
sub S_ISLNK  { ( $_[0] & 0170000 ) == 0120000 }
sub S_ISSOCK { ( $_[0] & 0170000 ) == 0140000 }
sub S_ISBLK  { ( $_[0] & 0170000 ) == 0060000 }
sub S_ISCHR  { ( $_[0] & 0170000 ) == 0020000 }
sub S_ISFIFO { ( $_[0] & 0170000 ) == 0010000 }
sub S_ISWHT  { 0 }
sub S_ISENFMT { 0 }

# --- platform macros not defined by this build (croak when used) ------
sub _unimpl { my $n = shift; die "Your vendor has not defined Fcntl macro $n\n"; }
sub F_ALLOCSP { _unimpl('F_ALLOCSP') }
sub F_ALLOCSP64 { _unimpl('F_ALLOCSP64') }
sub F_COMPAT { _unimpl('F_COMPAT') }
sub F_DUP2FD { _unimpl('F_DUP2FD') }
sub F_FREESP { _unimpl('F_FREESP') }
sub F_FREESP64 { _unimpl('F_FREESP64') }
sub F_FSYNC { _unimpl('F_FSYNC') }
sub F_FSYNC64 { _unimpl('F_FSYNC64') }
sub F_NODNY { _unimpl('F_NODNY') }
sub F_POSIX { _unimpl('F_POSIX') }
sub F_RDACC { _unimpl('F_RDACC') }
sub F_RDDNY { _unimpl('F_RDDNY') }
sub F_RWACC { _unimpl('F_RWACC') }
sub F_RWDNY { _unimpl('F_RWDNY') }
sub F_SHARE { _unimpl('F_SHARE') }
sub F_UNSHARE { _unimpl('F_UNSHARE') }
sub F_WRACC { _unimpl('F_WRACC') }
sub F_WRDNY { _unimpl('F_WRDNY') }
sub O_ALIAS { _unimpl('O_ALIAS') }
sub O_DEFER { _unimpl('O_DEFER') }
sub O_EXLOCK { _unimpl('O_EXLOCK') }
sub O_NOINHERIT { _unimpl('O_NOINHERIT') }
sub O_RANDOM { _unimpl('O_RANDOM') }
sub O_RAW { _unimpl('O_RAW') }
sub O_RSRC { _unimpl('O_RSRC') }
sub O_SEQUENTIAL { _unimpl('O_SEQUENTIAL') }
sub O_SHLOCK { _unimpl('O_SHLOCK') }
sub O_TEMPORARY { _unimpl('O_TEMPORARY') }
sub O_ALT_IO { _unimpl('O_ALT_IO') }
sub O_EVTONLY { _unimpl('O_EVTONLY') }
sub O_IGNORE_CTTY { _unimpl('O_IGNORE_CTTY') }
sub O_NOLINK { _unimpl('O_NOLINK') }
sub O_NOSIGPIPE { _unimpl('O_NOSIGPIPE') }
sub O_NOTRANS { _unimpl('O_NOTRANS') }
sub O_SYMLINK { _unimpl('O_SYMLINK') }
sub O_TTY_INIT { _unimpl('O_TTY_INIT') }
sub FCREAT { _unimpl('FCREAT') }
sub FDEFER { _unimpl('FDEFER') }
sub FDSYNC { _unimpl('FDSYNC') }
sub FEXCL { _unimpl('FEXCL') }
sub FLARGEFILE { _unimpl('FLARGEFILE') }
sub FRSYNC { _unimpl('FRSYNC') }
sub FSYNC { _unimpl('FSYNC') }
sub FTRUNC { _unimpl('FTRUNC') }
sub S_ISTXT { _unimpl('S_ISTXT') }
sub S_IFWHT { _unimpl('S_IFWHT') }
sub S_ENFMT { _unimpl('S_ENFMT') }

1;
