# Copyright (c) 2025-2026
# This is free software; you can redistribute it and/or modify it
# under the same terms as the Perl 5 programming language system itself.

package Config;

# Fake Config.pm for PCL transpiler
# Provides subset of values needed by Perl's test suite
# Values are appropriate for 64-bit Linux with SBCL

use strict;
use warnings;
use Exporter 'import';

our @EXPORT = qw(%Config);
our @EXPORT_OK = qw(config_vars config_re myconfig);

our %Config = (
    # Type sizes (standard 64-bit C ABI, used by pack/unpack tests)
    intsize      => 4,
    shortsize    => 2,
    longsize     => 8,
    ptrsize      => 8,
    ivsize       => 8,
    uvsize       => 8,
    nvsize       => 8,
    longdblsize  => 16,
    sizesize     => 8,
    doublesize   => 8,
    i8size       => 1,
    i16size      => 2,
    i32size      => 4,
    i64size      => 8,
    u8size       => 1,
    u16size      => 2,
    u32size      => 4,
    u64size      => 8,

    # Byte order
    byteorder => '12345678',  # Little-endian 64-bit

    # Float behavior
    doublekind        => 3,       # IEEE 754 double
    longdblkind       => 3,       # IEEE 754 extended
    d_double_has_inf  => 'define',
    d_double_has_nan  => 'define',
    d_double_style_ieee => 'define',
    nv_preserves_uv_bits => 53,   # IEEE double mantissa bits

    # Platform
    osname    => 'linux',
    archname  => 'x86_64-linux',
    osvers    => '6.17.0-8-generic',
    myuname   => 'linux pcl 6.0.0 x86_64',

    # Features - what PCL/SBCL supports
    useithreads    => '',          # No threading in PCL
    use64bitint    => 'define',    # 64-bit integers supported
    use64bitall    => 'define',    # All 64-bit
    usedl          => 'define',    # Dynamic loading (via SBCL)
    useperlio      => 'define',    # PerlIO layer
    uselongdouble  => '',          # Not using long double for NV
    usemallocwrap  => 'define',
    taint_support  => 1,           # Taint support exists (though PCL may not fully implement)

    # Capabilities - system calls / features
    d_fork      => '',              # PCL cannot fork (would crash on pipe/exec)
    d_symlink   => 'define',
    d_link      => 'define',
    d_lstat     => 'define',
    d_alarm     => 'define',
    d_crypt     => 'define',       # crypt(3) via FFI to libcrypt (p-crypt)
    d_truncate  => 'define',
    d_readdir   => 'define',
    d_chown     => 'define',
    d_chmod     => 'define',
    d_chroot    => 'define',
    d_fcntl     => 'define',
    d_flock     => 'define',
    d_getppid   => 'define',
    d_getpgrp   => 'define',
    d_setpgrp   => 'define',
    d_getpriority => 'define',
    d_setpriority => 'define',
    d_killpg    => 'define',
    d_pipe      => 'define',
    d_select    => 'define',
    d_socket    => 'define',
    d_socketpair => 'define',
    d_msg       => 'define',
    d_sem       => 'define',
    d_shm       => 'define',
    d_times     => 'define',
    d_waitpid   => 'define',
    d_pseudofork => '',            # No pseudofork
    ebcdic      => '',             # Not EBCDIC

    # Extensions / modules
    extensions => '',

    # Signal names and numbers (Linux x86_64)
    sig_name => 'ZERO HUP INT QUIT ILL TRAP ABRT BUS FPE KILL USR1 SEGV USR2 PIPE ALRM TERM STKFLT CHLD CONT STOP TSTP TTIN TTOU URG XCPU XFSZ VTALRM PROF WINCH IO PWR SYS',
    sig_num  => '0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31',
    sig_count => 32,

    # Compiler info (not really used, but some tests check)
    cc      => 'gcc',
    ccflags => '-fPIC -O2',
    ld      => 'gcc',
    ldflags => '',

    # Perl version info
    version     => '5.40.0',
    api_version => '5.40.0',
    subversion  => 0,

    # Path info
    startperl => '#!/usr/bin/perl',
    perlpath  => '/usr/bin/perl',

    # Pack/unpack related
    d_quad      => 'define',       # 64-bit integers
    quadtype    => 'long',
    uquadtype   => 'unsigned long',
    quadkind    => 2,              # long is quad
    nvtype      => 'double',
    ivtype      => 'long',
    uvtype      => 'unsigned long',

    # Misc
    d_setlocale => 'define',
    d_sprintf_returns_strlen => 'define',
    d_PRIfldbl  => 'define',
    d_PRIgldbl  => 'define',
    d_PRIeldbl  => 'define',
    d_longdbl   => 'define',
    d_sqrtl     => 'define',
    d_modfl     => 'define',
    d_frexpl    => 'define',
    d_isnan     => 'define',
    d_isinf     => 'define',
    d_fpclassify => 'define',
    d_copysign  => 'define',
    d_signbit   => 'define',
    d_strtod    => 'define',
    d_strtold   => 'define',
    d_strtoll   => 'define',
    d_strtoull  => 'define',
    d_atoll     => 'define',
);

# config_vars - print config values (used by some tests)
sub config_vars {
    my @vars = @_;
    for my $var (@vars) {
        if (exists $Config{$var}) {
            print "$var='$Config{$var}'\n";
        } else {
            print "$var='UNKNOWN'\n";
        }
    }
}

# config_re - return keys matching a pattern
sub config_re {
    my ($re) = @_;
    return grep { /$re/ } keys %Config;
}

# myconfig - return formatted config string
sub myconfig {
    return "PCL fake Config.pm - 64-bit Linux compatible";
}

# import - called by "import Config" or "use Config"
# PCL doesn't actually support symbol import, so this is a no-op
# No custom import: PCL imports @EXPORT (%Config) automatically on `use Config`.
# (The previous no-op import relied on that fake running — which it no longer
# does when a module defines its own import.)

1;
