# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later
#
# Time::HiRes without XS.  Real Time::HiRes is an XS module, so `use
# Time::HiRes qw(time sleep)` — line one of most scripts that time anything —
# died in PCL (task #1992).  Everything it offers below the signal-driven
# timers is arithmetic over ONE clock and ONE sleep, so those two (plus the
# clock-id pair) are the whole runtime surface, reached through the blessed
# `builtin::` shim-dispatch seam; the rest is plain Perl here.
#
# NOT IMPLEMENTED, and loud about it: ualarm, setitimer, getitimer,
# clock_nanosleep, clock() — they are signal/timer machinery, see
# docs/not-supported.md.

package Time::HiRes;
use strict;
use warnings;
use Exporter 'import';

our $VERSION = '1.9764';

our @EXPORT_OK = qw(
    time sleep usleep nanosleep
    gettimeofday tv_interval
    clock_gettime clock_getres
    stat utime
    CLOCK_REALTIME CLOCK_MONOTONIC
    d_hires_stat d_hires_utime d_file_times d_futimens d_utimensat
    d_nanosleep d_clock_gettime d_clock_getres d_clock_nanosleep d_clock
    d_gettimeofday d_usleep d_alarm d_ualarm d_setitimer d_getitimer
);
our %EXPORT_TAGS = (clock => [qw(CLOCK_REALTIME CLOCK_MONOTONIC clock_gettime clock_getres)]);

# The clock ids are PCL's own consistent pair (Linux's numbers); the runtime
# reads them and dies on any other id rather than answering a wrong time.
sub CLOCK_REALTIME  () { 0 }
sub CLOCK_MONOTONIC () { 1 }

# The two primitives.  `builtin::hires_time` is epoch seconds as a float;
# `builtin::hires_sleep` sleeps a fraction and answers the seconds actually
# slept (it returns early when a signal handler ran — perl's own rule).
sub time () { return builtin::hires_time() }

sub sleep (;@) {
    my $secs = @_ ? $_[0] : 0;
    return builtin::hires_sleep($secs);
}

sub usleep (;@) {
    my $usec = @_ ? $_[0] : 0;
    return builtin::hires_sleep($usec / 1_000_000) * 1_000_000;
}

sub nanosleep (;@) {
    my $nsec = @_ ? $_[0] : 0;
    return builtin::hires_sleep($nsec / 1_000_000_000) * 1_000_000_000;
}

# gettimeofday is a LIST of (seconds, microseconds) in list context and a
# float in scalar context — both derived from the one clock.
sub gettimeofday () {
    my $t = builtin::hires_time();
    return $t if !wantarray;
    my $s = int($t);
    my $u = int(($t - $s) * 1_000_000 + 0.5);
    if ($u >= 1_000_000) { $s++; $u -= 1_000_000 }
    return ($s, $u);
}

# tv_interval([$s,$u]) is "since then, until now"; with two it is the gap.
sub tv_interval {
    my ($a, $b) = @_;
    $b = [gettimeofday()] if !defined $b;
    return ($b->[0] - $a->[0]) + (($b->[1] || 0) - ($a->[1] || 0)) / 1_000_000;
}

sub clock_gettime (;$) {
    my $id = @_ ? $_[0] : CLOCK_REALTIME;
    return builtin::hires_clock($id);
}

sub clock_getres (;$) {
    my $id = @_ ? $_[0] : CLOCK_REALTIME;
    return builtin::hires_clock_res($id);
}

# The hi-res file-time pair.  PCL's own stat/utime already carry whatever
# resolution the host reports, so these are CORE's — they exist because core
# File::Copy imports them by name and because a program that asks for them
# must get a working function rather than an import error.
sub stat (;$)  { return CORE::stat(@_  ? $_[0] : $_) }
sub utime (@)  { return CORE::utime(@_) }

# The d_* capability flags real Time::HiRes exports, answered honestly.  They
# are how a program — and the module's own t/ — asks what this build can do,
# so a MISSING one is not a neutral absence: the dist's t/time.t, t/usleep.t
# and t/nanosleep.t died "Undefined subroutine &Time::HiRes::d_gettimeofday"
# where perl skips or runs (measured s495).  Every flag the real module
# exports is here, and each says what THIS shim actually implements.
sub d_hires_stat     () { 0 }   # stat is CORE's: whole seconds
sub d_hires_utime    () { 0 }   # utime is CORE's
sub d_file_times     () { 0 }   # ... so the hi-res file-time pair is absent
sub d_futimens       () { 0 }
sub d_utimensat      () { 0 }
sub d_nanosleep      () { 1 }
sub d_clock_gettime  () { 1 }
sub d_clock_getres   () { 1 }
sub d_clock_nanosleep() { 0 }
sub d_clock          () { 0 }   # no CPU clock primitive on the seam
sub d_gettimeofday   () { 1 }
sub d_usleep         () { 1 }
sub d_alarm          () { 1 }   # CORE alarm, whole seconds
sub d_ualarm         () { 0 }   # signal-driven: not-supported.md
sub d_setitimer      () { 0 }
sub d_getitimer      () { 0 }

1;
