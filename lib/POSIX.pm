# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later
#
# POSIX, in plain Perl (task #1997, which folds #1613).
#
# This was a 45-name stub with an EMPTY @EXPORT, so `use POSIX;` exported
# nothing and `use POSIX qw(strftime)` died AT THE IMPORT -- the two commonest
# spellings in scripts.  Nearly all of POSIX's useful surface is arithmetic,
# string work or a core builtin in disguise, so it is written here (rule 9a);
# only what no Perl can say goes through the blessed `builtin::` seam (a signal
# name's number, an immediate exit).
#
# @EXPORT is perl's own default list RESTRICTED to what this file implements,
# and a name that is not implemented stays LOUD -- Exporter's "not exported" at
# the `use`, or "Undefined subroutine" at the call.  Never a stub that answers
# a plausible value (CLAUDE.md rule 12).
#
# NOT IMPLEMENTED, deliberately (see docs/not-supported.md): the OO classes
# (SigSet, SigAction, Termios), sigprocmask/sigaction/sigpending/sigsuspend,
# the tty/termios family, sysconf/pathconf/confstr, setsid/setpgid/mkfifo/
# pause/nice/ttyname/dup2/chroot, and locale beyond answering "C".

package POSIX;
use strict;
use warnings;
use Exporter q{import};

our $VERSION = '2.13';

# ---------------------------------------------------------------- constants

# waitpid(2) flags
sub WNOHANG   () { 1 }
sub WUNTRACED () { 2 }

# Wait-status macros: arithmetic on the status WORD, exactly as <sys/wait.h>
# defines them, so they work on whatever `system`/`waitpid` put in $?.
sub WIFEXITED   ($) { ($_[0] & 0x7f) == 0 }
sub WEXITSTATUS ($) { ($_[0] >> 8) & 0xff }
sub WIFSIGNALED ($) { (($_[0] & 0x7f) + 1) >> 1 > 0 }
sub WTERMSIG    ($) { $_[0] & 0x7f }
sub WIFSTOPPED  ($) { ($_[0] & 0xff) == 0x7f }
sub WSTOPSIG    ($) { ($_[0] >> 8) & 0xff }

sub EXIT_SUCCESS () { 0 }
sub EXIT_FAILURE () { 1 }

# Floating-point limits
sub DBL_MAX     () { 1.7976931348623157e+308 }
sub DBL_MIN     () { 2.2250738585072014e-308 }
sub DBL_EPSILON () { 2.220446049250313e-16 }
sub DBL_DIG     () { 15 }
sub DBL_MANT_DIG () { 53 }
sub FLT_MAX     () { 3.4028234663852886e+38 }
sub FLT_MIN     () { 1.1754943508222875e-38 }
sub FLT_EPSILON () { 1.1920928955078125e-07 }
sub FLT_DIG     () { 6 }
sub LDBL_MAX    () { 1.1897314953572317e+4932 }
sub LDBL_MIN    () { 3.3621031431120935e-4932 }
sub LDBL_EPSILON () { 1.0842021724855044e-19 }

# Integer limits
sub CHAR_BIT  () { 8 }
sub CHAR_MAX  () { 127 }
sub CHAR_MIN  () { -128 }
sub SCHAR_MAX () { 127 }
sub SCHAR_MIN () { -128 }
sub UCHAR_MAX () { 255 }
sub SHRT_MAX  () { 32767 }
sub SHRT_MIN  () { -32768 }
sub USHRT_MAX () { 65535 }
sub INT_MAX   () { 2147483647 }
sub INT_MIN   () { -2147483648 }
sub UINT_MAX  () { 4294967295 }
sub LONG_MAX  () { 9223372036854775807 }
sub LONG_MIN  () { -9223372036854775808 }
sub ULONG_MAX () { 18446744073709551615 }

# Math constants
sub M_PI    () { 3.14159265358979323846 }
sub M_E     () { 2.71828182845904523536 }
sub M_SQRT2 () { 1.41421356237309504880 }
sub M_LN2   () { 0.69314718055994530942 }
sub M_LN10  () { 2.30258509299404568402 }
sub M_LOG2E () { 1.44269504088896340736 }
sub M_PI_2  () { 1.57079632679489661923 }
sub M_PI_4  () { 0.78539816339744830962 }

# seek(2) / open(2) / fcntl(2) flags.  These mirror lib/Fcntl.pm's values,
# which are Linux's; a portability note lives in docs/not-supported.md.
sub SEEK_SET () { 0 }
sub SEEK_CUR () { 1 }
sub SEEK_END () { 2 }
sub O_RDONLY () { 0 }
sub O_WRONLY () { 1 }
sub O_RDWR   () { 2 }
sub O_CREAT  () { 64 }
sub O_EXCL   () { 128 }
sub O_NOCTTY () { 256 }
sub O_TRUNC  () { 512 }
sub O_APPEND () { 1024 }
sub O_NONBLOCK () { 2048 }
sub F_GETFL  () { 3 }
sub F_SETFL  () { 4 }

# locale categories
sub LC_CTYPE    () { 0 }
sub LC_NUMERIC  () { 1 }
sub LC_TIME     () { 2 }
sub LC_COLLATE  () { 3 }
sub LC_MONETARY () { 4 }
sub LC_MESSAGES () { 5 }
sub LC_ALL      () { 6 }

# Error numbers.  These duplicate Errno's (which PCL also ships); both are
# Linux's numbers.
sub E2BIG () { 7 }        sub EACCES () { 13 }      sub EAGAIN () { 11 }
sub EBADF () { 9 }        sub EBUSY () { 16 }       sub ECHILD () { 10 }
sub EDOM () { 33 }        sub EEXIST () { 17 }      sub EFAULT () { 14 }
sub EFBIG () { 27 }       sub EINTR () { 4 }        sub EINVAL () { 22 }
sub EIO () { 5 }          sub EISDIR () { 21 }      sub EMFILE () { 24 }
sub EMLINK () { 31 }      sub ENFILE () { 23 }      sub ENODEV () { 19 }
sub ENOENT () { 2 }       sub ENOEXEC () { 8 }      sub ENOMEM () { 12 }
sub ENOSPC () { 28 }      sub ENOSYS () { 38 }      sub ENOTDIR () { 20 }
sub ENOTEMPTY () { 39 }   sub ENOTTY () { 25 }      sub ENXIO () { 6 }
sub EPERM () { 1 }        sub EPIPE () { 32 }       sub ERANGE () { 34 }
sub EROFS () { 30 }       sub ESPIPE () { 29 }      sub ESRCH () { 3 }
sub EWOULDBLOCK () { 11 } sub EXDEV () { 18 }

# Signal numbers come from the RUNTIME's own table (the one kill() and %SIG
# read), never from literals here: a literal would freeze this machine's
# numbers into Perl source.
BEGIN {
    no strict 'refs';
    for my $name (qw(HUP INT QUIT ILL TRAP ABRT BUS FPE KILL USR1 SEGV USR2
                     PIPE ALRM TERM CHLD CONT STOP TSTP TTIN TTOU URG XCPU
                     XFSZ VTALRM PROF WINCH IO SYS)) {
        my $n = builtin::signal_number($name);
        next if !defined $n;
        *{"POSIX::SIG$name"} = sub { $n };
    }
}
sub SIG_BLOCK   () { 0 }
sub SIG_UNBLOCK () { 1 }
sub SIG_SETMASK () { 2 }

# ------------------------------------------------------------------- math

sub floor ($) { my $x = $_[0] + 0; my $i = int($x); return $i == $x ? $x : ($x < 0 ? $i - 1 : $i) }
sub ceil  ($) { my $x = $_[0] + 0; my $i = int($x); return $i == $x ? $x : ($x > 0 ? $i + 1 : $i) }
sub fmod ($$)  { my ($x, $y) = @_; return $y == 0 ? 0 : $x - int($x / $y) * $y }
sub pow ($$)   { $_[0] ** $_[1] }
sub fabs ($)   { abs($_[0]) }
sub log10 ($)  { log($_[0]) / log(10) }
sub log2 ($)   { log($_[0]) / log(2) }
sub trunc ($)  { int($_[0]) }
sub round ($)  { my $x = $_[0] + 0; return $x >= 0 ? int($x + 0.5) : -int(-$x + 0.5) }
sub lround ($) { round($_[0]) }
sub fmax ($$)  { $_[0] > $_[1] ? $_[0] : $_[1] }
sub fmin ($$)  { $_[0] < $_[1] ? $_[0] : $_[1] }
sub fdim ($$)  { $_[0] > $_[1] ? $_[0] - $_[1] : 0 }
sub hypot ($$) { sqrt($_[0] ** 2 + $_[1] ** 2) }
sub cbrt ($)   { my $x = $_[0] + 0; return $x < 0 ? -((-$x) ** (1 / 3)) : $x ** (1 / 3) }
sub copysign ($$) { my ($m, $s) = @_; return $s < 0 ? -abs($m) : abs($m) }
sub signbit ($)   { $_[0] < 0 ? 1 : 0 }
sub isnan ($)     { my $x = $_[0]; return ($x != $x) ? 1 : 0 }
sub isinf ($)     { my $x = $_[0] + 0; return ($x == 9**9**9 || $x == -9**9**9) ? 1 : 0 }
sub nearbyint ($) { round($_[0]) }
sub remainder ($$) { my ($x, $y) = @_; return $y == 0 ? 0 : $x - round($x / $y) * $y }

# ------------------------------------------------------------------ string

# strtod/strtol answer TWO values in perl: the number, and how many characters
# were left UNPARSED (the stub always said 0, which is why nothing could use
# it to validate input).
sub strtod ($) {
    my $s = shift;
    $s = '' if !defined $s;
    $s =~ s/\A\s+//;
    my $num = 0;
    my $rest = $s;
    if ($s =~ /\A([+-]?(?:\d+\.?\d*|\.\d+)(?:[eE][+-]?\d+)?)/) { $num = $1 + 0; $rest = substr($s, length $1) }
    elsif ($s =~ /\A([+-]?(?:inf(?:inity)?|nan))/i)            { $num = $1 + 0; $rest = substr($s, length $1) }
    else                                                       { return wantarray ? (0, length $s) : 0 }
    return wantarray ? ($num, length $rest) : $num;
}

sub strtol ($;$) {
    my ($s, $base) = @_;
    $base = 10 if !defined $base;
    $s = '' if !defined $s;
    $s =~ s/\A\s+//;
    my ($sign, $body) = (1, $s);
    if ($body =~ s/\A([+-])//) { $sign = -1 if $1 eq '-' }
    if (($base == 16 || $base == 0) && $body =~ /\A0[xX][0-9a-fA-F]/) { $body =~ s/\A0[xX]//; $base = 16 }
    elsif ($base == 0) { $base = $body =~ /\A0[0-7]/ ? 8 : 10 }
    my $digits = $base <= 10 ? "0-" . ($base - 1) : "0-9a-" . chr(ord('a') + $base - 11) . "A-" . chr(ord('A') + $base - 11);
    my $n = 0;
    my $taken = 0;
    while ($taken < length $body) {
        my $c = substr($body, $taken, 1);
        last if $c !~ /[$digits]/;
        $n = $n * $base + hex($c);
        $taken++;
    }
    return wantarray ? (0, length $s) : 0 if $taken == 0;
    my $unparsed = length($body) - $taken;
    return wantarray ? ($sign * $n, $unparsed) : $sign * $n;
}

sub strtoul ($;$) {
    my ($n, $u) = strtol(@_);
    $n = abs($n);
    return wantarray ? ($n, $u) : $n;
}

sub tolower ($) { lc($_[0]) }
sub toupper ($) { uc($_[0]) }

sub isalpha ($) { $_[0] =~ /\A[[:alpha:]]*\z/ ? 1 : 0 }
sub isdigit ($) { $_[0] =~ /\A[[:digit:]]*\z/ ? 1 : 0 }
sub isalnum ($) { $_[0] =~ /\A[[:alnum:]]*\z/ ? 1 : 0 }
sub isspace ($) { $_[0] =~ /\A[[:space:]]*\z/ ? 1 : 0 }
sub isupper ($) { $_[0] =~ /\A[[:upper:]]*\z/ ? 1 : 0 }
sub islower ($) { $_[0] =~ /\A[[:lower:]]*\z/ ? 1 : 0 }
sub isprint ($) { $_[0] =~ /\A[[:print:]]*\z/ ? 1 : 0 }
sub ispunct ($) { $_[0] =~ /\A[[:punct:]]*\z/ ? 1 : 0 }
sub isxdigit ($) { $_[0] =~ /\A[[:xdigit:]]*\z/ ? 1 : 0 }

# ------------------------------------------------------------------- time

my @DAY   = qw(Sunday Monday Tuesday Wednesday Thursday Friday Saturday);
my @MONTH = qw(January February March April May June July August September October November December);

# Days from the civil epoch (1970-01-01) for a proleptic Gregorian y/m/d.
# Howard Hinnant's days_from_civil: exact for any year, no table, no module.
sub _days_from_civil {
    my ($y, $m, $d) = @_;
    $y -= $m <= 2 ? 1 : 0;
    my $era = int(($y >= 0 ? $y : $y - 399) / 400);
    my $yoe = $y - $era * 400;
    my $doy = int((153 * ($m + ($m > 2 ? -3 : 9)) + 2) / 5) + $d - 1;
    my $doe = $yoe * 365 + int($yoe / 4) - int($yoe / 100) + $doy;
    return $era * 146097 + $doe - 719468;
}

sub _timegm_fields {
    my ($sec, $min, $hour, $mday, $mon, $year) = @_;
    return _days_from_civil($year + 1900, $mon + 1, $mday) * 86400
         + $hour * 3600 + $min * 60 + $sec;
}

# mktime: the fields are LOCAL time, normalised (perl lets `mday` be 32).
# Two passes settle the UTC offset across a DST boundary, which is what a C
# mktime does internally; no Time::Local dependency.
sub mktime {
    my @f = @_;
    my $gm = _timegm_fields(@f[0 .. 5]);
    my $t  = $gm;
    for (1 .. 2) {
        my @l = CORE::localtime($t);
        my $off = _timegm_fields(@l[0 .. 5]) - $t;
        $t = $gm - $off;
    }
    return $t;
}

sub difftime ($$) { $_[0] - $_[1] }

sub asctime { return _asctime_fields(@_) }
sub ctime ($) { return _asctime_fields(CORE::localtime($_[0])) }

# asctime does NOT normalise and does NOT compute the weekday: it prints the
# `wday` field it was GIVEN, which is 0 (Sunday) when the caller omits it —
# probed 5.40.3, `asctime(0,0,12,1,6,100)` is "Sun Jul  1 …" for a Saturday.
sub _asctime_fields {
    my ($sec, $min, $hour, $mday, $mon, $year, $wday) = @_;
    $wday = 0 if !defined $wday;
    return sprintf("%s %s %2d %02d:%02d:%02d %d\n",
                   substr($DAY[$wday % 7], 0, 3), substr($MONTH[$mon % 12], 0, 3),
                   $mday, $hour, $min, $sec, $year + 1900);
}

# strftime(fmt, sec, min, hour, mday, mon, year, [wday, yday, isdst]).
# perl MKTIME-NORMALISES the fields first, so strftime("%Y", 0,0,0,32,0,100)
# is 2000-02-01 and not an error; the wday/yday arguments are IGNORED, exactly
# as perl's are.
sub strftime {
    my ($fmt, @f) = @_;
    my $t = mktime(@f[0 .. 5]);
    my @l = CORE::localtime($t);
    return _format_time($fmt, \@l, $t);
}

my %STRF_SIMPLE = (
    'n' => "\n", 't' => "\t", '%' => '%',
);

sub _format_time {
    my ($fmt, $l, $t) = @_;
    my ($sec, $min, $hour, $mday, $mon, $year, $wday, $yday, $isdst) = @$l;
    my $out = '';
    my $i = 0;
    while ($i < length $fmt) {
        my $c = substr($fmt, $i++, 1);
        if ($c ne '%') { $out .= $c; next }
        my $k = substr($fmt, $i++, 1);
        $k = '' if !defined $k;
        if (exists $STRF_SIMPLE{$k}) { $out .= $STRF_SIMPLE{$k}; next }
        $out .= _strf_one($k, $l, $t);
    }
    return $out;
}

sub _strf_one {
    my ($k, $l, $t) = @_;
    my ($sec, $min, $hour, $mday, $mon, $year, $wday, $yday) = @$l;
    return substr($DAY[$wday], 0, 3)        if $k eq 'a';
    return $DAY[$wday]                      if $k eq 'A';
    return substr($MONTH[$mon], 0, 3)       if $k eq 'b' || $k eq 'h';
    return $MONTH[$mon]                     if $k eq 'B';
    return sprintf("%02d", int(($year + 1900) / 100)) if $k eq 'C';
    return sprintf("%02d", $mday)           if $k eq 'd';
    return sprintf("%2d", $mday)            if $k eq 'e';
    return sprintf("%02d", $hour)           if $k eq 'H';
    return sprintf("%02d", $hour % 12 || 12) if $k eq 'I';
    return sprintf("%03d", $yday + 1)       if $k eq 'j';
    return sprintf("%2d", $hour)            if $k eq 'k';
    return sprintf("%2d", $hour % 12 || 12) if $k eq 'l';
    return sprintf("%02d", $mon + 1)        if $k eq 'm';
    return sprintf("%02d", $min)            if $k eq 'M';
    return $hour < 12 ? 'AM' : 'PM'         if $k eq 'p';
    return $hour < 12 ? 'am' : 'pm'         if $k eq 'P';
    return sprintf("%02d", $sec)            if $k eq 'S';
    return $t                               if $k eq 's';
    return $wday == 0 ? 7 : $wday           if $k eq 'u';
    return $wday                            if $k eq 'w';
    return sprintf("%02d", ($year + 1900) % 100) if $k eq 'y';
    return $year + 1900                     if $k eq 'Y';
    return sprintf("%02d/%02d/%02d", $mon + 1, $mday, ($year + 1900) % 100) if $k eq 'D';
    return sprintf("%04d-%02d-%02d", $year + 1900, $mon + 1, $mday)         if $k eq 'F';
    return sprintf("%02d:%02d", $hour, $min)                                if $k eq 'R';
    return sprintf("%02d:%02d:%02d", $hour, $min, $sec)                     if $k eq 'T';
    return sprintf("%02d:%02d:%02d %s", $hour % 12 || 12, $min, $sec, $hour < 12 ? 'AM' : 'PM') if $k eq 'r';
    return sprintf("%02d/%02d/%02d", $mon + 1, $mday, ($year + 1900) % 100) if $k eq 'x';
    return sprintf("%02d:%02d:%02d", $hour, $min, $sec)                     if $k eq 'X';
    return _asctime_fields($sec, $min, $hour, $mday, $mon, $year, $wday) =~ s/\n\z//r if $k eq 'c';
    return sprintf("%02d", int(($yday + 7 - $wday) / 7))                    if $k eq 'U';
    return sprintf("%02d", int(($yday + 7 - ($wday == 0 ? 6 : $wday - 1)) / 7)) if $k eq 'W';
    return _strf_zone($k, $t);
}

# %z / %Z / %V / %G / %g need the zone or the ISO week; anything else is a
# conversion this strftime does not implement and it DIES naming the letter
# (CLAUDE.md rule 12 -- a format letter produces a VALUE the program consumes,
# so a silent literal would be the wrong answer, not a missing one).
sub _strf_zone {
    my ($k, $t) = @_;
    if ($k eq 'z') {
        my $off = _timegm_fields((CORE::localtime($t))[0 .. 5]) - $t;
        my $sign = $off < 0 ? '-' : '+';
        $off = abs($off);
        return sprintf("%s%02d%02d", $sign, int($off / 3600), int(($off % 3600) / 60));
    }
    if ($k eq 'Z') {
        my $off = _timegm_fields((CORE::localtime($t))[0 .. 5]) - $t;
        return 'UTC' if $off == 0;
        my $sign = $off < 0 ? '-' : '+';
        $off = abs($off);
        return sprintf("UTC%s%02d%s", $sign, int($off / 3600),
                       ($off % 3600) ? sprintf(":%02d", int(($off % 3600) / 60)) : '');
    }
    my ($iy, $iw) = _iso_week($t);
    return sprintf("%02d", $iw) if $k eq 'V';
    return $iy                  if $k eq 'G';
    return sprintf("%02d", $iy % 100) if $k eq 'g';
    require Carp;
    Carp::croak("POSIX::strftime: unimplemented conversion %$k");
}

sub _iso_week {
    my ($t) = @_;
    my @l = CORE::localtime($t);
    my ($yday, $wday, $year) = ($l[7], $l[6], $l[5] + 1900);
    my $iso_wday = $wday == 0 ? 7 : $wday;
    my $week = int(($yday - $iso_wday + 10) / 7);
    if ($week < 1) { $year--; $week = _iso_weeks_in($year) }
    elsif ($week > _iso_weeks_in($year)) { $year++; $week = 1 }
    return ($year, $week);
}

sub _iso_weeks_in {
    my ($y) = @_;
    my $p = sub { my $n = shift; ($n + int($n / 4) - int($n / 100) + int($n / 400)) % 7 };
    return ($p->($y) == 4 || $p->($y - 1) == 3) ? 53 : 52;
}

sub tzset  { return }
sub tzname { return ('UTC', 'UTC') }
sub clock  { return (CORE::times())[0] }

# ------------------------------------------------------- errno and locale

sub errno    { return $! + 0 }
sub strerror ($) { local $! = $_[0]; return "$!" }

# PCL has no locale machinery, so setlocale ANSWERS the locale the process was
# started in (what perl's own setlocale reports before any change) rather than
# pretending a change took effect.
sub setlocale {
    return $ENV{LC_ALL} || $ENV{LANG} || 'C';
}
sub localeconv {
    return { decimal_point => '.', thousands_sep => '', grouping => '',
             int_curr_symbol => '', currency_symbol => '', mon_decimal_point => '',
             mon_thousands_sep => '', positive_sign => '', negative_sign => '',
             int_frac_digits => 127, frac_digits => 127 };
}

# ------------------------------------------------------------ process / IO

sub _exit { builtin::exit_immediately(@_ ? $_[0] : 0) }
sub isatty ($) { return -t $_[0] ? 1 : 0 }
sub getpid  { return $$ }
sub getppid { return CORE::getppid() }
sub getcwd  { require Cwd; return Cwd::getcwd() }

# access(2): the mode bits are R_OK 4 / W_OK 2 / X_OK 1 / F_OK 0, and the
# question each asks is a core filetest.
sub F_OK () { 0 }
sub R_OK () { 4 }
sub W_OK () { 2 }
sub X_OK () { 1 }
# access(2) answers C's way, which perl passes straight through: "0 but true"
# on success (C returns 0) and undef on failure — NOT a boolean (probed 5.40.3).
sub access ($$) {
    my ($path, $mode) = @_;
    return undef if !-e $path;
    return undef if ($mode & R_OK) && !-r $path;
    return undef if ($mode & W_OK) && !-w $path;
    return undef if ($mode & X_OK) && !-x $path;
    return '0 but true';
}

# POSIX::dup returns a RAW fd that stays open, so the handle the core dup-open
# made is parked here (dropping it would close the fd).
our @_dup_keep;
sub dup {
    my $fd = shift;
    open(my $fh, ">&", $fd) or return undef;
    push @_dup_keep, $fh;
    return fileno($fh);
}

# ----------------------------------------------------------------- exports

# perl's own default list, RESTRICTED to what is above.  Names that are also
# perl BUILTINS are NOT in perl's @EXPORT either (they are EXPORT_OK), and that
# matters here: an imported sub named like a builtin DISPLACES it (task #1870),
# so a default export of `abs` would silently change every `abs` in the file.
our @EXPORT = qw(
    WNOHANG WUNTRACED WIFEXITED WEXITSTATUS WIFSIGNALED WTERMSIG WIFSTOPPED WSTOPSIG
    EXIT_SUCCESS EXIT_FAILURE
    DBL_MAX DBL_MIN DBL_EPSILON DBL_DIG DBL_MANT_DIG
    FLT_MAX FLT_MIN FLT_EPSILON FLT_DIG LDBL_MAX LDBL_MIN LDBL_EPSILON
    CHAR_BIT CHAR_MAX CHAR_MIN SCHAR_MAX SCHAR_MIN UCHAR_MAX
    SHRT_MAX SHRT_MIN USHRT_MAX INT_MAX INT_MIN UINT_MAX LONG_MAX LONG_MIN ULONG_MAX
    SEEK_SET SEEK_CUR SEEK_END
    O_RDONLY O_WRONLY O_RDWR O_CREAT O_EXCL O_NOCTTY O_TRUNC O_APPEND O_NONBLOCK
    F_GETFL F_SETFL F_OK R_OK W_OK X_OK
    LC_CTYPE LC_NUMERIC LC_TIME LC_COLLATE LC_MONETARY LC_MESSAGES LC_ALL
    E2BIG EACCES EAGAIN EBADF EBUSY ECHILD EDOM EEXIST EFAULT EFBIG EINTR EINVAL
    EIO EISDIR EMFILE EMLINK ENFILE ENODEV ENOENT ENOEXEC ENOMEM ENOSPC ENOSYS
    ENOTDIR ENOTEMPTY ENOTTY ENXIO EPERM EPIPE ERANGE EROFS ESPIPE ESRCH
    EWOULDBLOCK EXDEV
    SIG_BLOCK SIG_UNBLOCK SIG_SETMASK
    floor ceil fmod pow fabs
    strftime mktime asctime ctime difftime
    strtod strtol strtoul
    errno strerror setlocale localeconv tzset tzname
    _exit isatty access dup
    INT_MAX
);
push @EXPORT, map { "SIG$_" } qw(HUP INT QUIT ILL TRAP ABRT BUS FPE KILL USR1
                                 SEGV USR2 PIPE ALRM TERM CHLD CONT STOP TSTP
                                 TTIN TTOU URG XCPU XFSZ VTALRM PROF WINCH IO SYS);

# Names perl keeps out of @EXPORT because they collide with a builtin or are
# rarely wanted by default.
our @EXPORT_OK = (@EXPORT, qw(
    M_PI M_E M_SQRT2 M_LN2 M_LN10 M_LOG2E M_PI_2 M_PI_4
    log2 log10 trunc round lround fmax fmin fdim hypot cbrt copysign signbit
    isnan isinf nearbyint remainder
    tolower toupper isalpha isdigit isalnum isspace isupper islower isprint
    ispunct isxdigit
    clock getpid getppid getcwd
));

our %EXPORT_TAGS = (
    sys_wait_h => [qw(WNOHANG WUNTRACED WIFEXITED WEXITSTATUS WIFSIGNALED
                      WTERMSIG WIFSTOPPED WSTOPSIG)],
    errno_h    => [grep { /\AE[A-Z0-9]+\z/ } @EXPORT],
    limits_h   => [qw(CHAR_BIT CHAR_MAX CHAR_MIN SCHAR_MAX SCHAR_MIN UCHAR_MAX
                      SHRT_MAX SHRT_MIN USHRT_MAX INT_MAX INT_MIN UINT_MAX
                      LONG_MAX LONG_MIN ULONG_MAX)],
    float_h    => [qw(DBL_MAX DBL_MIN DBL_EPSILON DBL_DIG DBL_MANT_DIG
                      FLT_MAX FLT_MIN FLT_EPSILON FLT_DIG
                      LDBL_MAX LDBL_MIN LDBL_EPSILON)],
    math_h     => [qw(floor ceil fmod pow fabs HUGE_VAL)],
    stdlib_h   => [qw(EXIT_SUCCESS EXIT_FAILURE strtod strtol strtoul _exit)],
    time_h     => [qw(strftime mktime asctime ctime difftime clock tzset tzname)],
    unistd_h   => [qw(F_OK R_OK W_OK X_OK access dup isatty _exit
                      SEEK_SET SEEK_CUR SEEK_END)],
    fcntl_h    => [qw(O_RDONLY O_WRONLY O_RDWR O_CREAT O_EXCL O_NOCTTY O_TRUNC
                      O_APPEND O_NONBLOCK F_GETFL F_SETFL
                      SEEK_SET SEEK_CUR SEEK_END)],
    locale_h   => [qw(setlocale localeconv LC_CTYPE LC_NUMERIC LC_TIME
                      LC_COLLATE LC_MONETARY LC_MESSAGES LC_ALL)],
    signal_h   => [grep { /\ASIG/ } @EXPORT],
);
$EXPORT_TAGS{math_h} = [grep { $_ ne 'HUGE_VAL' } @{ $EXPORT_TAGS{math_h} }];

1;
