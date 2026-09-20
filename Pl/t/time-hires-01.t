#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# time-hires-01.t — tasks #1992 + #2002.
#
# `use Time::HiRes qw(time sleep)` is line one of most scripts that time
# anything, and it DIED in PCL: the real module is XS.  `lib/Time/HiRes.pm`
# is plain Perl over two runtime primitives on the blessed `builtin::`
# shim-dispatch seam — a clock with sub-second resolution
# (`builtin::hires_time`, `builtin::hires_clock`) and a sleep that takes a
# fraction (`builtin::hires_sleep`).  Everything else the module offers below
# its signal-driven timers is arithmetic over those.
#
# #2002 rides here: perl's `sleep` RETURNS EARLY when a signal handler ran
# during it (a daemon's `while (!$done) { sleep 60 }` must see its flag at
# once), and PCL slept the full remaining time.  SBCL's `sleep` resumes after
# an interrupt, so the early return is a throw FROM the signal trampoline to a
# catch tag the sleeping code published.
#
# VALUES ARE ASSERTED AS RANGES, never as exact times.

use v5.30;
use strict;
use warnings;
use Test::More;
use File::Temp qw(tempfile);
use FindBin qw($RealBin);
use lib $RealBin;
use PCLCore;

my $project_root = "$RealBin/../..";
my $pl2cl        = "$project_root/pl2cl";
my $runtime      = "$project_root/cl/pcl-runtime.lisp";
my @sbcl_rt = PCLCore::sbcl_prefix($runtime);

plan skip_all => "pl2cl not found" unless -x $pl2cl;
plan skip_all => "sbcl not found"  unless `which sbcl 2>/dev/null`;

plan tests => 12;

sub run_cl {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;
    my $cl_code = PCLCore::transpile("$pl2cl $pl_file");
    my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cl_fh $cl_code;
    close $cl_fh;
    my $output = `sbcl @sbcl_rt --load $cl_file 2>&1`;
    $output =~ s/^;.*\n//gm;
    $output =~ s/^PCL Runtime loaded\n//gm;
    $output =~ s/^\s*\n//gm;
    return $output;
}

# Each row's program decides its own verdict against a RANGE and prints it, so
# no wall-clock number ever reaches an assertion here.
sub verdict {
    my ($code, $want, $desc) = @_;
    is(run_cl($code), "$want\n", $desc);
}

verdict('use Time::HiRes qw(time); my $t = time; print +($t > 1_600_000_000 && $t =~ /\./) ? "ok" : "bad:$t", "\n";',
        'ok', 'Time::HiRes::time is epoch seconds WITH a fraction');

verdict('use Time::HiRes (); my $t = Time::HiRes::time(); print +($t > 1_600_000_000) ? "ok" : "bad", "\n";',
        'ok', '... and the fully-qualified call works without importing');

verdict('use Time::HiRes qw(time sleep); my $a = time; my $s = sleep(0.05); my $e = time - $a;'
        . ' print +($s >= 0.04 && $s < 1 && $e >= 0.04 && $e < 1) ? "ok" : "bad:$s/$e", "\n";',
        'ok', 'the imported sleep takes a FRACTION and answers the seconds slept');

verdict('use Time::HiRes qw(sleep); print +(CORE::sleep(0.4) == 0) ? "ok" : "bad", "\n";',
        'ok', '... while CORE::sleep still TRUNCATES, as perl does');

verdict('use Time::HiRes qw(usleep nanosleep); my $u = usleep(2000); my $n = nanosleep(2000);'
        . ' print +($u >= 1000 && defined $n) ? "ok" : "bad:$u", "\n";',
        'ok', 'usleep and nanosleep are the same sleep, rescaled');

verdict('use Time::HiRes qw(gettimeofday); my @g = gettimeofday();'
        . ' print +(@g == 2 && $g[0] > 1_600_000_000 && $g[1] >= 0 && $g[1] < 1_000_000) ? "ok" : "bad:@g", "\n";',
        'ok', 'gettimeofday in LIST context is (seconds, microseconds)');

verdict('use Time::HiRes qw(gettimeofday); my $g = gettimeofday();'
        . ' print +($g > 1_600_000_000 && $g =~ /\./) ? "ok" : "bad:$g", "\n";',
        'ok', '... and a float in SCALAR context');

verdict('use Time::HiRes qw(tv_interval); printf "%.3f\n", tv_interval([1000, 500000], [1002, 250000]);',
        '1.750', 'tv_interval is exact arithmetic over the two stamps (perl-probed)');

verdict('use Time::HiRes qw(gettimeofday tv_interval); my $t0 = [gettimeofday];'
        . ' print +(tv_interval($t0) >= 0 && tv_interval($t0) < 10) ? "ok" : "bad", "\n";',
        'ok', '... and with ONE argument it measures until now');

verdict('use Time::HiRes qw(clock_gettime clock_getres CLOCK_REALTIME CLOCK_MONOTONIC);'
        . ' my $r = clock_gettime(CLOCK_REALTIME); my $m = clock_gettime(CLOCK_MONOTONIC);'
        . ' my $res = clock_getres(CLOCK_REALTIME);'
        . ' print +($r > 1_600_000_000 && $m > 0 && $res > 0 && $res <= 1'
        . '         && CLOCK_REALTIME == 0 && CLOCK_MONOTONIC == 1) ? "ok" : "bad", "\n";',
        'ok', 'clock_gettime/clock_getres over the CLOCK_REALTIME/MONOTONIC pair');

# ---- #2002: a handler that RETURNS ends the sleep ------------------------

verdict('my $got = 0; $SIG{ALRM} = sub { $got++ }; my $t0 = time; alarm 1;'
        . ' my $slept = sleep 6; my $el = time - $t0;'
        . ' print +($got == 1 && $el < 4 && $slept < 6) ? "ok" : "bad:$got/$el/$slept", "\n";',
        'ok', '#2002: sleep returns EARLY after a signal handler ran');

verdict('my $r = eval { local $SIG{ALRM} = sub { die "to\n" }; alarm 1; sleep 6; alarm 0; 1 };'
        . ' print +(!defined $r && $@ eq "to\n") ? "ok" : "bad", "\n";',
        'ok', '... and the classic die-in-handler timeout idiom still works');
