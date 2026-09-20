#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# posix-01.t — task #1997 (which folds #1613).
#
# `lib/POSIX.pm` was a 45-name stub with an EMPTY @EXPORT, so the two
# commonest spellings both failed: `use POSIX; floor(3.7)` died "Undefined
# subroutine &main::floor" and `use POSIX qw(strftime)` died AT THE IMPORT.
# It is now plain Perl (rule 9a) with perl's own default export list
# restricted to what it implements.
#
# perl is the ORACLE for every row: the expectation is what perl printed.
# The four RISKS the task named are rows here — a user sub of the same name
# in BOTH orders, builtin-named names staying out of @EXPORT, the warm cost
# of `use POSIX;` (measured 13 ms over a bare program with `pcl`, not
# asserted here), and a bareword constant parsing as a TERM (`INT_MAX - 1`
# was `INT_MAX(-1)` and swallowed the rest of the argument list until POSIX
# left `_extract_module_prototypes`'s skip list).

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

plan tests => 16;

sub write_pl {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;
    return $pl_file;
}

sub run_cl {
    my ($code) = @_;
    my $cl_code = PCLCore::transpile("$pl2cl " . write_pl($code));
    my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cl_fh $cl_code;
    close $cl_fh;
    my $output = `sbcl @sbcl_rt --load $cl_file 2>&1`;
    $output =~ s/^;.*\n//gm;
    $output =~ s/^PCL Runtime loaded\n//gm;
    $output =~ s/^\s*\n//gm;
    return $output;
}

# perl is the oracle: its stdout IS the expectation.
sub both_agree {
    my ($code, $desc) = @_;
    my $perl = `perl @{[ write_pl($code) ]} 2>/dev/null`;
    my $pcl  = run_cl($code);
    is($pcl, $perl, "$desc (perl: " . ($perl =~ s/\n/\\n/gr) . ")");
}

# ---- the two spellings that failed outright -----------------------------

both_agree('use POSIX; print floor(3.7), " ", ceil(3.2), " ", floor(-3.7), " ", ceil(-3.2), "\n";',
           'bare `use POSIX;` default-exports floor/ceil');

both_agree('use POSIX qw(strftime); print strftime("%Y-%m-%d", 0, 0, 0, 1, 0, 70), "\n";',
           '`use POSIX qw(strftime)` imports instead of dying at the import');

# ---- strftime, the most-used name after the constants -------------------

both_agree('use POSIX; print strftime("%Y-%m-%d %H:%M:%S", 5, 4, 3, 2, 0, 100), "\n";',
           'strftime over the everyday conversions');

both_agree('use POSIX; print strftime("%a %b %e %j %F %T %u %w %C %y %I %p", 0, 30, 14, 25, 11, 99), "\n";',
           '... and over %a %b %e %j %F %T %u %w %C %y %I %p');

both_agree('use POSIX; print strftime("%Y-%m-%d", 0, 0, 0, 32, 0, 100), "\n";',
           'an out-of-range field is MKTIME-NORMALISED, not an error');

both_agree('use POSIX; print strftime("%G-W%V|%U|%W", 0, 0, 12, 1, 0, 105), "\n";',
           '... and the ISO week / week-number conversions');

both_agree('use POSIX; my $t = mktime(0, 0, 12, 1, 6, 100);'
           . ' my @l = localtime($t); print "$l[3] $l[4] $l[5]\n";',
           'mktime round-trips through localtime');

both_agree('use POSIX; print asctime(0, 0, 12, 1, 6, 100);',
           'asctime prints the wday it was GIVEN (0 when omitted), as perl does');

# ---- the W* macros on a real status word --------------------------------

both_agree('use POSIX qw(:sys_wait_h); system("sh", "-c", "exit 3");'
           . ' printf "%d %d %d\n", WIFEXITED($?) ? 1 : 0, WEXITSTATUS($?), WIFSIGNALED($?) ? 1 : 0;',
           'the :sys_wait_h macros on a real `system` status');

# ---- strtol/strtod answer TWO values ------------------------------------

both_agree('use POSIX; print join(",", strtol("  0x1f junk", 16)), "|", join(",", strtol("42abc")),'
           . ' "|", join(",", strtol("zz")), "\n";',
           'strtol takes a BASE and reports the unparsed length (the stub said 0)');

both_agree('use POSIX; print join(",", strtod("3.25e2xyz")), "|", join(",", strtod("nope")), "\n";',
           'strtod likewise');

# ---- the four risks -----------------------------------------------------

both_agree('package A1; use POSIX; sub floor { "mine:$_[0]" } print floor(3.7), "\n";',
           'RISK 1a: a user sub defined AFTER the use wins (perl warns and redefines)');

both_agree('package A2; sub floor { "mine:$_[0]" } use POSIX; print A2::floor(3.7), "\n";',
           'RISK 1b: ... and in the other order POSIX wins, as perl says');

both_agree('use POSIX; print abs(-4), " ", int(log(exp(1))), " ", sleep(0), "\n";',
           'RISK 2: builtin-named names are NOT default-exported, so abs/log/sleep stay core');

both_agree('use POSIX; print +(INT_MAX > 0 ? "pos" : "neg"), " ", INT_MAX - 1, " ", EXIT_FAILURE + 1, "\n";',
           'RISK 4: a bareword constant is a TERM (was INT_MAX(-1), swallowing the list)');

# ---- an unimplemented name stays LOUD -----------------------------------

both_agree('use POSIX; my $ok = eval { POSIX::sigprocmask(0); 1 };'
           . ' my $ok2 = eval "use POSIX qw(nosuchname); 1";'
           . ' print +($ok ? "silent" : "loud"), " ", ($ok2 ? "silent" : "loud"), "\n";',
           'an unimplemented name is LOUD at the call AND at the import');
