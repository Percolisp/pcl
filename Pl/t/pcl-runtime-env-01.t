#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# pcl-runtime-env-01.t — $ENV{_PCL_RUNTIME_}, the one honest "am I running
# under PCL?" signal (USER ask s476, task #1529).
#
# There is no other: `$^V`/`$]` answer 5.30.0 on purpose, `$^X` DELIBERATELY
# points at real perl so a spawned subprocess runs perl, and `$^O` is the OS.
#
# THESE ARE NOT ORACLE-COMPARED ROWS.  Real perl prints undef for every one of
# them BY DESIGN — that is the whole point of the variable — so each row states
# its expectation explicitly.
#
# The load-bearing row is the CHILD one: the entry is SYNTHETIC, living at the
# %ENV read layer and NOT in the process environment, because `$^X` is real
# perl and a perl child that inherited it would believe it runs under PCL.

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
    my $cl_code = `$pl2cl $pl_file 2>/dev/null`;
    my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cl_fh $cl_code;
    close $cl_fh;
    my $output = `sbcl @sbcl_rt --load $cl_file 2>&1`;
    $output =~ s/^;.*\n//gm;
    $output =~ s/^PCL Runtime loaded\n//gm;
    $output =~ s/^\s*\n//gm;
    return $output;
}

sub test_cl {
    my ($name, $code, $expected) = @_;
    is(run_cl($code), $expected, $name);
}

# ── (1) it is TRUE, and its value is `pcl --version`'s version string ───────
# The version comes from the SAME source `pcl --version` reads: an installed
# tree's VERSION file, else `git describe` — so the row compares against that
# command rather than restating the rule.
my $want_version = do {
    my $v = `$project_root/pcl --version 2>/dev/null`;
    ($v // '') =~ /^pcl \(PCL\) (.+)$/m ? $1 : undef;
};

SKIP: {
    skip "pcl --version did not answer", 1 unless defined $want_version;
    test_cl('$ENV{_PCL_RUNTIME_} is `pcl --version`\'s version string',
        'print $ENV{_PCL_RUNTIME_}, "\n";', "$want_version\n");
}

test_cl('$ENV{_PCL_RUNTIME_} is TRUE',
    'print +($ENV{_PCL_RUNTIME_} ? "yes" : "no"), "\n";', "yes\n");

# ── (2) exists / keys / each / a %ENV copy all see it ───────────────────────
test_cl('exists, keys, each and a %ENV copy all see it',
    'my %e = %ENV; my $each = "";'
  . 'while (my ($k,$v) = each %ENV) { $each = "yes" if $k eq "_PCL_RUNTIME_" }'
  . 'print join(",", (exists $ENV{_PCL_RUNTIME_} ? "yes" : "no"),'
  . '  (scalar(grep { $_ eq "_PCL_RUNTIME_" } keys %ENV) ? "yes" : "no"),'
  . '  ($e{_PCL_RUNTIME_} ? "yes" : "no"), ($each || "no")), "\n";',
    "yes,yes,yes,yes\n");

# `values` walks the environment separately from `keys`, so the two lists must
# stay the same length and the synthetic value must sit at its key's index.
test_cl('keys and values stay aligned',
    'my @k = keys %ENV; my @v = values %ENV;'
  . 'my ($i) = grep { $k[$_] eq "_PCL_RUNTIME_" } 0..$#k;'
  . 'print join(",", (@k == @v ? "same" : "DIFFERENT"),'
  . '  ((defined $i && $v[$i] eq $ENV{_PCL_RUNTIME_}) ? "aligned" : "MISALIGNED")), "\n";',
    "same,aligned\n");

# ── (3) A REAL PERL CHILD DOES NOT SEE IT — the row the design exists for ───
test_cl('a real-perl child does NOT inherit it',
    'my $rc = system($^X, "-e", q{exit(exists $ENV{_PCL_RUNTIME_} ? 1 : 0)});'
  . 'print +($rc >> 8), "\n";', "0\n");

test_cl('…and it is absent from `env` output too',
    'my $out = `env`; print +(($out =~ /_PCL_RUNTIME_/) ? "leaked" : "clean"), "\n";',
    "clean\n");

# ── (4) A PCL CHILD SETS ITS OWN ───────────────────────────────────────────
{
    my ($cfh, $child) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $cfh qq{print(((\$ENV{_PCL_RUNTIME_} // "NONE") ne "NONE") ? "yes" : "no");\n};
    close $cfh;
    test_cl('a PCL child sets its OWN',
        qq{my \$out = `$project_root/runpcl $child 2>/dev/null`;\n}
      . qq{\$out =~ s/\\s+\\z//; print "\$out\\n";},
        "yes\n");
}

# ── (5) delete removes it for the process; assignment is an ordinary write ──
test_cl('delete removes it for the rest of the process',
    'delete $ENV{_PCL_RUNTIME_};'
  . 'print join(",", (defined $ENV{_PCL_RUNTIME_} ? "still" : "gone"),'
  . '  (exists $ENV{_PCL_RUNTIME_} ? "still" : "gone"),'
  . '  (scalar(grep { $_ eq "_PCL_RUNTIME_" } keys %ENV) ? "still" : "gone")), "\n";',
    "gone,gone,gone\n");

# An assignment goes through the ordinary %ENV write, so the value reads back
# AND — like every other %ENV write in perl — a child does inherit that one.
test_cl('assignment is an ordinary %ENV write, and a child inherits THAT',
    '$ENV{_PCL_RUNTIME_} = "custom";'
  . 'my $rc = system($^X, "-e", q{exit((($ENV{_PCL_RUNTIME_} // "") eq "custom") ? 1 : 0)});'
  . 'print join(",", $ENV{_PCL_RUNTIME_}, ($rc >> 8)), "\n";',
    "custom,1\n");

# ── scalar(%ENV) is the key COUNT (task #1392, s473h) ──────────────────────
# The MARKER *is* the hash for %ENV / %INC (task #736), and p-scalar had no arm
# for it, so the symbol fell to the catch-all and `scalar(%ENV)` printed
# HASH(0x1) where perl prints the count.  PRE-EXISTING; found while wiring
# #1529, and it is the same accessor family.

test_cl('scalar(%ENV) is the key count, and agrees with keys',
    'my $k = scalar(keys %ENV); my $s = scalar(%ENV);'
  . 'print +($k == $s ? "same" : "DIFFERENT($k/$s)"), "\n";',
    "same\n");

test_cl('scalar(%INC) is the key count too',
    'require Cwd; my $k = scalar(keys %INC); my $s = scalar(%INC);'
  . 'print +($k == $s && $k > 0 ? "same" : "DIFFERENT($k/$s)"), "\n";',
    "same\n");

test_cl('scalar(\\%ENV) is still the REFERENCE, not a count',
    'my $r = \%ENV; print ref(scalar($r)), "\n";', "HASH\n");
