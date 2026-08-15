#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# license-tag-01.t — every PCL code file carries the license tag (USER, s401:
# "License — same as Perl.  Tag all code files."), and every file EXCLUDED
# by name still exists, so an exclusion cannot outlive what it excuses.
#
# The definition of "code file", the tag text and the exclusions live in ONE
# place, tools/lib/PCLLicense.pm (shared with tools/tag-license, which fixes
# what this file reports).  Files from the Perl distribution or CPAN
# (perl-tests/, cpan-tests/, the two lib/ carry-overs) are NOT ours to tag
# (USER, s401) — they are excluded there, with reasons.
#
# One row per scanned root rather than one per file, so the gate's row count
# does not swing with the file count; offenders are listed in the diag, with
# the command that fixes them.  No SBCL — this file costs nothing.
use strict;
use warnings;
use FindBin;
use lib "$FindBin::Bin/../../tools/lib";
use Test::More;
use PCLLicense;

my $root = "$FindBin::Bin/../..";
my ($files, $excl_seen) = PCLLicense::code_files($root);

# group by root (Pl/, cl/, …; root-level scripts under "(root)")
my %by;
for my $rel (@$files) {
  my $grp = $rel =~ m{^([^/]+)/} ? $1 : '(root)';
  $grp = '.claude/hooks' if $rel =~ m{^\.claude/hooks/};
  push @{ $by{$grp} }, $rel;
}

plan tests => scalar(keys %by) + 3;

for my $grp (sort keys %by) {
  my @missing = grep { !PCLLicense::has_tag("$root/$_") } @{ $by{$grp} };
  ok(!@missing, sprintf("%s: all %d code files carry the license tag", $grp, scalar @{ $by{$grp} }))
    or diag("missing the tag (run: tools/tag-license --all):\n  " . join("\n  ", @missing));
}

# Every named exclusion must still exist — a stale exclusion is a lie.
my @gone = grep { !-f "$root/$_" } sort keys %PCLLicense::EXCLUDE;
ok(!@gone, sprintf("all %d named exclusions still exist", scalar keys %PCLLicense::EXCLUDE))
  or diag("excluded but no longer present — delete from PCLLicense::EXCLUDE:\n  " . join("\n  ", @gone));

# …and every exclusion was actually met by the scan (else it excuses nothing
# the scan would have found — a moved file, or a wrong path).
my @unmet = grep { !$excl_seen->{$_} } sort keys %PCLLicense::EXCLUDE;
ok(!@unmet, "every named exclusion was met by the scan")
  or diag("exclusions the scan never reached:\n  " . join("\n  ", @unmet));

# Sanity: the scan saw a plausible number of files (a broken walker that finds
# 3 files would pass every row above vacuously).
cmp_ok(scalar @$files, '>=', 200, "the scan reached a plausible number of code files (" . scalar(@$files) . ")");
