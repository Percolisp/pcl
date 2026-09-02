package PCLShortfall;
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# PCLShortfall — the ROW-SHORTFALL baseline, shared by BOTH measurement
# runners (task #993 / docs/plan-test-audit-s464.md §3 I2).
#
# WHAT A SHORTFALL IS: rows the population expected and PCL never produced.
# It is invisible to every other instrument, because the row simply is not
# there — the same blindness as the #138 drop census, one level up:
#
#   * the sweep's "OK" verdict means "no previously-passing row was lost", not
#     "the plan was produced", so perl-tests/pack.t read OK while 8,997 of its
#     14,722 planned rows never ran, and lc.t while 2,577 never ran;
#   * the companion's per-file counts are compared to PERL's, so a file that
#     dies before its first assertion is a DIFF with C 0/0 — one line, and the
#     2,031 rows behind the 10 TRANSPILE files are nowhere in any total.
#
# SHAPE: the drop census's, deliberately (baselines/parse-error-drop-census-s399.tsv).
# One row per file WITH A CAUSE; a row leaves BY EDIT when the shortfall is
# fixed; MORE shortfall than blessed fails the run like a NEW failure.
# `UNEXPLAINED` is a legal cause at the first bless and is what the audit's
# queue is made of — every run prints how many UNEXPLAINED rows remain, so the
# queue cannot silently grow.
#
# KEYS are population-prefixed exactly like the drop census, because ONE file
# holds both populations and a bare basename would collide (`t/op/sub.t` and
# `perl-tests/sub.t` are different files with different plans):
#
#   perl-tests/<name>    sweep         planned (1..N) minus (pass+fail+skip)
#   t/<rel>              companion     perl's TAP rows minus PCL's TAP rows
#
# The two definitions differ because the two runners have different oracles —
# the sweep has the file's own plan line, the companion has real perl's run —
# but they answer the same question and are never mixed in one row.

use strict;
use warnings;
use Exporter 'import';
our @EXPORT_OK = qw(read_shortfall write_shortfall shortfall_header);

# path -> { key => { rows => N, cause => TEXT } }.  A missing file is an empty
# hash, and every caller must read "no entry" as "no information", never as
# zero — the same rule as read_status_file's missing columns.
sub read_shortfall {
    my ($path) = @_;
    my %rows;
    return \%rows unless defined $path && -e $path;
    open my $fh, '<', $path or return \%rows;
    while (my $line = <$fh>) {
        chomp $line;
        next if !length $line || $line =~ /^\s*#/;
        my ($key, $n, $cause) = split /\t/, $line, 3;
        next unless defined $n && $n =~ /^\d+$/;
        $rows{$key} = { rows => $n, cause => (defined $cause && length $cause) ? $cause : 'UNEXPLAINED' };
    }
    close $fh;
    return \%rows;
}

sub shortfall_header {
    return <<'HDR';
# row-shortfall.tsv — rows the population expected and PCL never produced
# (task #993 / docs/plan-test-audit-s464.md §3 I2).  SHARED by both runners.
#
#   <key> <TAB> <rows never produced> <TAB> <cause>
#
# key = perl-tests/<name>  (sweep-perl-tests.pl:      planned - (pass+fail+skip))
#     = t/<rel>            (tools/run-perl-suite.pl:  perl's rows - PCL's rows)
#
# THE RULE, the drop census's: a row leaves BY EDIT when the shortfall is
# fixed; MORE shortfall than blessed fails the run like a NEW failure; a cause
# is a task number, a docs/not-supported.md anchor, or UNEXPLAINED — and
# UNEXPLAINED is the audit's queue, counted on every run so it cannot grow
# unnoticed.
#
# WHY IT EXISTS: "OK" used to mean "no previously-passing row was lost".
# perl-tests/pack.t is OK with 8,997 of 14,722 planned rows never produced;
# the companion's 10 TRANSPILE files are one line each and 2,031 rows behind
# them.  Neither is visible in any other bucket.
HDR
}

# Rewrite the whole file from a hash of the same shape.  BOTH populations must
# be present in the hash: a writer that only knows its own population must copy
# the other's rows through unchanged (both blessers do), or one bless silently
# erases the other runner's baseline.
sub write_shortfall {
    my ($path, $rows, $stamp) = @_;
    open my $out, '>', $path or die "write $path: $!\n";
    print $out shortfall_header();
    printf $out "# taken-at: %s\n", $stamp if defined $stamp;
    for my $key (sort keys %$rows) {
        my $r = $rows->{$key};
        next unless ($r->{rows} // 0) > 0;
        print $out join("\t", $key, $r->{rows}, $r->{cause} // 'UNEXPLAINED'), "\n";
    }
    close $out;
    return;
}

1;
