package PCLCauses;
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# PCLCauses — the CAUSE column's ONE reading, shared by every runner that
# blesses failing rows (task #993 / docs/plan-test-audit-s464.md §3 I3).
#
# THE RULE: a blessed failing row carries a CAUSE — a task number (`#1234`),
# a docs/not-supported.md anchor (`NS:<section>`), a parking note
# (`PARKED: …`), a catalogue pointer (`CATALOG …`) — and a cause-less row is
# QUEUE, not baseline.  Every run prints how many rows have no cause, for the
# same reason the UNEXPLAINED suite verdicts are counted rather than inferred
# from an absence: a queue that is not counted grows silently.
#
# `UNEXPLAINED` is a legal cause at the first bless and is what the queue is
# MADE of, so it counts as NO cause here — that is the whole point of the
# spelling.
#
# WHY IT IS A MODULE: three runners bless failing rows against three
# populations — tools/sweep-diff.pl (perl-tests/), tools/run-perl-suite.pl
# (perl's own t/) and tools/cpan-scoreboard.pl (the CPAN board) — and the line
# was written twice with two different behaviours: the sweep says NOT CHECKED
# when a baseline has no cause column at all, the board reported "N of N have
# no cause" for the same state, which reads as a queue of N rather than as a
# file that predates the column.  One reading, three askers (CLAUDE.md rule 11).

use strict;
use warnings;
use Exporter 'import';
our @EXPORT_OK = qw(has_cause causes_line read_fail_rows fail_row_line);

# A cause is present when it is a non-blank string that is not the UNEXPLAINED
# marker.  Called per row by the blessers as well as by the report, so the
# definition of "attributed" cannot drift between counting and writing.
sub has_cause {
    my ($cause) = @_;
    return 0 unless defined $cause && $cause =~ /\S/;
    return 0 if $cause =~ /^UNEXPLAINED/;
    return 1;
}

# ── the companion's ROW-level fail baseline (baselines/perl-suite-fails.tsv) ──
# The SIX-field parse and its inverse, together, because they are one format:
#
#   <rel> TAB <PERL's test#> TAB <perl verb> TAB <PCL verb> TAB <rowkey> [TAB <cause>]
#
# The rowkey may not contain a TAB — it is a field with a column after it, and
# PclTapAlign::rowkey_desc (the ONE key projection) normalizes one away.  A row
# with no cause is written with FIVE fields, so an unattributed row is
# byte-identical to the pre-column file and the column's arrival moved nothing.
#
# Returns (\%meta, \%cause):
#   %meta   rel  ->  [ [num, perl_verb, pcl_verb, rowkey, cause], ... ]
#   %cause  "rel\trowkey" -> cause      (the join key of the ROW DIFF)
# A file's rows may repeat a description; a repeated description gets ONE
# cause, because the rows are the same assertion text and the same divergence.
sub read_fail_rows {
    my ($path) = @_;
    my (%meta, %cause);
    open my $fh, '<', $path or return (\%meta, \%cause);
    while (<$fh>) {
        chomp;
        next if /^\s*(?:#|$)/;
        my ($rel, $num, $pv, $cv, $key, $c) = split /\t/, $_, 6;
        next unless defined $key;
        push @{ $meta{$rel} }, [$num, $pv, $cv, $key, $c];
        $cause{"$rel\t$key"} = $c if defined $c && length $c;
    }
    close $fh;
    return (\%meta, \%cause);
}

sub fail_row_line {
    my ($rel, $r) = @_;
    my @f = ($rel, @$r[0 .. 3]);
    push @f, $r->[4] if defined $r->[4] && length $r->[4];
    return join("\t", @f) . "\n";
}

# The report line, given every blessed row's cause (undef for a row that has
# no cause field at all) and the baseline's path for the NOT-CHECKED message.
# Three answers, because "0 of 0" and "N of N" are different facts:
#   * no rows at all          -> the baseline is empty, nothing to attribute
#   * rows, none attributed   -> the file predates the column; say which file
#                                and where the column is specified
#   * rows, some attributed   -> the queue, counted
sub causes_line {
    my ($causes, $path) = @_;
    my ($have, $none) = (0, 0);
    for my $c (@$causes) { has_cause($c) ? $have++ : $none++ }
    return "CAUSES: NOT CHECKED — the baseline has no rows\n" if !$have && !$none;
    return sprintf("CAUSES: NOT CHECKED — no cause column in %s"
                   . " (add one: docs/plan-test-audit-s464.md §3 I3)\n",
                   defined $path ? $path : '(unnamed baseline)')
        if !$have;
    return sprintf("CAUSES: %d of %d blessed row(s) have no cause"
                   . " — a cause-less row is QUEUE, not baseline (#993)\n",
                   $none, $have + $none);
}

1;
