package PCLTap;
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# PCLTap — the ONE reader of a TAP stream for the CPAN-board runners
# (tools/run-dist-t.pl, tools/cpan-scoreboard.pl).  Task #1502.
#
# WHY IT EXISTS: the board counted `ok`/`not ok` with two inline regexes in
# run-dist-t.pl and threw the stream away, so a t-file's verdict was a pair of
# integers with no row behind it — a file could lose a row and gain a row and
# read unchanged, and no row had a cause (#1502).  The counts and the rows must
# come from ONE parse or they will drift (rule 11).
#
# THE COUNTS ARE THE HISTORICAL ONES, deliberately: `ok` is a line matching
# /^ok \d+/ and `not ok` a line matching /^not ok \d+/, exactly as
# run-dist-t.pl matched them before this module existed.  A TODO/SKIP directive
# does NOT change the count — it is recorded on the row instead — because every
# blessed board snapshot was measured under the old rule and a counting change
# would move rows for a tool reason.
#
# parse_tap($text) -> {
#     ok       => N,               # rows matching /^ok \d+/
#     notok    => N,               # rows matching /^not ok \d+/
#     plan     => N | undef,       # from `1..N`
#     skip_all => TEXT | undef,    # from `1..0 # SKIP reason` (reason may be '')
#     rows     => [ { num, desc, got, expected, directive } ],   # FAILING rows only
#   }
#
# `desc` is Test::More's description — the JOIN KEY (docs/tap-assertion-audit.md),
# not the test number, which drifts whenever PCL's row count diverges from
# perl's.  An unnamed row gets the empty description, and #1041's rule then
# makes one key stand for all of a file's unnamed rows.
#
# `got`/`expected` come from the `#   got: …` / `# expected: …` diagnostics that
# follow a failing row (PCL's TAP layer, cl/pcl-test.lisp, and Test::More both
# write them).  Tabs and newlines are squeezed out: the row file is a TSV.

use strict;
use warnings;
use Exporter 'import';
our @EXPORT_OK = qw(parse_tap tsv_clean);

# A TSV field may not carry a tab or a newline, and a row that did would
# silently shift every column to its right.
sub tsv_clean {
    my ($s) = @_;
    return '' unless defined $s;
    $s =~ s/[\t\r\n]+/ /g;
    $s =~ s/\s+$//;
    return $s;
}

sub parse_tap {
    my ($text) = @_;
    my %r = (ok => 0, notok => 0, plan => undef, skip_all => undef, rows => []);
    return \%r unless defined $text;
    my $cur;    # the failing row currently collecting diagnostics
    for my $line (split /\n/, $text, -1) {
        if ($line =~ /^1\.\.(\d+)\s*(?:#\s*(?i:skip)\s*(.*))?$/) {
            $r{plan} = $1;
            $r{skip_all} = defined $2 ? $2 : '' if defined $2 || $1 == 0;
            $cur = undef;
            next;
        }
        if ($line =~ /^ok \d+/) { $cur = undef; $r{ok}++; next }
        if ($line =~ /^not ok (\d+)(.*)$/) {
            my ($num, $rest) = ($1, $2);
            $r{notok}++;
            my $directive = '';
            $directive = $1 if $rest =~ s/\s*#\s*((?i:todo|skip)\b.*)$//;
            $rest =~ s/^\s*-?\s*//;
            $cur = { num => $num, desc => tsv_clean($rest),
                     got => '', expected => '', directive => tsv_clean($directive) };
            push @{ $r{rows} }, $cur;
            next;
        }
        next unless $cur && $line =~ /^#/;
        # Diagnostics belonging to the row just seen.  `Failed test 'name'`
        # supplies the description when the row itself was unnamed.
        # `expected` is not always the bare word: `like` writes
        # "expected to match:" and the regex asserts write "expected: usable
        # regex …", so the qualifier is kept with the value rather than thrown
        # away — a row whose expected side reads '' is a row nobody can triage.
        if ($line =~ /^#\s*(got|expected)([^:]*):\s*(.*)$/i) {
            my ($field, $qual, $val) = (lc $1, $2, $3);
            $qual =~ s/^\s+|\s+$//g;
            $cur->{$field} = tsv_clean(length $qual ? "$qual: $val" : $val);
        }
        elsif (!length $cur->{desc}
               && $line =~ /^#\s*Failed test\s+'(.*)'\s*$/) { $cur->{desc}     = tsv_clean($1) }
    }
    return \%r;
}

1;
