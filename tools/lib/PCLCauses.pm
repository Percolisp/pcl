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
our @EXPORT_OK = qw(has_cause causes_line read_fail_rows fail_row_line
                    cause_class class_census census_line CLASSES
                    parse_cause class_group $NS_ID_RX baseline_causes);

# ── THE CLASS RULE (Fable ruling, s486a — recorded verbatim in docs/DECIDED.md
# under `## s486a`, explained in docs/failure-cause-classes.md) ──────────────
#
# A blessed row (failing, diverging, or never produced) falls into EXACTLY ONE
# class, decided from its CAUSE text by ONE function, `PCLCauses::cause_class`:
#
# 1. **`not-supported`** — the cause names a `docs/not-supported.md` section: an
#    `NS:` anchor ANYWHERE in the text, or the literal `not-supported.md` citation
#    (the spelling `baselines/perl-suite-expected.tsv` uses).  A row that ALSO
#    names a task is still not-supported: with every filed bug fixed PCL would
#    still fail it; the task owns the residue, not the row.
# 2. **`parked`** — `PARKED:` — a USER scheduling decision (pack/unpack today).
#    Reported beside not-supported, never folded into it.
# 3. **`bug`** — a task number (`#NNNN`) and nothing of the above: the queue.
# 4. **`unexplained`** — no cause, or `UNEXPLAINED…` (= `has_cause` false).
# 5. **`perl-skip`** — `PERL-SKIP` (the board): perl skips the file too, so it is
#    not a PCL failure; shown, and EXCLUDED from the share's denominator.
# 6. **`other`** — has a cause matching none of the above (today: `DECIDED "PCL
#    has no PVBM"` ×7, `PCL does not model use strict refs` ×2, the s473t1
#    shortfall notes).  A HYGIENE list the tool prints; it is expected to trend to
#    zero, because a DECIDED divergence must carry the `NS:` section it rests on.
#
# The order of the tests IS the rule: not-supported wins over a task number,
# because "with every filed bug fixed PCL would still fail this row".  Do not
# reorder without re-reading that sentence.
our @CLASSES = qw(not-supported parked bug other unexplained perl-skip);

# ── THE GRAMMAR (s531b, Fable ruling s531 #1; docs/failure-cause-classes.md
# "The grammar") ─────────────────────────────────────────────────────────────
#
# A cause is a LEADING MARKER with its KEY, then a free-text NOTE:
#
#   #N ...            bug          keyed on task N (the spelling since #993)
#   NS:<id> ...       not-supported keyed on a docs/not-supported.md id
#                                  (`<a id="ns-..."></a>`, tools/ns-ids.pl)
#   PARKED:<id> ...   parked       keyed on a not-supported id or a DECIDED
#                                  section label (`s485`)
#   PERL-SKIP ...     perl-skip    perl skips the file / row too
#   PERL-FAILS ...    perl-fails   perl fails or TODO-fails the row too
#   UNREDUCED #N ...  unreduced    ours, not isolated yet; N holds the hunt
#   HARNESS #N ...    harness      the board tooling, not PCL
#   UNEXPLAINED ...   unexplained  the legal first-bless marker (#993)
#
# Everything after the key is the NOTE, and the note is never classified:
# that is what makes the key checkable (Pl/t/verdict-register-01.t).
#
# A cell written BEFORE the grammar -- a task-first cell naming a ruling
# (`#930 (NS:Lvalue ...)`), an `NS:` followed by a section TITLE, a
# marker-less cell, `PARKED: free text`, the `not-supported.md §...` spelling
# -- is read by s486a's rule exactly as before (so no population's census
# moves) and comes back with LEGACY set: it is not in the grammar.  The
# validator calls a legacy cell red in a GATE population and counts it in a
# REPORT one; tools/migrate-causes.pl rewrites the board's.
our @GRAMMAR_CLASSES = qw(perl-fails unreduced harness);   # beyond s486a's six

# A not-supported id, as tools/ns-ids.pl reads it from the anchors.
our $NS_ID_RX = qr/ns-[a-z0-9]+(?:-[a-z0-9]+)*/;
# A DECIDED section label: `## s485`, `## s486a`, `## s527` ...
my $DECIDED_RX = qr/s\d+[a-z0-9]*/;

# What each class counts as in the board's split (ruling 6).
my %GROUP = ('bug' => 'ours', 'unreduced' => 'ours',
             'not-supported' => 'ruled', 'parked' => 'ruled',
             'perl-skip' => "perl's", 'perl-fails' => "perl's",
             'harness' => 'harness',
             'other' => 'unread', 'unexplained' => 'unread');
sub class_group { return $GROUP{ $_[0] // '' } // 'unread' }

# s486a's rule, verbatim, for a cell that is not in the grammar.
sub _s486a_class {
    my ($cause) = @_;
    return 'not-supported' if $cause =~ /NS:/ || $cause =~ /not-supported\.md/;
    return 'parked'        if $cause =~ /PARKED:/;
    return 'perl-skip'     if $cause =~ /PERL-SKIP/;
    return 'bug'           if $cause =~ /#\d+/;
    return 'other';
}

# parse_cause($cause) -> ($class, $key, $note, $legacy)
#   $key    the marker's key (a task number, an ns- id, a DECIDED label), or
#           undef for the keyless markers and for every legacy cell
#   $note   the text after the key (leading separators kept as written)
#   $legacy 1 when the cell is not in the grammar (read by s486a's rule)
sub parse_cause {
    my ($cause) = @_;
    if (!has_cause($cause)) {
        return ('unexplained', undef, $1, 0)
            if defined $cause && $cause =~ /^UNEXPLAINED(.*)\z/s;
        return ('unexplained', undef, $cause // '', 1);   # no cause at all
    }
    my $c = $cause;
    return ('not-supported', $1, $2, 0) if $c =~ /^NS:($NS_ID_RX)(?![\w-])(.*)\z/s;
    return ('parked',        $1, $2, 0) if $c =~ /^PARKED:($NS_ID_RX|$DECIDED_RX)(?![\w-])(.*)\z/s;
    return ('perl-skip',  undef, $1, 0) if $c =~ /^PERL-SKIP(?![\w-])(.*)\z/s;
    return ('perl-fails', undef, $1, 0) if $c =~ /^PERL-FAILS(?![\w-])(.*)\z/s;
    return ('unreduced',     $1, $2, 0) if $c =~ /^UNREDUCED\s+#(\d+)(?!\d)(.*)\z/s;
    return ('harness',       $1, $2, 0) if $c =~ /^HARNESS\s+#(\d+)(?!\d)(.*)\z/s;
    if ($c =~ /^#(\d+)(?!\d)(.*)\z/s) {
        my ($n, $note) = ($1, $2);
        my $old = _s486a_class($c);
        # s486a's order for a cell naming a ruling AND a task: the ruling is
        # the class.  Written task-first, that is the pre-grammar spelling.
        return ($old, undef, $c, 1) if $old ne 'bug';
        return ('bug', $n, $note, 0);
    }
    # Not in the grammar.  An `NS:` + section TITLE keeps the title as the
    # note, so the migration can map it to an id without reading the cell a
    # second way.
    return ('not-supported', undef, $1, 1) if $c =~ /^NS:\s*(.*)\z/s;
    return (_s486a_class($c), undef, $c, 1);
}

# A cause is present when it is a non-blank string that is not the UNEXPLAINED
# marker.  Called per row by the blessers as well as by the report, so the
# definition of "attributed" cannot drift between counting and writing.
sub has_cause {
    my ($cause) = @_;
    return 0 unless defined $cause && $cause =~ /\S/;
    return 0 if $cause =~ /^UNEXPLAINED/;
    return 1;
}

# THE RULE, as code: parse_cause's class.  `has_cause` stays the definition of
# "attributed", so `unexplained` cannot drift away from the count the three
# runners print; a legacy cell keeps s486a's class (not-supported FIRST: a row
# naming both a section and a task is not-supported — fixing every filed bug
# would not make it pass).
sub cause_class { return (parse_cause($_[0]))[0] }

# \@causes -> { class => count }: s486a's six classes always (a zero is a
# measurement, an absent key is not), the grammar's three only where they
# occur -- today only the board can spell them (the companion measures perl's
# verdict as a column of its own, ruling 8).
sub class_census {
    my ($causes) = @_;
    my %n = map { ($_ => 0) } @CLASSES;
    $n{ cause_class($_) }++ for @$causes;
    return \%n;
}

sub _commify { my $n = reverse shift; $n =~ s/(\d{3})(?=\d)/$1,/g; return scalar reverse $n }

# THE ONE formatter for the split.  `$label` is the line's prefix (every runner
# says CAUSES).  perl-skip is printed only when it occurs, because it exists in
# exactly one population (the board) and a permanent ", perl-skip 0" would make
# the other three lines wider for nothing.
sub census_line {
    my ($label, $n) = @_;
    my $total = 0; $total += $n->{$_} // 0 for @CLASSES, @GRAMMAR_CLASSES;
    my $attributed = $total - ($n->{unexplained} // 0);
    my @parts = map { sprintf "%s %s", $_, _commify($n->{$_} // 0) }
                    qw(not-supported parked bug other);
    # The grammar's three and perl-skip print only where they occur.
    # UNREDUCED in particular is always on the line when there is one: it is
    # the class that must not become a dumping ground (ruling 1).
    for my $k ('unreduced', 'perl-skip', 'perl-fails', 'harness') {
        push @parts, sprintf("%s %s", $k, _commify($n->{$k})) if $n->{$k};
    }
    push @parts, sprintf("unexplained %s", _commify($n->{unexplained} // 0));
    return sprintf("%s: %s of %s — %s\n", $label, _commify($attributed),
                   _commify($total), join(', ', @parts));
}

# ── the four blessed row baselines, read for their CAUSE ───────────────────
# ONE table of where each baseline keeps its cause, shared by
# tools/cause-census.pl and the gate row Pl/t/verdict-register-01.t, so the
# column is located in one place as well as read by one function (rule 11).
#   board      dist, t-file, num, description, got, expected, CAUSE
#   companion  rel, perl#, perl verb, PCL verb, rowkey, CAUSE
#   sweep      file, num, description, got, expected, CAUSE
#   shortfall  key, count, CAUSE                 (weighted by its count)
my %BASELINE = (
    board     => { n => 7, need => 3, key => sub { "$_[0] $_[1]\t" . (length $_[3] ? $_[3] : "#$_[2]") } },
    companion => { n => 6, need => 4, key => sub { "$_[0]\t$_[4]" } },
    sweep     => { n => 6, need => 2, key => sub { "$_[0]\t$_[2]" } },
    shortfall => { n => 3, need => 1, key => sub { $_[0] } },
);

# baseline_causes($path, $format) -> [ { key, cause, fields => [...], line } ]
# or undef when the file cannot be read (a missing baseline is NO
# information, never a clean zero).  `need` is the index of the last field a
# row must have; a shortfall row also needs a numeric count.
sub baseline_causes {
    my ($path, $format) = @_;
    my $f = $BASELINE{$format} or die "baseline_causes: unknown format $format\n";
    open my $fh, '<:raw', $path or return undef;
    my @rows;
    while (my $line = <$fh>) {
        next if $line =~ /^\s*#/ || $line !~ /\S/;
        chomp $line;
        my @v = split /\t/, $line, $f->{n};
        next unless defined $v[ $f->{need} ];
        next if $format eq 'shortfall' && $v[1] !~ /^\d+$/;
        push @rows, { key => $f->{key}->(@v), cause => $v[ $f->{n} - 1 ],
                      fields => \@v, line => $. };
    }
    close $fh;
    return \@rows;
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
#   * rows, some attributed   -> the SPLIT by class (s486a), which carries the
#                                unattributed count as `unexplained`
# The third answer is `census_line`, so the three runners print the class split
# without any of them holding a second reading of the CAUSE column (rule 11):
# "how many of the failures are from what we don't support?" is answerable from
# any run, not only from tools/cause-census.pl.
sub causes_line {
    my ($causes, $path) = @_;
    my ($have, $none) = (0, 0);
    for my $c (@$causes) { has_cause($c) ? $have++ : $none++ }
    return "CAUSES: NOT CHECKED — the baseline has no rows\n" if !$have && !$none;
    return sprintf("CAUSES: NOT CHECKED — no cause column in %s"
                   . " (add one: docs/plan-test-audit-s464.md §3 I3)\n",
                   defined $path ? $path : '(unnamed baseline)')
        if !$have;
    return census_line('CAUSES', class_census($causes))
         . sprintf("  (unexplained = %d cause-less blessed row(s): QUEUE, not"
                   . " baseline — #993; classes: docs/failure-cause-classes.md)\n",
                   $none);
}

1;
