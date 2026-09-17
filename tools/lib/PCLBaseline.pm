package PCLBaseline;
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# PCLBaseline — SPLICE a line-oriented baseline instead of rewriting it
# (task #1835).
#
# THE PROBLEM IT SOLVES.  Every blessed baseline in this tree is edited ROW BY
# ROW and reviewed as a `git diff`: that is the standing rule, and it is the
# only thing that makes "a row leaves by EDIT, with its cause" enforceable.
# The bless FLAGS (`--bless-fails`, `--bless-shortfall`, `sweep-diff.pl
# save-shortfall`) exist for the audited case — but they regenerated the WHOLE
# file even when the run had measured ONE test file, so a one-file bless
# produced a 4,691-line diff (every file's block re-sorted, the hand-written
# header replaced by the generator's, every zero-shortfall row dropped with the
# note recording WHY it closed).  A diff that large cannot be read, so the rule
# it was supposed to serve stopped being enforceable.
#
# THE CONTRACT.  A bless rewrites only the blocks of the keys the run MEASURED:
#   * every other data line is emitted BYTE-IDENTICALLY, in its original place;
#   * comment lines are emitted verbatim, in place (the hand-written header and
#     its per-session notes are the baseline's history — a generator may add to
#     it, never replace it);
#   * a measured key's block is rewritten AT THE POSITION of its first old line,
#     so block order never changes;
#   * inside a rewritten block, a row that is still there keeps its POSITION
#     (joined on the row's own text), and genuinely new rows are appended — so
#     the diff of a re-measured file is the rows that moved, not the block;
#   * a key the run measured into nothing disappears; a key the file did not
#     have is inserted at its sorted position among the existing keys.
# A file that does not exist yet is written whole from `header` — the first
# bless of a new baseline is still a generated file.

use strict;
use warnings;
use Exporter 'import';
our @EXPORT_OK = qw(splice_blocks);

# splice_blocks(%opt) -> (path, added, removed, blocks_rewritten)
#   path     the baseline file (read and rewritten in place)
#   blocks   key -> arrayref of complete LINES ("...\n") for measured keys
#   touched  key -> true for every key this run MEASURED.  A touched key with
#            no entry in `blocks` (or an empty one) is REMOVED; an untouched
#            key is copied through whatever `blocks` says.
#   key_of   coderef: a data line -> its key (default: the first TAB field)
#   keep     coderef: a data line -> true if it must survive a bless verbatim
#            (the shortfall's hand-placed zero rows; optional)
#   header   the generated header text, used ONLY when the file is absent
#   stamp    one comment line appended at the end of the leading comment block
sub splice_blocks {
    my (%opt) = @_;
    my $path    = $opt{path}    or die "splice_blocks: path required\n";
    my $blocks  = $opt{blocks}  || {};
    my $touched = $opt{touched} || { map { $_ => 1 } keys %$blocks };
    my $key_of  = $opt{key_of}  || sub { my ($l) = @_; my ($k) = split /\t/, $l, 2; return $k };
    my $keep    = $opt{keep};

    my @old;
    if (open my $in, '<', $path) { @old = <$in>; close $in }

    return _write_fresh($path, \%opt, $blocks) if !@old;

    # Pass 1: the OLD lines of every key, in order, so a rewritten block can
    # keep the rows that are still there where they were.
    my %old_lines;
    for my $l (@old) {
        next if $l =~ /^\s*(?:#|$)/;
        push @{ $old_lines{ $key_of->($l) } }, $l;
    }

    my (@out, %done, $in_header, $stamped);
    $in_header = 1;
    for my $l (@old) {
        if ($l =~ /^\s*(?:#|$)/) {
            push @out, $l;
            next;
        }
        if ($in_header) {                      # the leading comment block ends here
            push @out, $opt{stamp} if defined $opt{stamp} && !$stamped++;
            $in_header = 0;
        }
        my $k = $key_of->($l);
        if ($keep && $keep->($l)) { push @out, $l; $done{$k} = 1; next }
        if (!$touched->{$k})      { push @out, $l; next }
        next if $done{$k}++;                   # later lines of a rewritten block
        push @out, @{ _merge_block($old_lines{$k}, $blocks->{$k} || []) };
    }
    push @out, $opt{stamp} if defined $opt{stamp} && !$stamped++;   # data-less file

    # Keys the file did not have go in at their sorted position among the keys
    # it does, so a near-sorted baseline stays sorted and nothing else moves.
    for my $k (sort keys %$blocks) {
        next if exists $old_lines{$k} || !@{ $blocks->{$k} };
        @out = @{ _insert_sorted(\@out, $k, $blocks->{$k}, $key_of) };
    }

    open my $out, '>', $path or die "write $path: $!\n";
    print $out @out;
    close $out;
    return $path;
}

# A rewritten block, in minimum-diff order: every new line that is also an old
# line stays at the old line's position, the rest are appended in the order the
# blesser produced them.
sub _merge_block {
    my ($old, $new) = @_;
    my %want;
    $want{$_}++ for @$new;
    my @kept;
    for my $l (@{ $old || [] }) {
        next unless $want{$l};
        $want{$l}--;
        push @kept, $l;
    }
    my %seen_kept;
    $seen_kept{$_}++ for @kept;
    my @added;
    for my $l (@$new) {
        if ($seen_kept{$l}) { $seen_kept{$l}--; next }
        push @added, $l;
    }
    return [ @kept, @added ];
}

sub _insert_sorted {
    my ($out, $key, $lines, $key_of) = @_;
    my @new;
    my $placed = 0;
    for my $l (@$out) {
        if (!$placed && $l !~ /^\s*(?:#|$)/ && ($key_of->($l) cmp $key) > 0) {
            push @new, @$lines;
            $placed = 1;
        }
        push @new, $l;
    }
    push @new, @$lines if !$placed;
    return \@new;
}

sub _write_fresh {
    my ($path, $opt, $blocks) = @_;
    open my $out, '>', $path or die "write $path: $!\n";
    print $out $opt->{header} if defined $opt->{header};
    print $out $opt->{stamp}  if defined $opt->{stamp};
    print $out @{ $blocks->{$_} } for sort keys %$blocks;
    close $out;
    return $path;
}

1;
