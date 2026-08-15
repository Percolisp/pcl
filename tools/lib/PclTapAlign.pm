# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

package PclTapAlign;
# Pair two TAP streams (perl's and PCL's) BY DESCRIPTION, not by test number.
#
# WHY THIS EXISTS (task #177, found s321).  The suite runner used to join the
# two streams on the test NUMBER.  That silently mis-attributes every row after
# a count divergence, and the divergence is not exotic: op/do.t asserts that
# `do subname()` is a syntax error, and guards each assertion with a `fail()`
# that fires if the sub gets called.  PCL *does* call it (the blessed
# principle-9 divergence), so PCL emits two EXTRA rows and its numbering runs
# +2 ahead of perl's from there on.  The number join then accused t67 ("result
# of delete(helem) is copied") and t70 ("$@ is false on do dir") of failing —
# both PASS — while crediting two rows that don't.  A gate that manufactures
# failures burns triage sessions exactly like one that hides them.
#
# CONSERVATIVE BY CONSTRUCTION.  align_taps only re-syncs on positive
# evidence: an exact description match found ahead within a small window,
# CONFIRMED by a second, independent match for the following perl row.  Rows
# with empty descriptions, and rows whose description interpolates the compared
# value (which legitimately differs between perl and PCL exactly when a test
# fails), find no match and fall through to plain positional pairing — i.e.
# precisely the old behaviour.  It never invents a pairing it cannot evidence.
use strict;
use warnings;
use Exporter 'import';
our @EXPORT_OK = qw(tap_rows align_taps);

our $WINDOW = 20;   # how far ahead a re-sync may look (and be confirmed)

# TAP text -> ordered [ {verb, num, desc}, ... ].
# Horizontal whitespace ONLY after the number: `\s` there matches the newline
# of a description-less line ("ok 3\n"), swallowing the NEXT TAP line as the
# description and reporting its number as (missing) — 421 phantom rows on
# op/signatures.t (s316o).  Keep [ \t].
sub tap_rows {
  my ($out) = @_;
  my @r;
  while ($out =~ /^(not ok|ok)[ \t]+(\d+)[ \t]*[-#]?[ \t]*([^\n]*)$/mg) {
    push @r, { verb => $1, num => $2, desc => $3 };
  }
  return \@r;
}

sub _find {                       # first index in [$from,$to] whose desc eq $desc
  my ($rows, $from, $to, $desc) = @_;
  return undef if !defined $desc || !length $desc;
  $to = $#$rows if $to > $#$rows;
  for my $k ($from .. $to) { return $k if $rows->[$k]{desc} eq $desc }
  return undef;
}

# A re-sync needs a SECOND, independent description match: perl's next row must
# also turn up after the candidate.  Deliberately "somewhere in the window" and
# not "immediately next" — extras arrive in runs (do.t interleaves two), and an
# adjacency rule mis-pairs the first row of such a run.
sub _confirms {
  my ($prows, $pi, $crows, $ck) = @_;
  return 1 if $pi + 1 > $#$prows || $ck + 1 > $#$crows;
  my $next = $prows->[$pi + 1]{desc};
  return 1 if !length $next;
  return defined _find($crows, $ck + 1, $ck + $WINDOW, $next);
}

# (\@pairs, \@extras): pairs are [perl_row, pcl_row_or_undef] in perl order;
# extras are PCL-only rows (evidence in their own right — do.t's two extras
# ARE the principle-9 divergence firing).
sub align_taps {
  my ($prows, $crows) = @_;
  my (@pairs, @extras);
  my ($i, $j) = (0, 0);
  while ($i <= $#$prows) {
    my $p = $prows->[$i];
    if ($j > $#$crows) { push @pairs, [$p, undef]; $i++; next }
    my $c = $crows->[$j];
    if (!length $p->{desc} || !length $c->{desc} || $p->{desc} eq $c->{desc}) {
      push @pairs, [$p, $c]; $i++; $j++; next;
    }
    if (defined(my $k = _find($crows, $j + 1, $j + $WINDOW, $p->{desc}))) {
      if (_confirms($prows, $i, $crows, $k)) {          # PCL emitted extras
        push @extras, @{$crows}[$j .. $k - 1];
        push @pairs, [$p, $crows->[$k]];
        $i++; $j = $k + 1; next;
      }
    }
    if (defined(my $k = _find($prows, $i + 1, $i + $WINDOW, $c->{desc}))) {
      if (_confirms($prows, $k, $crows, $j)) {          # PCL skipped rows
        push @pairs, [$_, undef] for @{$prows}[$i .. $k - 1];
        push @pairs, [$prows->[$k], $c];
        $i = $k + 1; $j++; next;
      }
    }
    push @pairs, [$p, $c]; $i++; $j++;                  # no evidence: as-is
  }
  push @extras, @{$crows}[$j .. $#$crows] if $j <= $#$crows;
  return (\@pairs, \@extras);
}

1;
