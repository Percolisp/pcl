#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# What a BLESS is allowed to touch (task #1835).
#
# The standing rule is that a baseline is edited ROW BY ROW and the edit is
# reviewed as a `git diff`.  The bless flags exist for the audited case, and
# they used to regenerate the WHOLE file for a run that had measured ONE test
# file: 4,691 changed lines, every block re-sorted, the hand-written header
# replaced, every zero-shortfall row dropped with the note recording why it
# closed.  A diff that size cannot be read, so the rule stopped being
# enforceable.  These rows are that contract, written down.
#
# NOT part of the Pl/t gate: it measures a measurement tool.  Run it directly:
#   prove tools/t/baseline-splice.t
use strict;
use warnings;
use Test::More;
use File::Temp qw(tempdir);
use FindBin qw($RealBin);
use lib "$RealBin/../lib";
use PCLBaseline qw(splice_blocks);
use PCLShortfall qw(read_shortfall write_shortfall);

my $dir = tempdir(CLEANUP => 1);

sub slurp { my ($p) = @_; open my $fh, '<', $p or die "read $p: $!"; local $/; return <$fh> }
sub spew  { my ($p, $t) = @_; open my $fh, '>', $p or die "write $p: $!"; print $fh $t; close $fh; return $p }

# ── a one-key bless rewrites ONE block and nothing else ─────────────────────
{
  my $p = spew("$dir/fails.tsv", <<'BASE');
# hand-written header line 1
#   with a second, indented line the generator would never produce
# taken-at: s000 2026-01-01
op/zzz.t	1	ok	not ok	last block first, because the file is NOT re-sorted
op/aaa.t	7	ok	not ok	row seven	#111
op/aaa.t	3	ok	not ok	row three	#111
op/bbb.t	1	ok	not ok	untouched	#222
BASE
  my $before = slurp($p);

  splice_blocks(path => $p,
                blocks  => { 'op/aaa.t' => [ "op/aaa.t\t7\tok\tnot ok\trow seven\t#111\n",
                                             "op/aaa.t\t9\tok\tnot ok\trow nine\t#111\n" ] },
                touched => { 'op/aaa.t' => 1 },
                header  => "# generated header\n");
  my $after = slurp($p);

  my @b = split /\n/, $before;
  my @a = split /\n/, $after;
  my %gone = map { $_ => 1 } grep { my $l = $_; !grep { $_ eq $l } @a } @b;
  my %new  = map { $_ => 1 } grep { my $l = $_; !grep { $_ eq $l } @b } @a;
  is_deeply([sort keys %gone], ["op/aaa.t\t3\tok\tnot ok\trow three\t#111"],
            'a one-file bless removes only that file\'s rows');
  is_deeply([sort keys %new], ["op/aaa.t\t9\tok\tnot ok\trow nine\t#111"],
            'and adds only that file\'s rows');
  like($after, qr/^# hand-written header line 1$/m,
       'the hand-written header survives — it is the baseline\'s history');
  like($after, qr/^#   with a second, indented line/m, 'every comment line of it');
  unlike($after, qr/# generated header/, 'and the generated header does NOT replace it');
  like($after, qr/op\/zzz\.t.*\n(?:op\/aaa\.t.*\n){2}op\/bbb\.t/,
       'block ORDER is the file\'s, not the generator\'s sort');
  like($after, qr/op\/aaa\.t\t7\tok\tnot ok\trow seven\t\#111\nop\/aaa\.t\t9\t/,
       'a row that is still there keeps its position; a new row is appended');
}

# ── a key the file does not have is inserted at its sorted position ─────────
{
  my $p = spew("$dir/insert.tsv", <<'BASE');
# header
op/aaa.t	1	ok	not ok	a
op/ccc.t	1	ok	not ok	c
BASE
  splice_blocks(path => $p,
                blocks  => { 'op/bbb.t' => [ "op/bbb.t\t1\tok\tnot ok\tb\n" ] },
                touched => { 'op/bbb.t' => 1 });
  like(slurp($p), qr/op\/aaa\.t.*\nop\/bbb\.t.*\nop\/ccc\.t/,
       'a new key lands between its neighbours, so a sorted baseline stays sorted');
}

# ── a measured key with no rows left disappears ─────────────────────────────
{
  my $p = spew("$dir/drop.tsv", <<'BASE');
# header
op/aaa.t	1	ok	not ok	a
op/bbb.t	1	ok	not ok	b
BASE
  splice_blocks(path => $p, blocks => { 'op/aaa.t' => [] }, touched => { 'op/aaa.t' => 1 });
  my $after = slurp($p);
  unlike($after, qr/op\/aaa\.t/, 'a file whose rows all got fixed loses its block');
  like($after, qr/op\/bbb\.t/,   'and the rest of the baseline is untouched');
}

# ── the shortfall: a hand-placed ZERO row survives any bless ────────────────
# `t/op/kvhslice.t 0 <TAB> #1024 FIXED (was 1)` records WHY a shortfall closed.
# A whole-file rewrite dropped every one of them (that is how kvhslice.t,
# multideref.t and uni/method.t nearly lost their notes in s487).
{
  my $p = spew("$dir/shortfall.tsv", <<'BASE');
# row-shortfall.tsv — hand-written header
# taken-at: s000 2026-01-01
perl-tests/pack.t	8997	#148
t/op/kvhslice.t	0	#1024 FIXED (was 1)
t/op/sub.t	12	UNEXPLAINED
BASE
  my $rows = read_shortfall($p);
  is(scalar(keys %$rows), 3, 'the reader sees the zero row (it is information)');
  # A run that measured ONLY t/op/sub.t, whose shortfall closed.
  delete $rows->{'t/op/sub.t'};
  write_shortfall($p, $rows, 's999 2026-01-02', { 't/op/sub.t' => 1 });
  my $after = slurp($p);
  like($after,   qr/^t\/op\/kvhslice\.t\t0\t#1024 FIXED \(was 1\)$/m,
       'the hand-placed zero row survives a bless VERBATIM');
  like($after,   qr/^perl-tests\/pack\.t\t8997\t#148$/m,
       'the other population is copied through, byte for byte');
  unlike($after, qr/t\/op\/sub\.t/, 'the measured file whose shortfall closed loses its row');
  like($after,   qr/^# row-shortfall\.tsv — hand-written header$/m,
       'the hand-written header survives');
  like($after,   qr/^# taken-at: s000 2026-01-01\n# taken-at: s999 2026-01-02$/m,
       'and the new stamp is APPENDED to it, never written over it');
}

# ── a baseline that does not exist yet is still generated whole ─────────────
{
  my $p = "$dir/fresh.tsv";
  write_shortfall($p, { 'perl-tests/a.t' => { rows => 3, cause => '#1' },
                        'perl-tests/z.t' => { rows => 0, cause => 'gone' } }, 'sha 2026-01-01');
  my $after = slurp($p);
  like($after, qr/^# row-shortfall\.tsv/, 'the first bless of a new baseline writes the header');
  like($after, qr/^perl-tests\/a\.t\t3\t#1$/m, 'and its rows');
  unlike($after, qr/z\.t/, 'a zero row is never INVENTED — only an existing one is kept');
}

done_testing();
