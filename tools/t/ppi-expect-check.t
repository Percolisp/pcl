#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# Unit tests for the expect-state CHECKER (Pl::PPIExpect + tools/ppi-expect-check.pl,
# s525p): five catalogued mis-lexes are FLAGGED, and their correct spellings are
# NOT -- the breaking-case rule (a widened rule must be probed on the case it
# would break).
#
# NOT part of the Pl/t gate (it measures a measurement tool).  Run it directly:
#   prove tools/t/ppi-expect-check.t
use strict;
use warnings;
use Test::More;
use File::Temp qw(tempfile);
use FindBin qw($RealBin);
use lib "$RealBin/../..";
use Pl::PPIExpect qw(check_source);

my @CASES = (
  # [ §, the mis-lexed spelling, the kind it must raise, the correct spelling ]
  [  4, 'my $m = 7%-3;',                       'magic-hash-for-modulo', 'my $m = 7 % -3;' ],
  [ 11, 'sub ok {} ok /x/, "d";',              'slash-as-division',     'sub ok {} ok(/x/, "d");' ],
  [ 12, '$s += length($k)*length($k);',        'glob-for-multiply',     '$s += length($k) * length($k);' ],
  [ 28, 'my $x = $_.2;',                       'dot-in-number',         'my $x = $_ . 2;' ],
  [ 25, 'my $z = length("abc")-length("a");',  'minus-in-word',         'my $z = length("abc") - length("a");' ],
);
for my $c (@CASES) {
  my ($sec, $bad, $kind, $good) = @$c;
  my @r = check_source($bad);
  ok((grep { $_->{kind} eq $kind && $_->{sec} eq $sec && !$_->{cascade} } @r), "§$sec flagged: $bad")
    or diag join("\n", map { "$_->{kind} '$_->{text}' \@$_->{state} §$_->{sec}" } @r);
  my @g = check_source($good);
  is(scalar(@g), 0, "§$sec control is clean: $good")
    or diag join("\n", map { "$_->{kind} '$_->{text}' \@$_->{state} §$_->{sec}" } @g);
}

# The command: a file in, one TSV row per contradiction out, nothing for a clean file.
{
  my ($fh, $f) = tempfile(SUFFIX => '.pl', UNLINK => 1);
  print $fh "my \$m = 7%-3;\nmy \$n = 7 % -3;\n"; close $fh;
  my @out = `$^X $RealBin/../ppi-expect-check.pl $f`;
  is(scalar(@out), 1, 'tools/ppi-expect-check.pl: one row for one mis-lex');
  like($out[0] // '', qr/\t1\t\d+\t%-\tMagic\tXOPERATOR\t.*\tmagic-hash-for-modulo\t4\t0\t/, 'the row: line, text, class, state, kind, §, primary');
}

done_testing();
