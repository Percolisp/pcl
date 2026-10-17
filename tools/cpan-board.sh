#!/usr/bin/env bash
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# cpan-board.sh TREE OUTPREFIX -- run THE CPAN BOARD with TREE's tools.
#
# The population is baselines/cpan-board-dists.tsv (s515c, #2950): one line per
# dist, its flags, the session it joined.  The list is read from THIS script's
# own checkout, not from TREE, so a base extraction of an older sha (which may
# predate a dist joining) is measured over the SAME list as the tree -- the
# only comparison the blessed rows (baselines/cpan-board14-fails.tsv) mean.
# PCL_BOARD_LIST=FILE overrides the list (e.g. one dist while it is censused).
#
# Writes OUTPREFIX.tsv (per file) and OUTPREFIX.rows.tsv (per failing row).
# Compare:  tools/cpan-scoreboard.pl --diff baselines/cpan-board14-fails.tsv \
#             OUTPREFIX.rows.tsv
# PCL_BOARD_JOBS (default 4) and PCL_BOARD_TIMEOUT (default 120) are passed
# to the scoreboard.  Run it through the box's lock (it is a heavy leg).
set -u
if [ $# -ne 2 ]; then
  echo "usage: cpan-board.sh TREE OUTPREFIX" >&2
  exit 64
fi
TREE=$1
OUT=$2
case "$OUT" in /*) ;; *) OUT="$PWD/$OUT" ;; esac
HERE=$(cd "$(dirname "$0")/.." && pwd)
LIST=${PCL_BOARD_LIST:-$HERE/baselines/cpan-board-dists.tsv}
if [ ! -r "$LIST" ]; then
  echo "cpan-board.sh: cannot read the dist list $LIST" >&2
  exit 66
fi
cd "$TREE" || exit 1
TREE=$PWD
# Plain dists first, then `--no-dist-lib` and the dists that carry it: the
# scoreboard's flag applies to every dist AFTER it.
ARGS=$(perl -e '
  my ($list, $tree, $home) = @ARGV;
  open my $fh, "<", $list or die "cannot read $list: $!\n";
  my (@plain, @nolib);
  while (<$fh>) {
    chomp;
    next if /^\s*(#|$)/;
    my ($dist, $flags) = split /\t/;
    my $path = $dist =~ m{/} ? "$tree/$dist" : "$home/.cpan/build/$dist";
    die "cpan-board.sh: no dist directory $path\n" if !-d $path;
    if ($flags eq "--no-dist-lib") { push @nolib, $path }
    elsif ($flags eq "-") { push @plain, $path }
    elsif ($flags =~ /^--inc=(.+)$/) {
      # extra @INC dirs for THIS dist only (a dependency fetched under
      # ~/.cpan/build, never installed; the dist own inc/ that its make
      # test adds): relative paths resolve against ~/.cpan/build (s518e)
      my @d = map { m{^/} ? $_ : "$home/.cpan/build/$_" } split /,/, $1;
      -d $_ or die "cpan-board.sh: no --inc directory $_ for $dist\n" for @d;
      push @plain, "--dist-inc=" . join(",", @d), $path;
    }
    else { die "cpan-board.sh: unknown flags \"$flags\" for $dist\n" }
  }
  print join("\n", @plain, (@nolib ? ("--no-dist-lib", @nolib) : ())), "\n";
' "$LIST" "$TREE" "$HOME") || exit 1
mapfile -t DISTS <<< "$ARGS"
echo "cpan-board.sh: list $LIST, tree $TREE, $(grep -c . <<< "$ARGS") args"
exec tools/cpan-scoreboard.pl --jobs "${PCL_BOARD_JOBS:-4}" \
  --timeout "${PCL_BOARD_TIMEOUT:-120}" \
  --tsv "$OUT.tsv" --rows "$OUT.rows.tsv" "${DISTS[@]}"
