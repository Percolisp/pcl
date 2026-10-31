#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ns-ids.pl -- the stable ids of docs/not-supported.md (s531b, Fable ruling
# s531 #5).  The one reader is tools/lib/PCLNsIds.pm; this is its command line.
#
#   tools/ns-ids.pl --list    # id <TAB> level <TAB> line <TAB> status <TAB> heading
#   tools/ns-ids.pl --check   # exit 1 and name every problem; silent + exit 0 when clean
#   tools/ns-ids.pl --file F  # another copy of the document (a fixture, a worktree)
#
# A cause cites a section as `NS:<id>` (docs/failure-cause-classes.md, "The
# grammar"); Pl/t/verdict-register-01.t checks every cited id against this.

use strict;
use warnings;
use FindBin;
use lib "$FindBin::RealBin/lib";
use PCLPaths ();
use PCLNsIds qw(read_ids);
use Getopt::Long;

my ($LIST, $CHECK, $FILE);
GetOptions('list' => \$LIST, 'check' => \$CHECK, 'file=s' => \$FILE)
    && ($LIST || $CHECK)
    or die "usage: $0 --list | --check [--file docs/not-supported.md]\n";
$FILE //= PCLPaths::root($FindBin::RealBin) . '/docs/not-supported.md';

my $r = read_ids($FILE);
if ($LIST) {
    for my $id (@{ $r->{order} }) {
        my $s = $r->{ids}{$id};
        my $status = $s->{retired}
            ? 'RETIRED' . (defined $s->{successor} ? " -> $s->{successor}" : '') : 'live';
        print join("\t", $id, $s->{level}, $s->{line}, $status, $s->{heading}), "\n";
    }
}
if ($CHECK) {
    print STDERR "ns-ids: $_\n" for @{ $r->{problems} };
    exit(@{ $r->{problems} } ? 1 : 0);
}
exit 0;
