#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# tools/task-triage.pl's exit-criterion-1 count reads a task by its CURRENT
# label, not by its history (task #5162): the subject, then the newest
# `LABEL:` line, and only a description with no LABEL line whole.  A fixture
# store of six open tasks + one closed one; run it directly:
#   prove tools/t/task-triage.t
use strict;
use warnings;
use Test::More;
use File::Temp qw(tempdir);
use JSON::PP;
use FindBin qw($RealBin);

my $tool  = "$RealBin/../task-triage.pl";
my $store = tempdir(CLEANUP => 1);
my %task = (
  1 => { subject => 'SILENT WRONG: in the subject', triage => 'in',
         description => "filed\n=== s543c (x) ===\nLABEL: loud (the subject was not updated)\n" },
  2 => { subject => 'LOUD: a drop now', triage => 'in',
         description => "FILED as a SILENT WRONG\n=== s543c (x) ===\nLABEL: silent (old)\n"
                      . "=== s543x (y) ===\nLABEL: loud -- the newest verdict\n" },
  3 => { subject => 'a gap', triage => 'in',
         description => "history: LOUD\n=== s543c (x) ===\nLABEL: silent (no subject label)\n" },
  4 => { subject => 'no verdict block', triage => 'in',
         description => "this is a SILENT WRONG, filed before the verdict pass\n" },
  5 => { subject => 'SILENT WRONG: but OUT', triage => 'out', description => "x\n" },
  6 => { subject => 'SILENT WRONG: closed', triage => 'in', status => 'completed', description => "x\n" },
  7 => { subject => 'LOUD: fixed down to a refusal', triage => 'in',
         description => "the SILENT WRONG it was\n=== s543c (x) ===\nLABEL: loud (subject now says so)\n" },
);
my $json = JSON::PP->new->utf8->canonical;
for my $n (sort keys %task) {
  my $t = { id => $n, status => 'pending', blockedBy => [], blocks => [], %{ $task{$n} } };
  open my $fh, '>:raw', "$store/$n.json" or die "$store/$n.json: $!";
  print $fh $json->encode($t);
  close $fh;
}

local $ENV{PCL_TASK_STORE} = $store;
my $out = `$^X '$tool' 2>&1`;
is($?, 0, 'the tool runs on the fixture store');
like($out, qr/^IN and marked silent wrong \(exit criterion 1\): 3$/m,
     'criterion 1 counts the subject (1), the newest LABEL (3) and an unlabelled description (4) -- not a loud task whose history says silent (2, 7), nor an OUT one, nor a closed one');
my $in = `$^X '$tool' --list in-silent 2>&1`;
is(join(' ', $in =~ /^#(\d+)\t/mg), '1 3 4', '--list in-silent prints the counted ids');
my $all = `$^X '$tool' --list silent 2>&1`;
is(join(' ', $all =~ /^#(\d+)\t/mg), '1 3 4 5', '--list silent prints every open task marked silent, whatever its class');
done_testing();
