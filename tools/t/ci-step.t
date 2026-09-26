#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# tools/ci-step's annotations (task #2376, s498c).  CI run 36192024430 failed
# three rows of Pl/t/signals-01.t and the public annotation carried only the
# last 40 lines of the step — the summary, not the rows' got/expected, which
# the gate printed minutes earlier.  ci-step now re-emits every failed row
# WITH its diagnostic lines.  Outside GitHub Actions ci-step is a transparent
# wrapper, so the annotation is asserted on the printed `::error` lines.
#
# NOT part of the Pl/t gate (a CI tool, not the transpiler):
#   prove tools/t/ci-step.t
use strict;
use warnings;
use Test::More;
use File::Temp qw(tempdir);
use FindBin qw($RealBin);

my $ci_step = "$RealBin/../ci-step";
my $dir = tempdir(CLEANUP => 1);

sub write_file {
    my ($name, $body) = @_;
    open(my $fh, '>', "$dir/$name") or die "$name: $!";
    print {$fh} $body;
    close $fh;
    return "$dir/$name";
}

# Run ci-step on ARGV; returns (exit status, all output, the ::error lines).
sub run_step {
    my @argv = @_;
    my $out = qx{$ci_step @argv 2>&1};
    my $st = $? >> 8;
    my @err = grep { /^::error / } split /\n/, $out;
    return ($st, $out, \@err);
}

my $failing = write_file('two-fail.t', <<'T');
use Test::More tests => 3;
ok(1, 'first');
is('got-this', 'wanted-that', 'second');
diag("a diag with 100% of a percent sign");
is(1, 1, 'third');
T

# 1-5: prove -v, the `not ok` line with its diag.
{
    my ($st, $out, $err) = run_step('prove', '-v', $failing);
    isnt($st, 0, 'a failing command keeps its non-zero exit status');
    my ($rows) = grep { /failed rows \(1 of 1\)/ } @$err;
    ok($rows, 'one "failed rows" annotation') or diag($out);
    like($rows // '', qr/not ok 2 - second/, 'it carries the `not ok` line');
    like($rows // '', qr/got: 'got-this'%0A#\s+expected: 'wanted-that'/,
         'and the got/expected lines right after it, newline-encoded');
    ok(grep({ /^::error title=prove failed \(exit \d+\)::/ } @$err), 'the tail annotation is still there');
}

# 6-7: plain prove (as the gate runs it): no `not ok` line, the STDERR block.
{
    my ($st, $out, $err) = run_step('prove', $failing);
    my ($rows) = grep { /failed rows/ } @$err;
    like($rows // '', qr/#\s+Failed test 'second'/, 'non-verbose prove: the `Failed test` block');
    like($rows // '', qr/100%25 of a percent/, 'a percent sign is encoded as %25');
}

# 8: a passing command annotates nothing.
{
    my $ok = write_file('pass.t', "use Test::More tests => 1;\nok(1, 'fine');\n");
    my ($st, $out, $err) = run_step('prove', $ok);
    ok($st == 0 && !@$err, 'a passing step: exit 0 and no annotation') or diag($out);
}

# 9-10: many long failures stay under the cap and are split.
{
    my $big = write_file('big.t', "use Test::More tests => 40;\n"
                         . join('', map { "is('" . ('x' x 150) . "$_', 'y', 'row $_');\n" } 1 .. 40));
    my ($st, $out, $err) = run_step('prove', '-v', $big);
    my @rows = grep { /failed rows/ } @$err;
    ok(@rows > 1 && @rows <= 4, 'long failures are split over at most four annotations')
        or diag(scalar(@rows) . " row annotations");
    ok(!grep({ length($_) > 3200 } @$err), 'every annotation stays under the cap');
}

done_testing();
