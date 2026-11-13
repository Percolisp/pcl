#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# tools/board-timing.pl (task #4787, s540a) times a board file under perl and
# under PCL and splits PCL's time into transpile / load / run with a hooks
# fasl that ENCAPSULATES runtime entry points by name.  A renamed entry point
# would leave its bucket at ZERO, silently -- so these rows are the positive
# control: a one-file fixture dist whose run MUST land time in each bucket it
# exercises (a `use`d module of the dist, the Test::More layer, a string
# eval), the comparison class, and the report over the written TSV.
#
# NOT part of the Pl/t gate (it spawns SBCL a few times).  Run it directly:
#   prove tools/t/board-timing.t
use strict;
use warnings;
use Test::More tests => 14;
use File::Temp qw(tempdir);
use FindBin qw($RealBin);

my $tool = "$RealBin/../board-timing.pl";
my $tmp = tempdir(CLEANUP => 1);
my $dist = "$tmp/Fixture-Timing-1.0";
mkdir $_ or die "$_: $!" for $dist, "$dist/t", "$dist/lib";
open my $m, '>', "$dist/lib/BtMod.pm" or die $!;
print $m "package BtMod;\nsub f { return 42 }\n1;\n";
close $m;
open my $t, '>', "$dist/t/a.t" or die $!;
print $t "use strict;\nuse Test::More tests => 3;\nuse BtMod;\n"
       . "is(BtMod::f(), 42, 'module');\nmy \$x = eval \"1 + 1\";\nis(\$x, 2, 'eval');\nok(1, 'three');\n";
close $t;
# An hour old: the runtime's module cache is valid only when its entry is
# NEWER than the source, in whole seconds -- a module written in the same
# second as the cold run's entry would be transpiled again by the warm run.
my $old = time - 3600;
utime $old, $old, "$dist/lib/BtMod.pm", "$dist/t/a.t" or die "utime: $!";
open my $l, '>', "$tmp/list.tsv" or die $!;
print $l "$dist\t-\ts540a\tthe fixture\n";
close $l;

my $out = "$tmp/out.tsv";
my $log = `"$^X" "$tool" --list "$tmp/list.tsv" --out "$out" --cache-root "$tmp/cache" --modes cold,warm 2>&1`;
is($?, 0, 'the run exits 0') or diag($log);

my (@cols, %row);
open my $fh, '<', $out or die "no $out: $!";
while (<$fh>) {
  chomp;
  next if /^#/;
  my @v = split /\t/, $_, -1;
  if ($v[0] eq 'sha') { @cols = @v; next }
  my %r;
  @r{@cols} = @v;
  $row{$r{mode}} = \%r;
}
close $fh;
my ($cold, $warm) = @row{qw(cold warm)};
ok($cold && $warm, 'one cold and one warm row');
is($warm->{cmp}, 'C', 'the file is COMPARED: PCL ran its plan with perl\'s ok count');
is("$warm->{p_ok}/$warm->{c_ok}", '3/3', 'perl and PCL both pass 3');
is($warm->{rec}, 'last', 'the hooks wrote their record after the END phase');
cmp_ok($warm->{tcompile}, '>', 0, 'tcompile: the Test::More layer is compiled');
cmp_ok($warm->{mcompile}, '>', 0, 'mcompile: the dist module (a -I module) loads from its text');
cmp_ok($warm->{ecompile}, '>', 0, 'ecompile: the string eval is compiled');
cmp_ok($cold->{xpile}, '>', 0, 'xpile: the cold run transpiles the module');
cmp_ok($cold->{n_xpile}, '>=', 1, 'n_xpile counts it');
is($warm->{n_xpile}, 0, 'the warm run transpiles no module');
my $sum = $warm->{A_wall} + $warm->{B_wall} + $warm->{C_wall};
cmp_ok(abs($sum - $warm->{c_wall}), '<', 0.02, 'A + B + C = the whole PCL wall');
cmp_ok($warm->{x_wall}, '>', 0, 'the test file transpile is timed');

my $rep = `"$^X" "$tool" --report "$out" 2>&1`;
like($rep, qr/compared \(C\) 1, partial-equal \(PE\) 0/, 'the report counts the compared file');
