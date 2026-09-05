#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# manifest-01.t — `pl2cl --manifest` (task #1171, Part B item B2;
# docs/ir-spec.md §10b).
#
# The manifest says what a program demands of a target: `uses` (every runtime
# op with its count), `needs` (the obligation classes) and `facts` (which
# Kind-A/Kind-B licences fired, `fired / candidates`).  It is computed by ONE
# walk of the lowered CLForm tree, hooked into Pl::Passes::run.
#
# The rows below are the three the plan asks for — a known op set → exact
# `uses`; `local` + string eval + `die` → exact `needs`; a foreach-raw loop →
# `facts` — plus two invariants that are cheap here and expensive to discover
# later:
#
#   * `--manifest` must not change the CL.  Asserted by transpiling the same
#     file with and without the flag and byte-comparing (the flag prints JSON
#     INSTEAD of the CL, so the check is that the CL path is untouched when
#     the flag is absent — the corpus-diff bar in the large).
#   * every op name Pl/Manifest.pm's obligation table mentions must EXIST in
#     docs/ir-op-inventory.tsv.  That table is this module's own data, not the
#     §10 taxonomy, so it can drift; a renamed op would silently drop out of
#     `needs` and no other row in the tree would notice.
#
# Cost: one `pl2cl` per fixture (no SBCL), so this file is fast — wall time is
# the gate's metric (CLAUDE.md §6).

use strict;
use warnings;
use Test::More;
use FindBin qw($RealBin);
use File::Temp qw(tempdir);
use JSON::PP;
use lib "$RealBin/../..";       # Pl::Manifest, read in row group 6

my $root  = "$RealBin/../..";
my $pl2cl = "$root/pl2cl";
my $tmp   = tempdir(CLEANUP => 1);

sub write_pl {
  my ($name, $src) = @_;
  my $p = "$tmp/$name";
  open my $fh, '>', $p or die "manifest-01.t: $p: $!";
  print {$fh} $src;
  close $fh;
  return $p;
}

sub manifest {
  my (@args) = @_;
  my $out = `cd "$root" && ./pl2cl --manifest @args 2>/dev/null`;
  die "manifest-01.t: pl2cl --manifest @args produced nothing\n"
    unless defined $out && length $out;
  return JSON::PP->new->decode($out);
}

# ── 1. a known op set → exact `uses` ─────────────────────────────────────
{
  my $f = write_pl('uses.pl', <<'PL');
my $x = 1 + 2;
my $y = $x . "s";
print $y;
PL
  my $m = manifest($f);
  is($m->{mode}, 'program', 'mode is program for a .pl');
  is($m->{manifest_version}, '1.0', 'the manifest states its own version');
  like($m->{generation}, qr/^v2-\d+$/, 'the manifest carries the generation stamp');
  # The ops this program must use, and the counts.  Exact, not "at least":
  # a manifest that over-counts is as wrong as one that under-counts.
  is($m->{uses}{'p-+'},   1, 'uses: one p-+');
  is($m->{uses}{'p-.'},   1, 'uses: one p-.');
  is($m->{uses}{'p-print'}, 1, 'uses: one p-print');
  # MEASURED, not assumed: two consecutive `my` STATEMENTS are two `p-let`
  # forms (one per declaration statement, each nesting its block remainder),
  # not one form with two entries.
  is($m->{uses}{'p-let'}, 2, 'uses: two p-let forms, one per `my` statement');
  ok(!exists $m->{uses}{'p-sort'}, 'uses: no p-sort in a program that does not sort');
  # nothing is silently dropped: the CL kernel heads land in uses_other
  ok(exists $m->{uses_other}, 'uses_other is present (no head is dropped)');
}

# ── 2. local + string eval + die → exact `needs` ─────────────────────────
{
  my $f = write_pl('needs.pl', <<'PL');
our $g = 1;
sub f {
  local $g = 2;
  my $r = eval "1 + 1";
  eval { die "boom" };
  return $r;
}
print f(), "\n";
PL
  my $m = manifest($f);
  is($m->{needs}{dynamic_scope}{local}, 1, 'needs: one `local` site');
  is($m->{needs}{string_eval}{eval},    1, 'needs: one string eval');
  is($m->{needs}{nonlocal_exit}{die},   1, 'needs: one die');
  is($m->{needs}{nonlocal_exit}{eval_block}, 1, 'needs: one eval BLOCK');
  is($m->{needs}{tie},      0, 'needs: tie is PRESENT and zero, never absent');
  is($m->{needs}{overload}, 0, 'needs: overload zero');
  is($m->{needs}{formats},  0, 'needs: formats zero');
  is($m->{needs}{xs},       0, 'needs: xs zero');
  is($m->{needs}{regex}{tier}, 'unclassified',
     'needs: the regex TIER is declared unclassified, not guessed');
  cmp_ok($m->{needs}{io}, '>', 0, 'needs: io counted (the print)');
}

# ── 3. a foreach-raw loop → `facts` ──────────────────────────────────────
{
  my $f = write_pl('facts.pl', <<'PL');
my @a = (1, 2, 3);
my $t = 0;
foreach my $v (@a) { $t = $t + $v }
print "$t\n";
PL
  my $m = manifest($f);
  my $fr = $m->{facts}{'foreach-raw'};
  is($fr->{candidates}, 1, 'facts: one foreach candidate');
  is($fr->{fired},      1, 'facts: foreach-raw FIRED on the read-only loop var');
  # the declaration classes are the #1035 verdicts, the raw material a fast
  # backend reads (plan-speed-and-ir §B.3)
  is($m->{facts}{'declaration_classes'}{':array'}, 1, 'facts: one :array binding');
  cmp_ok($m->{facts}{'raw-slot'}{candidates}, '>', 0,
         'facts: raw-slot has candidates (the scalar bindings)');
  # `fired` can never exceed `candidates` — the arithmetic of a ratio
  for my $name (sort keys %{ $m->{facts} }) {
    my $v = $m->{facts}{$name};
    next unless ref $v eq 'HASH' && exists $v->{fired} && exists $v->{candidates};
    cmp_ok($v->{fired}, '<=', $v->{candidates},
           "facts: $name fired <= candidates");
  }
}

# ── 4. module mode ───────────────────────────────────────────────────────
{
  my $f = write_pl('Mod.pm', <<'PL');
package Mod;
sub hello { return "hi" }
1;
PL
  my $m = manifest('--module', $f);
  is($m->{mode}, 'module', 'mode is module under --module');
  cmp_ok($m->{facts}{sub_facts}{subs}, '>', 0, 'module mode sees the sub');
}

# ── 5. the flag does not change the CL ───────────────────────────────────
{
  my $f = write_pl('same.pl', "my \$x = 1; print \$x + 1;\n");
  my $a = `cd "$root" && ./pl2cl "$f" 2>/dev/null`;
  my $b = `cd "$root" && ./pl2cl "$f" 2>/dev/null`;
  ok(length $a, 'the plain transpile produced CL');
  is($a, $b, 'the plain transpile is deterministic (the A/B baseline)');
  my $j = `cd "$root" && ./pl2cl --manifest "$f" 2>/dev/null`;
  unlike($j, qr/^\(in-package/m,
         '--manifest prints JSON INSTEAD of the CL, never both');
  my $c = `cd "$root" && ./pl2cl "$f" 2>/dev/null`;
  is($c, $a, 'the CL is byte-identical after a --manifest run in between');
}

# ── 6. the obligation table cannot drift out of the inventory ────────────
# Pl/Manifest.pm's %OBLIGATION is its own data (the classes a TARGET must
# implement machinery for), not docs/ir-spec.md §10's taxonomy (what an op
# IS).  So it can drift; this row is what makes the drift loud.  `p-qr` is
# the one legitimate absence: the emitter writes it package-QUALIFIED, so it
# is an INTERNAL :pcl symbol and the inventory (which lists exports) has no
# row for it — the manifest canonicalises `pcl::p-qr` to `p-qr` before
# counting.
{
  my %exported;
  my $tsv = "$root/docs/ir-op-inventory.tsv";
  open my $fh, '<:raw', $tsv or die "manifest-01.t: $tsv: $!";
  while (my $l = <$fh>) {
    next if $l =~ /^#/;
    my ($n) = split /\t/, $l;
    $exported{$n} = 1 if defined $n;
  }
  close $fh;
  ok(scalar keys %exported > 400, 'the inventory TSV was read');

  # Read the table out of the module rather than duplicating it here.
  require Pl::Manifest;
  my $src = do {
    open my $mh, '<', "$root/Pl/Manifest.pm" or die "manifest-01.t: $!";
    local $/;
    <$mh>;
  };
  my ($tbl) = $src =~ /my %OBLIGATION = \((.*?)\n\);/s;
  ok(defined $tbl && length $tbl, 'found %OBLIGATION in Pl/Manifest.pm');
  my %named;
  while ($tbl =~ /'(p-[^']+)'/g)             { $named{$1} = 1 }
  while ($tbl =~ /\bqw\(([^)]*)\)/g)         { $named{$_} = 1 for split ' ', $1 }
  cmp_ok(scalar keys %named, '>', 40, 'the obligation table names many ops');
  my %ALLOWED_ABSENT = ('p-qr' => 'emitted package-qualified; internal to :pcl');
  my @missing = grep { !$exported{$_} && !$ALLOWED_ABSENT{$_} } sort keys %named;
  is_deeply(\@missing, [],
            'every op the obligation table names is in the generated inventory')
    or diag("not in docs/ir-op-inventory.tsv: @missing\n"
            . "  either the op was renamed (fix Pl/Manifest.pm's \%OBLIGATION)\n"
            . "  or the inventory is stale (tools/ir-inventory.pl)");
}

done_testing();
