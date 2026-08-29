#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# proto-cache-01.t: the on-disk memo for the module prototype pre-scan
# (Pl::ProtoCache, task #560).
#
# `_extract_module_prototypes` walks a module — recursively, through its own
# `use` statements — for the PARSE FACTS a using file needs, and that walk used
# to be memoized only for the life of the process.  It is now memoized on disk,
# one entry per module, consulted before the walk recurses.
#
# THE BAR THE WHOLE FEATURE STANDS ON: the cache changes COST, never emission.
# So every row here transpiles the same program twice and byte-compares, and
# the rest of the rows are the ways an entry must be REFUSED — a corrupted
# file, a key that does not match, a module or a DEPENDENCY that changed, a
# dependency that now resolves somewhere else.  Each of those is a silent
# wrong if it is missed: the file would compile against yesterday's facts.
#
# No SBCL here — every row is a pl2cl transpile, and the cache is redirected
# to a temp dir with PCL_CACHE_DIR so the user's own cache is never touched.

use v5.20;
use strict;
use warnings;
use Test::More;
use File::Temp qw(tempdir);
use File::Path qw(make_path);
use FindBin qw($RealBin);

my $project_root = "$RealBin/../..";
my $pl2cl        = "$project_root/pl2cl";
plan skip_all => "pl2cl not found" unless -x $pl2cl;

plan tests => 12;

my $tmp   = tempdir(CLEANUP => 1);
my $cache = "$tmp/cache";
my $lib   = "$tmp/lib";
make_path($lib);

# A module whose PROTOTYPE is a parse fact (a `(&)` makes the trailing block a
# block-form argument) plus a DEPENDENCY it re-exports from, so the entry for
# the top module is dependency-inclusive — which is exactly what makes a
# dependency's change matter to the top module's entry.
sub write_file {
  my ($path, $text) = @_;
  open my $fh, '>', $path or die "$path: $!";
  print {$fh} $text;
  close $fh;
}

write_file("$lib/PCDep.pm", <<'PM');
package PCDep;
use Exporter 'import';
our @EXPORT = qw(dep_blk);
sub dep_blk (&@) { my ($c, @r) = @_; return $c->(@r) }
1;
PM

write_file("$lib/PCTop.pm", <<'PM');
package PCTop;
use PCDep;
use Exporter 'import';
our @EXPORT = qw(top_blk dep_blk);
sub top_blk (&@) { my ($c, @r) = @_; return $c->(@r) }
1;
PM

my $prog = "$tmp/prog.pl";
write_file($prog, <<'PL');
use lib LIBDIR;
use PCTop;
my $x = top_blk { 42 };
my $y = dep_blk { 7 };
print "$x $y\n";
PL
{ # splice the real lib dir in (a heredoc must not interpolate the rest)
  open my $fh, '<', $prog or die; my $t = do { local $/; <$fh> }; close $fh;
  $t =~ s/LIBDIR/'$lib'/;
  write_file($prog, $t);
}

sub transpile {
  my (%opt) = @_;
  local $ENV{PCL_CACHE_DIR} = $opt{cache_dir} // $cache;
  local $ENV{PCL_NO_PROTO_CACHE} = $opt{off} ? 1 : 0;
  delete local $ENV{PCL_NO_PROTO_CACHE} if !$opt{off};
  my $out = qx{"$pl2cl" "$prog" 2>/dev/null};
  return $out;
}
sub entries { return sort glob("$cache/proto/*.json") }

# 1. The facts really do cross: without them the `(&)` block-form call would
#    not parse this way.  This is the SUBJECT of every byte-compare below.
my $cold = transpile();
like $cold, qr/pl-top_blk/, 'the module prototype reaches the call site (cold)';

# 2. A cold run leaves entries behind.
my @cold_entries = entries();
ok scalar(@cold_entries) >= 2, 'the cold walk wrote one entry per module'
  or diag "entries: @cold_entries";

# 3-4. Warm emission is byte-identical, and the warm run adds nothing.
my $warm = transpile();
is $warm, $cold, 'warm emission is byte-identical to cold';
is_deeply [entries()], [@cold_entries], 'a warm run writes no new entry';

# 5. …and so is a run with the cache switched off entirely.
is transpile(off => 1), $cold, 'PCL_NO_PROTO_CACHE=1 emission is identical';

# 6. A CORRUPTED entry is refused, not used: truncate every entry to garbage.
for my $e (@cold_entries) { write_file($e, '{"key": "' . ('x' x 20)) }
is transpile(), $cold, 'a corrupted entry is ignored and the walk redone';

# 7. …and the run rebuilt them (the corruption is not sticky).
{
  my $ok = 1;
  for my $e (entries()) {
    open my $fh, '<', $e or next;
    my $t = do { local $/; <$fh> }; close $fh;
    $ok = 0 if $t !~ /"prototypes"/;
  }
  ok $ok, 'the corrupted entries were rewritten with real records';
}

# 8. An entry whose recorded key does not match is refused (that is the
#    generation / compiler / mtime check, and a hash collision).
for my $e (entries()) {
  open my $fh, '<', $e or next;
  my $t = do { local $/; <$fh> }; close $fh;
  $t =~ s/"key":"[^"]*"/"key":"WRONG"/;
  write_file($e, $t);
}
is transpile(), $cold, 'a key mismatch inside the entry is ignored';

# 9. The MODULE changed: a new mtime is a new key, so the old entry is
#    unreachable rather than stale.
{
  my @before = entries();
  utime time + 5, time + 5, "$lib/PCTop.pm";
  is transpile(), $cold, 'a touched module re-walks and emits the same';
  my @after = entries();
  ok @after > @before, 'the touched module got its own new entry'
    or diag "before " . scalar(@before) . ", after " . scalar(@after);
}

# 10. The DEPENDENCY changed.  PCTop's entry holds facts merged from PCDep, so
#     it must not survive a change to PCDep — the entry records what it
#     resolved and re-checks it.  Here the change is REAL: dep_blk loses its
#     block prototype, so the emission must change too.
write_file("$lib/PCDep.pm", <<'PM');
package PCDep;
use Exporter 'import';
our @EXPORT = qw(dep_blk);
sub dep_blk { my ($c, @r) = @_; return $c->(@r) }
1;
PM
{
  my $after = transpile();
  isnt $after, $cold,
    'a changed DEPENDENCY invalidates the depending module\'s entry';
  # and the answer is the one a cache-free compiler gives
  is $after, transpile(off => 1), 'the post-change emission is the uncached one';
}
