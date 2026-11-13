# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# claude-md-size-01.t -- the two files Claude Code loads into EVERY session
# have size limits, and passing one loses information silently:
#
#   CLAUDE.md   40,000 characters.  Past it the session is warned that the
#               file "will impact performance"; at 194,000 (s540) a rule in
#               the middle was no longer reliably applied.
#   MEMORY.md   24,400 bytes (dot-claude-in-home-dir/memory/MEMORY.md, the
#               memory index).  Lines past the limit are NOT LOADED.
#
# Both grew by the same habit: a session's narrative appended to a "current
# state" line.  Since s541 CLAUDE.md states each rule shortly and has exactly
# TWO companion files: docs/claude-md/REFERENCE.md (the full text of the
# working sections, under the SAME HEADINGS) and docs/claude-md/HISTORY.md.
#
# The second half of this file guards what the split must not break: a reader
# (a subagent with a brief) who is sent to a section of CLAUDE.md by NAME must
# find it, and must find its long form under the same name in REFERENCE.md --
# never in a third file.  When a row fails: MOVE text into REFERENCE.md, never
# delete a rule to make room, never add a third companion file, never rename
# a section.

use strict;
use warnings;
use Test::More tests => 9;
use FindBin qw($Bin);

my $root = "$Bin/../..";

sub slurp_utf8 {
  my ($path) = @_;
  open(my $fh, '<:encoding(UTF-8)', $path) or return;
  local $/;
  my $text = <$fh>;
  close($fh);
  return $text;
}

my $claude = slurp_utf8("$root/CLAUDE.md");
ok(defined $claude, 'CLAUDE.md is readable');
$claude = '' if !defined $claude;
my $chars = length($claude);
cmp_ok($chars, '<=', 40_000,
  "CLAUDE.md is $chars characters (limit 40,000): move text to docs/claude-md/REFERENCE.md");

# Exactly two companion files, both named in CLAUDE.md.
my @companions;
if (opendir(my $dh, "$root/docs/claude-md")) {
  @companions = sort grep { !/\A\./ } readdir($dh);
  closedir($dh);
}
is("@companions", 'HISTORY.md REFERENCE.md',
  'docs/claude-md/ holds exactly HISTORY.md and REFERENCE.md (no third piece)');
my @unnamed = grep { index($claude, "docs/claude-md/$_") < 0 } @companions;
is("@unnamed", '', 'CLAUDE.md names every companion file');

# The section names that briefs, docs and code comments cite must exist in
# CLAUDE.md (measured s541: these are the names the tree refers to).
my @cited = (
  'the lookup order', 'Design Principles', 'Quick Reference',
  'WHAT TO RUN WHEN', 'THE RUNTIME IS KEPT COMPILED AND CACHED',
  'Test Status', 'CURRENT NUMBERS', 'Key Files to Read',
  'Semantic Deep-Dives', 'Dependencies', 'Generated artifacts',
);
my @gone = grep { index($claude, $_) < 0 } @cited;
is("@gone", '', 'every section name the tree cites is still in CLAUDE.md');

# Principles are cited BY NUMBER from code comments: 1..13 and 9a.
my @no_rule = grep { $claude !~ /^\s{0,3}\Q$_\E\.? \*\*/m } (map { "$_." } 1 .. 13), '**9a.';
@no_rule = grep { $_ ne '**9a.' || $claude !~ /\*\*9a\. / } @no_rule;
is("@no_rule", '', 'principles 1-13 and 9a keep their numbers in CLAUDE.md');

# The long form of each working section is under the SAME NAME in REFERENCE.md.
my $ref = slurp_utf8("$root/docs/claude-md/REFERENCE.md");
$ref = '' if !defined $ref;
my @working = ('Design Principles', 'Quick Reference', 'WHAT TO RUN WHEN',
  'Semantic Deep-Dives', 'Dependencies');
my @no_long = grep { $ref !~ /^#{2,3} .*\Q$_\E/m } @working;
is("@no_long", '', 'REFERENCE.md has a heading for every working section of CLAUDE.md');
(my $flat = $claude) =~ s{\s+}{ }g;   # a pointer may wrap across lines
my @no_ptr = grep { $flat !~ /REFERENCE\.md`?\s+"\Q$_\E/ } @working;
is("@no_ptr", '', 'CLAUDE.md points each working section at its REFERENCE.md section by name');

SKIP: {
  my $mem = "$root/dot-claude-in-home-dir/memory/MEMORY.md";
  skip 'no memory index in this tree', 1 if !-e $mem;
  my $bytes = -s $mem;
  cmp_ok($bytes, '<=', 24_400,
    "MEMORY.md is $bytes bytes (limit 24,400): shorten index lines, move detail into topic files");
}
