#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# s486b scratch: write task #1787's JSON record.
use strict;
use warnings;
use JSON::PP;

my $desc = <<'END';
DONE s486b (2026-09-16, Opus).  The skip registry is now COUNTABLE, and its
stale entries are cleaned out.  Ruling verbatim in docs/DECIDED.md ## s486b;
long form in docs/test-skip-registry.md "How skips are counted".

THE PROBLEM.  cl/skip-registry.lisp relabels a matched FAILING assertion as a
TAP skip.  The sweep merged those rows into ONE "skipped" total together with
the skips the test files ask for themselves (EBCDIC, threads, miniperl,
XS::APItest, Devel::Peek), recorded no registry count anywhere, and the
headline fail count therefore EXCLUDED those rows while INCLUDING 267 others
whose cause cites the same docs/not-supported.md.  Two mechanisms owned one
fact and neither was countable.

WHAT SHIPPED.
1. cl/pcl-test.lisp: the registry branch emits `ok N # skip [registry]
   <reason>` -- TAP-legal (a skip directive's reason is free text), and it
   makes a registry skip separable from a test-own skip AT THE TAP LINE,
   independently of the reason text.  NO summary line: a file that aborts
   mid-run never reaches one.  The branch is the registry branch only, so the
   companion suite (loads cl/pcl-test.lisp, never cl/skip-registry.lisp)
   cannot reach it -- no companion run is owed.
2. tools/sweep-perl-tests.pl: counts the marked lines per file as
   registry_skips and `# REGISTRY-STALE` lines as registry_stale; carries both
   through the worker result file (inserted BEFORE $status -- the free-form
   snippet stays last); prints a `Reg` column and
     TOTAL: N passing, M failing, K skipped (R by the registry) across F files
     REGISTRY-STALE: S entries in T files
   and writes both as the LAST TWO columns of .faillog/_status.tsv, AFTER the
   tab-scrubbed `note`, so every index-based reader of the first ten columns
   is unaffected.  Documented in the header comment above write_status_file
   (which is what s486a's census tool reads).
3. tools/sweep-diff.pl: one REGISTRY line --
     REGISTRY: R rows relabelled in T files (not in the fail count)
   or `REGISTRY: NOT COUNTED` when the column is absent.  A reader that finds
   no column treats it as UNKNOWN, never zero (baselines/pass-baseline.tsv has
   neither column).

THE FINDING THE INSTRUMENT PRODUCED IMMEDIATELY.  The brief's per-file table
was built by matching each skip's REASON text against the registry file and
said 19 REGISTRY-STALE lines.  Measured with the marker: SEVEN, in four files.
The 12 extra were lex.t (8), join.t (2) and sub.t (2) -- LIVE registry skips
the text classifier mis-read (lex.t registry=8 stale=0, join.t registry=2
stale=0).  That is the case for the marker in one measurement.

THE CLEANUP (rule 3: narrow or remove so no passing row matches and every
still-failing row stays covered).
  * ref.t "^(Scalar|Array|Hash|Code|Glob) dereference$" -> "^(Scalar|Array|
    Glob) dereference$" + integer keys 38 and 39.  The file's `foreach $ref
    (*STDOUT{IO}, *STDERR{FORMAT})` loop emits the same four descriptions
    TWICE; the FORMAT iteration's %$ref (42) and &$ref (43) now die with a
    matching message while the IO iteration's (38/39) do not, so a DESCRIPTION
    cannot separate them.  Test-number keys are the mechanism chop.t 48-51
    already uses, and the stale-detector stays the backstop.
  * ref.t "UTF8 representation is 3 chars|via the UTF8 byte sequence|via the
    correct name works" -> "via the correct name works": the first two
    alternatives (rows 140, 144) pass now; the third still covers all ten
    still-failing rows, 148-174.
  * array.t "\@_ alias to nonexistent" -> "...nonexistent neg index": row 130
    passes, row 131 (negative index) still fails.
  * chop.t "chomp @a when.*eq 0 and" DROPPED -- one row (148), now passing.
  * state.t "^Reference to state variable$" DROPPED -- one row (76), passing.

MEASURED per file, (pass, fail, skip, registry, stale) before -> after, with
the sweep's own loader (p-load-with-recovery; scratch/s486b/measure.pl in the
worktree -- tools/runt's plain --load aborts tr/ref/readline/substr/state
early and must not be used to measure these):
  ref.t    (199, 12, 31, 25, 4) -> (199, 12, 31, 25, 0)
  array.t  (171, 15,  9,  9, 1) -> (171, 15,  9,  9, 0)
  chop.t   (144,  0,  4,  4, 1) -> (144,  0,  4,  4, 0)
  state.t  ( 88,  0,  4,  4, 1) -> ( 88,  0,  4,  4, 0)
Count-neutral by construction: the registry only ever relabels a FAILING row,
so a stale entry contributes no skip and dropping it moves nothing.

WHAT WAS DELIBERATELY NOT DONE: no row migrated between the registry and the
cause column, and nothing changed about what counts as pass or fail (ruling
part 2).  OPEN USER QUESTION: retire the registry in favour of the cause
column (Fable recommends it, after the tag; it would move the headline
649 -> ~830 fails) or keep both.
END

my $rec = {
  id => 1787,
  subject => "The skip registry is COUNTABLE: a [registry] marker at the TAP line, registry_skips/registry_stale as the last two _status.tsv columns, one REGISTRY line in sweep-diff, and the 7 stale entries narrowed or dropped",
  description => $desc,
  status => "completed",
  created => "2026-09-16",
  blockedBy => [],
  blocks => [],
};
my $path = "$ENV{HOME}/.claude/tasks/pcl/1787.json";
open my $fh, '>:raw', $path or die "open $path: $!";
print $fh JSON::PP->new->utf8->pretty->canonical->encode($rec);
close $fh;
print "wrote $path\n";
