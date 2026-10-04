---
name: project-everyday-battery
description: "The EVERYDAY-PERL battery (#1994) — ordinary programs vs perl, byte for byte; why it exists, where it lives, what it found (s491–s492), what is owed"
metadata: 
  node_type: memory
  type: project
  originSessionId: b5cdc640-b86e-4dd2-88a8-ef752d0742b4
  modified: 2026-09-20T12:34:10.379Z
---

USER question (s491, 2026-09-19): "how many more central Perl features are failing?" — and on seeing the first results: "really interesting ... continue looking at this carefully".  Task **#1994**, Fable's own standing investigation (probing + filing + designing; fixes go to Opus batches).

**Why:** the suite pass rate (96.5 %) and the cause census rank EXOTIC failures first, because perl's t/ tests features in isolation and in bulk.  What an ordinary script meets only shows when ORDINARY Perl is compared with perl.  The instrument is a battery of small everyday programs, stdout + exit status compared byte for byte, PCL's stderr KEPT.

**Where:** the record is `~/pcl-agent-scratch/s491/probes/central/WRITEUP.md` (method, every table, caveats; s492 continuation appended).  Batteries: `~/pcl-agent-scratch/s491/probes/central/` (42 idioms, `gen.pl`), `~/pcl-agent-scratch/s492/battery2/` (33 areas, `gen2.pl` + `run.pl`), `~/pcl-agent-scratch/s492/modwork/` (28 modules, ONE ordinary call each).  Narrowing probes with perl's answers: `~/pcl-agent-scratch/s492/probes/`.

**State at s492 (2026-09-20):** 103 programs, 68 identical to perl (36/42, 21/33, 11/28) — NOT a compatibility percentage (the programs cover ground, they are not a sample).  27 tasks filed in one day (#1995–#2009, #2050–#2051, #2080–#2085), ~10 SILENT WRONGS in everyday code.  The seams where the bugs were: (1) list-operator argument CONTEXT ([[2004-context-leak]]: scalar context leaks into sprintf/pack/die/warn args since v0.1.0 and breaks core Time::Local), (2) the MODULE SURFACE (POSIX stub, File::Spec shim kills `tempdir()`, Carp without location, `->VERSION` stub, `\X` kills Text::Wrap, circular `use` kills IO::Socket, a PPI mis-lex kills File::Copy, `local *_ = \my $a` makes File::Find find nothing, `flock` missing), (3) implicit RESOURCE rules (no filehandle close at scope exit — #2006(b) needs a Fable design), (4) NAME hygiene (#2003: a sub/method named like a TAP function is shared across packages).

**How to apply:**
- "It loads" is NOT "it works" — Text::Wrap, Time::Local, File::Temp, File::Find all LOAD and then fail or answer wrongly at the first ordinary call.  Never report a module as working from a load probe.
- Run every probe on perl FIRST (several battery probes were themselves invalid Perl), then keep PCL's stderr; narrow each difference to a CAUSE before filing ([[feedback_cause_not_count]]); grep the task store, DECIDED and not-supported.md first — about a third of the misses were documented non-support or already-filed tasks.
- A method that worked well: instrument a COPY of the core module (`use lib` a temp dir) with `print STDERR` lines to find where it stops (File::Find → `local *_`).
- **THE INSTRUMENT EXISTS (s495, #2099, main `ba30b0f7`)**: `tools/everyday-smoke.pl` over the checked-in corpus `everyday/` (the 122 seed programs, each admitted under perl by a three-run determinism test; `.expect` = perl 5.40.3's bytes) with `baselines/everyday-baseline.tsv` (one row per NON-identical program: verdict, first-diff-line, CAUSE = task number; buckets NEW / FIXED / MOVED / UNEXPLAINED / STALE; rows leave BY EDIT, no bless option) and `baselines/everyday-history.tsv` (`--record`, from a CLEAN tree = the merging agent's worktree, never main's checkout which carries the USER's README edit).  First number: **85 of 122 = 69.7 %**; USER goal > 90 %; the project STEERS by this line, not by the suite rate — the baseline's cause column says which task buys the most programs (at s495: #2093 missing builtins 5, #2084 module edges 3).  `--corpus DIR --baseline FILE` measures another population (Rosetta #2104) with the same tool.  Doc: `docs/everyday-battery.md`.  Every agent batch reports the line before → after (COMMON.md + CLAUDE.md).  The corpus must GROW (battery 3) or "90 % of 122" replaces "90 % of everyday Perl" as the goal.
- OWED: battery 3 (list in the write-up), and telling the USER that README's `@_`-aliasing bullet is stale (7 of 10 shapes alias) and that "close your filehandles explicitly" belongs beside the DESTROY bullet.
