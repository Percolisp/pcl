# CPAN board: the known failures, by cause, with a plan (s521, 2026-10-18)

The USER asked for an overview of the known failures in the CPAN modules the board checks, with a plan
for fixing them.  This is a read-only review: nothing in the tree, the baselines or the task store was
changed, and no board, gate or sweep leg was run.  Inputs, all on main `2a27700c` (notes only above
`8ae0b5e6`; `baselines/`, `cl/`, `Pl/`, `lib/` identical to it; last code commit `5e6e241d`, gen v2-6886):

* `baselines/cpan-board-dists.tsv` -- the 19 dists;
* `baselines/cpan-board-s519r1.tsv` -- the per-file table from ROUND 1 (s519, on MAIN-READY-28 `4bdaa831`);
* `~/pcl-agent-scratch/s519/merged-scratch/s519b/ioc.tsv` -- IO-Compress's per-file table from its board
  join (s519b, 2026-10-18 11:16), because IO-Compress joined AFTER round 1 and is not in `s519r1`;
* `baselines/cpan-board14-fails.tsv` -- the blessed row baseline: **1,803 rows** = 1,628 assertion rows +
  83 `*PLAN*` rows (a file that stopped short of its plan) + 92 `*FILE*` rows (a file with no TAP at all);
* the task store `~/.claude/tasks/pcl/NNNN.json`, `docs/DECIDED.md`, `docs/not-supported.md`;
* 15 one-file dist runs (`tools/run-dist-t.pl --rows` through `probe.sh`) plus about 20 small direct
  probes, outputs in `~/pcl-agent-scratch/s521/cpan-review/` (`probes.log`, `probes2.log`, `p*.txt`, `q*.txt`).

Every count below comes from one of those files.  "Verified s521" means a probe in this review reproduced
the row; "unverified" means the count is the baseline's and was not re-run.

## 0. Update, 2026-10-18 evening (s521): what the review set in motion, and where the board stands

The review was written at 15:4x; by 20:4x the same day its batches 0, 1 and 2 were merged, plus a filler batch,
and the board was re-measured.  This section is the news; sections 1-5 below are the record as written and are
NOT rewritten (their numbers are the 15:4x measurement).

**Merged (all six MAIN-READYs of the day):**

| batch | merged as | what it carried for the board |
|---|---|---|
| 0 (s521a) | MAIN-READY-33 `73d65d6f` | #3431 the embedded `my` per iteration, #3433 the glob-ref re-bless, #3435 the deref operand's scalar context, #3436 `like` with a `'/RE/'` string, #3362 CvFLAGS -- **33 IO-Compress files run to their plan** (002any-*, 005defhdr 115/115, 100generic-* 800/800, 102tied-* 250/250, 108anyunc-*, 110encode-*, 011-streamzip); 107multi-* 15 -> 1227 of 1829; 33 `*PLAN*` / `*FILE*` lines out, 713 newly exposed failing rows in under the umbrella #3494 (sorted later, below) |
| 1 (s521b) | MAIN-READY-34 `81afe6b0` | the `\Q`-newline bug **#3510** fixed in #2501's shape (`Pl/CaseMods.pm`; the runtime pass deleted), **`PerlIO::get_layers` exists** (#3326 = #139(b), the applied layers only, `:pop` as perl), SEEK_END by fstat (#3023); #201 item 3 measured -> #3530 -- **1,121 Capture-Tiny rows out** (682 #3326, 438 #3327, the 11-stderr-string plan row) + Path-Tiny digest.t |
| 2 (s521c) | MAIN-READY-35 `cfed8a78` | `subtest` (#1576), `eq_set` (#3432), the TAP stream through the layer's own fd-1 dup (#3530 = #3434 part 2), the io/open.t heredoc refusal (#3512); #3434 part 1 measured and PARKED (a tie-by-handle design) -- Mojo-DOM58 entities, HTTP-Tiny 160_cookies, Path-Tiny parent / recurse / size / subsumes / symlinks / has_same_bytes now PASS in full; Capture-Tiny 06 + 10 run 872/0 + 873/0; 19 `*FILE*` / `*PLAN*` lines out, 22 in with causes |
| fillers (s521d) | MAIN-READY-36 `fb1c369f` | **#3518** `$.` keeps an assigned value with no handle current (480 board rows out), **#3516** the round-2 regression (below), #3492 (a) the anon-sub home package, #3514 #3369 #3452 #3470; #3513 identified |

**The round-2 board** (on MAIN-READY-34, i.e. after batches 0 and 1, before 2 and the fillers;
`baselines/cpan-board-s521r2.tsv`): **232 PASS / 128 PARTIAL / 89 FAIL of 449, TOTAL passing 28,489** --
against round 1's 198 / 156 / 95 and 14,106 in section 1.  The movers: Capture-Tiny 8 / 14 / 2 -> 19 / 3 / 2
(5,648 -> 10,256 ok), IO-Compress 10 / 53 / 30 -> 28 / 37 / 28 (1,855 -> 11,568), Class-Method-Modifiers 22 -> 24
PASS, HTTP-Tiny 25 -> 26, Path-Tiny 11 -> 13, Role-Tiny 11 -> 12.  The row diff: 24 FIXED (left by edit with
causes -- the stale rows of 2d among them: CMM 002 / 003 = #3373, Path-Tiny children.t = #3374; HTTP-Tiny
100_get.t x4 = #3436), 11 NEW (ALL the documented 105oneshot flap plus temp-file names inside two row keys -- not
re-blessed), **1 LOST = a REGRESSION no gate, sweep or probe had seen**: Data-Dump glob.t -- a glob VALUE passed
through `@_` stringified with its class after #3433 (**#3516**; fixed in s521d).  The round cadence's board half
earned its place.

**The blessed rows: 1,803 -> 857.**  Section 4's projection ("the failing lines roughly halve after batch 1,
to ~660") held in direction and was beaten in size because batch 0's one-file runs retired 33 whole-file lines
and the census below re-caused what they exposed.  Of the 857, about 360 are still the ruled remainder of
"NOT IN THE PLAN" (the `NS:Error messages` family GREW from 112 to 188 rows now that IO-Compress's rows run --
every one of them #1361 behind the #233 deferral), 47 are perl-skips.

**The #3494 umbrella sorted** (`docs/ioc-row-census-s521.md`, a second read-only census in this review's shape):
480 of the 710 rows = **#3518** (`$. = N` lost when no handle has been read -- fixed in s521d), 84 = **#3520**
(the BOARD RUNNER: a dist test's `$^X` child is the oracle perl with its older core Compress::Raw::Zlib -- not
PCL), 76 = #1361, 45 = #330 (5 direct + 40 knock-on), 11 = **#3521** (a second-package `use constant` loses its
`()` prototype under an import list -- every Zip64 unzip silently undef), 8 (+ 2,408 never-run) = **#3519** (a
scalar holding a glob ref keeps the glob when assigned a scalar ref -- pre-existing; what still stops 107multi-*
at 1227 of 1829), 5 = #3530, 1 = NS.  Round 2 fixed none of those rows (703 of 710 identical got / expected).

**Found and filed along the way** (each with a perl-vs-PCL reproducer): #3369, #3510, #3511 (the stash
hash-slice delete through a symbolic name is a silent NO-OP, worse than the read error 2c reports), #3512,
#3513 (op/fork.t's flap = a forked child outliving the harness's transpile server; RULED), #3514, #3515,
#3516, #3517, #3518-#3521, #3522 (the TAP layer exits 0 after failures), #3523, #3530, #3531, #3550-#3557
(s521c's board probes: a List::Util block without the enclosing `@_`, `is_deeply` and `system LIST` ignoring
a `""` overload, `lstat` then `-d _` following the link, PPI §5's nested anon-hash repair, ...), #3570, #3571.

**Resolved notes from section 5:** the IO-Compress oracle question -- `tools/run-dist-t.pl` has NO perl oracle
(its `PLAN N` is the TAP plan line, its rows are PCL's), so the "Compress::Raw::Zlib 2.214 required" death is a
HAND-RUN artifact (perl from the dist root with `-Ilib` loads the dist's IO::Compress 2.214 against the system
2.212); the board's numbers are unaffected.  The same mismatch IS what kills the `$^X` children of 011-streamzip
(#3520).  The stale not-supported sentence about `die` locations is still to edit (#1361's text).

**The plan's state and what is left (the rough count asked for at 20:4x):**

| batch | state |
|---|---|
| 0, 1, 2 | DONE (s521a, s521b, s521c); residues: #3327 re-scoped to the merged-capture order, #3434 part 1 parked behind a tie-by-handle design |
| the census residues | **`s521/s521e-prompt.md`, WRITTEN, NOT LAUNCHED** (the USER's 20:0x hold): #3519 (2,408 never-run rows), #3521 (Zip64), #3520 (the runner's `$^X` child environment), #3515, #3517, #3513 (ruled option 3) |
| 3 | #3431 DONE (s521a); #3061 (the Parser2 refusal hiding ~51,000 Text-CSV rows), #3375, #3347 remain -- ONE subjob, the sweep its gate, then a census of what Text-CSV exposes |
| 4 | #3511 filed; #1570 #1572 #3321 #1506 remain -- one subjob |
| 5 | measure-first: #1568, #1511, #3282 (24,500 rows behind a silent exit), #2900, #966 -- two subjobs, then a census of the JSON-PP rows |
| 6 | module surface -- one subjob |
| fillers | the ~16 small tasks filed today -- two subjobs |

About **ten execution subjobs plus two read-only censuses, four to five half-day sessions at two concurrent**
(DECIDED ## s521's end line), for everything the standing rulings do not hold.  The count is a frontier: each
newly reached population (Text-CSV's 51,000, JSON-PP's 24,500, 107multi's 2,400) exposes honest new failures
that the next census sorts.  The ruled remainder is the USER's call, with its cost in the table of §4.

## 0b. Update, 2026-10-20 (s523): round 3

Batches 3-6 are merged or in flight (the headings in §4 say which).  The ROUND-3 board on MAIN-READY-38 (s522a merged): **254 PASS / 125 PARTIAL / 70 FAIL of 449, TOTAL passing 65,073** (round 2: 232 / 128 / 89 and 28,489) -- Text-CSV's 20 files run for the first time after #3061 (55_combi.t alone 25,119 rows), Path-Tiny / Mojo-DOM58 / IO-Compress gained files; 50 files improved, 2 lost = the 105oneshot flap (#3493, both states blessed).  Row diff against the blessed file after the round's edits: 0 NEW; the 908 new not-ok rows of SLU exotic_names.t (a file that now LOADS) are one shim gap, **#3529** set_subname, batch 6's.  `baselines/cpan-board-s523r3.tsv` is the record.  Found beside the round: **#3526 / #3528** -- the weekly install-matrix CI has been red since 2026-10-17 because the SBCL 2.5.2 floor is broken for `pack` (a compiler VOP error on an emitted shape; a 45-line reproducer in the task), and **#3527** -- main's CI is red on one gate row, PCL's synthesized readdir order (s523a's filler).

**2026-10-21 (between s523 and s524): #3650 DECIDED** -- the USER lifts the pack park for the ONE U0 rule (a template beginning with `U` is in character mode), on two conditions: the change is small, and it costs no execution speed (bench `pack` / `packunpk`).  Queued for s524; it unlocks JSON-PP's sixteen 099_binary files (24,512 rows) and the ascii / latin1 encoders.  The "NOT IN THE PLAN" table's pack-park row narrows to everything BUT that rule.

## 0c. Update (s526): the board widens

The USER asked for new CPAN modules (2026-10-24); Fable installed ten dists into the oracle perl and two census briefs joined them.  **Half 1 (s526w1): TimeDate, HTTP-Date, URI, HTTP-Message, YAML-Tiny (the repo copy), Data-Dumper (its own 2.183 from `blib/lib`) -- 133 test files**, each joining with a row census by cause; the population goes from 20 dists / 449 files to 26 / 582 (half 2, s526w2, adds Pod-Simple, Log-Log4perl, YAML-PP and Template-Toolkit).  Blessed rows per dist: TimeDate 0, HTTP-Date 0, URI 60, HTTP-Message 15, YAML-Tiny 23, Data-Dumper 54 (152; the cause tables are in the session log, ## Session s526w1).  The widened board on the s526w1 tree: 362 PASS / 136 PARTIAL / 84 FAIL of 582, TOTAL passing 69,476 -- the old 20 dists are round 4's 277 / 107 / 65 and 66,729 exactly.  Four fillers rode along (#1025 + #1026 `s///e`, #3825 a sub named like a runtime operator lost its context bind, #3826 `lib/Clone.pm`, #3832 `local $/;` is slurp); the biggest open causes in the new rows are #1389 (29 rows, 28 of them URI's), #3831 (YAML-Tiny, 16), #3837 (Data-Dumper, 14) and the `use bytes` not-supported rows (Data-Dumper, 26).


## 1. The board today

From `cpan-board-s519r1.tsv` (header: 187 / 103 / 66 of 356, TOTAL passing 12,250; summing its rows gives
188 / 103 / 65 and 12,251 because the header predates its one hand-edited Capture-Tiny row), plus
IO-Compress from `ioc.tsv`:

| dist | files | PASS | PARTIAL | FAIL | rows ok | rows not ok |
|---|---:|---:|---:|---:|---:|---:|
| Algorithm-Diff | 2 | 0 | 2 | 0 | 134 | 23 |
| Capture-Tiny | 24 | 8 | 14 | 2 | 5,648 | 1,344 |
| Class-Inspector | 6 | 5 | 0 | 1 | 86 | 0 |
| Class-Method-Modifiers | 29 | 22 | 3 | 4 | 73 | 8 |
| Data-Dump | 15 | 13 | 2 | 0 | 147 | 2 |
| File-Which | 3 | 3 | 0 | 0 | 21 | 0 |
| Getopt-Long | 8 | 8 | 0 | 0 | 109 | 0 |
| HTTP-Tiny | 32 | 25 | 2 | 5 | 448 | 5 |
| IO-Compress (s519b join) | 93 | 10 | 53 | 30 | 1,855 | 154 |
| JSON-PP | 65 | 35 | 16 | 14 | 967 | 48 |
| Mojo-DOM58 | 5 | 2 | 0 | 3 | 3 | 0 |
| Path-Tiny | 30 | 11 | 4 | 15 | 1,438 | 1 |
| Role-Tiny | 23 | 11 | 7 | 5 | 129 | 23 |
| Safe-Isa | 2 | 2 | 0 | 0 | 88 | 0 |
| Scalar-List-Utils | 38 | 20 | 15 | 3 | 493 | 48 |
| Sort-Versions | 1 | 1 | 0 | 0 | 96 | 0 |
| Sub-Uplevel | 10 | 3 | 5 | 2 | 35 | 123 |
| Text-Balanced | 14 | 5 | 4 | 5 | 1,052 | 42 |
| Text-CSV | 38 | 10 | 25 | 3 | 1,221 | 17 |
| Try-Tiny | 11 | 4 | 4 | 3 | 63 | 8 |
| **total (20 lines, 449 files)** | **449** | **198** | **156** | **95** | **14,106** | **1,846** |

Four dists are clean (File-Which, Getopt-Long, Safe-Isa, Sort-Versions).  Two things to know when reading it:

* **The table predates two merges.**  MAIN-READY-29/30 (s519a, s519b) landed after round 1: Capture-Tiny's
  tied / badtie files now run to their end (13 / 14 / 23 at 744 / 744 / 747 passing, DECIDED ## s519b), and
  their newly reached rows are already in `cpan-board14-fails.tsv`.  The row baseline is therefore newer
  than the per-file table; round 2 reconciles them.
* **FAIL is "zero ok", not "PCL is wrong".**  47 of the 1,803 baseline rows are `PERL-SKIP` (real perl skips
  the file too: author tests, network tests, missing optional modules); of the 95 FAIL files, 28 are such
  skips (Class-Inspector 1, HTTP-Tiny 4, IO-Compress 15, Path-Tiny 1, Text-Balanced 5, Text-CSV 1, Try-Tiny 1 -- by
  the cause column).  A PARTIAL with "0 not ok" (IO-Compress has 39 of them) is a file that died part-way:
  its unseen rows are counted in section 5, not here.

## 2. The failures by cause

### 2a. The census as blessed (first task number in the cause column)

Rows = assertion rows; P = `*PLAN*` rows; F = `*FILE*` rows; "hidden" = rows the `*PLAN*` rows say were
planned and never ran (from their `ran X of N` text; a `*FILE*` row's hidden count is in 2c).

| cause | rows | P | F | hidden | dists (files) | task status | one-line mechanism |
|---|---:|---:|---:|---:|---|---|---|
| #3326 | 682 | 0 | 0 | 0 | Capture-Tiny (9) | pending | captured STDOUT under `:utf8` comes back as bytes -- **real mechanism verified s521: `PerlIO::get_layers` does not exist, Capture::Tiny installs `sub { () }` in its place, so the capture file is never relayered (#139)** |
| #3327 | 438 | 0 | 0 | 0 | Capture-Tiny (13) | pending | "STDERR lands before STDOUT" -- **the stated mechanism does not fail these rows; verified s521: `qr/\Q$x\E/` never matches when `$x` contains a newline** (2b) |
| no `#` (text) | 154 | 3 | 46 | 891 | 71 files | -- | classified in 2b |
| #233 | 124 | 0 | 0 | 0 | Sub-Uplevel (7), Class-Method-Modifiers 060 | pending, **USER-DEFERRED s488** | caller() fidelity: 4 elements, .lisp filename, `#line`, CORE::GLOBAL::caller, PCL frames visible |
| #1568 | 40 | 0 | 0 | 0 | Text-Balanced 06 07 09 | pending | `extract_quotelike` / `extract_tagged` return undef on `s{a}{b};`, the heredoc shape, a `qr//` tag with `ignore` |
| #3431 | 0 | 28 | 3 | 7,203 | IO-Compress (31) | pending | SILENT WRONG: `my $l = X->n(my $t)` in a loop -- the embedded `my` is the file cell, not a per-iteration lexical; LexFile croaks "NO!!!!" |
| #1571 | 29 | 0 | 0 | 0 | Scalar-List-Utils (8), Class-Inspector, Try-Tiny | pending (a BUNDLE) | introspection primitives: refaddr identity, reftype REF/IO, get-magic count, taint, DESTROY timing, subname |
| #1575 | 0 | 0 | 28 | -- | JSON-PP (13), Path-Tiny (13), Text-CSV (2) | **completed** s516b | the board did not chdir -- FIXED; every row is now another cause (2d) |
| #1389 | 25 | 0 | 0 | 0 | JSON-PP (5), Text-CSV, HTTP-Tiny | pending, **RULED DIVERGENCE** s473h | `utf8::is_utf8` answers 1 for every string (PCL has no downgraded form) |
| #1511 | 23 | 1 | 0 | 847 | Algorithm-Diff base.t oo.t | pending | the hunk iterator answers the wrong items after `->Next`; then a croak escapes `eval {}` |
| #1749 | 19 | 0 | 1 | -- | JSON-PP (6) | pending, **pack/unpack USER-PARKED** | `pack`/`unpack "U"` works in UTF-8 octets where perl is character mode |
| #3061 | 0 | 18 | 0 | 51,129 | Text-CSV (18) | pending | sigil-blind capture refusal: `$e` + `$e[1]` in one sub refuses `t/util.pl`, which 20 files require |
| #135 | 16 | 0 | 1 | 0 | Role-Tiny (4) | pending | role composition: constant-installed subs, a role's `use overload`, stub torture |
| #221 | 14 | 0 | 0 | 0 | Scalar-List-Utils (3), Text-CSV | pending, unscheduled design | warnings-gated diagnostics absent (+ `@_` aliasing in pairgrep, ruled NS) |
| #3433 | 0 | 12 | 0 | 7,608 | IO-Compress (12) | pending, **in the s521a brief** | SILENT WRONG: re-blessing a COPY of a glob ref does not reach the referent (`ckMagic`) |
| #2370 | 3 | 6 | 0 | 71 | IO-Compress 104destroy-* (4), Try-Tiny context finally | pending, **USER-PARKED s495** | scope-exit DESTROY |
| #3161 | 6 | 3 | 0 | 468 | IO-Compress 109merge-* (3) | completed (zlib shim); rows = its NS residue | `Compress::Raw::Zlib::InflateScan` / `inflateScanStream` not covered by the libz shim |
| #930 | 7 | 1 | 0 | 361 | CMM 140-lvalue, JSON-PP 019_incr | pending, **USER-DEFERRED s464** | user `:lvalue` subs |
| #330 | 7 | 0 | 0 | 0 | IO-Compress (7) | pending | no read-only scalars: "not writable" / "output buffer is read-only" never fires |
| #3282 | 4 | 3 | 0 | 4,544 | JSON-PP 099_binary00-02 | pending | the 099 driver runs 48 / 8 / 0 of 1,536 rows and EXITS 0 silently |
| #3436 | 6 | 0 | 0 | 0 | IO-Compress (4) | pending, **ruling reopened s520, in s521a** | `like $x, '/RE/'` -- the TAP layer matches the slashes literally |
| #3321 | 4 | 0 | 0 | 0 | JSON-PP 118 | pending | stash alias `*A:: = *B::` ignored |
| #3434 | 4 | 0 | 0 | 0 | Capture-Tiny 13 14 23 | pending, **in s521a** | the last rows "STDOUT is still tied"; the no_plan trailer lost after restore |
| #3323 | 4 | 0 | 0 | 0 | HTTP-Tiny 100_get | pending (= #3436's mechanism) | `like($x, "/re/")` |
| #966 | 4 | 0 | 0 | 0 | Text-CSV (4) | pending | `pos(ELEM)` reads undef after `ELEM =~ /x/g` (+ #1747 goto) |
| #1576 | 0 | 2 | 2 | ? | Mojo-DOM58 collection, Path-Tiny (2) | pending | `Test::More::subtest` not implemented -- the file dies at the first call |
| #1572 | 0 | 0 | 3 | -- | CMM 002 003 081 | pending | CLOS: forward-referenced class / "method 'orig' not found" -- **002 and 003 PASS now** (2d) |
| #1391 | 2 | 0 | 0 | 0 | Text-CSV 75_hashref | pending | a literal is not read-only |
| #3412, #3413 | 3 | 0 | 0 | 0 | Path-Tiny filesystem.t | completed in s520a (not merged) | failed stat in a call argument; `unlink` of an overloaded object |
| #2900 | 2 | 0 | 0 | 0 | Text-Balanced 05 | pending | SILENT WRONG: `\G\b` at pos > 0 ignores the previous character |
| #201 | 0 | 0 | 2 | -- | Capture-Tiny 06 10 | pending | reopening a closed fd 1: "fd-stream for descriptor 1 is closed" |
| #2006 | 1 | 1 | 0 | ? | Path-Tiny filesystem, sig_die | pending (half b) | a handle returned out of a block is not closed at scope exit |
| #1570 | 0 | 0 | 2 | -- | Role-Tiny extend-role-tiny, proto | pending | an `@ISA` parent whose CL package is never created: the READER dies, whole file lost |
| 16 single-row causes | 7 | 5 | 4 | 494 | | | #3375 #3001 #1506 #2066 #3347 #1573 #3432 #1510 #3284 #3262 #3373 #3435 #2016 #3023 #2955 #1361 -- see 2e |

### 2b. The 203 rows with no task number

| class | rows | where | verdict |
|---|---:|---|---|
| `NS:Error messages: no "at FILE line N" location info` | 112 | IO-Compress (13 files: 004gziphdr, 01misc, 100generic-*, 102tied-*, 105oneshot-gzip/-zip, cz-06gzsetp) | **verified s521 (01misc rows 4-16, 004gziphdr 290-294)**: the croak TEXT is right, the pattern requires `\s+at `; the location missing is Carp's (`lib/Carp.pm` ends every message with `"\n"`).  This is **#1361**, held by the #233 deferral (DECIDED line 135).  The not-supported entry's first sentence ("PCL does not track source file/line") is stale since #1240 -- `die` carries its location; only croak/carp, which must name the CALLER, do not. |
| `PERL-SKIP` | 47 | 36 files | not PCL failures: perl skips them too |
| `NS:DynaLoader / XS binary extensions` | 23 | IO-Compress bzip2 (11 files + 000prereq / 001bzip2 rows), Mojo-DOM58 dom entities, Role-Tiny namespace-clean, SLU exotic_names subname, CMM 120-fresh | the bzip2 files are real (Compress::Raw::Bzip2 is XS, pclxs PARKED); **the three Mojo / Role-Tiny `*FILE*` rows are STALE** (2d) |
| `NS:Error message text and format` | 9 | Role-Tiny load-module role-duplication, SLU first reduce, Try-Tiny erroneous_usage | ruled: exact text is not a goal (USER s494) |
| `NS:prototype` | 6 | SLU prototype.t scalarutil-proto.t | ruled residue (`set_prototype` at run time does not reach later parses, #2610) |
| `NS:Internals::*` | 4 | SLU readonly.t | ruled |
| `NS:given/when` | 2 | Try-Tiny given_when when | ruled refusal |

### 2c. `*FILE*` rows (no TAP at all) and the rows they hide

A `*FILE*` row is one line in the baseline however many rows the file has; its real cost is the perl row
count, measured here by running perl on each file from its dist root (top-level plan; `~/pcl-agent-scratch/s521/cpan-review/perlcounts.txt`).
28 of the 92 are `PERL-SKIP`; the other 64:

| dist | file(s) | perl rows (top level) | current cause (s521) |
|---|---|---:|---|
| JSON-PP | 099_binary03..15 (13 files) | 13 x 1,536 = 19,968 | #3282 (the task says so; blessed text still says #1575) |
| Capture-Tiny | 06-stdout-closed, 10-stdout-string | 872 + 873 | #201 |
| Scalar-List-Utils | exotic_names / subname / dualvar | 1,554 / 21 / 41 | XS (ruled) / XS (ruled) / #1506 |
| Path-Tiny | input_output x3, parent, temp, size, subsumes, has_same_bytes, locking, recurse, symlinks, mutable_tree | 171 + 11 + 15 + 5 + 5 + 3 + 2 + 2 + 2 + 2 = 219 | #1576 subtest (verified for parent, mutable_tree) |
| Path-Tiny | digest.t | 7 | #139 (`PerlIO::get_layers` in spew's atomic write -- verified s521) |
| Path-Tiny | children.t | 4 | **PASSES 4/4 now** (fixed by #3374, s519a) -- STALE row |
| Mojo-DOM58 | collection, dom, entities | 20 + 126 + 13 = 159 | #1576 subtest (verified entities; dom.t still 0 rows, every test in it is a subtest) |
| Text-CSV | 47_comment, 90_csv | 61 + 128 | #3061 |
| IO-Compress | cz-03zlib-v1 / 050interop-gzip / 105oneshot-deflate, -rawdeflate / 11 bzip2 files | 472 / 8 / ? / ? | #3347 / #3431 / #3431 / XS |
| Class-Method-Modifiers | 002-cache, 003-basic | 3 + 6 | **PASS 3/3 and 6/6 now** (fixed by #3373, s519b) -- STALE rows |
| Class-Method-Modifiers | 081-sub-and-modifier / 120-fresh | 28 / 12 | #1572 ("method 'orig' not found", verified) / XS |
| Role-Tiny | extend-role-tiny, proto | 1 + 5 | #1570 (READ ERROR "Package Role::Tiny / Exporter does not exist", verified) |
| Role-Tiny | namespace-clean | 4 | **NEW cause**: READ ERROR "too many colons in Class" -- `delete @{"Local::Class::"}{@imports}` emits `(p-delete-hash-slice %Local::Class:: @imports)`; unfiled |
| Role-Tiny | concrete-methods / role-long-package-name | 7 / 7 | #135 / #1510 (both verified) |
| HTTP-Tiny | 160_cookies | 2 | #1576 subtest now (blessed text: #3262, completed) |
| JSON-PP | 020_faihu | 3 | #1749 (parked) |
| Try-Tiny | given_when | 2 | ruled |

About **24,500 perl rows** sit behind these 64 lines; JSON-PP's 099 files are 80 % of it.

### 2d. Stale rows and stale causes found by this review

The baseline is edited row by row with each merge's own one-file runs, and since s519 the whole board runs
once per round, so a row a merge fixes without naming it stays until round 2.  Found:

| row(s) | blessed as | now (s521 probe) |
|---|---|---|
| CMM 002-cache.t, 003-basic.t `*FILE*` | #1572 | PASS 3/3, 6/6 -- #3373 (s519b) fixed the same forward-referenced-class death |
| Path-Tiny children.t `*FILE*` | #1575 + #300 | PASS 4/4 -- #3374 (s519a, the while-condition `my`) |
| Path-Tiny mutable_tree_while_iterating.t, HTTP-Tiny 160_cookies.t, Mojo-DOM58 entities.t (and dom.t) | #1575 + #300 / #3262 (completed) / XS Storable, Encode | all die on `subtest` (#1576); the Storable and Encode shims load |
| Role-Tiny namespace-clean.t | XS (B::Hooks::EndOfScope) | the module loads now (#3281); the file dies at the READER (new, above) |
| JSON-PP 099_binary03..15 | #1575 | #3282 |
| Capture-Tiny 288 multiline + 150 unicode `capture_merged`/`tee_merged` rows | #3327 "order" | the order IS different (probed: perl prints STDOUT first, PCL STDERR first), but every one of these assertions is a separate `like($got, qr/\Q$expected\E/)` that order cannot fail.  They fail because **`$x =~ qr/\Q$e\E/` is false whenever `$e` holds a newline** (probe `q.pl`: `"xx A:b\nC d\n yy" =~ qr/\Q$e\E/` perl match, PCL no; `"Hi!\n" =~ /\Q$e3\E/` perl match, PCL no).  Cause, read: `perl-regex-to-ppcre` (`cl/pcl-runtime.lisp` ~36565) rewrites `\Q(.*?)(?:\E|$)` at RUN time with a `.` that does not cross a newline and a `$` that stops before a final newline.  Unfiled; it is #2501's code path |
| Scalar-List-Utils refaddr.t rows 24-31 (8 unnamed rows, one join key) | #1571 | 3 fail now (28 29 31): an unnamed-row key cannot report FIXED rows inside it |
| the `NS:Error messages: no "at FILE line N"` section of not-supported.md | -- | its rationale is stale for `die` (#1240), still true for Carp |

### 2e. The long tail (one or two rows each, status from the task store)

#3375 `my $x = EXPR for LIST` dies "Fell through" (verified; SLU uniqnum, 3 rows unseen) -- #3001 Role::Tiny
second subclass keeps the inherited method -- #1506 `import constant X => 0` -- #2066 / #1573 Data::Dump
rendering (base64 branch; blessed scalar ref) -- #3347 F6 oversized top-level `my` (472 rows unseen) --
#3432 `eq_set` missing in the TAP layer (004gziphdr dies at row 450 of 919) -- #1510 a stash lists only subs
-- #3284's read (Role-Tiny subclass.t, Live symbol-table hashes NS) -- #3435 a call dereferenced by `->{}`
runs in list context (in s521a) -- #2016 JSON integers re-encode as strings -- #3023 SEEK_END on a `sysopen`
handle (Capture-Tiny 11: 2 of 873 rows run) -- #2955 Data::Dumper IV flag (ruled residue) -- #1361 Path-Tiny
croak location -- #3373 CMM 041 row 2 (perl TODO-fails it too).

## 3. Verdict per cause

Layers per CLAUDE.md 9a: **lib** = a `lib/` shim; **Pl** = parser/codegen (generic mechanism); **cl** =
runtime core semantics; **tap** = the harness `cl/pcl-test.lisp`; **dist** = the dist's test is wrong.
Sizes are from the tasks' own notes; "rows" = baseline rows that would leave, "+N" = rows that would start
running for the first time (and can fail honestly).

| cause | layer | size | blocked by | rows unlocked | discriminating measurement (s521 result) |
|---|---|---|---|---|---|
| \Q-newline (new; #3327's real cause) | cl (the runtime `\Q` pass) -- or Pl if done #2501's way | 1-line regex fix (`(?s)` + `\z`), or #2501's shape (apply `\Q` at the literal, delete the runtime pass): ~0.5 session | nothing | **288 certain + 150 with #139** (assertions) | `perl q.pl` vs `./pcl q.pl`: `m1 m2 m3` vs `n1 m2 n3` -- **confirmed** |
| #3326 = #139(b) `PerlIO::get_layers` | cl (core builtin) | small-medium: synthesize the list from PCL's one discipline (`unix perlio` + `utf8` / `encoding(X)` / `crlf`) and make `binmode :pop` agree with it (Capture::Tiny's `_relayer` loops `while (1 < get_layers) { binmode :pop }` -- a list that never shrinks HANGS it) ~1 session | a Fable ruling: #139 is marked "needs a design call" | **682** (+150 above), + Path-Tiny digest.t (7), perl-tests readline.t's last 4, io/crlf.t's get_layers rows | `capture { print "A\x{263a}\n" }` after `binmode STDOUT, ':utf8'`: perl len 3, PCL len 5; `PerlIO::get_layers(*STDOUT)` -> "Undefined subroutine" -- **confirmed** (row 02-capture 5: got `STDOUT:Hi! e2 98 ba`, expected the character) |
| #233 caller fidelity | cl (frame stack) | (a)(b)(c)(h) "independently cheap" per the task; (d)(e)(f) need design | **USER deferral s488** | 124 | 02_uplevel row 6: got `Sub::Uplevel - /tmp/pcl-nocache-InF0b6.lisp - 59 - main::foo`, expected `main - .../t/02_uplevel.t - 25` -- **confirmed** |
| #1361 croak location (the 112 + 1 text rows) | lib Carp.pm + cl (a per-frame saved location) | needs a per-frame location slot; the cheap shape was tried and REVERTED (s492c: it names Carp.pm) | **held behind #233's deferral** (DECIDED line 135) | 113 | 01misc row 4: got `': Expected even number of parameters, got 1`, expected `/... got 1\s+at /` -- **confirmed** |
| #1568 Text::Balanced | cl (regex/pos) or Pl -- undiagnosed | open: "bisect the failing alternation of `_match_quotelike` by hand" | nothing | 40 | 06_extqlk row 173: got `'s'`, expected `';'` -- **confirmed** (228/8 = s519r1) |
| #3431 embedded `my` in a `my` RHS in a loop | Pl (Parser2 lowering; scoping change -> the sweep is the gate) | medium: one lowering path (statement-level already binds it) | nothing | 31 lines, **+7,203** | 01misc stops at 65 of 164 with "NO!!!!" -- **confirmed**; repro `r1.pl`: perl `U 2 y`, PCL `D 2 y` |
| #1571 bundle | cl / lib per line | per line; the bundle says "split it when picked up" | refaddr identity half is ruled NS | 29 (refaddr already down to 3: 2d) | refaddr.t 29/3 -- **confirmed, baseline stale by 5** |
| #1575 leftovers | -- | 0 | -- | 0 (re-cause only) | 2c/2d |
| #1389 is_utf8 | -- | -- | **RULED DIVERGENCE s473h** (Fable) | 25 | 003_types rows 18 21 22 -- **confirmed** |
| #1511 Algorithm-Diff | cl or Pl -- undiagnosed; two bisect points named in the task | open | nothing | 24, **+847** | oo.t 102/20 of 969 -- **confirmed** |
| #1749 pack U | cl/pack-impl.pl | known fix shape (the task's oracle table) | **pack/unpack USER-PARKED** | 20, +3 | unverified (not re-run) |
| #3061 sigil-blind capture refusal | Pl (name resolution -> the sweep is the gate, gate-SET scan) | small: "delete/rename, never narrow" (standing MEMORY rule) | nothing | 20 lines, **+51,318** (Text-CSV 18 files + 47_comment, 90_csv) | 20_file.t: `Parser2 TODO: file lexical 'ebcdic' captured by sub _readable`, then a raw SBCL "Unhandled simple-error" -- **confirmed** |
| #135 Role::Tiny | cl/Pl stash | medium, several mechanisms | partly the deferred "Live symbol-table hashes" | 17 | concrete-methods: `Undefined subroutine &MyRole1::before_constant_deflate` -- **confirmed** |
| #221 warnings | Pl + cl | design exists (one lexical boolean) | **unscheduled design** (trigger fired s399) | 14 (shared with @_ aliasing NS) | unverified |
| #3433 glob re-bless | cl `p-bless` | small (fix shape in the task) | **in s521a** | 12 lines, +7,608 | repro `r1.pl`: perl `C`, PCL `A` -- **confirmed** |
| #2370 DESTROY | cl | 4-6 agent-days | **USER-PARKED s495** | 9, +71 | unverified |
| #3161 InflateScan | lib (Compress::Raw::Zlib shim) | small-medium | the libz shim is TEMPORARY by USER ruling s516 | 9, +468 | unverified |
| #930 lvalue subs | Pl + cl | lvalue-context propagation, not bounded | **USER-DEFERRED s464** | 8, +361 | unverified |
| #330 / #1391 read-only scalars, literals | cl | per-VALUE read-only (#1391's measurement) | design | 7 + 2 | 01misc row 12 got `''` -- **confirmed** |
| #3282 JSON-PP 099 | undiagnosed; exits 0 silently (a rule-12 bug in itself) | open: "cut 099_binary.pl to one chunk-03 iteration" | nothing | 20 lines, **+24,512** | unverified |
| #3436 / #3323 like '/RE/' | tap | small (fix shape in #3436) | ruling REOPENED s520; **in s521a** | 10 | 004gziphdr row 295 -- **confirmed** |
| #1576 subtest | tap | ~1 session (the task: one function + nested indentation) | nothing | 4 lines + 12 re-caused `*FILE*` lines, **+~380 top-level** (+1,500 nested) | `subtest 'inner' => sub {...}` -> "Undefined subroutine &main::subtest" -- **confirmed** |
| #3432 eq_set | tap | tiny (beside `eq_array`) | nothing | 1, +469 | 004gziphdr stops at 450 of 919 -- **confirmed** |
| #201 closed fd 1/2 | cl I/O | open | nothing | 2 lines, +1,745 | unverified |
| #3023 SEEK_END on sysopen | cl I/O (`%p-seek-impl`) | small | nothing | 1 line, +871 | Capture-Tiny 11: 2 of 873 rows -- **confirmed** |
| #1570 + namespace-clean read error | Pl (emission: every package a form names must exist at READ time; a stash name in a hash slice) | small each | nothing | 3 lines, +10 | both read errors -- **confirmed** |
| #1572 (081 only now) | cl (CLOS method combination for `orig`) | medium | nothing | 1 line, +28 | **confirmed** |
| #3375, #3347 | Pl | small / F6 family | nothing | +3 / +472 | #3375 **confirmed** (`r1.pl`) |
| #1510 stash lists subs only | cl | the deferred "Live symbol-table hashes" | **NS DEFERRED** | 1 line +7, + Role-Tiny subclass `*PLAN*` | **confirmed** (`Can't locate Foo__WITH__R/...pm`) |
| PERL-SKIP | -- | -- | -- | 0 | -- |
| XS bzip2 / Sub::Util subname / exotic_names | xs | -- | **pclxs PARKED (USER s485)** | 23 lines, +1,575+ | -- |

## 4. The plan

Ordered by rows unlocked per unit of effort, the certain-and-cheap first.  **Batch 0 already exists**:
Fable's next correctness brief `s521/s521a-prompt.md` (SHARED-BOX) carries #3436, #3433, #3434, #3435 (and
non-board members), so those are not repeated below.  **Batch 0 = s521a, DONE = MAIN-READY-33 (see §0).**

### Batch 1 -- Capture-Tiny's two mechanisms + the I/O tail (layer: cl) -- **DONE s521b = MAIN-READY-34 (members 1-3; #201 item 3 measured -> #3530, done in batch 2)**
Members: (1) the `\Q`-newline bug -- FILE it first (Fable's free ID), then fix; prefer #2501's shape if the
member can carry it (one mechanism, and #2501 is a silent-wrong in the same pass), else the one-line regex fix;
(2) #3326 = `PerlIO::get_layers` (#139 part b) with `:pop` consistent with the list; (3) #3023 SEEK_END on a
`sysopen` handle; (4) #201 closed fd 1/2, MEASURE FIRST.
Acceptance: one-file runs of Capture-Tiny 02-capture, 03-tee, 13-stdout-tied, 20-stdout-badtie, 23-all-tied,
06, 10, 11 and 19-relayering (it passes today and must not hang), Path-Tiny digest.t; the 1,120 #3326/#3327
rows leave by edit as they pass (438 of them re-caused first).  Risks: every regex with `\Q` -- the sweep's
`re/` files and `perl-tests/pat.t` / `regexp.t`, `Pl/t` regex guards (grep `\\Q`); io layers -- companion
`io/layers.t`, `io/crlf.t`, `io/utf8.t`, `perl-tests/readline.t`; Capture::Tiny's `_relayer` loop (a
non-shrinking list = a hang, so a timed row).  WHAT-TO-RUN-WHEN: `cl/` => full sweep + the named companion files.
Fable rulings needed BEFORE: #139's design call (a synthesized list from the one discipline is acceptable;
what `:pop` does to it); #2501-shape or narrow fix for member 1.

### Batch 2 -- the TAP layer (layer: tap) -- **DONE s521c = MAIN-READY-35 (+ #3530, #3512; #3434 part 1 parked)**
Members: (1) #1576 `subtest` (nested plan, indentation, one parent row, child failure fails the parent);
(2) #3432 `eq_set`.  (#3436 / #3323 are in batch 0.)
Acceptance: Mojo-DOM58 collection / dom / entities, Path-Tiny parent, input_output, temp, size,
mutable_tree, filesystem, HTTP-Tiny 160_cookies, IO-Compress 004gziphdr one-file runs; ~16 `*FILE*` / `*PLAN*`
lines leave by edit; the newly reached rows are blessed with causes.  Risks: the harness row of the table --
the companion `--all --quick` IS a per-batch leg here, plus the sweep; `Pl/t/tap-assert-01.t`; the row parser
`tools/lib/PCLTap.pm` must count a subtest as one top-level row (perl's own TAP shape).
Fable rulings needed: none on semantics (Test::More defines them); confirm the board counts top-level rows only.

### Batch 3 -- Parser2 scoping: the refusal and the silent-wrong that hide 58,000 rows (layer: Pl) -- **#3431 DONE (s521a); #3061 #3375 #3347 DONE s522a = MAIN-READY-38 (2026-10-20): Text-CSV's 20 files run for the first time (55_combi.t 25,119 rows); #3347 measured = #3061**
Members: (1) #3061 sigil-blind capture refusal (delete/rename, never narrow); (2) #3431 embedded `my` in a `my`
RHS in a loop; (3) #3375 `my $x = EXPR for LIST`; (4) #3347 the F6 extent in cz-03zlib-v1 (measure; may be its
own family).  Acceptance: Text-CSV 20_file, 47_comment, 85_util, IO-Compress 01misc, 100generic-gzip,
050interop-gzip, SLU uniqnum one-file runs; Text-CSV's 18 `*PLAN*` + 2 `*FILE*` lines and IO-Compress's 31
leave by edit.  **Expect honest new failures**: Text-CSV runs ~51,000 rows it never ran (its `pos(ELEM)` #966 and
`goto EOLX` #1747 are already known to sit behind them) -- the batch blesses them with causes, it does not
chase them.  Risks: name resolution -> the full sweep IS the gate, the gate-SET scan over both populations
(the refusal stops firing), `Pl/t` grep for the refusal's message text (stale-guard rule).

### Batch 4 -- OO emission and the stash (layer: Pl, cl) -- **DONE s522b = MAIN-READY-40 (2026-10-20): all five members under the s522 bounded stash ruling (+ #135 measured); the delete of a non-sub symbol announces (#3691 = the view removal, open)**
Members: (1) #1570 a parent package must exist at READ time; (2) the new `%Local::Class::` hash-slice read
error (file first); (3) #1572's remaining 081 (`orig`); (4) #3321 stash alias; (5) #1506 `import PKG LIST`.
Acceptance: Role-Tiny extend-role-tiny, proto, namespace-clean; CMM 081; JSON-PP 118; SLU dualvar.
Risks: `p-defclass` emission -> corpus-diff + emission-ab over `lib/**`; mro companion files.
Ruling needed: whether #1510 / #135's stash parts may touch the deferred "Live symbol-table hashes".

### Batch 5 -- diagnosis batch: regex/pos and the silent exits (layer: cl, measured first) -- **DONE s523a = MAIN-READY-39 (2026-10-20): #2900 #1568 #1511-A #3527 fixed (every Text-Balanced and Algorithm-Diff file passes), #966 already fixed, #3282 measured -> #3650 (pack U0 mode, a USER decision) + #3651 (TAP exit status, ruled -> s523c)**
Members, each "measure, then fix if one mechanism": #1568 Text::Balanced (40), #2900 `\G\b` (2), #966
`pos(ELEM)` (4 + Text-CSV parse), #1511 Algorithm-Diff (24 + 847), #3282 the JSON-PP 099 silent exit (+24,512).
Acceptance per member as in its task.  Risks: the regex engine is ONE (portability ruling) -- the sweep's `re/`
files and the companion's `re/` leg.

### Batch 6 -- small module-surface items -- **= MAIN-READY-41 (2026-10-22, s523b merged d6d2d227; all five members DONE).  #2016 a decimal literal whose MAGNITUDE is above UV_MAX (or a radix literal above 64 bits) is perl's double -- JSON-PP 110_bignum.t 9/0 (row out); #3529 set_subname / subname read and write the ONE sub-name record -- SLU exotic_names.t 652 -> 912 of 1560 (908 rows out, 648 back re-caused: 260 NS #233 anon-sub frame, 388 #3670 / #3671), subname.t re-caused NS B::Deparse, Try-Tiny named.t 3 rows re-caused #233; #1573 + #2066 Data::Dump measured -- the bundle passes but quote-unicode.t 5 (#932) and quote.t 12 (#1389), re-caused, MIME::Base64 encode / decode aliases; #3001 Role::Tiny measured fixed by s522b (role-basic-composition.t 34/0); #1571's lines: #3672 fixed -- a reference stored into a tied scalar reaches STORE as a reference, reftype / refaddr fetch once -- SLU getmagic-once.t 6/0 (4 rows out), reftype.t's IO row re-caused #3674, openhan.t 21/0 already.  Open: #233 (USER ruling), #3670 #3671 #3673 #3674 #3675.  The HARNESS batch s523c and a RESIDUES batch s523d were checkpointed with it.  s523c = MAIN-READY-42 (2026-10-22, merged 0f7fc8bd) -- SHIPPED: #3651 a Test::More run ends as Test::Builder ends it (exit status min(F,254) / 255 + the closing lines; every runner asks `exit_is_verdict`), so the seven board `*PLAN*` rows carry rc F / 255 instead of rc 0 (edited row by row from one-file runs); #3528 the SBCL 2.5.2 floor (`p-int`'s double arm ranged; `tools/t/sbcl-floor.t`); #3526 residue 1 (`tools/ci-step` keeps the error MESSAGE as its own annotation).  OPEN: #3700 (`int` of a double beyond IV/UV answers an exact integer where perl answers an NV); #3526 CLOSED (the install-matrix ran GREEN on the -42 push, 2026-10-22).  s523d (the RESIDUES batch) = MAIN-READY-43 (2026-10-22, merged ef5df65a) after one send-back (the first #3691 unintern crashed top-level reads under form-at-a-time loading; now a mark set) -- SHIPPED: #3613 `utf8::valid`, #3611 `%{ELEM} = LIST` vivifies, #3612 `for $\ / $/` keeps the pre-loop separator, #3691 a stash delete of a non-sub removes the glob from the view, symbolic reads and the @ISA readers -- 226 blessed Text-CSV rows out (85_util 154 incl. #3617, 80_diag 26, 45_eol 16 incl. #3614, 46_eol_si 14), 50_utf8 / 70_rt run to their ends or to the `use bytes` row, perl-tests/ref.t 200 and mro/basic.t 51 63 64 66 restored.  OPEN: #3632 #3610 #3616 #3633 #3634 #3635 #3720 + #3691 residue (a symbolic write after the delete shares the old cell)**
#1573 / #2066 Data::Dump, #2016 JSON integers (also an everyday row), #3001 Role::Tiny, the #1571 lines
that are not ruled (reftype REF/IO, get-magic count), #3161's InflateScan if the shim stays.

### NOT IN THE PLAN (what each ruling holds)

| holder | rows held (+ unseen) | what would release it |
|---|---:|---|
| #233 caller fidelity (USER deferral s488) | 124 | a USER word; items (a)(b)(c)(h) are the cheap half |
| #1361 croak location (behind #233, DECIDED line 135) | 113 | the same word, narrowed: a per-frame saved location |
| #1389 is_utf8 (Fable ruling s473h) | 25 | a downgraded string form (the ruling's own reopen clause) |
| #1749 pack U (USER park on pack/unpack) | 20 (+3) | lifting the pack park |
| #221 warnings (unscheduled design) | 14 | scheduling the one-boolean design |
| #2370 scope-exit DESTROY (USER park s495) | 9 (+71) | a USER word |
| #930 lvalue subs (USER deferral s464) | 8 (+361) | a USER word |
| XS: bzip2, Sub::Util subname, exotic_names, CMM 120-fresh (pclxs parked s485) | 23 (+1,575 and the bzip2 files) | pclxs |
| ruled NS: error text, prototype, Internals, given/when, @_ aliasing | 21 | -- |
| PERL-SKIP | 47 | not failures |

Together: about **360 baseline lines** are held by rulings (plus the 47 PERL-SKIP lines), 237 of them by the caller-fidelity deferral and
its Carp corollary.

### Projection (an estimate, not a measurement)

Starting from the baseline's 1,803 lines and section 1's 449 files:
* after batch 0 (s521a): about -22 lines, +7,600 IO-Compress rows reached (#3433);
* after batch 1: Capture-Tiny's 1,120 lines leave if both mechanisms close as measured; +2,600 rows reached
  (06, 10, 11) -- the board's failing lines roughly halve (to ~660);
* after batch 2: ~16 more `*FILE*` / `*PLAN*` lines leave, ~380 top-level rows reached, Mojo-DOM58 and Path-Tiny
  move from FAIL to PARTIAL/PASS;
* after batch 3: ~55 lines leave, but ~58,000 rows run for the first time (Text-CSV's 51,000 dominate) -- the
  failing-row count will likely RISE, honestly, which is the point;
* after batches 4-6: ~100 more lines, depending on what the diagnoses find.
What would remain is about 360 ruled lines, the 47 skips, plus whatever the newly reached rows expose.

## 5. What the board does not see

* **~98,000 unseen rows**: 73,616 planned-but-never-run rows behind the 79 parseable `*PLAN*` lines, plus
  ~24,500 perl rows behind the 64 non-skip `*FILE*` lines (2c).  The board's TOTAL passing (12,250 + 1,855) is
  smaller than what it hides.  Text-CSV (51,129 behind #3061) and JSON-PP's 099 files (24,512 behind #3282) are
  three quarters of it.
* **Unnamed rows share one join key** (#1041's rule): a key standing for 8 rows cannot report the 5 that pass
  now (SLU refaddr.t); `05_extmul.t` is 14 such rows.
* **Stale lines wait for the round**: since s519 the board runs once per round, so fixes that are not named
  in their batch (CMM 002 / 003, Path-Tiny children) remain blessed failures until the round-2 diff reports
  them FIXED.
* **TIMEOUT**: one allowance, Capture-Tiny 00-report-prereqs.t (300 s; 70 s alone, killed at 120 s under the
  4-job board).  No board file is registered as a hang.
* **Flags**: Scalar-List-Utils runs `--no-dist-lib`, so its 38 files measure PCL's List::Util / Scalar::Util /
  Sub::Util SHIMS, not the dist's module -- a pass there says the shim agrees with perl.  No dist uses `--inc=`
  today.
* **IO-Compress's oracle on this box** (unverified, worth a look before blessing more of its rows): run by hand
  from the dist root with `-Ilib -It -It/compress`, perl 5.40.3 dies "Compress::Raw::Zlib version 2.214
  required--this is only version 2.212" in its adapters (e.g. 105oneshot-deflate.t, 050interop-gzip.t); the
  join's perl counts were taken some other way, which this review did not trace.
  **RESOLVED (s521 evening): `tools/run-dist-t.pl` has no perl oracle -- its `PLAN N` is the TAP plan line and its rows are PCL's; the death is a hand-run artifact (the dist's IO::Compress 2.214 against the system Compress::Raw::Zlib 2.212).  The same mismatch kills the `$^X` children of 011-streamzip: #3520.**
* **Untidy deaths**: three of the confirmed whole-file losses end in a raw SBCL "Unhandled ... in thread"
  banner (the #3061 transpile refusal, the two #1570 READER errors and namespace-clean's) -- the USER's
  "fail tidily" rule applies to them independently of the fix.
