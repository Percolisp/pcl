# A verdict register for the CPAN board (and the other test populations) -- PROPOSAL

Status: **proposal** (s530, 2026-10-31, Fable; the USER asked for a register of CPAN test failures that says which ones perl skips too and which ones come from features PCL deliberately does not support).  Not built.  Queued for s531 (see the end).

**REVIEWED s531 (Fable 5.1, 2026-11-01) -- RULED: REWORK, SMALLER.  Do not build this as written; the rulings at the end (`## Review`) replace sections 1-5 and the open question, and `briefs-and-rules-for-claude-subagents/s531/s531b-prompt.md` is the batch that builds the ruled shape.**  Sections 1-5 below stay as the record of what was proposed.

**BUILT s531b (2026-11-01) in the ruled shape**: the grammar, the ids, the validator, the oracle + SKIP + the split and the migration are described where they live -- `docs/failure-cause-classes.md` ("The grammar", "The KEY is checked", "The CPAN board"), `docs/not-supported.md` ("How to read an entry"), DECIDED `## s531b`.

## The problem, measured

The CPAN board's file verdicts are PASS / PARTIAL / FAIL, and FAIL mixes three different things.  At s530 (30 dists, 896 files) **138 files read FAIL**, and a review had to join three files by hand to find that only **41** of them are PCL bugs:

| what the FAIL really is | files |
|---|---|
| perl SKIPS the file too (`1..0 # SKIP`: a missing optional module, an author test) | 69 |
| fixed after the run started (stale) | 4 |
| perl itself fails the rows (the dist's own TODO) | 2 |
| a feature PCL ruled out (XS 14, `caller` fidelity 4, source filters, B::Deparse, given/when, >U+10FFFF) | 22 |
| **a PCL bug** | **41** |

The row record that should answer this is the CAUSE column of `baselines/cpan-board14-fails.tsv` (2,233 rows), but it is free text:

| how the cause is written today | rows |
|---|---|
| a task number (`#3282 (...)`) | 1,368 |
| `NS:...` (not supported) -- in **18 different spellings** for the same rulings | 761 |
| `PERL-SKIP (...)` | 69 |
| a task AND an NS reason in one cell | 21 |
| "perl itself TODO-fails this row" as prose | 11 |
| "cause not isolated" | 2 |

Consequences seen this session: a cause can go **stale** silently (13 JSON-PP rows still cite #1575, fixed in s516b; the real cause is #3282); the cost of a ruling ("what does not supporting `caller` fidelity cost?") has to be re-derived by hand every time (the s521 review built that table once); and the headline number cannot say "assertions PCL gets wrong that perl gets right, in features we support" -- the number that matters.

perl's own test suite already solved half of this: `cl/skip-registry.lisp` (`docs/test-skip-registry.md`) keys a not-supported row on its description, cites a `docs/not-supported.md` reason, still runs the assertion, and flags itself stale when the row starts passing.  The CPAN board never got the same discipline.

## The proposal

### 1. A closed vocabulary for the CAUSE column

Every row's cause is exactly one CLASS with one KEY, plus an optional free-text note in a new last column:

| class | key | meaning | counts as |
|---|---|---|---|
| `PERL-SKIP` | -- | perl's own run skips this file / row too | **not a failure** |
| `PERL-FAILS` | -- | perl fails or TODO-fails the row too | **not a failure** |
| `NS` | an id from `docs/not-supported.md` | a feature PCL deliberately does not support | ruled out |
| `BUG` | `#NNNN`, an OPEN task | a PCL bug | **ours** |
| `UNREDUCED` | `#NNNN`, an open task holding the hunt | not isolated yet | **ours** |
| `HARNESS` | `#NNNN` | the board / runner, not PCL | instrument |

A row that today names a task AND a ruling (`#233 + NS:caller`) gets the ruling as its class when the ruling is the reason it stays failing, and the task in the note.

### 2. Stable ids for the rulings

Each section of `docs/not-supported.md` (107 today) gets a short stable anchor (`ns-xs`, `ns-caller-fidelity`, `ns-destroy-scope-exit`, `ns-lvalue-sub`, `ns-source-filters`, ...).  The CPAN register, `cl/skip-registry.lisp` and the companion suite's baselines all cite THE SAME ids -- one ruling, one name, countable across every population.

### 3. A validator in the gate (`Pl/t/`, cheap, no SBCL)

- every cause parses into the vocabulary;
- every `NS` id exists in `docs/not-supported.md`;
- every `BUG` / `UNREDUCED` / `HARNESS` task exists and is NOT completed -- **a cause pointing at a closed task fails the gate** (the stale #1575 rows would have been caught the day #1575 closed);
- `PERL-SKIP` / `PERL-FAILS` are not taken on trust: the board already runs perl as its oracle, so the scoreboard checks them against perl's own TAP for that file at every board run and reports a mismatch.

### 4. The scoreboard reports by class

- File verdicts gain **SKIP** (#4160): a file perl also skips is never FAIL.
- The per-dist line and the totals split the not-ok rows: **ours** (BUG + UNREDUCED) / **ruled** (NS) / **perl's** (PERL-*) / harness.
- The headline becomes: *"N assertions PCL gets wrong that perl gets right, in supported features"* -- with the ruled-out cost beside it per NS id (the s521 "NOT IN THE PLAN" table becomes a query, not a review).

### 5. Migration (one batch, rows BY EDIT)

A one-shot script maps the 2,233 causes: the 1,368 task causes and the 69 PERL-SKIP mechanically (checking each task's status -- closed ones are surfaced for re-attribution, never auto-mapped); the 761 NS rows by mapping the 18 spellings onto the new ids; the 21 mixed and 13 prose rows by reading them.  The baseline's blessing rule does not change: rows leave by EDIT, never by re-blessing a run.

## Size and order

Tools + baselines + one docs pass; **no compiler or runtime change**.  About one agent session.  It replaces #4160 (the SKIP verdict is its item 4) as **s531b's member 0**; the companion suite's alignment to the same ids (item 2) can follow in a later batch.

Open question for the USER: whether the companion suite's (perl's own `t/`) failure baselines should move to the same vocabulary in the same batch, or after the CPAN board has used it for a round.

## Review (s531, Fable 5.1) -- RULED

Reviewed 2026-11-01 at the USER's request, from first principles and from the files on main `9c450fa2` (every number below re-counted; the counting scripts and notes are in `~/pcl-agent-scratch/s531/review/`).  The USER asked for the review's JUDGMENT, not a choice between the proposal's options, so this section is written as rulings.  **Verdict: REWORK, SMALLER -- do not build a register; harden the cause column the project already has, in four pieces.**  The ready-to-launch brief for that smaller batch is `briefs-and-rules-for-claude-subagents/s531/s531b-prompt.md`.

### 1. First principles: the need, and what already answers it

The need (USER, s530): for every failing CPAN test, see whether perl skips it too, whether it comes from a feature PCL deliberately does not support, or whether it is a PCL bug.

Measured on main, that question is ALREADY answered at the row level by a rule and a tool the proposal does not cite: `PCLCauses::cause_class` (Fable ruling s486a, DECIDED `## s486a`, `docs/failure-cause-classes.md`) classes every blessed row of every population into six classes -- not-supported / parked / bug / other / unexplained / perl-skip -- in a ruled ORDER ("not-supported wins over a task: with every filed bug fixed PCL would still fail the row"); `causes_line` prints the split in the sweep, the companion and `cpan-scoreboard.pl --diff`; `tools/cause-census.pl` sums it.  Its answer today for the board: **not-supported 790 / bug 1,370 / other 3 / perl-skip 69** of 2,232 rows.  The three-way split the USER wants exists; a second vocabulary beside it would be the second mechanism owning one fact that the s488 ruling (#1836) is retiring the skip registry for.

What the existing answer gets WRONG, measured:
* **The verdict everyone quotes is the FILE table, and it is wrong by half**: 138 FAIL files, 69 of them files perl skips too (`1..0 # SKIP`).  The row causes know it; the per-file verdict and the per-dist line do not (#4160).
* **Nothing checks a cause's key**: 20 rows are keyed on a CLOSED task (#1575 13, #3530 4, #966 2, #564 1) and 1 on a task that does not exist (#8212, Pod-Simple encod04.t); the companion has 273 closed keys and 45 missing ones, the sweep 20.  A stale key is a wrong record and today it is found by a reader's memory.
* **The rulings have no stable name**: 761 `NS:` cells spell ~14 not-supported sections in 15 ways (the companion's 5,243 in 45), so "what does ruling X cost across the populations" is a hand join (the s521 review built that table once).
* **Two classes have no spelling**: "perl itself TODO-fails this row" is prose in 4 cells (plus 2 of the 7 `#930` rows), "cause not isolated / probable / NOT REDUCED" is prose inside ~52 task cells.

What the existing answer gets RIGHT, and the proposal would churn: **2,226 of the 2,232 cells already begin with their class and key** (`#NNNN` 1,396, `NS:` 761, `PERL-SKIP` 69; 6 prose).  The "mixed" cells the proposal counts as 21 are 542 (514 NS-first `+ #233`-style and 28 task-first), and s486a already classes all 542 as not-supported, which is the intended reading.  Re-spelling 1,396 task cells to `BUG:#N` and adding an eighth column would move every row of a file two running batches (s531a, s531c) are editing, for no information the leading marker does not carry.

**RULED: no new register, no new vocabulary document, no eighth column, no re-spelling of the task cells.**  The register IS the cause column + the s486a class rule, hardened in four pieces: (A) the FILE verdict learns SKIP, measured against a perl oracle; (B) a gate row validates every cause's KEY; (C) the not-supported sections get stable ids and the 761 `NS:` cells are canonicalised to them; (D) the two missing classes get a leading marker.  `docs/failure-cause-classes.md` is THE document and gains the grammar; this file stays as the design record.  Everything is tools + baselines + docs; nothing under `Pl/`, `cl/` or `lib/`.

### 2. The rulings

1. **RULED -- the grammar is the LEADING MARKER, read by `PCLCauses::parse_cause` (class, key, note), with `cause_class` as its wrapper.**  `#N ...` = BUG keyed on N (unchanged spelling); `NS:<id> ...` = not-supported keyed on the id; `PERL-SKIP ...`; `PERL-FAILS ...` (new); `UNREDUCED #N ...` (new); `HARNESS #N ...` (new); `PARKED:<id> ...` (kept: 47 sweep + 88 companion rows carry a USER scheduling decision, which is not a not-supported section; the key is a not-supported id or a DECIDED anchor); `UNEXPLAINED` (kept: the legal first-bless marker, "queue, not baseline", #993).  Everything after the marker's key is the note, and the note is never classified -- that is what makes the key checkable (ruling 3).  s486a's order stands for a cell that names a ruling and a task: the ruling is the class, the task is in the note.  `HARNESS` = the board tooling (`run-dist-t.pl`, the scoreboard, the oracle); `cl/pcl-test.lisp` is PCL's product and its bugs are BUG.  The `UNREDUCED` dumping-ground guard: its key must be an OPEN task that names the file and the measurement taken, the CAUSES line prints the UNREDUCED count, and `cause-census.pl --hygiene` lists every UNREDUCED row older than two rounds.  Candidates today: #4087 34, #3862 6, ten "probable", two "cause not isolated" (NO task -- file one) = ~52 rows.
2. **RULED -- the validator is a gate row (`Pl/t/`, no SBCL) that checks the KEY only, never every `#NNNN` in the cell.**  Measured: 432 of 2,232 cells mention a closed task somewhere (264 of them the history note "since s523b #3529" inside `NS:caller` cells); only 20 + 1 have a bad KEY.  "Closed" = status in {completed, done}: the task store spells six statuses (completed 970, done 76, pending 837, open 41, in_progress 4, in-progress 1) and three files have no `id` field (407-409); the validator normalises, and a missing or id-less task file is red.  It reads the TREE's store (`dot-claude-in-home-dir/tasks/pcl`, 1,930 tracked files, so CI has it); in an agent worktree that is a stale checkout, so **the row is authoritative on MAIN's gate (the merge gate) and advisory in a worktree**.  It runs over all four baselines from day one: GATE for the board (`cpan-board14-fails.tsv`), REPORT -- counts printed, never red -- for `perl-suite-fails.tsv`, `fail-baseline.tsv` and `row-shortfall.tsv` until ruling 8's batch flips them.
3. **RULED -- CLOSE-WITH-ROWS is the rule that makes a closed-task gate row safe**: a task is closed only in a commit that also leaves or re-causes every row keyed on it, on the strength of a one-file run (s526w2 already requires the row edit; this adds "and the close rides with it").  The agent does it inside its batch; the reviewing session's close-at-merge then finds no rows.  The merge-order trap, named: batch Y adds rows keyed on T while batch X closes T -- Y's final rebase turns the row red and Y re-causes at the rebase (seconds, no SBCL).  A task closed as duplicate or superseded (#564 `done`, #966) is the same event: its rows move to the successor -- the point, not a trap.
4. **RULED -- the board gets a perl ORACLE leg, because the proposal's "the board already runs perl" is false.**  `tools/run-dist-t.pl` runs pl2cl + SBCL only (the s521 review recorded it: "run-dist-t.pl has no perl oracle"); the baseline header's "REAL PERL WAS RUN BESIDE EVERY FILE" was a hand measurement at bless time; the companion DOES run perl (its perl-verb column: `not ok/ok` 12 rows = perl fails, PCL passes).  `run-dist-t.pl --rows --oracle` runs `perl -I... t/FILE` from the dist root under the same @INC rule and emits one `PERL\tplan\tskip_all\tok\tnotok` line plus perl's TODO rows; the result is CACHED under `~/.pcl-cache/oracle/` keyed by perl's version + the t-file's path, size and mtime (`--refresh-oracle` at the round run: a module installed on the box changes perl's skips and the key cannot see it), so the per-run cost after the first is nil; the first run's cost is measured in the batch.  With it the scoreboard (a) gives verdict **SKIP** only when BOTH sides print `1..0 # SKIP` (#4160's own wording), (b) prints a **MISMATCH** report line (PERL-SKIP claimed but perl ran rows; PERL-FAILS claimed but perl's row is ok) in the board run -- never a gate row, because perl's answer depends on the box's installed modules -- and (c) can assign `PERL-FAILS` mechanically (perl's row `not ok # TODO`).  **Two of the 69 PERL-SKIP rows are not skips under this rule**: HTTP-Tiny 190_find_CA.t and 200_live_local_ip.t read "no TAP rows (rc=0)" -- PCL exits 0 without the `1..0 # SKIP` line perl prints; re-measured in the batch (a divergence, probably BUG).
5. **RULED -- stable ids are EXPLICIT anchors**, `<a id="ns-..."></a>` on the line above each `##` AND `###` heading of `docs/not-supported.md` (107 + 21 headings; the board cites two `###` subsections today: `use bytes` 35 rows, `>U+10FFFF` 5).  The Index's 121 GitHub auto-anchors derive from heading text and move whenever a status tag in a heading changes -- they cannot be the key.  The section's author assigns the id; the gate row checks uniqueness and that every cited id exists; `tools/ns-ids.pl --list|--check` is the one reader.  A retired section keeps its id with `RETIRED -> <successor>` (the validator treats it like a closed task); a split keeps the parent's id.  The 761 board `NS:` cells are canonicalised to `NS:<id>` by a 15-row spellings table -- mechanical, and the one mass edit this batch makes.  **`cl/skip-registry.lisp` is NOT touched**: the USER ruled it RETIRES into the `NS:` column after the next tag (s488, #1836, "two mechanisms own one fact"); the batch writes the title -> id table into #1836 so its step 1 writes `NS:<id>` directly.  `perl-suite-expected.tsv`'s `not-supported.md §title` spelling (111 files / 2,319 XDIFF rows) migrates with the companion (ruling 8).
6. **RULED -- the headline is defined over ASSERTION rows and says what it hides.**  `*FILE*` rows (115: PERL-SKIP 69, NS 17, task 29) and `*PLAN*` rows (56: NS 20, task 36) are one line each for a whole file; 42 of the 56 `*PLAN*` rows parse `ran M of N` and hide **31,134** never-run assertions today, the 46 non-skip `*FILE*` rows hide an unknown count until the oracle supplies perl's plan (the s521 review's ~98,000 was 73,616 + ~24,500 at its date).  The board's per-dist line and totals therefore read: PASS / PARTIAL / FAIL / **SKIP** files; not-ok rows split **ours** (BUG + UNREDUCED) / **ruled** (NS + PARKED) / **perl's** (PERL-SKIP + PERL-FAILS) / harness; "+ M assertions never reached behind K whole-file rows (J of them ours)"; the ruled cost per NS id on request (`cause-census.pl --by-id`).  "That perl gets right" is literal once the oracle has run.  The SKIP verdict's interplay, checked: `--rows` must still write the `*FILE*` row for a SKIP file (else the first `--diff` reports 69 FIXED); the per-file table gains SKIP; `--diff`'s LOST compares `ok` counts only and does not move; `$grand_bad` counts FAIL alone; the prose verdict counts in `cpan-board-dists.tsv`'s why column (YAML-PP, Template-Toolkit) are edited by hand.
7. **RULED -- the migration is one idempotent script kept in the tree (`tools/migrate-causes.pl`), re-run at the batch's final rebase.**  SHARED-BOX rule 1 ("the register migrates whatever main holds at ITS final rebase") is sound only on that condition, because s531a (the 13 #1575 rows -> #3282, the #1815 rows) and s531c (#4083's 316 rows) edit rows in today's format while s531b runs -- and under ruling 1 their edits are ALREADY in the grammar (a leading `#N`), so the overlap is only the `NS:` cells.  Mechanical: the 761 `NS:` spellings -> ids; `#930 (NS:Lvalue ...)`-style task-first cells naming a ruling -> `NS:<id> -- #930 ...` (28 cells; s486a already reads them as not-supported, the edit makes the key explicit).  NOT auto-mapped -- decided by hand and recorded in a table inside the script keyed (dist, file, description) so the rebase re-run reproduces them: the 20 closed-key rows and the #8212 row, the ~52 UNREDUCED candidates, the 6 prose rows (-> PERL-FAILS / UNREDUCED), the 7 `#930 ... perl itself TODO-fails 2 of these rows` rows (which two = the oracle's TODO rows), the 2 HTTP-Tiny no-TAP rows.  Any row new since the script was written that it cannot map is LISTED at the rebase and mapped by hand.  One agent session: yes -- 761 mechanical cells + ~90 hand rows + the three tools.
8. **RULED -- the companion suite does NOT move in this batch; its rows move in #1836's batch, after the v0.2.0 tag.**  Sized from the files: `perl-suite-fails.tsv` holds 10,141 rows with **273** closed keys (17 tasks: #376 62, #1719 50, #155 39, #2380 29, #1870 28, #3492 18, ...) and **45** missing tasks; its 5,243 `NS:` rows use 45 spellings; the sweep's 459 rows add 20 closed keys and 29 spellings; the shortfall's 217 rows add 46 free-text starts.  Each closed-key row needs a one-file companion run (~1 min) to learn whether it now passes before it can be re-caused: 17 tasks over tens of files is a session of its own, fourteen times the board's 20.  The companion already measures perl's verdict per row, so `PERL-FAILS` there is a column fact the grammar must not duplicate.  And the USER has already ruled the companion-side rewrite this would ride on: #1836 retires the skip registry into `NS:` rows -- the same files, the same kind of edit; the ids and the validator (REPORT mode from s531b, so the 273 + 45 are a visible queue from day one) make that batch mechanical.  The proposal's open question is thereby replaced, not handed back.

### 3. The seven questions, short

1. *Sound?*  As a register, no (a second vocabulary beside s486a); as the hardening of the existing column, yes (rulings 1-6).  Counts re-measured: **2,232** rows (not 2,233); causes starting with a task **1,396** (not 1,368 -- the 28 task-first mixed cells are inside); `NS:` **761** (matches); PERL-SKIP **69** (matches); prose 6; 11 cells say TODO (matches); NS spellings **15** for ~14 rulings (not 18); mixed cells **542** (not 21); the 13 stale #1575 rows confirmed, the true bad-key set 20 + 1.
2. *The validator as a gate row*: safe under CLOSE-WITH-ROWS and the key-only check (rulings 2-3); the PERL-SKIP / PERL-FAILS check is a MISMATCH report line of the board run, not a gate row (ruling 4).
3. *Ids*: explicit anchors, author-assigned, gate-checked, retire-by-pointer; the registry is left to #1836 (ruling 5).
4. *The headline*: well-defined over assertion rows with the hidden count beside it; SKIP does not disturb `--diff` (ruling 6).
5. *Migration*: one session, by script, re-run at the rebase; the hand table is what must not be auto-mapped (ruling 7).
6. *Companion*: ruled, not handed back (ruling 8).
7. *Cheaper or wrong*: the cheaper shape IS the verdict -- 761 + ~90 cells instead of 2,232, no new column, no new document; the oracle is the only new cost and the only way to make "perl gets right" literal, and its cache makes it a one-time cost.  Wrong in the proposal: the second class rule, the assumed oracle, the eighth column, the dropped PARKED / UNEXPLAINED, "21 mixed", the registry alignment against its ruled retirement, the headline silent on `*FILE*` / `*PLAN*`, four stale counts.

### 4. Open to the USER

Nothing technical.  The one dependency the USER holds is an event already ruled: the v0.2.0 tag, which releases #1836 (and with it the companion's rows, ruling 8).  If the USER would rather see the companion's 273 + 45 bad keys fixed BEFORE the tag, that is a priority call -- it is a second agent session, and it can run as its own batch any time after s531b lands.

