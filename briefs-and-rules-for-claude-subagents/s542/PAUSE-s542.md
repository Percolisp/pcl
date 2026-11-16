# s542 running notes (the reviewing session, 2026-11-15 18:5x ..)

Read bottom-up after a cut.  The stop state this session started from is `s541/resume-after-stop.md`.

## 18:5x -- start.  The USER's word: continue; THREE subtasks at a time
* Box: REBOOTED (up since 18:47), `/tmp` empty, nothing running, `HEAVY.holder` empty, 10 GB available.  Main `6a8ab355` = origin/main, clean; CI green on it (the `CI` workflow; the install matrix did not run on these pushes).
* The three stopped batches are clean in their worktrees at the shas `resume-after-stop.md` names: s540c `83e9f337`, s540d `7f7628a5`, s539a `a589037f`; each STOP.md starts NOT MERGE-READY; each MODEL.txt reads `claude-opus-5-5`.
* `s542/resume-s542.md` written: the reboot, main's state, and THE EXCLUSIVE-LEG ORDER (one exclusive leg queued on the box at a time; s540d's timing legs first while the box is quiet, s540c's and s539a's bounded legs meanwhile, s539a's companion `--all --quick` last).

## 18:5x -- the three batches RELAUNCHED in their existing worktrees (opus / xhigh, no isolation)
* s540d (`agent-a3aa03b36ccfc5f5d`), s540c (`agent-ab2915f40902e154e`), s539a (`agent-a1557aa31c883cbc0`); each told: MODEL.txt first, COMMON.md, `s541/resume-after-stop.md`, `s540/resume-s540.md`, `s542/resume-s542.md`, its STOP.md, its brief; its place in the exclusive-leg order; what the merge review will read; never edit CLAUDE.md.
* Main `4d429921` (the s542 notes; records only).

## 19:1x -- s540d's three timing legs ended 19:08 (bench1, bench2, comptr, each with its END line); the bounded legs of s540c (sweep) and s539a (everyday, Pod-Simple corpus.t) started at once: no queueing this time
* MERGE-REVIEW PREP for s540d, done while it writes its records: its rewritten rows read (constants-01.t, constant-scope-01.t, module-fasl-cache-01.t, vendor-ppi-01.t: the old expectation is kept verbatim under `PCL_OPT=-const-fold`, a new row pins the fold; the fasl-default row follows the s540 ruling); 16 const-fold probes perl -> base -> tree (`**` on a negative constant, autoquote and hash-key slots, redefinition by glob, a second `use constant`, packages, string and number spellings, contexts, method calls, a use above the declaration, string eval, list / ref / undef constants, the hash form, the `print` slot, an exporting module): **0 REGRESSION, 2 FIXED vs base** (a redefinition by glob no longer reaches a folded use; a same-named sub in another package), 10 same, 4 differ on base and tree alike:
  - `18446744073709551615 + 1`: integers are unbounded, by design (not-supported.md).
  - a second `use constant X => 2` below a sub that used the first: both 2 (perl 1 then 2, with its mandatory "redefined" warning) -- not filed (code perl warns about).
  - **#4792 FILED**: `defined NAME` with an ALL-CAPS constant is lowered as a FILEHANDLE test and answers false (silent wrong).
  - **#4793 FILED**: a `()` sub installed by a BEGIN-time glob assignment is unknown to the parse (`Y + 1, "\n"` read as Y's arguments); the detector is silent even under `PCL_DETECT=die`.
* s539a's `Pl/` diff read against the s541 ruling (#4881): the mechanism is keyed on the record's `import_directive` fact, the data is the attribute on the shim's forward declaration; no module or function literal in the consumer.  Probes owed at its merge.
* #4986 probed for s540d's merge (a `-I` module, a private cache): a same-size rewrite with the SAME or an OLDER mtime answers the old text -- on the tree AND on the base (the text cache had the same validity rule), and it is the blessed `ns-cache-same-size-rewrite` (#4165, s532): no finding.  (A `PCL_CACHE_DIR` must be mode 700, or PCL refuses it: say so in a probe recipe.)

## 20:3x -- the USER asked whether things hang (few tokens, little CPU).  They do not; the box is SERIALIZED behind one exclusive leg
* s539a put its whole exclusive turn into ONE leg (`comp2.sh`): everyday before -> after, two bench A/Bs, the companion `--all --quick --jobs 4`, and then 33 candidate movers each ALONE on base and tree (two `--jobs 1` runs).  It has held all three slots since 19:46; since 20:17 it is in the mover phase, ~2 cores busy.  s540c (bench, everyday `--record`) and s540d (sweep2, ir-conform, gate3 after its two fix-forwards) wait behind it; the agents sleep on their END lines, hence no tokens.
* The first mover rows read the SAME on base and tree (`comp/retainedlines.t`, `io/pvbm.t`, `op/bop.t`: "REAL MOVE" against the snapshot, which dates from MAIN-READY-63) -- so they are the unrounded merges' movers, the ROUND's to attribute, not s539a's.
* RULE FOR THE NEXT BRIEFS (goes into the next resume file): the companion's mover re-runs are `--jobs 1` work and go in a BOUNDED leg (two slots: base and tree), never inside the exclusive leg; an exclusive leg holds ONE measurement.  And a companion `--all --quick` whose snapshot is many merges old produces dozens of movers that are not the batch's: the round must run BEFORE a batch is asked for that leg.

## 20:4x .. 21:0x -- **s540c MERGED = MAIN-READY-93 `0df490cf`** (code sha `75904a35`, generation v2-14500 on main)
* MERGE-READY `d7e44ddf` at 20:41; ff 17 commits after a clean rebase onto `4a6fad81` (the ff verified by its own exit status).
* REVIEW: its probes re-run perl -> base -> tree (`r1`, `keep`, `r2`): as its table says (tree = perl where the base hangs; at the drop tree = base = 0 bytes, perl 5).  The rule-12 read of the two drain functions: an unknown MODE behaves as `:write` (three internal callers; left as is).  My own probe (`undef $fh` / `$fh = undef` / `delete $h{fh}` / `undef $obj` on an escaped handle): 0 bytes on base and tree -> **#4794 FILED** (a flush at the literal statement, never a close).
* Bench cost kept and flagged: `fhopen` +4.0 %, `fhread` +5.1 % (not re-measured by me).
* MERGED-MAIN gate `~/pcl-agent-scratch/s542/merge/c-gate.log` END rc=0: **375 files / 12,156 rows PASS** (load 23 with three agents' legs beside it: 649 s wall).  This is also the re-measurement of main's gate the s541 stop asked for.
* Records: CLAUDE.md numbers paragraph REPLACED (the old one to the top of HISTORY.md part 1), rule 10 + Dependencies got s540c's two pointers (REFERENCE.md the same), STATUS.md gate row, DECIDED `## s542`, session-log `## Session 542`, COMMON.md "HEAVY SLOTS".  `heavy.sh`: every file argument is read for the exclusive test; `HEAVY_BOUNDED=1` (the s530 script kept beside it as `heavy.sh.three-slot-s530`).
* s540d, s539a, s540e TOLD (rebase over `75904a35`; what to re-take; s539a's companion is NOT re-taken: tree vs base stands).
* **s540e LAUNCHED 20:4x** (speed, the shim half; a new isolated worktree; opus / xhigh; told to take its base only after this merge).  RUNNING: s540d, s539a, s540e.  NEXT freed slot: s539c (PPI batch 9, v2-14900), then s539b (v2-15000, + #4822), s541a after s540d merges.
* PUSHED 21:1x: `6a8ab355..0e8830fe` (MAIN-READY-93 + the records + a comment line in `install-matrix.yml` so the matrix re-runs: s540c uses SBCL's finalizer-thread internals).  CI and the install matrix to be read at the next check.
* THE ROUND (owed: five unrounded code merges now) is planned for right after s540d and s539a merge -- one round then covers MAIN-READY-88..-95 -- and is run while the newly launched batches are still reading (it needs the box: the companion `--all --quick` is exclusive).
* 21:3x: **CI GREEN on `0e8830fe`, BOTH workflows** (`CI` and `install-matrix`: the floor images pass with s540c's finalizer code).  All three slots busy with bounded legs (s540d rc4, s540e's first gate, s539a's everyday on the rebased tree); load 11; nothing queued exclusive.

## 21:4x -- **s539a MERGED = MAIN-READY-94 `de3bf103`** (code sha `00c6f785`, generation v2-14800 on main)
* MERGE-READY `de3bf103` at 21:3x, already on main's head: ff 14 commits, verified by its own exit status.
* REVIEW: its STOP.md sections 1-5 read (the mechanism grep, eleven probes, the bench inside the control band, the companion: no row differs between tree and base).  MY probes (`scratchpad/p539a/b1 b2 b3`), perl -> main -> tree: the block-form `first { } LIST` and `sum(...) + 1` after `BEGIN { use_ok('List::Util', qw(...)) }`, `use_ok(...) or BAIL_OUT`, `blessed {}`, the builder methods: tree = perl, main fails; tied elements copied (FETCH counts): tree = main = perl.
* MERGED-MAIN check: 11 guard + records files / 381 rows PASS; the two probes pass on main.  No second full gate: merged main IS its gated tree (ff without a rebase; the commits after its gate are records).
* TASKS CLOSED (each has its DONE note; no board row is keyed on any): #3823 #3495 #3619 #4881 #3852 #4167 #4962 #4883 #4884 and s540c's #4866.  #2006 stays open (its residue keys 36 board rows).  #4790 not started, open.
* Records: CLAUDE.md numbers paragraph replaced again (the MAIN-READY-93 one to HISTORY.md), STATUS gate row 12,193, REFERENCE.md `shipped-modules.md` description gains the import-directive fact, DECIDED + session-log lines.
* s540d TOLD: rebase; RENUMBER to **v2-14850** (main passed its v2-14600); `_proto_shape_key` is edited by both (keep both deletions); read s539a's TAP-layer fasl change against its own fasl work.  s540e TOLD (v2-14860 if its emission changes).
* **s539c LAUNCHED 21:4x** (PPI batch 9, #4784 #3555 #4761 + the expect checker; v2-14900; a new isolated worktree; opus / xhigh).  RUNNING: s540d, s540e, s539c.  NEXT freed slot: s539b (v2-15000, + #4822); s541a after s540d merges.

## 22:0x -- PUSHED `0e8830fe..87694cbc` (MAIN-READY-94 + records).  The Try-Tiny `finally.t` question BISECTED (one-file runs, `~/pcl-agent-scratch/s542/round/finally-NN.out`)
| tree | rows printed | ok / not ok | failing rows |
|---|---|---|---|
| MAIN-READY-89, -90, -91 | 30 | 23 / 7 | 10 11 12 15 17 18 23 |
| MAIN-READY-92 (s539d, the sub-call frame) | **16** | 12 / 4 | 10 11 12 15 |
| MAIN-READY-93 (s540c) | 16 | 12 / 4 | 10 11 12 15 |
| MAIN-READY-94 (s539a) = main | **30** | **26 / 4** | 10 11 12 15 |
* So: s539d CUT the file at 16 rows (the mover the s541 stop asked about); s539a's TAP-layer work restored all 30, and rows 17 18 23 now PASS on main (s540c's #4866 fix, reachable again).  CAUSE of the cut not reduced (it is gone on main); if a later round sees 16 again, start at the TAP layer's END / global-destruction ordering against s539d's frame vector.
* FOR THE ROUND's board `--diff`: `finally.t` rows 17 18 23 leave the baseline BY EDIT (keyed "NOT REACHED" by s540c: they are reached and pass); the baseline's row 30 "finally ran" (#1571) against the run's row 15 "finally ran" is to be read there (a row NUMBER is meaningful only within its own run).
* 22:4x: **CI GREEN on `87694cbc`** (MAIN-READY-94 + records).  s540d has rebased over MAIN-READY-94 (v2-14850), re-taken everyday (115) and is on its last records legs; s540e has its records written and its full bench queued (exclusive); s539c is at its first fix leg.

## 22:5x .. 23:2x -- **s540d MERGED = MAIN-READY-95 `75146bf6`** (code sha `cc36604b`, generation v2-14850 on main)
* MERGE-READY `fc028717` at 22:45; ff 17 commits after a clean rebase onto main's records (the ff verified by its own exit status).
* REVIEW: my 16 const-fold probes on the FINAL tree, perl -> main -> tree: 0 REGRESSION, 2 FIXED, the same 4 differing on both as before.  Under `PCL_OPT=none` and `-const-fold` vs the default: 12 of 14 identical; `a03` = the ruled redefinition (ir-spec); `a05` = a PRE-EXISTING silent wrong the fold hides for literals -> **#4795 FILED** (a `use constant X` used in its own package is the string "X" when a LATER package of the file defines `sub X`; reproducer + the discriminating measurement in the task).
* ITS "2-ROW GAP" (gate rows 66 over main where it counted 68 added, chased through rc3-rc5): `Pl/t/perf-levers-16.t` has **20 rows, not 22** -- one commit, nothing ever removed, every member has its mechanism row.  So the batch adds 66 and every gate of the day agrees (12,156 -> +37 -> 12,193 -> +66 -> 12,259).  No row was lost.
* MERGED-MAIN gate `~/pcl-agent-scratch/s542/merge/d-gate.log` END rc=0: **377 files / 12,259 rows PASS**.
* FLAGGED, open: #5024 (blessnew-d / blessdes ~7 % slower than main, cause not found; the tree allocates less) -- a speed residue of this merge, for the next perf slot.  The first run of a program with an own module costs +0.8 s once (its one-time compile).
* TASKS CLOSED: #4961 #4963 #4981 #4982 #4983 #4984 #4986 #4987 (DONE notes; no baseline row keyed).  Records: CLAUDE.md numbers paragraph replaced, `const-fold` + "every module compiled once" + the `caching.md` index line; REFERENCE.md the same two; STATUS gate row; DECIDED + session-log.
* s540e and s539c TOLD (rebase over `cc36604b`; the fold appears in lib A/B diffs as s540d's, not theirs).
* **THE ROUND IS A BATCH: s542r LAUNCHED 23:0x** (`s542/s542r-prompt.md`; the board in chunks + `--diff`, the companion `--all --quick` then the full run with stamps, baselines row by row with causes, the 13 `*PLAN*` texts, `finally.t`; a measurement batch: no code).  RUNNING: s540e, s539c, s542r.  NEXT freed slot: s539b (v2-15000, + #4822), then s541a (#4791; s540d is merged now, so it may start).

## 23:4x -- the USER: everything else on the box is turned off -> **FOUR heavy slots for this sitting**
* `heavy.sh` = the four-slot script again (the s536 one + the s542 changes: every file argument read for the exclusive test, `HEAVY_BOUNDED=1`; plus a guard: slot 4 is not taken while a leg of the OLD three-slot script holds the whole box).  The three-slot + s542 script is kept as `heavy.sh.three-slot-s542`: PUT IT BACK at the next session start unless the USER says the box is idle again (memory `project_heavy_slots_memory_pressure`: four slots beside a browser got GNOME oomd-killed in s536).
* State at the switch: 5 GB used / 7 available, load 17; slots 1-3 held by s539c (1) and s540e (2); the round's first board legs had waited since 23:09.  s542r, s540e, s539c TOLD (only legs queued from now on can take slot 4; s542r re-queues its waiting board legs; s540e asked to hold one bounded leg at a time while the round waits).
* Still THREE subtasks (the USER's number at the session start); the fourth slot shortens the queue, it is not a fourth batch.
* 23:5x: **CI GREEN on `0027faa5`** (MAIN-READY-95 + records) and on the memory commit.  The round took slot 4 at 23:42 (board chunk c with the oracle refresh); s540e queued its exclusive bench at 23:52, so s539c's two bounded legs wait behind it until the chunk ends (three slots idle for one chunk: the price of a quiet bench; the round runs in chunks for exactly this).  7-9 GB available.

## 00:1x .. 00:4x (2026-11-16) -- **s540e MERGED = MAIN-READY-96 `7e29313c`** (code sha `d2bc17d4`; no emission change, generation v2-14850 stands)
* MERGE-READY `7e29313c` at 00:11 (3.5 hours after its launch), sitting on main's head: ff 9 commits, verified by its own exit status.
* REVIEW: four probe files of mine perl -> main -> tree (`scratchpad/p540e/`; written with the file tool -- a shell heredoc holding them was refused by a safety check): dclone 14 lines tree = main = perl; MD5 / base64 tree = main (= perl but a croak text); File::Spec tree = main (= perl but `rel2abs` with `//` in the base -> noted on #5044); strftime tree = perl where main was an hour late in a DST gap (its #5047), a seconds field of 60 differs on both (-> noted on #5042).  No regression.
* MERGED-MAIN check: 12 guard + records files / 203 rows PASS; no second full gate (main IS its gated tree: gate 378 / 12,277).
* NOT re-taken by it on the last base (said plainly in its report): the full 77-row bench (taken before s540d merged: mean -0.3 %) and eleven board one-file runs.  Left: #5046 (Clone onto the copier).  The deep-copy depth limit stays 1,000.
* TASKS CLOSED: #4985 #4988 #4989 #4990 #4991 #5047.  Records: CLAUDE.md numbers paragraph replaced, STATUS, DECIDED, session-log.
* **s539c is MERGE-READY too (`a8336235`, on `cc36604b`)**: next.  It needs the rebase over this merge; the two diffs do not meet (PPI + parser vs shims + runtime primitives).
* **LAUNCHED 00:3x: s539b** (binder port 4 + #4822; v2-15000) **and s541a** (#4791, the run-phase board timing; told to cut its timings into exclusive legs of ~10 minutes because the round shares the box) -- both told to take their base after s539c merges.  RUNNING: s542r, s539b, s541a.  The written queue is EMPTY after these: the next brief is mine to write (the silent wrongs the reviews of this session filed: #4792 #4795 #5005 #5021 #4794 #4887 are the candidates).

## 00:5x .. 01:4x -- **s539c MERGED = MAIN-READY-97 `62677d00`** (code sha `c1fe4930`, generation v2-14900 on main)
* MERGE-READY `a8336235` (on `cc36604b`) at 00:5x.  REVIEW, my five probe files perl -> main -> tree (`scratchpad/p539c/`): the `x3` shapes, `{` inside constructors, a CR LF file, lone CRs, eval strings with CRs -- all five tree = perl, main wrong on each.
* Rebased by me over the s540e merge: two append-only conflicts (CHANGELOG.md, baselines/everyday-history.tsv), both sides kept -> `62677d00`.  FULL gate on the rebased tree `~/pcl-agent-scratch/s542/merge/c9-gate.log` END rc=0: **378 files / 12,283 rows PASS**; then ff (verified by its own exit status).
* ITS FLAKE #4904 (one `Heap exhausted` row, `use-base-phase-01.t` row 2, in its first full gate on a cold cache; suspect = s540d's deferred fasl build under concurrency) MEASURED on merged main: the full gate with a FRESH `PCL_CACHE_DIR` at load 12-15, `c9-cold.log` END rc=0: **PASS 378 / 12,283** (the cache grew from 0 to 122 MB).  Not reproduced; the task carries the measurement and stays open as a flake to watch (CI starts cold every time).
* `tools/t/install-container.t` queued 01:2x as the last merge bar (the vendored PPI changed; the s538 precedent) -- the push of main and of the PPI fork waits for it.
* TASKS CLOSED: #4784 #3555 #4761.  Records: CLAUDE.md numbers paragraph replaced + the `keep_line_endings` clause in Dependencies (REFERENCE.md the same), STATUS, DECIDED, session-log.  Prepared: the new body of upstream pull request #459 (133 commits, 53 groups, 459 cases, rows AY AZ BA) in the scratchpad, to PATCH after the fork push.
* s539b and s541a TOLD (take the base from `c1fe4930`).

## 01:3x -- **THE USER: do not start more subtasks.**
* Nothing is launched from now on.  RUNNING and left to finish: s542r (the round), s539b (binder port 4 + #4822), s541a (#4791).  When one ends its slot stays EMPTY; merges of what they deliver still happen.  The brief I was going to write next (the silent wrongs the reviews filed: #4792 #4795 #5005 #5021 #4794 #4887) is NOT written into a launch; the list stays here for the next session.

## 01:5x -- s539c's last merge bar DONE and everything PUSHED
* `tools/t/install-container.t` on merged main: 17 / 17 PASS (`~/pcl-agent-scratch/s542/merge/c9-container.log`, END rc=0 01:49; it waited 20 minutes for the box).
* PUSHED: main `11a6f6df..4e7c1d8c` (MAIN-READY-97 + records); the PPI fork `Percolisp/PPI` `pcl` `c2f21706..66e690d2` (fast-forward, from the bare mirror).
* Upstream pull request Perl-Critic/PPI #459 REFRESHED (`gh api -X PATCH`): title "53 groups ... (133 commits)", the counts paragraph (133 commits, 53 groups, 459 cases; the CR case's TODO tests are now answered by group BA), table rows AY AZ BA; the API answers head `66e690d2`, 133 commits, open.
* CI for `4e7c1d8c` to be read at the next check.
* RUNNING (nothing new is launched, the USER's word): s542r, s539b, s541a.
* 02:3x: **CI GREEN on `4e7c1d8c`** (MAIN-READY-97 + records) and on the notes commit after it.  s539b has its records for the binder port half written and four legs queued (its own companion leg waits behind its own bounded leg); s541a is building its run-phase measurement into `board-timing`; the round is at board chunk c (IO-Compress).  Load 4, 10 GB available.

## 03:3x .. 03:5x -- **s539b MERGED = MAIN-READY-98 `a3d00a12`** (code sha `c5f618f2`, generation v2-15000 on main)
* MERGE-READY `a3d00a12` at 03:3x, on main's head: ff 11 commits, verified by its own exit status.
* REVIEW: its three probes (`pr/b1 r1 s3`) perl -> main -> tree: tree = perl, main wrong on each.  My rename battery (`scratchpad/p539b/e1-renames.pl`, 30 lines): tree = main; = perl except ONE line wrong on both -> **#4796 FILED** (a string eval's `my $x = $x + 1` over a captured lexical reads the new variable: 1 where perl says 8; three of five reduced lines wrong; the discriminating look is in the task).
* MERGED-MAIN check: 12 guard + records files / 376 rows PASS; its probes = perl on main.  No second full gate (main IS its gated tree: 379 / 12,303; the SWEEP, the gate for a rename change, was clean on it).
* TASKS CLOSED: #4921 #4923 #4822.  Kept open: #4922 (blockers the binder made unnecessary, none fires in the gate-SET populations), #3694 (the parser program; notes).  Records: CLAUDE.md numbers paragraph replaced, STATUS, DECIDED (+ the USER's no-more-subtasks word), session-log.
* RUNNING: s542r (the round), s541a.  Two slots stay EMPTY by the USER's word.

## 04:2x .. 04:4x -- **THE ROUND s542r MERGED** (baselines + docs only; main's code stays `c5f618f2`, MAIN-READY-98)
* MERGE-READY `b99763e3` at 04:1x (5 h 15 min after its launch), on main's head: ff 6 commits.
* THE BOARD on `cc36604b` (MAIN-READY-95): **661 PASS / 144 PARTIAL / 24 FAIL / 67 SKIP of 896, 109,714 ok / 1,225 not ok** (s539 round: 641 / 161 / 27 / 67, 109,797 / 1,740).  `--diff` 9 NEW / 21 FIXED / 4 UNVERIFIED / 1,005 LOST passing rows; I read the six LOST lines: 970 = IO-Compress `105oneshot-zip.t` (the #4643 heap flap), 27 = three load timeouts (pass alone; allowances registered with measured times), 5 = the zip-bomb file (#5083), 3 = `060-caller.t` rows now failing as perl's TODO rows do.
* THE COMPANION: `--all --quick` 132 rows FIXED out (each attributed to its merge), 78 NEW in (69 keyed to existing causes, 9 filed); the full run with stamps 0 NEW / 0 FIXED / 0 LOST against the edited baselines.  The snapshot is current as of MAIN-READY-95: a batch's `--all --quick` is readable against it again (the three merges after -95 are the next round's).
* REVIEW: added baseline rows read by cause; 9 baseline-reading gate / tools files 259 rows PASS on merged main; CI green on `9e0702b6` (MAIN-READY-98).
* CLOSED with their rows: #3493 (DONE note), #1840 (no DONE note: re-probed by me, main = perl on six compound assignments to a tied scalar; note written).
* **THREE FINDINGS FOR THE NEXT BRIEF** (none started: the USER's word):
  1. **#5082 -- a REGRESSION of s538a (MAIN-READY-91)**: `opendir $d, DIR` on a variable that held a dirhandle before (opened, read, closed) dies "Not a GLOB reference".  An ordinary idiom; `run/switches.t` lost 7 rows to it.  Bisected by the round to s538a's place check (#3618).
  2. **#5083 -- less heap headroom since s539a (MAIN-READY-94)**: HTTP-Message's zip-bomb file went 5 of 9 -> 0 rows at the 1 GB default and 9 -> 8 at 1.8 GB.  Same direction as the gate flake #4904 (one `Heap exhausted` row in a cold full gate).  The discriminating measurement is in the task (dynamic usage after a full GC at the first test row, MR-93 vs MR-94).
  3. #5081 -- `select NAME` / `open(NAME, ...)` through a symbolic name creates no glob that `defined *{NAME}` sees.
* Runner gaps it reported (not patched): `cpan-board.sh` cannot pass `--refresh-oracle`; `heavy.sh`'s exclusive-waiting marker misses an exclusive leg that has not got slot 1 yet; a batch's back-to-back benches starve waiting bounded legs (s541a: ~30 minutes).
* RUNNING: s541a only.
