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
