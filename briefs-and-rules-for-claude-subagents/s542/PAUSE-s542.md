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
