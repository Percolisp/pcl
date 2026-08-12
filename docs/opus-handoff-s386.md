# Handoff: Fable s386 → next Opus session (2026-08-12)

The ENTRY POINT for the next Opus session.  Task bodies are authoritative
and self-sufficient; this note is the map and the guardrails, not a second
copy of the detail.

## State snapshot

- **main = `804d5be`**, tree clean.  Pl/t gate **138 files / 5103 tests
  PASS**, independently re-verified cold this session.  Cache generation
  v2-135.
- **Branch `wip/s385-296`** (parent `4d0a38f`): #296 built in full, gate
  green at 5105, two known regressions (B1/B2) with **ruled fix shapes** —
  see below.  Do not rebuild it; check it out and finish it.
- s385 (#297 C-for head `my`, #301 heredoc predicate) APPROVED as shipped.
  s386 rulings: `docs/fable-answers-s385.md`.  s386b duplication review:
  `docs/compiler-duplication-review-s386.md`.
- `runpcl`/`runt` no longer strip the program's blank lines — byte-compares
  against perl through them are now valid.

## The queue (in order; STOP after item 4 and hand back to Fable)

**1. Finish #296** (`git checkout wip/s385-296`).  Both blockers have
ruled fix shapes in the task body (do not re-derive, do not use progv):

- **B1** — eval-mode name RESOLUTION ORDER: an exception name (`$a`/`$b`/…)
  whose spelling is a capture-alist key compiles as the renamed captured
  lexical (the ordinary `__shadow__`-capture path — read, write, and the
  #295 pad chain come free); no key → today's special-table path,
  unchanged.  The likely seam is where `%runtime_vars` /
  `_forward_global_decls` short-circuits known specials before consulting
  the alist.  The eval-mode comparator-lambda emission follows the same
  in-scope conditional the branch already implemented for COMPILED sorts —
  reuse it, no second copy.  Acceptance = the five-row perl probe table +
  two reproducers in the task.
- **B2** — in `_rename_decl_within`, an earlier decl's rewrite region must
  STOP at a SIBLING redeclaration of the same canon name (B-ii only
  covered nested redecls).  Two-line reproducer in the task = the guard
  row.  Note the corrected row mapping: split.t rows 79/81 are lines
  294/302 (the sibling-redecl `ok()`s), NOT the #18195 block.
- **Merge bar (non-negotiable):** gate + corpus-diff (expect the four
  explained buckets, ~42/111) + **FULL SWEEP with TOTAL/LOST read** — the
  Pl/t gate was green with both regressions live, so for this change the
  sweep IS the gate.  The nine Pl/t expectation edits on the branch stand
  (ruled s386 §4).  On merge: emission changes → bump
  `*pcl-cache-generation*` + regenerate the three checked-in artifacts.

**2. #291** — delete the poisoned-my machinery per family (`__shadow__`,
`__cond__`, `__emb__`); #205 closes with it.  The s384 build is in reflog
`f5ff1ae` for reference; its four blockers are now all cleared (#294,
#297, #298, #299 answered, #296 merged by step 1).  Same verification
class as #296: full sweep mandatory, TOTAL/LOST, gate-SET file-by-file
over both populations where a checker widens.

**3. #292** — the extra net over the flipped emission: difftest-ops fuzz +
perl-suite companion sweep + the owed **pass-baseline re-bless** (the
standing +8; follow the #223 procedure — per-file audit, gate-green run
only, `# taken-at:` stamp).

**4. #303** — the dead-code deletion batch (~3.5k lines; task body has the
full list and the per-sub verification rule).  Deliberately AFTER #291 —
same files, the deletions must not collide.  Step 0 (the `PExpr::DEBUG`
constant, corpus-diff-identical) may be taken earlier as a filler.

**Then STOP.**  #153 chunk 0 (registry ownership move) + the FOLD are
Fable-led.  If a session ends mid-queue, update the MEMORY STATE line +
session log as usual and file the review request
(`docs/opus5-review-requests-sNNN.md` + a Fable-review task), listing
every commit and every measurement actually run.

## Guardrails active for this batch (grep DECIDED.md before probing)

- **A wide rename/scoping change can be Pl/t-GREEN and sweep-RED** — the
  full sweep is the gate for #296/#291; read the TOTAL line, not just the
  diff buckets.
- **TAP row numbers are only meaningful within the run that produced
  them** (s386 rule): join by description; for unnamed rows, re-derive
  number→source from the CURRENT tree's own TAP.
- corpus-diff BEFORE spending a sweep; normalize compiler line numbers in
  gate-SET stderr diffs; `grep -a` on emitted CL and any `.faillog`/
  `docs` tsv; never `nohup` the gate; a worktree compare drops the 14 xs
  rows (set `PCLXS_DIR=~/pclxs` or subtract).
- 50% compile-budget cap per change (~65 s corpus); sweep + suite every
  3rd–5th change EXCEPT where this note says mandatory.
- A `lib/` or `cl/` change makes the sweep non-optional (invisible to
  corpus-diff).
