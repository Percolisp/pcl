---
name: feedback_sweep_cadence
description: "What to run when is keyed on WHAT CHANGED (the CLAUDE.md WHAT-TO-RUN-WHEN table, ruled s401) — the old 'every 3rd-5th change' count rule is RETIRED; the negative matters: corpus-diff identical + lib byte-identical + not a name-resolution change ⇒ do NOT run the sweep"
metadata: 
  node_type: memory
  type: feedback
  originSessionId: f6f17a82-ccd1-40ac-8a50-95e3ccc249e7
  modified: 2026-08-15T18:08:35.415Z
---

**USER (s323/s324, 2026-08-02): never run the full sweep or the full
companion suite after every individual change.**  **RE-RULED s401
(2026-08-15, user's portfolio ask #345): the COUNT rule ("every 3rd–5th
change") is RETIRED** — it cannot say WHY a run happens: it under-fires where it hurts (s386/#296:
a Pl/t-green RENAME change with two live sweep regressions) and invites
"run it to be safe" elsewhere (s399 mis-read its own criterion in both
directions — fable-answers-s400.md §7.5).  It is replaced by a decision table keyed on WHAT CHANGED —
**CLAUDE.md, Quick Reference → "WHAT TO RUN WHEN"**; rationale in
`docs/fable-answers-s400.md` §8.  Do not re-derive it; read the table.

**The shape of the table (the CLAUDE.md copy is authoritative):**
- Always: `tools/prove-core` (~4.5 min); if `Pl/` changed, `tools/corpus-diff.pl`
  (READ its SILENT-DROP line) + `tools/emission-ab.pl --list lib/**/*.pm`.
- `Pl/` change, corpus-diff IDENTICAL, lib byte-identical, NOT name
  resolution ⇒ **the sweep CANNOT move — do not run it "to be safe"**.
- Name-resolution / scoping / rename / capture change ⇒ **the sweep IS the
  gate** (#296), `--quick` companion once, gate-SET scan when a checker
  widens.
- `cl/` or `lib/` change ⇒ sweep YES (invisible to corpus-diff), companion
  dirs the change touches.
- harness (`perl-tests/t/test.pl`, `cl/pcl-test.lisp`, skip-registry) ⇒
  sweep + `--all --quick`.
- runner change ⇒ run that runner once, compare verdicts file-by-file,
  `PCL_SHOW_SBCL=1` before/after.
- docs only ⇒ nothing beyond the gate.
- Companion: `--quick` is the default form (#345); the full `--all` at most
  once per session and only when a row says so.

**Why:** each measurement is blind to a known set of change kinds; the table
names the blindness instead of guessing from a count.

Related: [[project_test_core_fast_path]], [[feedback_no_redundant_bg_wait]],
[[feedback_check_total_not_just_diff]].
