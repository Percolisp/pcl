---
name: project_fully_passing_regression_hunt
description: "Open investigation into the sweep fully-passing drop (69 → 63ish); defins.t root-caused, older 69→66 drop still unidentified"
metadata: 
  node_type: memory
  type: project
  originSessionId: f520542b-a0e1-4e4e-acd7-2f27e8c6fa3a
---

# Fully-passing sweep regression hunt (started 2026-06-27, PARKED)

User flagged that the perl-tests sweep fully-passing count fell from a **peak of
69** (held many sessions ~s238–250) to **66–67** (s257–260) to **61–63** in raw
runs now. Investigation so far:

## Established
- **This session (2026-06-27) is CLEAN.** Full-sweep diff at session-start commit
  `4fc586b` vs HEAD: 0 files regressed, **+2 gained** (`concat.t`, `die_exit.t` —
  the compound-assign double-eval fix completed `concat.t`). Method: ran full
  sweep in a worktree at the old commit, `comm`-diffed the fully-passing lists
  (LC_ALL=C sort first — comm needs identical sort order).
- **Raw 63 vs recorded-66 gap = crash-under-load FLAKINESS** (the careful
  `sweep-diff` per-file process recovers those) PLUS the one real defins.t regression.
- **`qr.t` is a RED HERRING** — never fully-passing. The session-log "+1 fixed
  (qr.t)" entries were baseline-diff deltas, not fully-passing. Bisect: qr.t has
  had ~14 fails for weeks (its SV-identity tests 6/24–28/36–37 are documented
  not-supported, NOT skip-registered — see skip-registry.lisp:316 comment).

## REAL regression found + root-caused — FIXED 2026-06-28 (`4e26245`)
- **`defins.t` test 16** ("saw file in glob hash while() ternary"): 26/27 → **27/27
  FIXED**. Construct: `while (($seen ? $dummy : $name) = glob('*')) {...}`.
- **Fix:** `p-glob` (`cl/pcl-runtime.lisp`) now mirrors `p-readline` — when
  `*p-in-list-assign-rhs*` is t (bound by `p-list-=` around its RHS), glob uses
  SCALAR (iterator) mode regardless of `*wantarray*`, so the while-condition
  iterates one file/loop. `@a = glob` unaffected (compiles to `p-array-=`, no flag).
  Guard: 2 cases in `Pl/t/glob-01.t`. Reuse-not-duplicate (CLAUDE.md 11).
- **Culprit commit: `490df0c` (2026-06-25, "consolidate wantarray-leak wrapping")**
  — MY t/io work last week. It wraps `glob`/`readline` on a `p-list-=` RHS in
  LIST context `(let ((*wantarray* t)) (p-readline 'FILE))`. In a
  `while ((lvalue-ternary) = glob)` the glob must be SCALAR (iterator,
  one-file-per-call); list context returns all files at once → loop semantics
  break. This is a side effect of the INTERIM wantarray-leak fix (see
  [[reference... wantarray-leak]] / `docs/wantarray-leak-review.md`; permanent
  split deferred). Fix must give the list-= RHS scalar context when the LHS is a
  single scalar lvalue / in a while-condition, WITHOUT reintroducing the leak.

## METHODOLOGY GOTCHA (important)
- **`./runt defins` bisect is UNRELIABLE** for this test: it does real
  `glob('*')` on CWD, and worktrees have different file contents → flaky
  pass/fail across commits. **Use deterministic CODEGEN comparison instead**:
  `./pl2cl perl-tests/defins.t | grep 'p-if $seen $dummy $name'` and check for
  the `(let ((*wantarray* t)) (p-readline` wrap. That cleanly pinned 490df0c.

## STILL OPEN (next session)
1. **Fix defins.t** (490df0c wantarray over-wrap on glob/readline in
   while-list-assign scalar context).
2. **Find the OLDER 69→66 drop** (~3 files, predates ~2026-06-17 = "not last
   week" per user). NOT yet identified. defins.t is last-week & separate, so not
   these. To find: run a FULL sweep at a ~2026-06-13 commit (69-era, e.g.
   `3579520` or `cb8234c`), capture its fully-passing list, `comm`-diff vs the
   current 63-list (saved approach above). Then bisect each named file by
   deterministic codegen, not `./runt`.

Guard rule reminder: [[feedback_fully_passing_regression]] — a drop after a sweep
means fix the regression before moving on.
