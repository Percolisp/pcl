# Handoff s413 → Opus 5 (Fable, 2026-08-18)

*Fable time is short this week; the design/review half of the s413 batch is
done and this file is the hand-over.  Read it top to bottom before running
anything.  Standing rules that changed this session are in §4.*

## 1. Where the project is (plain terms)

- **One-compiler plan (`docs/plan-one-compiler-s411.md`) — Phases R, A, B,
  C and #391 are DONE (s411–s412).**  ONE expression compiler, ONE seam
  function into what is left of v1 (12 statement classes + `local`, post-
  release), the embedded-block declines 17 → 3, v1's file-level `parse()`
  deleted, compile −21 %.  Baselines: gate 150 files / 5516 rows (only the
  13 pclxs xs rows fail — pclxs is a separate project's state), sweep TOTAL
  18513, drops 13 = census.
- **s413 = the first EXTRACT batch of the duplicate-code worklist (#387,
  `docs/dup-census-worklist-s411.md`)** — the USER's s409 ask ("comprehensive
  search for doubled code; small bits extracted to subs").  Landed on `main`
  (each corpus-diff IDENTICAL over 111 files, gate green): families 6, 5,
  34, 3, 4, 8, 17+20, 44 (`s413a`–`s413i`), plus fix #393.  Prepared,
  verified and committed on **branch `s413-lisp-dedup`** (§2): the six
  runtime families 23, 13, 21 (+ fix #394), 37, 42, 46–48.
- **Three pre-existing silent-wrongs found by the family reviews**, each "the
  sibling that disagreed was the bug": #393 (`"\b"` in dq context was `b`,
  perl = backspace — FIXED on main, `s413h`) and #394 (`@a["12"]` exploded
  into characters; `delete @a[1..2]` deleted element 0 — FIXED on the
  branch, `s413l`).  Guard rows in `Pl/t/transpile-test-10.t`.
- **After this batch the pre-s411 queue resumes** (§3).

## 2. What Opus must do FIRST — verify and merge the branch (task #395)

Branch `s413-lisp-dedup` = `main` (`19a4db0`) + six commits `s413j`…`s413o`,
one per family, each message stating what was verified by Fable (parens,
load-warning set identical to HEAD, macroexpansion / inlining inspected,
probes) and what is STILL TO RUN.  The bar is the runtime row of the
WHAT-TO-RUN table, taken once over the branch tip (every commit is a strict
subset of the tip; a mover bisects by commit — six worktrees, cheap):

```
git worktree add /tmp/wt-lisp s413-lisp-dedup     # or check it out
cd /tmp/wt-lisp
tools/prove-core                                   # gate: expect only the 13 xs rows;
                                                   #   transpile-test-10.t gains the #394 row (FAILS on main by design)
BENCH_K=3 perl tools/bench-exec.pl arrfill slices sliceasgn ovlsub symref arrhash collatz strcat
#   ... and the SAME command in a worktree of main: the pcl(s) column must not
#   be slower beyond noise (every family is macro-/inline-identical by
#   construction; this is the measurement the rule asks for, not a hope)
perl sweep-perl-tests.pl --jobs 8                  # full sweep WITH its own gate:
#   expect GATE clean, TOTAL >= 18513 (the #394 fix can only ADD rows),
#   LOST 0, drops 13 = census.  Read the TOTAL line, not just "0 new".
```

If all three hold: `git checkout main && git merge --ff-only s413-lisp-dedup`,
then the docs half of the merge (§5), then `git worktree remove /tmp/wt-lisp`
and `git branch -d s413-lisp-dedup`.  If a leg fails: bisect by commit, fix
in place ON THE BRANCH (rebase is fine — nothing else references it), re-run
the leg, and record the cause in task #395.  Do NOT merge with a red leg and
do NOT "simplify" a guard row.

Also on the branch: five bench rows added to `tools/bench-exec.pl` (arrfill,
slices, sliceasgn, ovlsub, symref — the paths this batch touched; snippets
validated perl == PCL).

## 3. The queue after the merge (unchanged from `docs/plan-one-compiler-s411.md` §6 item 4)

1. **The rest of the EXTRACT worklist is now COMPILER + RUNTIME ONLY (§4).**
   What remains in scope: families 2 + 10 (`ExprToCL::gen_funcall_form`'s
   `_elem_container_key`, per-call, free — Phase A is done, so it is
   unblocked), 14/26 (`Parser.pm` `_expr_compiler`, the six
   `Pl::PExpr->new … Pl::ExprToCL->new` constructions — after Phase A, so
   also unblocked), 38 (`InterpScan` `_scan_name_after`), and the tail
   (anything EXACT ≥ 8 lines in a surviving `Pl/**` or runtime file — re-run
   `tools/dup-census.pl` first; carry the verdict RULES, not s411's numbers).
   Bar per family: corpus-diff IDENTICAL + prove-core; the runtime adds the
   sweep + bench.  DELETED-BY / SUPERSEDED-BY verdicts stand (do not touch
   v1's `_process_*`, StringInterpolation's scanners — those go with E5.3 and
   the InterpScan consumers 2+3 port, #388).
2. **`docs/plan-post-s408.md` §2 resumes**: I = #281 items 1+2+6 (emission-
   changing: gen bump + `tools/rebuild-pack` + artifacts), then J–L = Option
   B phase 2 (#371 refusals → #372 the operand grammar — **Fable designs the
   grammar first; do not start #372 without that design** → #343 → #369/#370
   → the announce→DIE flip), then M–N = release phases (#279 → #280 → #282
   → #283).
3. **Fable's next own item**: the B1 operand grammar (Option B phase 2) and
   the review of whatever this queue produces — file asks in a per-session
   `docs/opus5-review-requests-sNNN.md` as before.

## 4. Rules that changed or were made explicit this session

- **USER (s413): "Only the compiler matters for duplicated code, the tools
  might be replaced" — and the runtime matters too.**  The dup-census
  population is `Pl/**` + `cl/pcl-runtime.lisp` (the product); `tools/**`
  and the two runner scripts are scaffolding that may be replaced — their
  families are OUT OF SCOPE (worklist §1 rule 7).  Family 6
  (`tools/lib/PCLProc.pm`) landed before the ruling and stays.  Test files
  are never an optimization target — only guard rows are ADDED to them.
- **The census as a bug finder**: three of the ten families reviewed today
  hid a real bug as "the one sibling that spells the rule differently".
  When extracting, probe the DIFFERENCE between the copies against perl
  before unifying — the difference is either a reason to keep two copies or
  a bug; it is never noise.  (Filed as they were found: #393, #394; each
  fixed in the same session because the fix was one line inside the family
  being extracted — rule 7.1 of the plan.)
- **One sweep can cover a batch of commits when every other commit in the
  batch is corpus-diff IDENTICAL** — a mover then bisects to the one
  non-identical commit.  Say so in each commit message (done for `s413b`).
- **Interrupted tool calls may have RUN.**  s413's `\b` fix was committed
  under the wrong message because an interrupted command had already
  executed to completion; check `git log --stat` before re-running a
  commit command after an interruption.
- **`is_hash_braces` etc. are called as FUNCTIONS from PExpr** (`s413c`): the
  Moo accessor + method dispatch was the only call layer on the hottest
  predicate; the other TokenUtils delegators keep the delegating shape
  because some call sibling predicates on `$self`.

## 5. Docs half of the merge (Opus, after §2 passes)

- `docs/dup-census-worklist-s411.md` §2: mark families 23, 13, 21, 37, 42,
  46–48 DONE with their `main` shas (rows 6, 5, 34, 3, 4, 8, 17, 20, 44 are
  already marked).
- `docs/DECIDED.md`: one line under s413 for the branch merge (sweep TOTAL,
  bench verdict); `docs/session-log.md` s413: append the merge paragraph.
- Tasks: #395 → done (with the three measurements), #394 → done, #387 →
  progress note (families done / left).
- Memory: `project_one_compiler_state.md` STATE line (Fable wrote the s413
  line; append "branch merged sNNN").

## 6. Files this session touched / created

`tools/lib/PCLProc.pm` (new) + `tools/t/pclproc.t` (new); `Pl/Parser2.pm`,
`Pl/PExpr.pm`, `Pl/PExpr/TokenUtils.pm`, `Pl/ExprToCL.pm`;
`Pl/t/transpile-test-10.t` (+#393 row on main, +#394 row on the branch);
`cl/pcl-runtime.lisp` + `tools/bench-exec.pl` (branch); this file; the
worklist, DECIDED, session-log, plan §6, memory.  Tasks filed: #393, #394,
#395.
