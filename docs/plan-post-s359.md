# Plan after s359 — next sessions (written at session end, 2026-08-08)

> **UPDATED s362 (Fable review).**  s361 APPROVED — gate, the one-disagreement
> inventory (reproduced from a `1279be6` worktree over perl's 604 t/ files),
> and fresh oracle probes all independently verified; session-log s362 has the
> evidence.  One same-family residual filed: **#262** — the statement-MODIFIER
> form `$_ = "w" for ($s);` still doesn't write back (pre-existing at
> `197dcd9`; d2bb91c wrapped both lowering sites but the VarAnnotator
> `foreach-alias-list` veto fires only for the block form).  Fix shape is in
> the task; it belongs WITH #153 steps 4–5 as a first-commit warm-up or a
> filler — it is small, probe-ready, and completes d2bb91c's family.
>
> **Execution order stands: Opus next, on §1 (#153 steps 4–5, taking #262
> along), then §2 (#254).  Fable's next session = review those flips, then
> the E5.1/E5.2 + boxed-aggregates designs (§3).**

> **UPDATED s361 (Opus).**  §1's **step 3 is DONE** — both operand sites are
> on the walker (`3509115` named unaries, `ece9d35` strictly-1-arg, plus the
> prototype-arity precondition `1279be6` and the foreach silent-wrong
> `d2bb91c`).  State at s361 close: gate **132 / 4719 PASS**, cold-cache
> sweep **GATE clean** (0 new / 0 fixed / 0 LOST, TOTAL 18498 = baseline
> after two cause-documented baseline EDITs), gen **v2-115**.
>
> **NEXT, in order:**
> 1. **#153 steps 4–5** — §1's remaining bullets: widen `_term_extent` to the
>    shapes it still DECLINES (cast-block slice groups `@{$r}[0]`,
>    `->method(args)`, prefix-op runs `~`/`!`, bare words), each widening
>    getting rows in `Pl/t/reduce-term-01.t` FIRST; then fold the postfix-`->`
>    reduction + subscript/slice builder into `_reduce_term` and delete
>    `$deref_skip`; then delete the dead `$end_pars` branch chains at both
>    sites.  Acceptance probes at the end: #147's `[] // 0` and the s317
>    general-bareword probe (both in task #153).
> 2. **#254** (§2) — still unstarted, still approved.
> 3. Fillers (§4) now also include **#258–#261**, the four probe-confirmed
>    pre-existing bugs s361 found: `\@a[0,1]` ref-distribution; the `(;$;)`
>    prototype call that VANISHES behind a PARSE-ERROR comment (also a rule-12
>    question); the `_` prototype's missing `$_` default; and `$_[N] =~ /\G/`
>    matching a copy instead of the arg box (#261 — the row pos.t t21 now
>    fails honestly, having previously passed under a wrong parse).
>
> **Standing rule added s361 (§5 amendment): a "measured then flipped" step
> measures over BOTH the 111-file corpus AND perl's own `t/*/*.t` (604
> files).**  The corpus alone reported ZERO disagreements while perl's t/
> produced three real shapes, two of them live silent-wrongs.  Helper:
> `tools/term-diff-sweep.pl`.

State at close: everything green at HEAD — gate `tools/prove-core` 132 files
/ 4717 PASS; cold-cache sweep GATE clean (0 new / 0 fixed / 0 LOST, TOTAL
18499 = baseline); corpus emission identical 111/111; gen v2-114 (s359 was
measured byte-identical, no bump); board at the s358 level (Text::Balanced
restored, 933 ok rows).  **E4.1 COMPLETE.  #153/E5.0 steps 1–2 SHIPPED.**

## 1. Main line — Opus, next session(s): #153 steps 3–5

Migrate the remaining operand sites onto `_term_extent` and burn the
`$end_pars` maze down.  Full recipe in task #153; the short form:

- **Per site, measured then flipped** (the s359 `defined` pattern): run
  corpus + gate with `PCL_TERM_DIFF=1`, explain every disagreement, then
  flip that site (walker answers → use it; decline → legacy).  One site per
  commit; corpus-diff + gate per commit; full sweep every 3rd–5th change.
  Sites: the named-unary branch chain first (`ref`, `exists`, `delete`,
  `keys`, `values`, `length`, `uc`/`lc`, …), then the strictly-1-arg block.
- **Widen the walker to the declined shapes as sites need them** —
  `->method(args)` (consume the args List and continue), cast-block slice
  groups (`@{$r}[0]`: accept Constructor/Block as slice postfix after a
  cast deref), prefix-op runs.  Every widening gets rows in
  `Pl/t/reduce-term-01.t` FIRST (pure perl, instant).
- **Step 4**: fold the postfix-`->` reduction + subscript/slice builder
  into `_reduce_term` so the main loop stops reducing opportunistically;
  delete `$deref_skip`.  **Step 5**: delete the dead `$end_pars` branch
  chains; final gate + full sweep; any moved row leaves the baseline by
  EDIT with cause.
- **Acceptance probes at the end**: #147 (`[] // 0` under a `$`-prototype)
  and the s317 general-bareword probe (`print "x=", Foo::init;` must
  CALL) — both recorded in task #153.
- Read before touching: `docs/pexpr-term-parsing-review.md` (the maze, the
  Option B design) and task #142 (the three failed s316v attempts — why
  guards in that region don't work; the walker replaces them, never joins
  them).

## 2. In parallel or next after — #254 (**APPROVED s360, 2026-08-08**)

Phase 2 of the post-flip recovery: the capture/package-spanning family
(9 suite files) + poisoned condition-`my` family (4 files), ~12k rows.
Plan: `docs/e41-suite-families-plan.md`; the Opus-ready recipe (session 1
= measurement, per-shape predicate-widen vs mechanism-gap, stop-rule,
verification) is in task #254.  It touches Parser2 (capture/spanning),
not PExpr, so it interleaves cleanly with #153 steps 3–5.  §4's residue
registration is NOT yet signed off — comes back with real shapes.

## 3. Fable, next Fable session

1. Review the Opus per-site flips (independent reproduction: re-run one
   site's PCL_TERM_DIFF inventory cold, spot-check the widened walker rows).
2. Then the next E5 design items: E5.1 seam object / E5.2 embedded-block
   totality (`docs/v2-endgame-plan.md` §E5), and the boxed-aggregates
   design (standing ruling: Fable designs it; nobody starts it early).

## 4. Fillers (half-session sized, anytime, can wait)

- #257 — fail-baseline cause column + DRIFTED bucket in sweep-diff (the
  s359 audit's two structural changes; user: this can wait).
- Two new fuzzer axes from `docs/bug-review-s359.md` §3c (interp subscript
  chains; prototype visibility/ordering) + one fuzzer run.
- Near-green list: #236 → #234 → #235.  Side items: mro/inconsistent_c3_utf8.t
  STALE row; comp/hints.t + mro/inconsistent_c3.t XDIFF → DIFF.
- ~~Bug-hunt sequencing recommendation awaits a user nod~~ **RULED s360**:
  the big hunt is "in the future when things are stable" — E5 exit gate,
  pre-R2, as `docs/bug-review-s359.md` §4 recommended.  No campaign now.

## 5. Standing verification checklist (unchanged)

- Per change: `tools/prove-core` + a targeted single-file run; corpus-diff
  for anything touching emission.  Full sweep every 3rd–5th change; COLD
  cache (`rm -rf ~/.pcl-cache/*`) whenever module-level emission could have
  changed.  Bump `*pcl-cache-generation*` on any emission-changing commit.
- Gate arithmetic: 132 files / 4719 rows with the pclxs sibling built (s361),
  4705 without; a worktree compare silently drops the 14 xs rows (set
  PCLXS_DIR).
- `grep -a` on any .tsv under docs/ or .faillog/ (NUL bytes).
- Baselines change by EDIT with cause, never by re-bless-from-run (except
  pass-baseline re-bless after a per-file audit, with `# taken-at:`).

## 6. Where the fresh context lives

s359 session-log entry (walker design, zero-disagreement measurement, the
`defined` flip, the baseline-drift audit); `docs/DECIDED.md` s359 section
(decline-not-guess, measured-then-flipped, the PPI document-GC gotcha);
tasks #153 (updated recipe), #254 (awaiting approval), #256 (open), #257
(parked); `docs/bug-review-s359.md`.
