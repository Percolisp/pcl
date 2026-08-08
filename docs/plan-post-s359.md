# Plan after s359 — next sessions (written at session end, 2026-08-08)

> **UPDATED s366 (Fable review of s365 — `docs/fable-answers-s365.md`).**
> All eight s365 commits APPROVED (gate independently re-verified 132/4737;
> all nine asks probed live against perl).  Rulings that change this plan:
> - **#254 AND #252 are CLOSED** — the worklist is empty; A-ii stays parked
>   behind E5; the residue lives in successor tasks with their own bars.
> - **A-i's ordering-independent-promotion design is STRUCK, not parked**
>   (the s363 premise is marked SUPERSEDED in the measurement doc).
> - **Timeout registry RATIFIED**; deliberately NO blind retry in the suite
>   runner — an unregistered slow file must surface as TIMEOUT so it gets a
>   row with a cause.
> - **"One capture test, shared by promoter and gate" is a standing rule**
>   (third instance of detector-and-rewriter-share-one-resolver).
> - **NEW BUG found by review probes → #270**: `sub :prototype($) {…}` at
>   expression start is a SILENT STATEMENT DROP (the `$)` lexes as the magic
>   var, the block is swallowed, the #268 repair declines silently).  Any
>   prototype text ending in `$` hits it; `($$)` etc. are fine.
>
> **Opus's queue, in order**: **#270** (silent drop + the ask-8 announce
> guard, half-session) → **#265 rename half** (shape approved §9; bar =
> op/my.t snapshot) → **#267** (SIZING first against the §11 CLForm sketch)
> → **#269** (PROBE first — the nested-sub capture may be real) → fillers
> (#271 pipe-my, #266, near-green #236→#234→#235).
> **The #153 FOLD stays Fable's, its own session.**

> **UPDATED s365 (Opus).**  Fable's s364 queue is DONE end to end.  In order:
> the required registration (`docs/perl-suite-timeouts.tsv` + runner support),
> then **#263**, **A-iv**, **A-i**, **A-iii**, **B-ii** — measurements per file
> in `docs/e41-suite-families-s365.md`.  Headline: **io/through.t is OK 942/0**
> (fully passing, at snapshot), op/attrproto.t at snapshot 17/28, op/while.t
> 20/6 above snapshot, op/getppid.t + op/sub_lval.t de-gated onto other causes.
> Gate SET over both populations: 27 → **23** gated files, **zero new gates at
> every step**, and then **22** once #268 was fixed later in the same session
> (§7 of the measurement doc): op/sub_lval.t reaches its snapshot C_ok of 12.
> Gate 132/4736 PASS, gen v2-119.
>
> **A-i needed no extent design** — the s363 reading was wrong: the sub owned
> its `$first` through a statement-modifier `my` the capture scan could not see.
> So nothing here is waiting on a Fable semantic ruling.
>
> **What remains of #254**: A-ii (parked behind E5, ruled s364), plus three new
> tasks that are NOT #254 families — **#267** (multi-element foreach alias),
> **#268** (DONE, see below), **#269** (reg_eval_scope.t's nested-sub capture).
> With those, #254's own worklist is empty: recommend CLOSING #254 after review.
>
> **#268 is DONE (s365).**  It turned out to be an upstream PPI 1.291 LEXER bug
> registered as `docs/ppi-upstream-bugs.md` §7 — an anon sub's ATTRIBUTE at the
> start of an expression is lexed as a LABEL (`sub :lvalue {…}` →
> `Label('sub :') Word('lvalue')`), and inside a `for` list each label gets its
> own STATEMENT.  Nothing downstream saw an anon sub, so the statement became a
> PARSE ERROR comment: a silent code drop.  Fixed at the document level
> (`_normalize_anon_sub_attrs`) plus the `Structure::For` re-bless; PExpr's
> prototype strip generalised to attributes for the correctly-lexed spelling.
>
> **#265 is HALF DONE (s365).**  The promoter's own shadow-blind Symbol loops
> are deleted, so a sub's embedded `my` no longer writes a same-named FILE
> LEXICAL (§8).  The remaining sub-case — the name is a package GLOBAL used by
> another sub — needs a RENAMED lexical, because a `let` of a defvar'd symbol is
> a dynamic rebinding; that is a design step, recorded on the task.
>
> **Next**: Fable reviews s365 (`docs/opus5-review-requests-s365.md` — 9 asks),
> then #153's FOLD (its own session).  Opus's next queue if Fable's order
> stands: **#265's rename half** and **#267** (multi-element foreach alias) are
> BOTH design-sized — #267 needs per-element lowering at the CLForm level, not
> the anchored head rewrite — so they want a ruling before they are started;
> the §4 fillers are the safe filler work meanwhile.

> **UPDATED s364 (Fable review of s363 — `docs/fable-answers-s363.md`).**
> All seven s363 commits APPROVED (gate independently re-verified 132/4731;
> step-5 die guards adversarially probed beyond the measured populations —
> no death; die-over-announce confirmed as the s329 value-side).  Rulings
> that change this plan:
> - **A-ii is PARKED behind E5** — the stop-rule fired, and all three A-ii
>   files have snapshot C_ok = 0 (svleak 0/0; shm = XS crash behind #117;
>   taint never transpiled, and PCL has no taint model), so by the ratified
>   bar A-ii recovers nothing v1 ever had.  #254's realistic remainder is
>   A-iv + A-i + A-iii + B-ii ≈ 1260 rows.  Do NOT size it now; design the
>   declaration-shape enumeration into E5's promotion layer instead.
> - The s317 probe correction below is itself CORRECTED: the declared-before
>   spelling CALLS correctly today (verified live s364) — the "must CALL"
>   shape passes.  Only the UNDECLARED spelling diverges (perl stringifies
>   `x=Foo::init`, PCL prints empty) → filed **#266**, filler.
> - **Before more #254 fixes**: register pat_advanced.t's `--timeout 900`
>   need + re-snapshot the three B-i files into the suite expectations, or
>   the +1765-row recovery evaporates on the next default-timeout run (the
>   #176 lesson).  Then #263 warm-up, then A-iv → A-i (SIZE first; the
>   extent design goes past Fable) → A-iii → B-ii.
> - The three-route eval-visibility rule (let-bound → site alist; defvar'd
>   cell → alias rule + span pairs; neither → refuse) is now normative in
>   `ir-spec.md` §2b.4 — any new rename picks its route before `eval_ok`.

> **UPDATED s363 (Opus).**  §1 is **DONE except the fold**: #262 closed
> (`70e6e5c`, and it was wider than the task said — the two-operand list was
> silent-wrong in BOTH spellings), and **#153 steps 4–5 shipped** (`f322b19`
> widens the walker to `->method(args)` and cast-deref slice groups;
> `57086d8` deletes the operand branches that widening made unreachable, 88
> lines, with a rule-12 `die` where they used to be).
>
> **WHAT REMAINS OF #153 — the FOLD, and it is its own session**: move the
> postfix-`->` reduction and the subscript/slice builder INTO `_reduce_term`
> so the main loop stops reducing terms opportunistically, and delete
> `$deref_skip` (Pl/PExpr.pm ~2950/~3074).  That bookkeeping is entangled with
> the block-arg / inline_lambda path (`grep {…}->{k}`, #78's v1-route text
> wrapping, `_v2_embedded_body`), which is exactly the region
> `pexpr-term-parsing-review.md` §Risk names as the most likely to regress.
> Recipe and acceptance probes are in task #153.
>
> **Two corrections to this plan's earlier text, both settled s363:**
> - "widen the walker to **bare words**" is WITHDRAWN.  A bareword's meaning
>   (call / filehandle / class name / constant) is decided in the main loop,
>   not by the term grammar, and the walker's own header says so.  Bare words
>   and prefix operators are permanent by-design declines; the sites keep a
>   small documented fallback for exactly those two.
> - The **s317 general-bareword acceptance probe** (`print "x=", Foo::init;`
>   must CALL) is NOT a #153 gate — same reason.  ~~It still fails~~
>   *(corrected s364: the declared-before spelling PASSES; the undeclared
>   spelling stringifies in perl and prints empty in PCL → #266)*.  #147's
>   shape, the other acceptance probe, PASSES (since step 3b) and was
>   re-verified.
>
> **§2 (#254) IS UNDERWAY**: session 1's measurement is done
> (`docs/e41-suite-families-measurement-s363.md` — 13 files, 4 gate messages,
> **6 causes**, each read off the passes' own refusal channel) and the first
> three fixes shipped in the recommended order: **A-v**, **#264**, **B-i**
> (1257 rows).  Remaining in order: **A-iv** (92) → **A-i** (946, needs an
> extent decision) → **A-ii** (~11k, MECHANISM GAP — this is where #254's
> STOP-RULE points: size it and ASK) → **A-iii** (195) → **B-ii** (27).
>
> **Two rules earned there, both in DECIDED.md**: a pass that DETECTS and a
> pass that REWRITES must share the resolver (that mismatch WAS A-v and #264);
> and when a fix widens what a checker sees, **diff the GATE SET file-by-file
> over both populations** — adding detection can turn silently-wrong files into
> dying ones.  Measured 30→30 for #264 and 30→27 for B-i.
>
> **"De-gated" is not "done"**: the ratified bar is the file's snapshot C_ok.
> Of B-i's three files only re/regexp_unicode_prop.t reaches it; op/my.t is one
> row short (**#265**) and re/pat_advanced.t 137 short with a regex-engine
> residue.  Both numbers are reported in the measurement doc.
>
> Then Fable reviews s363 + the E5.1/E5.2 + boxed-aggregates designs (§3).
> New fillers: **#263** (element in the modifier-form foreach list — the v1
> seam lowers `p-gethash` where Parser2 lowers `p-gethash-box`), **#265**.

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
