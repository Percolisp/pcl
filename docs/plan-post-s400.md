# Plan after s400 — the coming sessions (Fable, s401, 2026-08-15)

Written at the user's request ("check status and write a plan for the coming
sessions") after the s399+s400 review (`docs/fable-answers-s400.md`, which
holds every ruling this plan rests on).  Supersedes the queue paragraphs of
`docs/fable-answers-s396.md` §7 and `docs/opus5-review-requests-s400.md` §5.
Session numbering below is relative (A, B, …) — sessions are Opus's unless
marked Fable, and one letter is roughly one session.

## 0. Status, independently verified this session

| measurement | value | note |
|---|---|---|
| Gate `tools/prove-core` | **143 files / 5278 rows**, PASS except the 13 pclxs xs rows | user: ignore xs rows while pclxs is on ABI 8 |
| Full perl-tests sweep | **GATE clean, 0 new / 0 fixed, TOTAL 18516 = baseline**, 2 UNSTABLE (crash-file noise), 8 unverified | re-run this session, `--jobs 8` |
| Companion suite snapshot (`docs/perl-suite-run.tsv`, s400) | 523 rows: 86 OK, 106 XDIFF, 280 DIFF, 30 NOTAP, 12 TRANSPILE, 5 TIMEOUT, 2 NOT-RUN | #324's verification finished s400; 9 TIMEOUT-shaped files not comparable across runs |
| Cache generation | v2-148, all three artifacts stamped v2-148 | `artifact-staleness-01.t` green |
| CPAN board (s344 last measured) | 65 PASS / 65 PARTIAL / 53 FAIL of 183 | re-bless waits for #208 |
| XS conformance | 398/398 (s339) | not in the gate |
| Working tree | clean at `ad7fba8` before this session's docs commit | — |

**Two NEW silent-wrongs found by this session's probes** (details
`fable-answers-s400.md` §9): **#349** the checked-in artifacts' program
preamble RESETS `@INC` at the first `pack`/`unpack`/`mro`/`warnings::enabled`
call (a runtime `push @INC` is lost — silent until a later `require`); and
**#350** a file-top `require Bareword;` is hoisted above a preceding runtime
`push @INC` (loud, one companion file).  #349 re-scopes #217 and unblocks
#277.

## 1. The two standing goals, unchanged

- **v0.1 public release** (`docs/release-plan-v0.1.md`, USER s375c): phases 1
  install → 2 IR pass (#281, Fable) → 3 neatness → 4 the bug hunt → 5
  fresh-machine gate + CI.  The tag precondition is #282 green + phase 4,
  not a date.
- **Correctness first, then speed** (R2).  Everything below is ordered so
  the release ships with the known SILENT families closed or loud, and the
  measurement portfolio cheap enough to run every session (§8 of the answers:
  the WHAT-CHANGED table replaces the count rule).

## 2. Opus queue, in order

**Session A — diagnostics and tools (no compiler risk; makes every later
measurement cheaper).**
1. **#339 (b)**: the drop announcement moves from PExpr's decline site to
   Parser.pm's two `PARSE ERROR` emitters (`PCL: statement dropped at F line
   L: <text> — <reason>`, stderr, exit 0), the PExpr `warn` deleted, the two
   `$SIG{__WARN__}` workarounds deleted with it if it was all they silenced;
   the `ref=''` probe FIRST.  Bar: corpus-diff identical (comment text
   unchanged), gate-SET scan over both populations, gate.
2. **#343 pieces**: the `add_node` internal-error drop (one probe, one
   file); the **DROPS runner column** — the sweep records per-file drop
   counts in `.faillog/_status.tsv`, `sweep-diff.pl` gains a DROPS bucket vs
   `docs/parse-error-drop-census-s399.tsv` (the census is the baseline; a
   drop leaves by EDIT), `run-perl-suite.pl` records the same column.  Add
   the one sentence to `not-supported.md` §lvalue subs (6.3).
3. **`ir-spec.md`**: the gen-stamp promise (7.3) — one paragraph; the two
   guards then cite it.

**Session B — the portfolio (#345) + the two new silent-wrongs.**
4. **#345**: `--quick` (skip the #326 hang set, cap registered allowances at
   120 s, LIST every skipped/capped file as NOT-RUN) + register
   re/pat_psycho.t and re/speed.t in `perl-suite-timeouts.tsv` (7.4); one
   `--all` vs `--all --quick` comparison as the bar; the cadence table is
   already in CLAUDE.md (this session) — implement, do not re-derive.
5. **#349** (closes **#217**): `pl2cl --extension` omits the program
   preamble; regenerate the three artifacts (zero machine paths →
   `no-hardcoded-paths-01.t` tightens to zero exclusions); rule-12 guard in
   `p-load-extension` (`@INC` unchanged across the load, or die naming the
   extension); population measured first with a temporary before/after
   compare over one sweep.  Emission for user files untouched → corpus-diff
   identical, no gen bump.
6. **#350**: measurement — emit the file-top `require` at its own position,
   corpus-diff over both populations, explain every diff; flip if clean.

**Session C — the harness hole and the rule-12 violation.**
7. **#346**: run/cloexec.t hang under a PCL child — `/proc/<pid>/fd` at the
   stall (the task's discriminating measurement); fd 3 leaking into every
   `pclperl-for-tests` child is a bug of its own — close it here.  Also the
   blank-line stderr noise the task notes.
8. **#342 piece 1**: an `s///e` replacement that fails to compile DIES
   instead of emitting `(lambda () nil)`; population from the gate-SET scan
   grepping "Failed to compile s///e expression" — that number is the
   finding.  Piece 2 (the heredoc inside `${\ }` inside `s///e`) is sized
   AFTER the file transpiles, not promised.

**Session D — op/try.t (#340).**  perl 5.34 `try`/`catch`/`finally`: one
statement recognizer + one lowering (`unwind-protect` for finally, the
existing error machinery for catch, a `let` for the catch var, `$@`
UNTOUCHED, `return`/`next`/`last` pass straight through because the body
lowers in place).  Pl/t battery from the six probed semantics FIRST; then the
28 rows.  Corpus-diff (shape occurs in no perl-tests file); gate; splice
op/try.t's snapshot row.  A real feature CPAN code now uses — in v0.1.

**Session E — v0.1 phase 1: #277 the installer.**  USER-ruled shape
(runtime + saved core COMPILED AT INSTALL, deps check, smoke gate); item 2
(artifact regeneration) is GONE once #349 lands — the artifacts are
machine-independent; the installer only warns if the checkout's compiler
is newer than the artifact stamps (the staleness test says so).  Absorb the
quoting unification (7.1) here.  #280 README/CHANGELOG draft in the same
session if time allows (it is text; the numbers come from §0 and CLAUDE.md).

**Session F — `my sub` (#337 → #341).**  #337 is a name-resolution change:
the full sweep IS the gate; probe the ~10 shapes on the task vs perl first
(nested scopes, recursion — perl DIES on `my sub rec { rec() }`, `state sub`
/ `our sub` siblings, a same-named package sub in scope, `\&x` identity).
Then #341 step 2 (the capture refusal that actually blocks op/lexsub.t) and
MEASURE what the file scores before sizing anything further; op/const-optree.t
re-registers when its four rows pass.

**Session G — the closure gap (#347) → #348 lands.**  A NAMED sub inside an
ANON sub closing over the enclosing foreach/filescope variable (the s63 TODO,
24 rows measured).  `Pl/t/closure-01.t` extended first.  Then the four-line
`$PCLPERL` switch in `pl-which_perl` lands, the 19 companion callers + the
sweep re-run, moved rows spliced with causes, baselines edited ROW BY ROW.

**Then the rest of the v0.1 mechanical track**: #279 (pure mechanics now —
the process-docs question is RULED, §4), #280 if not done in E, then **#282** (fresh-machine
container install from README alone — AFTER #281 lands, so the emission it
gates is the one that ships) and **#283** CI (Pl/t gate + corpus-diff on
PRs).

**Fillers, any gap, in this order:** #338 (b1 via direction D — delete the
uniqueness rule if D makes it redundant, gate-SET measured), #330 (read-only
scalars, 6 rows), #321 (coderef `@INC` hooks — note #350 touches the same
`require` path), #322 (attribute protocol), #328, #326 (the six re/regexp*.t
hangs — an R2 regex-engine item unless a cheap `(?{})`-free reproduction
shows up), #333–#336 (the s398 probe fillers, all silent-wrongs: #333/#335/
#336 are `$end_pars`-adjacent — check whether Option B phase 2 absorbs them
before touching them in place).

## 2b. Progress against §2 (s405)

Sessions **A** (s402), **B** (s404), **C** (s404–s405), **D** and **E** are
done, and **G's first half** landed early because the measurement that closed C
put it in reach:

* **C** — #346 turned out to be **#358**, a runtime `open` bug: `"<&=N"` on a
  CLOSED descriptor built a stream whose first read spun on EBADF forever.
  run/cloexec.t TIMEOUT → DIFF 16/6.  #342 piece 1 shipped in s404.
* **D** — **#340** try/catch/finally, with the PPI repair its `finally` needs
  (op/try.t TRANSPILE-FAIL → 23/28; the five left are four other registered
  families).
* **E** — **#277** `tools/install-pcl`: deps checked before anything is copied,
  the tree installed in its repo-relative shape, the core COMPILED AT INSTALL,
  wrappers not symlinks, replace-not-merge, and a smoke test the install must
  pass.  Phase 1 now needs only #278 and #128.
* **G, first half** — **#347** was one missing `next`: the hoist gate did not
  know the W5 exemption its sibling scan has always had (a PROMOTED lexical is
  legitimately captured).  op/closure.t under PCL children 235/27 → **267/3**.
  **So #348 has no blocker left** — it is the next session's first item, and it
  wants its own measurement pass (19 companion callers + the sweep).
* **F** (#337 → #341) is untouched, but `t/op/lexsub.t` now transpiles as a
  side effect of #347 and scores 6/8 with 8 drops — a starting measurement it
  did not have before.

## 2c. Instruction for the NEXT Opus session (Fable, s406, 2026-08-16): the s405 review is DEFERRED — do not wait for it

Fable is short of time this round (USER instruction, 2026-08-16), so
`docs/opus5-review-requests-s405.md` is **not reviewed this time**; it will be
reviewed together with the next request (`opus5-review-requests-s406.md`, or
whatever the next session writes) as one batch.  Consequences for the next
session:

* **Do NOT block on the answers.**  Take the queue as §2b leaves it: **#348**
  first (its own measurement pass — the 19 companion callers + the sweep, rows
  spliced with causes, exactly as §2b says), then the fillers in the order §2
  gives (#355, #342 piece 2, then **F** = #337 → #341 with the sweep as its
  gate), then #278 / #128.
* **Interim standing for the six s405 asks, until ruled** — proceed on the
  choice already shipped, none of them is a blocker:
  1. `$@`/`finally` model: as ir-spec §6.3 states it (normative until said
     otherwise).
  2. op/try.t stays DIFF; do not register XDIFF.
  3. #360: **do not start** — leave it filed with its two causes; the layer
     question is answered next review (default if nothing is said: (a),
     `not-supported.md` entry only).
  4. #359: **behind the release** unless it turns out to be a one-line
     `:preserve-fds` widening while #348 is being measured — then it may ride
     along as a filler with its own row diff.
  5. / 6. installer prefix (`$HOME/.local`, tree in `$PREFIX/lib/pcl`) and
     `<root>/pcl.core` by pattern: **stand as shipped**; #282's README may
     describe them.
* **Write the next review request as usual** (`opus5-review-requests-s406.md`),
  and put at its top a one-line pointer saying the s405 request is still
  unreviewed, so the batch review reads both in order.  Anything in s406 that
  DEPENDS on an s405 ask being ruled a particular way must say so in that
  request, so a reversal can be costed.

## 3. Fable queue

1. **This session (s401)**: the review + this plan; DECIDED, CLAUDE.md (the
   WHAT-CHANGED table, the pointer), tasks #349/#350 filed and #217/#221/
   #338/#339/#343/#345/#348 updated with their rulings.
2. **#281 — the IR pass (release phase 2), DESIGN first.**  Re-measure the
   `generated-cl-ir-review.md` friction list against v2-148 emission (raw
   seams, context-bind noise, host-idiom constructors, #218 nested p-if
   chains, #219 void-position comma arms), then the macro vocabulary where it
   clarifies at zero speed cost (a macro that expands to today's form is
   free; one that changes the expansion needs the bench), `ir-spec.md`
   updated normatively.  Fable designs the vocabulary and writes the arms as
   a worklist; Opus executes the mechanical arms in parallel with sessions
   E–G.  Emission-changing: corpus-diff explained per file, gate, sweep
   TOTAL/LOST, gen bump, artifacts regenerated.  Likely 2+ sessions.
3. **Option B phase 2 — SIZE, then execute.**  The `$end_pars` collapse
   (`docs/pexpr-term-parsing-review.md` §Phase 2; task #153
   `option_b_status_s398`): first decide whether the fold's non-postfix
   terms become nodes (A/B over four populations, byte-identical bar), then
   the operand-boundary regions collapse to "take the next term node".
   Acceptance set: the #343 reproducer + #259 + #335 (+ #147, #271 already
   there); metric: `tools/drop-census.pl` falls, every remainder explained;
   LAST step: the `PARSE ERROR` emitter flips from announce to DIE (6.4).
   This is E5 work and belongs BEFORE release phase 4 (the bug hunt), so
   the hunt runs on the final term grammar.
4. **Boxed aggregates** (E5, Fable design) — after v0.1, unchanged.
5. **#221** (the minimal warnings model) — first item of the POST-release
   correctness backlog (answers §4).

## 4. Decisions that are the USER's — open, please rule when convenient

From `docs/release-plan-v0.1.md` (all still open):
1. **Public name** — "PCL" collides with Portable Common Loops and *Practical
   Common Lisp*; `percolisp` exists as an org.  Cheap before release.
2. ~~Publish the process docs?~~ **RULED by the USER (s401): it is open
   source — they stay AS-IS under `docs/`, nothing archived or pruned.**
   **#279** is unblocked and is now pure mechanics (root junk, the 29 loose
   planning `.md`s, `.gitignore`).
3. ~~LICENSE body~~ **RULED by the USER (s401): same as Perl; every PCL code
   file tagged (`tools/tag-license`; gate row `Pl/t/license-tag-01.t`);
   nothing from the Perl distribution or CPAN is tagged.**
4. **pclxs bundling** — recommendation: release PCL first, mention pclxs as
   the experimental XS sibling; its GitHub push stays your deferred call
   (#92).
5. **Hosting / remote** — the repo has no remote.

Ruled by Fable, override if you disagree: #348 lands only after #346 and
#347 (answers §7.2); the WHAT-CHANGED cadence table replaces "every 3rd–5th
change" (answers §8; CLAUDE.md); #221 is post-release (answers §4); the
file-level lvalue-sub refusal keeps dropping, loudly (answers §6.3).

## 5. Guardrails for every session in this plan

- The WHAT-CHANGED table in CLAUDE.md decides what runs; do not run the
  sweep "to be safe" when the table says it cannot move, and do not skip it
  when the row says the sweep IS the gate.
- A census uses `grep -a` or perl; a guard reads bytes (three falsified
  measurements in two sessions).
- Name-resolution / scoping / rename changes: probe the breaking case,
  full sweep as the gate, gate-SET scan when a checker widens.
- Every silent-wrong found by a probe is FILED with its reproducer before
  the session ends, even when it is not fixed (#333–#336, #349, #350 all came
  from review probes, not from tests).
- Baselines are edited ROW BY ROW with causes; `save-status` re-blesses only
  gate-green after a per-file audit.
- Do not touch the `$end_pars` region in place (Option B phase 2 owns it);
  #333/#335/#336/#343/#259 wait for it or are absorbed by it.
- Review request per Opus session, as before: commits, measurements, the
  asks that need a ruling — `docs/opus5-review-requests-sNNN.md`.
