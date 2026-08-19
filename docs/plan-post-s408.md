# Plan after s408 — the coming sessions (Fable, s409, 2026-08-16)

Written at the user's request ("review what Opus 5 did and write a plan to
continue") after the s408 review (`docs/fable-answers-s408.md`, which holds
every ruling this plan rests on).  **Supersedes the queue of
`docs/plan-post-s400.md` §2d** — that file stays as the record of sessions
A–F; this one is the live queue from here.  Session letters are relative;
sessions are Opus's unless marked Fable, one letter ≈ one session.

**SUPERSEDED as the live queue by `docs/plan-one-compiler-s411.md` (s411,
Fable, 2026-08-18; USER: structural first, not at any cost).**  Phases R–C of
that plan run FIRST; this file's §2 items I (#281 items 1+2+6), J–L (Option B
phase 2) and M–N (release phases 3–5) resume after them, in this order, and
§3.0 (#379) is DONE — the plan is written.  Nothing else here changes.

## 0. Status, independently verified this session

| measurement | value | note |
|---|---|---|
| Gate `tools/prove-core`, COLD (cache cleared) at `74c1b93` | **149 files / 5442 rows**, PASS except the 13 pclxs xs rows (4+4+5) | the xs files produced 14 rows this run; they move on their own (§4 rule) |
| Full perl-tests sweep, `--jobs 8` | **GATE clean, 0 new / 0 fixed, TOTAL 18513 = baseline, drops 12 = census**; 6 UNSTABLE + 10 unverified, all inside the three PARTIAL files #363 moved (crash-file noise) | 4 min wall on this machine |
| Companion `--all --quick --jobs 4` | 523 files: 87 OK / 30 NOTAP / 110 XDIFF / 1 FIXTURE / 295 UNEXPLAINED; **3 measured movers**: op/signatures.t +8 (a WIN, re-blessed), **op/sub.t −26 (#368's die aborts the file → #378)**, one rows-unstable | run because the batch touched name resolution AND the runners; the run also exposed a #366×#345 runner bug (NOT-RUN rows re-run serially), fixed |
| Cache generation | **v2-154**, three artifacts stamped v2-154 | `artifact-staleness-01.t` green in the cold gate |
| Probes vs perl 5.40.3 | 27 shapes; **19 identical**, 4 registered (loud) as documented, **4 pre-existing divergences → tasks #376 / #377** | §2 of the answers |
| Working tree | clean at `74c1b93` before this session's docs commit | — |

**Three NEW tasks from the review** — two from probes (both verified
PRE-EXISTING at `b7ce704`, i.e. not s408 regressions — but both sit in the
family s408 opened) and one from the companion run: **#376** the lexical-sub rename's three uncovered spellings (the
`my sub c; sub c {…}` forward-declaration idiom is SILENT WRONG across two
scopes; a plain `sub NAME` inside the region defines the LEXICAL in perl; a
use from another package crashes) and **#377** `sub outer { my $x = shift; my
sub inner { $x } }` CRASHES with an unbound `$x__file__0` (the promotion pass
and the raw-params lowering disagree about who owns the declaration).
**#378** anon `__SUB__` must be IMPLEMENTED (a self-reference rewrite): #368's
die is right by rule 12 but it aborts `op/sub.t` at line 214 (51/14 → 25/6, 26
rows), and the shape is modern Perl's recursive closure.


## 2a. Progress (Opus, s410 — `03cc639`…`1484246`)

**Session H is COMPLETE.**  #378 (anon `__SUB__` implemented as the ruled
self-reference rewrite — op/sub.t 25/6 → 52/13, one row BETTER than the 51/14
before #368), #377 (both halves: the raw-params/promotion disagreement AND
`my ($x) = @_`, which the probe showed was worth joining — the promoter never
saw a one-element list declaration), #376 (all three spellings), #341 measured
and CLOSED.  Its bar needed one thing the plan did not know about: **an `our`
statement's TAIL was being lost in both pipelines** (`our $ok++, return if …`
is what op/sub.t:214's recursion terminates on), fixed in `03cc639` with the
remainder filed as #380.

**Session I is DONE** (s414 + s415): item 5 (#342 piece 2) in `1484246`, and
**item 6 (#281 items 1+2+6) shipped in `s414f` and merged in s415** after its
four-leg bar (full sweep GATE clean / TOTAL 18513 +0 / drops 13 = census;
pack.t 5636/89 = baseline against the regenerated artifact; bench A/B no
move; `docs/ir-spec.md` §4 / §5.4 / §10 / §12 normative).  **The queue is now
at sessions J–L, Option B phase 2 — starting with #371** (Track A refusals);
#372 still waits on Fable's B1 operand grammar.  The historical note below
stands as the record of how item 6 was scoped.

Item 6 — it was begun and reverted
unfinished, and `docs/opus5-review-requests-s410.md` §6.2 has the full
inventory (four macros not three, 31 string sites, 6 form sites, the two
multi-binding lets that must stay lets) plus **a correction to item 2's
premise** (sort.t's ten `(defvar $a …)` are ten DIFFERENT symbols, one per
`in-package` section; the true duplicate count is 11, of a different shape).
Asks 7.7/7.8 in that file are the two rulings that would change how it is done.

**#341's answer changes what J–L should expect**: op/lexsub.t's ~139
unmeasured rows are behind the INDIRECT-OBJECT call `h F` (task **#381**), not
behind anything lexsub-shaped — and a Track A refusal (#371) does not recover
them, because a refusal dies too.  Also filed: **#380** (`our $x = E while C`,
population zero) and **#382** (the document-level heredoc pre-pass).

## 1. The two standing goals, unchanged

- **v0.1 public release** (`docs/release-plan-v0.1.md`, USER s375c): phase 1
  install ✓ (#277, #278) → 2 IR pass (#281) → 3 neatness (#279, #280) → 4
  the bug hunt → 5 fresh-machine gate + CI (#282, #283).  Tag precondition =
  #282 green + phase 4, not a date.
- **Correctness first, then speed** (R2).  Ordered so the release ships with
  the known SILENT families closed or loud.  s408 closed two silent families
  (lexical subs, eval-mode drops) and exposed one (`qq {…}`), so the family
  count is moving the right way; the residue is in §2.

## 2. Opus queue, in order

**Session H — lexical subs and `__SUB__`, the residue (name resolution: the
sweep IS the gate).**
0. **#378** — implement anon `__SUB__` at the PPI entry that already rewrites
   a named sub's to `\&name`: `sub {…__SUB__…}` → `do { my $__SUB__N; $__SUB__N =
   sub {…$__SUB__N…}; $__SUB__N }`, innermost-enclosing wins.  Bar: op/sub.t
   back to ≥ 51/14, five probe shapes in the task, the #368 not-supported
   paragraph deleted in the same commit.  Small; do it first — it recovers 26
   companion rows and closes a refusal on a feature real code uses.
1. **#377** next — it is the shape a `my sub` user hits immediately (a
   private helper reading the enclosing sub's `shift`ed param) and it is a
   crash.  Fix shape (i) in the task: a promoted `__file__N` name is never a
   raw-params binding; the shift-lowering consults the promotion table and
   falls back to the cell form.  Guard the `my $x = 70` twin (unchanged) and
   `my ($x) = @_` (stays the refusal or joins (i) — probe first).
2. **#376** — three edits in `_rename_lexical_subs`: (1) a bodiless `my sub
   NAME;` opens a region; (2) a Word after `sub` heading a PACKAGE sub
   statement inside a covering region is renamed (it is the lexical's
   definition, as perl reads it); (3) a cross-package use rewrites to the
   QUALIFIED renamed name via the existing package-at-token resolver — or
   DIES naming the shape if that is not cheap.  Carry BOTH lists (fires /
   must-not-fire: a package `sub f` outside any region; `sub f;` inside one —
   probe perl first).
3. **#341 measured** after both: run `t/op/lexsub.t` alone, read its next
   blocker (`undef-fn:main::pl-F` at s408 — likely #376(c)'s shape), file
   what remains.  Its ~150 unmeasured rows are the payoff of #337/#374(a).
4. **#373** stays a filler (population ~0): a sixth protocol line — the
   sub-capture alist (name → renamed symbol for every lexical sub whose
   region covers the eval site), keyed into the eval cache like the features.
   Do it only if #341's read shows rows behind it.

Bar for H: corpus-diff explained per file (the three files that carry `my
sub`, plus whatever carries anon `__SUB__`), full sweep TOTAL/LOST, `--quick`
companion once (op/sub.t, op/current_sub.t, op/lexsub.t read per row), gate.

**Session I — the small rule-12 hole + the FREE IR items.**
5. **#342 piece 2** — heredoc body inside `${\ …}` inside an `s///e`
   replacement: a heredoc pre-pass on the replacement text (the nil
   replacement thunk is a rule-12 violation today).  Small; base/lex.t is the
   file.
6. **#281 items 1+2+6** — `p-list-ctx`/`p-scalar-ctx`/`p-caller-ctx`
   context-bind macros, per-file defvar dedupe, `p-sort-cmp` — same
   expansions, FREE (measured s407); `ir-spec.md` updated normatively.
   Emission-changing: corpus-diff explained per file, gate, sweep TOTAL/LOST,
   gen bump, the three artifacts.  This is release phase 2's mechanical half;
   the design half (§3.2 string-literal escape, `p-cond` bench) is Fable's.

**Sessions J–L — Option B phase 2, as sized (`docs/option-b-phase2-plan.md`).**
7. **Track A #371** — feature-ABSENCE drops become RULED REFUSALS at the two
   PARSE-ERROR emitters (given/when ~117, class ~25, smartmatch infix,
   indirect object, defer/format/hexfloat).  No parser risk; one session.
   **op/smartmatch.t's 99 rows** (the −46 companion trade, ruled in the
   answers §3.4) are the smartmatch-infix arm: same verdict, but the `$@`
   text becomes the ruled `PCL: unsupported …` refusal naming the feature.
   Bar: census falls by the classified count, every remainder explained.
8. **Track B1 #372** — a named unary's operand may BEGIN with a named unary
   (stacked filetests `-f -d $x`); A/B by the fold recipe.  Fable designs
   the operand grammar first (§3 item 2) — Opus executes.
9. **Track B2 #343** — the parenless-call × named-unary × low-prec shape.
10. **Fillers #369** (`qx{}` delimiters DROPPED — silent wrong) + **#370**
    (PPI lexes term-initial `~~` as smartmatch; rule 13 already logged) →
    **re-census** → **the announce→DIE flip** for file mode (the last step;
    plan-post-s400 §3 item 3).  #374 half (b) (a keyword-named lexical sub is
    a call only in TERM position — corrected in the task this session) and
    #365 (imported `()`-sub bareword) wait for `_reduce_term` here.

**Sessions M–N — release phase 3–5.**
11. **#279** repo hygiene (USER ruled s401: docs stay AS-IS under `docs/`;
    root junk + the 29 loose planning `.md`s + `.gitignore`) → **#280**
    README/STATUS/CHANGELOG → **#282** fresh-machine gate → **#283** CI.
    **#359** stays behind the release (fd-3 announces).

**Cross-cutting, every session:** the WHAT-TO-RUN table in CLAUDE.md decides
what runs; a review request per session (`docs/opus5-review-requests-sNNN.md`)
with the asks that need a ruling; every probe-found silent-wrong FILED with
its reproducer (this session: #376, #377).

## 3. Fable queue

0. **NEXT FABLE SESSION — USER (s409, end of session): "we should make a plan
   to reorganize all that, it seems much too complex — can it be done in a
   simpler way?"**  "All that" = the two-compiler entanglement, re-measured
   s409: one pipeline, but **88.2 % of expressions (16,898 / 19,166 per
   corpus) still lower through v1's ExprToCL** and 1,050 statements per
   corpus through v1's statement layer; the strings→forms conversion is
   already DONE (ExprToCL: 41 `*_form` generators, 0 string ones), so the
   remaining complexity is ENTANGLEMENT — the bucket/`_emit` side channel the
   seam drains around every fallback, embedded blocks calling back into a
   statement compiler, 12 statement classes handed to v1 whole, PExpr's
   destructive parse (88 % of expressions parsed TWICE), and the ~1 h
   measurement bar per change.  **Task #379 holds every number and the
   starting hypothesis** (E5.4 first: one parse, one generator, ~2–3
   sessions; return hoisted forms instead of draining buckets; ask whether
   "one compiler" for the release means one GENERATOR or zero v1 lines).
   Write the plan BEFORE executing anything; the E5.1–E5.5 estimate (9–17
   sessions, `v2-endgame-plan.md`) is the number to beat.
   **Same plan, second half (USER, end of s409): "a comprehensive search for
   doubled code — lots of small bits can be extracted to subs."**  A
   systematic duplicate-code census over `Pl/**`, `cl/pcl-runtime.lisp`,
   `tools/**` (the "same 5–20 lines written two or three times" kind — the
   four inlined modifier regexes until #374a, the two bucket-dance copies,
   the sibling autoquote copies of #266, the two delimiter hand-strips of
   #375), ranked by size × count, each cluster a candidate "extract to one
   sub" under CLAUDE.md 11.  Deliverable: a worklist Opus executes
   mechanically (corpus-diff IDENTICAL per extraction).  Task #379 §"ADDED".
1. **This session (s409)**: the review, `docs/fable-answers-s408.md`, this
   plan, DECIDED s409, CLAUDE.md pointer, tasks #376/#377 filed and #374
   corrected.
2. **#281 design half**: the string-literal escape (§3.2 of
   `generated-cl-ir-review.md`) and `p-cond` (#218) behind the bench; the
   macro vocabulary where it clarifies at zero speed cost.  Fable writes the
   arms as a worklist; Opus executes (session I takes the FREE items now).
3. **Option B phase 2 B1 design** — the operand grammar (`_reduce_term` /
   `_term_extent`: a named unary's operand may begin with a named unary,
   without touching the `$end_pars` region in place).  Needed before session
   K; one Fable session, A/B recipe attached.
4. **Boxed aggregates** (E5) — after v0.1, unchanged.  Its first consumer is
   already waiting: `lib/experimental.pm` is a shim whose delete-when trigger
   is `for values %h` aliasing (`Pl/t/feature-pragma-01.t` guards it).
5. **#221** (the minimal warnings model) — first item of the POST-release
   correctness backlog.

## 4. Standing rules added by the s408 review (also in DECIDED s409)

- **A census INCREASE is legal when it converts a WORSE failure into a counted
  drop** (a crash-form the census cannot see → an announced drop): the edit
  note names the form it replaced and the task that owns the residue, the
  file's verdict must not regress, sweep TOTAL/LOST unchanged.  Holding such a
  change until the drop is fixed would freeze the census as a ratchet on a
  metric that does not count crash-forms.
- **A gate row count is compared against a measurement of the SAME tree**,
  never against a number in a doc — the pclxs xs files (`xs-01/02/03.t`)
  contribute 0–14 rows depending on where pclxs's current state aborts them.
  When only a written number is at hand, subtract the xs rows PRODUCED IN
  EACH RUN before comparing.
- **An eval-mode drop DIES; "announce over the protocol and continue" is
  rejected** — it would keep the wrong VALUE (undef, `$@` empty) that the
  program consumes, which is rule 12's value-flows-onward case.  A lost row
  whose assertion is "no error" about a construct PCL cannot compile is not a
  cost.
- **A fragment mini-parse (`PPI::Document->new` on an interpolated span) is
  the codebase's established pattern** (28 sites in StringInterpolation /
  ExprToCL / Parser); one more that reuses the token-stream predicate is
  fine.  Full documents still have ONE construction site (`_ppi_new`).

## 5. Decisions that are the USER's — still open (from plan-post-s400 §4)

1. **Public name** — "PCL" collides; cheap to settle before #280.
2. **pclxs bundling** — release PCL first, mention pclxs as the experimental
   XS sibling; its GitHub push stays your call (#92).
3. **Hosting / remote** — the repo has no remote; #283 (CI) waits on it.

## 6. Guardrails (unchanged from plan-post-s400 §5, restated for the reader)

- The WHAT-CHANGED table decides what runs; name-resolution / scoping /
  rename ⇒ the full sweep IS the gate (sessions H and J–L are all of that
  kind).
- A census uses `grep -a` or perl; a guard reads bytes.
- Every silent-wrong found by a probe is FILED with its reproducer before the
  session ends.
- Baselines are edited ROW BY ROW with causes; `save-status` re-blesses only
  gate-green after a per-file audit.
- Do not touch the `$end_pars` region in place (Option B phase 2 owns it).
- **Never edit the compiler while a measurement runs.**
- **A `cl/**` change re-runs the companion DIRS it touches** — #368 re-ran one
  file (op/current_sub.t) and missed op/sub.t in the same dir; the row says
  "the dirs the change touches", and it means the dir.
