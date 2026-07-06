# V2 Transfer Plan — finishing the move to one pipeline

**Written:** 2026-07-07 (session 277c), immediately after `state`-in-named-subs
went native. **Baseline:** cache generation v2-8, census **66 of 111
perl-tests fully v2-native**, 45 gated to v1; Pl/t gate 114 files green.
**Prereq reading:** `docs/v2-completion-plan.md` §0–§3 (the working method —
this plan inherits it wholesale and does not repeat it),
`docs/generated-cl-ir-review.md` §4b (why one dialect matters),
`docs/ir-spec.md` §2b (declarations/renames).

**The goal, stated by the user (2026-07-07):** in the end there is **one**
compiler. v1 is deleted, not merely unused.

---

## 0. What v1 still does today — the three roles to retire

1. **Whole-file fallback.** Any `die "Parser2 TODO: …"` sends the entire
   file through v1 (45 perl-tests files today, plus an unmeasured fraction
   of CPAN module transpiles). Output is the *v1 dialect* (defvar-`my`,
   `p-eval-always`, void wraps) — the two-dialect problem.
2. **The seam backend inside v2.** Every v2-native file still routes
   unported statements/expressions through the embedded v1 machinery
   (`_fallback_stmt` → `Parser.pm _process_element`; `_lower_expr` →
   `ExprToCL.pm _parse_expression` → raw text leaves). This is not a
   fallback in the whole-file sense — it is v2's expression code
   generator for most ops.
3. **Eval-mode.** `eval "string"` transpiles at runtime through v1
   *always* (`parse_with_fallback` routes any call with opts to v1); plus
   the `PCL_V1=1` escape hatch and the pipeline-keyed cache.

"One compiler" therefore means: **(1) zero whole-file gates, (2) the seam
backend either ported or re-housed as a CLForm emitter, (3) eval-mode on
v2, then (4) delete `Pl/Parser.pm`'s statement layer and the text-emitting
parts of `Pl/ExprToCL.pm`.** `Pl/PExpr.pm` (expression parsing → OpcodeTree)
and `Pl/Environment.pm` are *shared infrastructure* and stay regardless.

Rules carried over unchanged from the completion plan: the working loop
(census + parity sweep after every item), copy-v1-shapes-exactly when
porting, gate-don't-half-implement, one commit per item, bump
`*pcl-cache-generation*` on emission changes, §0.5 escalation.

---

## T0. Measurement first (small, do before anything else)

- **T0.1 Pipeline marker** (review §4b.2): one header line
  `;;; pcl: pipeline=v2 gen=v2-9 …` / `pipeline=v1`. Without it neither we
  nor consumers can audit the corpus dialect mix. Trivial; do first.
- **T0.2 Seam census.** Instrument `_fallback_stmt` and `_lower_expr`'s
  fallback branch with counters, dumped to stderr under
  `PCL_V2_VERBOSE=1` (`seam-stmt: N seam-expr: M` per file + a per-head
  histogram of what the raw text starts with). Run over perl-tests +
  Pl/t + the CPAN suite set. **This produces the T-C worklist ranked by
  frequency** — without it, seam retirement is guesswork.
- **T0.3** Re-run the CPAN suites (Try-Tiny, Scalar-List-Utils, Role-Tiny)
  recording which *module* transpiles gate to v1 — the perl-tests census
  under-measures real-code gates (e.g. `state` was invisible in it).

## T-A. Whole-file gate retirement (66 → 111 native)

Census 2026-07-07, first-gate only — expect hidden second gates behind
each; the census after each item is the tracker. In order of leverage:

- **A1. `package` inside a block — 18 files. The big one.**
  (bless, concat2, each, exists_sub, hash, index, join, length, local,
  magic, method, multideref, parent, pos, reset, scalar, sort, vec.)
  The idiom is almost always a top-level bare block declaring a helper
  class: `{ package Foo; sub new {…} … }`. Two designs, in order:
  - **(a) Segment-split the common idiom:** when a *top-level* bare block
    contains `package` statements as direct children, split it into
    package segments like the W1 block form (the enclosing block's scope
    is the complication: `my` before the package statement inside the
    block must stay visible after it — start with the subset where the
    block has no own leading lexicals, gate the rest).
  - **(b) v1-style qualification** for the general case (package
    statement anywhere): track the current package in the Environment
    only and emit package-qualified symbols for the remainder — no
    reader-package switch. Bigger; only if (a)'s census residue
    justifies it.
- **A2. The capture/rename blocker family — ~11 files.** The recurring
  blocker across W5/W10/shadow/cond gates is **interpolation** (rename
  can't reach `"$x"` inside quote tokens). One shared feature clears most:
  - **A2.1 Rename inside interpolating tokens** (W8.5 open item 1):
    guarded `s/\$\Qname\E\b/…/` on Quote::Double / Regexp / heredoc
    token contents; keep the gate for `${x}`-in-string and
    backslash-adjacent occurrences. Clears: `my-shadow … (interpolated
    use)`, `poisoned condition-my (interpolated use)`, and several
    `captured by sub` files.
  - **A2.2** The remaining capture misses (multi-declaration/shadowing
    spans: ref.t's `$test`, END-block reference, DESTROY capture) —
    per-file triage; some may extend the W5 subset, some stay gated
    until T-C makes them moot.
- **A3. Small singles**, one commit each:
  - `loop with continue block` (1) — the bare-block/C-for continue cases
    deliberately left in W6.
  - `self-referential init: my @bee = @bee` (1) — needs v1's
    init-in-binding dance for containers (scalar case already done, D27).
  - `foreach over an aliasable lvalue element` (2) — foreach var aliasing
    an array/hash *element* lvalue; needs the element's box (not
    `ensure-boxed` copy) as the loop binding.
  - `state` leftovers (2): per-closure state (anon subs — needs
    per-instance cells, a real feature: cell allocated at closure
    creation) and `state` outside block-level decls (signatures.t) /
    outside named subs (for.t) — file-level state persists across loop
    iterations; small once the cell story is chosen.
  - `CORE:: declarator prefix` (2, chop/substr) — torture-test artifact.
    Candidate for **permanent gate → skip-registry discussion with the
    user** rather than a port (CLAUDE.md §5: discuss first, never
    silently).
- **Acceptance for T-A:** census 111/111 native (minus any gates the user
  explicitly blesses as permanent), full-sweep parity vs the same-day v1
  baseline, Pl/t green, CPAN-suite module transpiles audited via T0.3.

## T-B. Eval-mode on v2 (after T-A)

Port the runtime string-eval transpile path to Parser2: an entry point
that (a) skips preamble/section assembly (single anonymous segment
producing the `p-eval-thunk` lambda shape v1 emits), (b) pre-registers the
capture alist's names as let-bound so reads/writes resolve to the passed
boxes, (c) keeps `docs/eval-lexical-capture.md` semantics bit-for-bit.
Gate: `Pl/t/eval-capture-01.t` identical under both, plus eval.t parity.
Note the ordering dependency: an eval'd string hitting a v2 gate must
still *work* — so T-B lands only when T-A has made gates rare, and keeps
a per-eval v1 retry until T-D (the eval body corpus is arbitrary user
code).

## T-C. Seam retirement (the long road — data-driven)

Two sub-decisions, made **after** T0.2 data exists:

1. **Port the head of the distribution natively.** Take the seam
   histogram's top constructs (expect: regex binding forms, sprintf/pack
   family, list ops in odd contexts, local-wrap statements,
   loop-modifier statements) and lower them in ExprToCL2 one at a time,
   byte-copying v1's emitted shapes. Each item: guard tests + parity
   sweep. This is the same motion as W11/W14 — well-understood, just
   long.
2. **Decide the tail's fate at a review checkpoint.** The honest options
   for the last N% of ops:
   - **(i) Port everything** — one codebase, maximal work.
   - **(ii) Re-house the seam**: refactor `ExprToCL.pm`'s emitters to
     produce CLForm nodes instead of text (mechanical but wide). The
     "v1 expression backend" then stops being v1 — it becomes v2's op
     emitter library; `Parser.pm`'s *statement* layer still dies. This
     achieves "one pipeline, one printer, no text islands" without
     porting every op emitter by hand, and instantly fixes the review's
     §3.1 (formatting) and §4b.4 (per-call seam defvars).
   Recommendation now, to be re-validated against T0.2 data: **(ii)** for
   the tail, (1) for the top constructs that block raw-slot/perf wins.
   Special case to plan for either way: map/grep `inline_lambda` bodies
   are pre-generated CL *strings* (`body_cl`) — they must become CLForm
   subtrees or the VarAnnotator's structural `seam` walk and the printer
   unification both keep their special cases.

## T-D. Deletion endgame (small, gated on everything above)

Preconditions, all hard: census 111/111 + CPAN module transpiles native;
eval-mode on v2; seam usage zero (or seam re-housed per T-C(ii)); one full
session cycle with `PCL_V1` unused and the v1 sweep baseline retired.

1. Make `Parser2 TODO:` dies **hard errors** (remove the
   `parse_with_fallback` retry). Run everything; the count must already
   be zero.
2. Delete `PCL_V1` handling from pl2cl and all runners; delete the
   pipeline component from `p-compute-cache-path` (bump generation).
3. Delete `Pl/Parser.pm`'s statement layer and `Pl/ExprToCL.pm`'s text
   emission (whatever T-C left); keep `Pl/PExpr.pm`, `Pl/Environment.pm`.
4. Purge pipeline-aware test branches (`begin-end-01` etc.), the
   v1-dialect rows in `docs/ir-spec.md` §2b, the two-dialect section of
   the review doc, CLAUDE.md's pipeline paragraph, memory.
5. Re-baseline: without v1, the parity oracle is gone — **the perl-diff
   sweep (PCL vs perl outputs) and the difftest fuzzer become the only
   correctness oracles.** Strengthen `tools/difftest-ops.pl` coverage
   *before* flipping this switch, not after.

## Sequencing and effort

| phase | size | depends on |
|---|---|---|
| T0.1 marker | hours | — |
| T0.2/T0.3 seam+module census | hours | — |
| T-A1 package-in-block | days (the largest single item) | — |
| T-A2 interp-rename + capture triage | 1–2 days | — |
| T-A3 singles | hours each | — |
| T-B eval-mode | 1–2 days | T-A mostly done |
| T-C port-the-head | weeks, incremental | T0.2 |
| T-C(ii) re-house tail | ~a week, mechanical | T-C checkpoint |
| T-D deletion | a day | ALL of the above |

Perf work (`docs/v2-completion-plan.md` §W15, notably W15.8 string
append) interleaves freely — it is orthogonal to the transfer.

## Risks

- **Hidden second gates**: 45 files show only their first gate; T-A's
  real size is discovered as it burns down. The census after every item
  is the tracker; do not estimate from this snapshot alone.
- **v1 rot before T-D**: until deletion, v1 must stay green (it is the
  parity oracle). Any change touching shared infrastructure
  (PExpr/Environment/runtime) still runs the v1 gate.
- **Oracle loss at T-D** (see T-D.5) — the one irreversible step; gate it
  on fuzzer strength, not calendar.
- **CPAN blind spot**: the perl-tests census systematically misses
  real-code constructs (state proved this). T0.3 exists to fix the
  sampling, and T-A acceptance includes it.
