# V2 Transfer Plan — finishing the move to one pipeline

> ## SESSION 278c STATUS (2026-07-07, Opus 4.8) — typed-my done; spanning-file blockers measured
>
> **Census now 75 v2-native / 36 gated** (cache gen **v2-13**).  Two W10-ext
> items had already landed before this session (commits 0f0af0f, 04480ef:
> AST-based span detection + block-extent facts + per-name eval narrowing),
> which cleared each.t and others (66→74).  **This session: typed lexicals
> `my Foo $f`** (commit f88accb) — PPI keeps the class name as an inert Word
> token between declarator and symbol; a normalization pass
> (`_strip_typed_lexical_classes`, run right after PPI parse) removes it so
> downstream sees `my $f`.  Cleared **multideref.t** (parity 43+9/65,
> byte-identical v2==v1).
>
> **THE 8 SPANNING FILES ARE MEASURED — don't re-derive.**  Added a
> temporary `PCL_V2_RENAME_DEBUG` probe (removed after use) reporting which
> `next` in `_rename_spanning_lexicals` refuses each spanning name.  Results:
 > - **`eval_unsafe` (dominant)**: bop.t `$strval`, caller.t `$i`, sprintf2.t
>   (`$doubledouble`/`$hexfloat`/`$tests`), eval.t (many), ref.t `$x`.  The
>   rename **mangles** the name (`$x__file__N`); a dynamic `eval $var` whose
>   runtime string references the bare `$x` can't see the mangled cell (the
>   session-250 capture alist keys on the original name).  **CORRECTION (do
>   not repeat the earlier "correctness wall / impossible" claim — it was
>   wrong, see [[feedback_dont_write_off_fixable]]):** the mangle is the
>   cause, not a barrier.  The `__file__N` suffix only exists to avoid
>   poisoning some *other* `let $x` in the file.  **bop.t and sprintf2.t each
>   have exactly ONE binding of the name** — so renaming to the plain
>   `$Pkg::name` global (no mangle) makes eval'd code in package Pkg resolve
>   `$name` to the same cell, and the guard becomes unnecessary.  Fix:
>   *unmangle-when-unique* (rename to `$Pkg::name` when the bare name has no
>   other lexical binding file-wide).  Clears bop.t + sprintf2.t; check
>   caller/eval/ref per-file for a competing binding or cross-package eval.
>   Residual divergence (dynamic eval in a *different* package referencing
>   the bare name) is IDENTICAL to v1's behaviour (v1 defvars file lexicals),
>   so not a regression.
> - **container / multi-decl (`disq` + `dc>1`)**: method.t `%methods`
>   (hash), sort.t `@list`/`@output` (arrays), scalar.t `$fh`/`$x`
>   (dc=6/dc=4 — multiple file-level decls), ref.t `$test` (dc=2).  These
>   need **W10-ext-3 (container spanning)** — rename `%h`/`@a` file-lexicals
>   to a defvar'd table/vector cell (the scalar path already lowers renamed
>   decls as defvar boxes: Parser2.pm:1760, verified).  method.t's ONLY
>   blocker is `%methods` ⇒ **W10-ext-3 clears method.t cleanly** and is the
>   best next single-file target.  sort.t needs BOTH containers and the
>   `dc>1` block-shadow fix (a file-level decl's extent wrongly counts a
>   block-nested `my $answer` of the same name as a second decl).
> - **caller.t `$line`**: `dc=2` (two file-level decls) — genuinely two
>   variables, needs shadow-aware extent scoping.
>
> Recommended next: **unmangle-when-unique** (clears bop.t + sprintf2.t — see
> the `eval_unsafe` bullet above) and **W10-ext-3 container spanning** (clears
> method.t, dents sort/scalar/ref).  NEITHER is a permanent gate; the earlier
> "correctness wall" claim was retracted.
>
> ---
> ## SESSION 278b STATUS (2026-07-07, Opus 4.8)
>
> **T-A1 is DONE and DEFAULT-ON** (flag `PCL_V2_PKGBLOCK` removed).  The
> join.t miscompile was NOT a section-ordering bug (the s278 diagnosis was a
> red herring — package sections were already in source order).  Root cause:
> the block `{ package X; … tie my $t, 'X'; … }` declares `$t` embedded in a
> statement; v2 forward-defvars such embedded-`my` names, but the
> forward-decl exclusion (`_all_lex`) was keyed on the BARE name file-wide,
> so the `let`-bound `$t` in join.t's later `package main` S/SM blocks
> suppressed the `X::$t` defvar in the X section → unbound at load (tests
> 42–43).  Fix: make `_all_lex` and the `_forward_global_decls` exclusion
> **package-aware** (name → pkg → 1) — a defvar poisons only its own
> package's symbol, so a name `let`-bound solely in a different package's
> section is safe to declare.  join.t 43/43; parity on all 18 files
> (byte-identical to v1); corpus-wide full sweep identical (12475 pass / 955
> fail / 64 fully-passing) OFF vs ON.  Cache gen bumped **v2-9 → v2-10**.
> Census after: **70 v2-native** (was 66); package-in-block gate down to **5**
> (bless/index/local/magic/reset — the nested/in-sub-body/direct-`local`
> residue, design (b) territory; discuss permanent-gate vs port with user).
>
> **NEXT FRONTIER (measured by the s278b census):** the top whole-file gate is
> now **"my-lexical spans a package boundary" — 16 files** (array, bop,
> caller, do, each, eval, length, method, pos, ref, scalar, sort, sprintf2,
> substr, undef, vec).  That is the W10-ext worklist below (§"next-gate
> worklist"): block-scoped facts (W10-ext-1) clears sort/method; container
> spanning (W10-ext-3) clears each; multi-decl spanning (W10-ext-2) clears
> vec; string-eval blanket (W10-ext-4) clears pos; typed-my clears
> multideref.  **START HERE next.**
>
> ---
> ### (historical) SESSION 278 STATUS (Fable → Opus 4.8 handoff)
> T0 is **DONE** (marker, seam census, module census — data below and in
> `docs/v2-census-2026-07-07.md`).  T-A1 was IMPLEMENTED BUT FLAGGED OFF
> (`PCL_V2_PKGBLOCK=1`) with one open miscompile (now fixed — see s278b
> above).  One real pre-existing annotator bug found and fixed on the way.
>
> ### What landed this session
> 1. **T0.1 pipeline marker** — every transpile starts with
>    `;;; pcl: pipeline=v2 gen=v2-9`.  One chokepoint (`pipeline_marker` in
>    pl2cl's `parse_with_fallback`) + the two direct-v1 sites (server mode,
>    bundle mode — NOTE: **bundle mode is v1-only today**, route it through
>    `parse_with_fallback` at T-D).  gen= is read from the runtime's
>    `*pcl-cache-generation*` (bumped v2-8 → **v2-9**).
> 2. **T0.2 seam census instrumentation** — `PCL_V2_SEAM_CENSUS=1` makes
>    Parser2 dump per-file TSV histograms to stderr (`pcl-seam` lines): the
>    statement seam, fallen-back expression roots, and the **blame frontier**
>    (per fallen-back tree, re-run `gen_form` post-order; blame nodes that
>    fail while all their children succeed — the exact constructs whose
>    porting unblocks expressions).  Zero cost when unset.  Driver:
>    `perl tools/v2-census.pl [--jobs N] [files...]` → markdown report
>    (defaults to perl-tests/*.t; give it .pm paths for the module corpus).
>    Note when reading: a `word:NAME` blame row means "call to NAME"
>    (unknown/builtin callee — the Word leaf is blamed, not the funcall).
> 3. **T0.3 module census** run over cpan-tests/modules + Try-Tiny +
>    Role-Tiny + Scalar-List-Utils + lib/*.pm.
> 4. **VarAnnotator fix (default-on, real bug)**: a chained deref write
>    `$r->{A}[0] = 5` left `$r` **unboxed** (`(p-undef)` raw let) because the
>    `=`-handler's h_acc/a_acc branch treated every element write as
>    "container write, keys are reads".  With `$r` unboxed, autovivification
>    cannot write the vivified hash back through the box → every deref
>    re-vivifies a fresh hash (exists_sub.t t13 returned false).  Fix: when
>    the access BASE is not a plain %h/@a Symbol (checked via PPI
>    `->symbol`), `_tw_mark` the base subtree `write-deref-viv`.
>    Single-level `$r->{a}` was never affected (h_ref_acc root → write-list
>    branch).  exists_sub.t: 12+1F → **18/18**.
>
> ### Census headlines (full data: docs/v2-census-2026-07-07.md)
> - perl-tests: **66 native / 45 gated** (bit-identical to the s277c
>   baseline).  Gates: 18× package-in-block, 4× spanning-my, 6× capture
>   family, 2× CORE::, + singles.
> - modules (119 files): **94 native / 25 gated** — and ZERO
>   package-in-block: real-module gates are **17× "file lexical captured by
>   sub"** (the W5 subset misses), 2× BEGIN-introspection, 2× **"my
>   array/hash in condition"** (a gate the perl-tests census never shows!),
>   2× poisoned-cond-my-interp.  **For CPAN value, A2 (capture family)
>   outranks A1.**
> - Expression seam: **88.9%** of expressions fall back (perl-tests corpus),
>   **81.2%** (module corpus).  The v1 expression generator is v2's main
>   backend, not an edge case → T-C option (ii) (re-house ExprToCL.pm's
>   emitters as CLForm producers) is confirmed as the strategy; per-op
>   porting can never cover this.  Module-corpus frontier top: `sym:@`,
>   `op:+` (the `+{…}` disambiguator), `node:h_ref_acc` ($self->{k}!),
>   `cast` (%$self), `magic:@_`, `word:shift` — the OO family; that subset
>   is ALSO the perf-relevant one (extends W11/W14).
> - Statement seam is small and closed-class: use/no/require, BEGIN family,
>   `local`, proto/sig `sub` — structural, handled by re-housing, not
>   per-op ports.
>
> ### T-A1 STATUS — implemented, flagged off, one bug to fix
> Implementation (all in `Pl/Parser2.pm`): `_flattenable_pkg_block`
> (predicate + conservative refusals: labeled block; `last/next/redo/goto`
> that could target the block — a bare block is a one-iteration loop;
> direct-child `local`), the `$consume_pkg` closure (shared package-stmt
> consumer for top level and flattened blocks), blk-tagged segments +
> restore segment, and **blk-extent live-ranges** in `_check_my_spanning` /
> `_rename_spanning_lexicals` (a block lexical dies at its block's end —
> both for spanning detection and for bounding the qualified rewrite; this
> is what stops whole-file text-scan false positives on short names like
> `$r`).  Enable with `PCL_V2_PKGBLOCK=1`.
>
> Verified with the flag on: concat2 4/4, exists_sub 18/18, parent 7+2F
> (**identical to v1**, pre-existing), spanning + closure-write reductions
> match perl.  **Open bug (the reason for the flag): join.t 41/43.**
> Diagnosis so far: in the full join.t transpile the `package o { use
> overload … }` section and the final bare block (tests 42–43) are emitted
> **out of source order** — they appear at output lines ~262–295, BEFORE
> the SM sections (~307+), and the two `pl-is` forms abort at load
> (`p-load-with-recovery` reports "2 top-level form(s) aborted").  In
> isolation the same code transpiles and runs correctly, so it is an
> interaction with the preceding flattened blocks/segments — suspect the
> section-assembly bookkeeping (`_captured_decls`/`_sched_defs` snapshots
> per section) or the leading/restore segments my flattening inserts.
> Repro:
> ```
> PCL_V2_PKGBLOCK=1 perl -I. pl2cl --no-cache perl-tests/join.t > /tmp/j.lisp
> grep -n ';;; package' /tmp/j.lisp   # o-section sits before SM-sections
> ```
> Also note: `(defvar $overloaded …)` gets forward-declared from a TEST
> DESCRIPTION string ("join, $overloaded, LIST") — the forward-global text
> scan reads string contents; harmless here, but a known sharp edge.
>
> **Definition of done for T-A1:** fix the ordering bug → full-sweep parity
> on all 18 files (each must be ≥ its v1 numbers) → remove the
> `PCL_V2_PKGBLOCK` flag (default on) → bump `*pcl-cache-generation*` →
> census re-run shows package-in-block ≤ 5 (bless/index/local/magic/reset
> stay gated: nested-compound/in-sub-body/direct-local cases — design (b)
> territory, likely not worth it; discuss permanent gates with the user).
>
> ### The next-gate worklist behind T-A1 (measured, in leverage order)
> With flattening on, 13 files slide from "package-in-block" to
> **"my-lexical spans a package boundary"** — the W10 rename subset
> refusals, each classified this session:
> - **W10-ext-1 — block-scoped facts** (clears sort.t, method.t, more): the
>   subset demands `decl_count == 1` FILE-wide; for a blk-tagged declaring
>   segment the facts (decl_count, disq) only need to hold over the block's
>   extent, since the rewrite already stops there.  sort.t has two
>   `my $answer` in different blocks; method.t two `my $o`.
> - **W10-ext-2 — multi-decl spanning** `my ($bar, $baz)` (vec.t): extend
>   `scalar_decl` to per-name positions inside a multi-decl.
> - **W10-ext-3 — container spanning** `my %h` (each.t, hash.t): rename to
>   a defvar'd vector/table package cell — same mechanism, `@/%` sigils.
> - **W10-ext-4 — narrow the string-eval blanket refusal** (pos.t): today
>   ANY `eval STRING` at/after the declaring segment refuses every rename;
>   only evals that could SEE the renamed name matter (same-scope test, or
>   at least skip when the eval'd literal never mentions it).
> - **typed `my Foo $f;`** (multideref.t): unsupported-declaration die in
>   `_lower_stmt`; lower by ignoring the class name (v1 does).
> - length.t's `$u`: subset refusal not yet classified — debug first
>   (likely tie/interp disq).
>
> ### Rules that bit this session (don't relearn them)
> - **Sweep parallel runs are flaky** (known): parallel `--jobs 4` gave
>   exists_sub 12+1F while `--jobs 1` gave 18/18.  ALWAYS re-verify a
>   suspicious per-file result with `--jobs 1` before debugging it.
> - The v1-comparison sweep run **overwrites `.faillog/`** — capture v2's
>   faillog before running the v1 baseline.
> - `./runpl perl-tests/foo.t` cannot run test.pl-based files (BEGIN
>   `require './test.pl'` needs CWD=perl-tests and the sweep's pcl-test.lisp
>   provides plan/is/ok) — reproduce sweep failures with the sweep line:
>   `timeout 60 sbcl --control-stack-size 512 --noinform --non-interactive
>   --load cl/pcl-runtime.lisp --eval '(setf pcl::*pcl-skip-cache* t)'
>   --load cl/pcl-test.lisp --load cl/skip-registry.lisp --eval
>   '(pcl::p-load-with-recovery "/tmp/foo.lisp")'`
>

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
