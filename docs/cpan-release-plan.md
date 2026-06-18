# CPAN Bug-Finding → First Release Plan

**Written:** 2026-06-19 (session 259). Supersedes the ad-hoc survey direction in
`memory/project_cpan_convergence_survey.md` / `project_cpan_module_survey.md` —
those remain the running log; this is the release-oriented plan on top of them.

## Decisions (user, 2026-06-19)
- **Release bar = a curated CPAN corpus passes its own `t/` suites.** Pure
  transpiler-correctness bar. The `pcl` runner / packaging
  (`docs/pcl-rollout-plan.md`) is NOT a v1 gate (do it after).
- **Corpus = Moo ecosystem + a breadth tranche** (list below). Breadth is
  deliberate: it tests the convergence thesis past the cluster we keep
  re-testing.
- **DESTROY-via-GC = investigate scope-exit finalizers for v1** (not a flat
  "not-supported"). Decide in/out on cost after a spike.

## The thesis
New modules keep landing in the **same finite bucket set**, not an infinite
tail. Moo collapsed to a handful of *general* fixes; sessions 256–259 each fixed
one general bug that helped beyond its trigger module. So: **fix buckets, not
modules**, and make the bucket histogram measurable so "are we converging?" is a
number, not a feeling.

---

## The release corpus
Each must **load** and pass **100% of its own `t/` tests that are not genuine
PCL not-supported** (those get skip-registry entries citing `not-supported.md`),
with **zero crashes**.

**Moo ecosystem (depth):**
- Moo, Role::Tiny, Sub::Quote, Class::Method::Modifiers, Try::Tiny, Safe::Isa,
  Scalar::Util, List::Util, Sub::Util.

**Breadth tranche:**
- Carp, Data::Dumper, JSON::PP, Test::Deep, PPI.
  (PPI is the big one — large pure-Perl, and PCL conceptually depends on it; it
  is the strongest single breadth probe.)

Add to the corpus only with the standing **ASK-before-install** rule. Prefer
already-installed dists; ask before fetching a new tarball for its `t/`.

---

## Phase 0 — Make convergence measurable (do first)
Turn the hand-run survey into a **scoreboard**:
- `tools/cpan-scoreboard.pl`: runs each corpus dist's `t/` suite (wraps
  `tools/run-dist-t.pl`), records `dist⇥tfile⇥pass⇥fail⇥crash⇥first-bucket`, and
  emits both a committed TSV (the burndown) and a bucket histogram.
- Re-runnable each session; a fix that regresses a previously-passing module is
  caught corpus-wide (today only the perl-tests sweep catches regressions, not
  the dist suites).
- Output is the release dashboard: % of corpus green, and the histogram that
  proves (or disproves) convergence.

## Phase 1 — Robustness multiplier: per-statement `handler-case`
Wrap each top-level statement so an unsupported construct degrades gracefully
(continue / emit a TAP `not ok`) instead of aborting the whole file. Payoff:
1. real CPAN code survives hitting one gap instead of dying;
2. the scoreboard measures *partial* instead of *crash*;
3. converts the remaining sweep "Partial (early stop)" crashers (bop.t, eval.t)
   into honest counts.
See `docs/test-skip-registry.md` §3.1 (the deferred wrapper) for the design seed.

## Phase 2 — Highest-fanout buckets, in order
1. **General identifier ↔ CL-builtin collision.** `package Car`→`car`, `has log`,
   `list`, … → `SYMBOL-PACKAGE-LOCKED-ERROR`. The #1 open item; a name-mangling
   gap distinct from the s252 *case*-collision fix. Likely clears several corpus
   modules at once — do it first. See
   `memory/project_case_sensitivity_general_fix.md` for the related machinery.
2. **wantarray/context propagation** at the remaining sites: Try::Tiny
   `context.t` (VOID-in-catch), Safe::Isa `is_deeply`, eval-string calling
   context (`not-supported.md` "Context propagation into string eval"). See
   `docs/wantarray-context.md`, `memory/project_wantarray_followup.md`.
3. **Module compile-load double-exec / FASL-cache correctness.** Currently
   worked around by disabling the module FASL cache (`*pcl-cache-fasl* nil`) —
   both a perf and a correctness item for real use. See
   `docs/fasl-caching-design.md`.
4. **Codegen gaps** surfaced as `Handle single node of unknown type` (YAML::PP).
   Enumerate via the scoreboard; fix per missing PExpr node type.

## Phase 3 — Re-litigate the not-supported items the corpus needs
Principle 4 (no easy write-offs): **DESTROY-via-GC** blocks File::Temp,
Scope::Guard, Try::Tiny `finally`. Spike `trivial-garbage` / `sb-ext:finalize`
for *scope-exit* DESTROY (object leaves its lexical scope → fire DESTROY), NOT
full refcount semantics. Decide in/out for v1 on the spike's cost. See
`not-supported.md` "DESTROY called by garbage collector",
`reference_box_magic_hook.md`.

## Phase 4 — Long-tail primitives via the fuzzer (parallel track)
CPAN finds *idiom* bugs; `tools/difftest-ops.pl` finds *primitive* bugs CPAN
never exercises (the tier-(a) net). Keep running it, add axes, and make a clean
fuzzer run a release gate. See `docs/difftest-fuzzer.md`.

---

## Cross-cutting discipline (enforced every change)
- **Fix at the right layer** — module behavior → `lib/*.pm` shim; generic
  mechanism → `Pl/` keyed on the mechanism (never a module/function name);
  core semantics → `cl/pcl-runtime.lisp` (CLAUDE.md principle 9a).
- **Regression-test in `Pl/t/` FIRST**, then fix (bug-finding-strategy.md Tool 5).
- **Gate stays 100% green and fully-passing ≥ current** after every change
  (`memory/feedback_fully_passing_regression.md`).
- **Bucket every new failure** into the histogram; a new bucket is the signal to
  watch (convergence vs long tail).
- **ASK before installing** any CPAN module.
- Keep `not-supported.md` + `cl/skip-registry.lisp` honest; never weaken a test.

## Release gate (exit criteria)
- [ ] Every corpus module loads + passes 100% of non-not-supported `t/` tests, 0 crashes.
- [ ] `prove -j8 Pl/t/` 100% green; sweep fully-passing not regressed.
- [ ] Fuzzer (`difftest-ops.pl`) clean on all axes.
- [ ] `not-supported.md` accurate; every gap fixed OR documented + skip-registered.
- [ ] Scoreboard TSV committed and 100% green for the corpus.

## See also
- `docs/bug-finding-strategy.md` — the per-file triage loop (Tools 1–6).
- `docs/test-skip-registry.md` — marking genuine not-supported tests.
- `docs/shipped-modules.md` — shim vs CL-backed module architecture.
- `docs/pcl-rollout-plan.md` — the `pcl` runner (post-v1, when the bar shifts).
- `memory/project_cpan_convergence_survey.md`, `project_cpan_module_survey.md` —
  the running survey log + known buckets.
