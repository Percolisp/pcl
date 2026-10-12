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

## The CPAN board: its population and how a dist joins (s515c)

The board is a BUG FINDER and a GATE, never a scoreboard.  Its population is
`baselines/cpan-board-dists.tsv` (one line per dist: directory under
`~/.cpan/build` or a repo-relative path, flags, the session it joined, why);
every board run is `tools/cpan-board.sh TREE OUTPREFIX`, which reads that list
(from its own checkout, so a base extraction is measured over the same list) and
passes the `--no-dist-lib` dists last.  The blessed failing rows are
`baselines/cpan-board14-fails.tsv` (the name is kept; the population is the
list file's); compare with `tools/cpan-scoreboard.pl --diff
baselines/cpan-board14-fails.tsv OUT.rows.tsv`.  Rows enter and leave BY EDIT
with a cause, never by re-blessing a run.

THE RUNNER'S CONTRACT (s516b, #1575 + #3241):
- A t-file runs with CWD = ITS DIST ROOT, for the transpile and the run alike
  (`tools/run-dist-t.pl` chdirs; every path it hands on is absolute) -- as
  `prove` / `make test` and the board's perl oracle run it.  Before #1575 a
  test reading a relative path (`do "./t/util.pl"`, a `corpus/` fixture)
  failed for a reason that was not PCL's.
- PASS means: at least one ok, zero not-ok, the PLAN MET (the rows that ran =
  `1..N`, up front or `done_testing`'s trailer) and SBCL's exit 0.  A file that
  ran rows and misses the plan or exits non-zero is PARTIAL (FAIL at zero ok)
  and carries ONE synthetic row the blessed file can hold with a cause:

      dist <TAB> t-file <TAB> 0 <TAB> *PLAN* <TAB> ran M of N, rc R <TAB> N row(s)

  (`ran M of no plan` / `a plan` when no plan line was printed; R = SBCL's
  exit, 124 for a timeout).  A file that produced NO rows keeps its `*FILE*`
  row.  Before #3241 the verdict read run-dist-t.pl's own exit and never the
  plan, so a file that died after row 1 of 25,119 read PASS 1/0.
- A file that legitimately needs more than the 120 s cap is registered in
  `baselines/cpan-board-timeouts.tsv` with the measurement it was sized from;
  the allowances in effect are printed per run.

HOW A DIST JOINS (the s515c recipe):
1. Run it ALONE: `tools/cpan-scoreboard.pl --jobs 2 --timeout 120 --tsv
   OUT.tsv --rows OUT.rows.tsv DIST` (a light run).
2. Compare the PER-FILE counts with real perl's, not only the failing rows: the
   board's verdict says PASS only when the plan is met (#3241) and the runner
   runs from the dist root (#1575) -- still read every `*FILE*` / `*PLAN*`
   row's file against perl's count.
3. Read every failing row's got/expected; probe the cause vs perl (a minimal
   program, perl -> base); cite the task or file one with the reproducer.  A
   cheap discriminator for a suspected primitive: run the file under REAL perl
   with the primitive forced to PCL's answer (s515c: `*utf8::is_utf8 = sub {1}`
   reproduced the #1389 rows of Text-CSV and JSON-PP exactly).
4. Add the rows BY HAND with their causes; a TIMEOUT gets its hang filed and a
   `*FILE*` row.
5. Add the line to the list file and re-run the whole board once: 0 NEW / 0
   FIXED against the blessed rows.

The s515c widening (14 -> 19 dists: Text-CSV, Getopt-Long, JSON-PP, HTTP-Tiny,
Path-Tiny) is recorded in `docs/session-log.md` (Session s515c) with its
per-dist cause tables.

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
