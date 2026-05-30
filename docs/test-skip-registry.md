# Test handling: the declarative skip-registry

How PCL marks `perl-tests/` assertions that it deliberately does not support, and
how crashing/aborting tests are characterized. Introduced session 216.

## Two databases — don't conflate them

There are **two separate databases**, built two different ways. **Neither is built by
diffing our test files against upstream Perl.**

| | **Skip-registry** (`cl/skip-registry.lisp`) | **Fail-baseline** (`docs/fail-baseline.tsv`) |
|---|---|---|
| What it is | hand-curated allow-list of *not-supported* failures | auto-captured snapshot of *all current* failures |
| How it's built | **manually, entry by entry** — a human reads each `not ok` and decides not-supported vs real bug, then writes one `(regex category reason)` line citing `not-supported.md`. No automation, no diff. | **automatically** — the sweep's failure log (`.faillog/*.fails.tsv`) snapshotted by `tools/sweep-diff.pl save` |
| Any diff involved? | **No.** | **Yes** — but it diffs *failure sets* (current vs baseline), never test files. |
| Touches `perl-tests/*.t`? | No (that's the whole point) | No |

**On "diffing against the originals":** that is a *consequence* of the registry design,
not how it's built. Because we no longer edit `perl-tests/*.t` (we register skips instead),
those files stay byte-identical to upstream Perl's `t/op/*.t` and so *can* be diffed against
a fresh upstream copy to catch transcription drift. That audit is optional and separate; it
plays no role in populating the registry. The old `ok(1,'SKIP')`-inline approach destroyed
that diffability — the registry restores it.

The registry's only automation is the **stale-detector**: if a registered test ever passes,
`test-ok` prints `# REGISTRY-STALE …` so the entry is removed. That keeps the hand-built
list honest — it can never silently cover a passing test.

## The problem with the old approach

Historically, a not-supported test was disabled by **hand-editing the `perl-tests/*.t`
file** — replacing the assertion with `ok(1, 'SKIP: ...')` or commenting it out
(done in ~14 files: sort.t, state.t, reset.t, lex.t, …). That approach:

- **Mutates the authoritative test files** — they can no longer be diffed against real
  Perl's `t/op/*.t` to catch transcription drift.
- **Destroys the original assertion** — we lose what was being verified.
- **Fakes a pass** — every `ok(1,'SKIP')` inflates the pass count, so the headline
  number diverges from real coverage.
- **Is manual, per-test, and TAP-number-fragile.**
- Conflicts with CLAUDE.md principle 5 ("never simplify tests").

## The mechanism

A single declarative registry, consulted at the one place every assertion funnels
through. **The `perl-tests/*.t` files are never edited** — they stay byte-identical to
upstream Perl.

### Chokepoint

Every `Test::More` assertion in the CL shim (`pl-ok`, `pl-is`, `pl-like`, `pl-cmp_ok`, …)
calls **`test-ok (pass name &rest diag)`** in `cl/pcl-test.lisp`. That is the only place
a pass/fail line is printed, so it is the only place the registry needs to hook.

### Registry data

`cl/skip-registry.lisp` — keyed by **test-file basename** and a **matcher**. A matcher is
either:
- a **regex on the test DESCRIPTION** (the `name` argument) — *preferred*, because it is
  robust to TAP-number shifts (PCL sometimes over/under-counts vs. Perl; see array.t); or
- an **exact test NUMBER** (integer) — *fallback for UNNAMED tests*. Some assertions have no
  description (e.g. `eval{…}; like($@, qr/…/)` with no name — undef.t 16/17), so a regex
  can't address them; the integer matches `*test-count*` instead. The stale-detector still
  guards numeric entries.

```lisp
(in-package :pcl)
(register-skips "tr.t"
  ("RT #130198 eval:" :principle9 "chop/chomp of a tr/// result must die … not-supported.md: …")
  ("y///r error message" :error-msg "… not-supported.md: 'Error message text and format'."))
(register-skips "undef.t"
  (16 :read-only "undef of a read-only value must die … not-supported.md: 'Read-only constants …'.")
  (17 :read-only "… (same)"))   ; unnamed tests -> keyed by number
```

`register-skips` is a macro (entries are unquoted literal lists); it compiles each string
matcher once via `ppcre:create-scanner` (integers are stored as-is) into `*skip-registry*`
(basename → list of `(matcher category reason raw-pattern)`).

Categories: `:principle9` (error detection of invalid Perl), `:error-msg`,
`:warning-emit`, `:read-only`, `:utf8`, `:destroy-gc`, `:lvalue`, `:alias`, `:tie`.
Every entry must cite a `docs/not-supported.md` section (or be `:principle9`).

### Behaviour in `test-ok`

For each assertion, `%skip-registry-lookup` matches the description against the entries
for `*current-test-file*`:

- **Match + the assertion FAILS** → emit a real TAP `ok N # skip <reason>`. Counts as a
  skip, not a pass and not a fail.
- **Match + the assertion PASSES** → emit a normal `ok N` **and** print
  `# REGISTRY-STALE: <file> test N now passes; drop skip-registry pattern "<pat>"`. This
  surfaces stale entries and accidental real fixes — a registry entry must never quietly
  cover a passing test.
- **No match** → unchanged pass/fail behaviour.

**The assertion still runs** — the registry only relabels a *failure* we have already
judged not-supported. Nothing is weakened; if PCL later supports the feature, the test
passes for real and the stale-detector flags the entry for removal.

### Adding an entry — the curation loop

1. `./runt <file>` → see `not ok` lines and `# skip` lines.
2. For a genuinely not-supported failure: add one `(regex category reason)` line under the
   file's `register-skips`, with a regex narrow enough to match *only* the failing tests.
3. Re-run. If you see `# REGISTRY-STALE` warnings, your regex is too broad (it matched a
   passing test) — narrow it. (This is exactly how the over-broad `RT #130198` pattern was
   caught and split into `RT #130198 eval:` + `RT #130198 warn: cho(p|mp)\(@a`.)

### Wiring

`runt` and `sweep-perl-tests.pl` both `--load cl/skip-registry.lisp` and
`--eval "(setf pcl::*current-test-file* \"<name>.t\")"` before loading the transpiled
file. In the `Pl/t/` gate the registry is inert (`*current-test-file*` is nil), and those
tests use real Perl `Test::More` anyway, so this never affects the gate.

### Sweep reporting

The sweep now reports **three columns: Pass / Fail / Skip** (test-own `skip` directives
and registry skips both count as Skip). `TOTAL:` prints all three. "Fully passing" =
`fail == 0` (skips allowed), and `all_ran` counts `pass + fail + skip` against the plan.

## Crashing / aborting tests are NOT skipped — they are fix targets

The registry hooks `test-ok`, so it only ever sees **assertion-level** failures. A test
that **crashes** (unhandled SBCL condition) or **aborts** (`die` during setup) never
reaches `test-ok`, and the process stops. This is deliberate:

- **A crash where Perl would not die is a PCL bug.** Auto-converting it to a skip would be
  the "easy write-off" CLAUDE.md forbids. We want crashes loud.
- The sweep characterizes them separately (unchanged this session):
  - **CRASH** — nonzero SBCL exit: `CRASH (P+F/planned ran) <error snippet>`.
  - **PARTIAL** — clean exit but `pass+fail+skip < planned`: a `die` aborted the rest of
    the file: `PARTIAL (P+F/planned ran)`.

So the full taxonomy is **four buckets**:

| Bucket | Meaning | Handled by |
|--------|---------|------------|
| pass | real pass | — |
| fail | assertion failed | fixable bug, OR add a registry entry if documented |
| skip | documented not-supported assertion | skip-registry (or test's own `skip`) |
| CRASH / PARTIAL | process-aborting | **fix** (PCL defect); never auto-skipped |

### The one case the registry can't reach

A *minority* of crashes are genuinely not-supported features that abort during
compile/run (e.g. `(?{code})` regex won't compile; Tie::Array hang). For those:

- **Coarse, available today:** the file-level `@SKIP` list in `sweep-perl-tests.pl`
  (heredoc.t, list.t) — only right when the *whole* file is unsupported.
- **Better, deferred:** a per-statement `handler-case` wrapper emitted for transpiled test
  files, turning an abort into a single `not ok`/skip and letting the file *continue*
  (rescues the tests after the crash point; a crash-registry could then skip just the
  offending statement). Trade-off: honest desync if the crashed statement was setup. Not
  built — bigger change, own risks; revisit only if the crash-not-supported set is worth it.

## Files

| File | Role |
|------|------|
| `cl/skip-registry.lisp` | the registry data (`register-skips` per file) |
| `cl/pcl-test.lisp` | registry: `*current-test-file*`, `*skip-registry*`, `register-skips`, `%skip-registry-lookup`, `*test-skipped*`, hook in `test-ok`. Failure log: `*test-log-stream*`, `%test-log-stream`, `%test-log-failure`, `%test-log-clean` |
| `runt` | loads the registry + sets `*current-test-file*` for single-file runs; inherits `PCL_TEST_LOG_DIR` if exported |
| `sweep-perl-tests.pl` | same wiring; 3-column Pass/Fail/Skip reporting; auto-sets `PCL_TEST_LOG_DIR=.faillog` (cleared each run) |
| `tools/sweep-diff.pl` | regression watchdog over the failure log (summary / diff / save) |
| `docs/fail-baseline.tsv` | committed known-fail baseline (560 keys) for `sweep-diff diff` |
| `.faillog/*.fails.tsv` | generated per-file failure DB (gitignored) |
| `docs/not-supported.md` | the rationale each registry entry cites |

---

## Leveraging the instrumentation to simplify debugging

The payoff of routing every assertion through `test-ok` is not the skip column — it is
that **one point now knows `{file, test#, description, pass/fail, got/expected, category}`**.
That turns the debugging loop from "re-run and re-read" into "query and diff". Four
accelerators build on this. **#1 and #2 are BUILT (session 216); #3 and #4 are planned.**

### 1. Structured failure log — BUILT
`test-ok` appends one TSV line per FAILING assertion to `<dir>/<file>.fails.tsv`, gated by
the env var `PCL_TEST_LOG_DIR` (one file per `*current-test-file*`, so parallel sweep
workers never interleave). Line format:

```
file<TAB>num<TAB>description<TAB>got<TAB>expected
```

`got`/`expected` are parsed from the diag strings `pl-is`/`pl-like` already build. With the
var unset there is **zero overhead** (the stream is never opened) — normal runs and the
`Pl/t` gate are unaffected. Only failures are logged, so a full-sweep DB is ~854 lines.

- The **sweep sets it automatically** to `$project_root/.faillog` (cleared each run;
  gitignored) and prints the path. So a plain `perl sweep-perl-tests.pl` always produces
  the DB. `runt` inherits any `PCL_TEST_LOG_DIR` you export.
- This removes the slowest step of the old loop — inspecting got/expected meant re-running
  SBCL from inside `perl-tests/` and grepping. Now: `grep <desc> .faillog/<file>.fails.tsv`.
- Impl: `*test-log-stream*`, `%test-log-stream`, `%test-log-failure`, `%test-log-clean` in
  `cl/pcl-test.lisp`; call site in `test-ok`'s `not ok` branch.

### 2. Baseline-diff regression watchdog — BUILT (`tools/sweep-diff.pl`)
A committed baseline `docs/fail-baseline.tsv` records the known fail set. `sweep-diff.pl`
compares a fresh `.faillog` to it **keyed on `(file, description)`** — NOT the test number,
so it is robust to the TAP-number shifts PCL keeps hitting — and prints only **NEW fails**
(regressions, with got/expected) and **FIXED** (newly passing). Replaces the manual "run
the full sweep and eyeball ~900 lines to confirm no regression" step (e.g. after the
session-216 preprocessing change) with "2 tests changed: both newly passing."

```sh
perl sweep-perl-tests.pl --jobs 8                       # writes .faillog/*.fails.tsv
tools/sweep-diff.pl .faillog                            # summary: per-file fail counts
tools/sweep-diff.pl diff docs/fail-baseline.tsv .faillog # regressions + fixes (exit!=0 if regressions)
tools/sweep-diff.pl save .faillog docs/fail-baseline.tsv # re-bless the baseline after intended changes
```

**Caveat:** if a file with real failures *flaky-crashes* in the parallel sweep (transient
`SIMPLE-FILE-ERROR` under `-j8` — see grent.t note), its failures are absent that run, so
re-bless the baseline only from a clean sweep, and treat a crashed file's "FIXED" lines as
suspect (re-run that file in isolation).

### 3a. CRASHED-vs-FIXED in the diff — BUILT (session 217)
The sweep writes `<faillog>/_status.tsv` (`name⇥status⇥pass⇥fail⇥planned`) for every file.
`tools/sweep-diff.pl diff` consults it: a baseline failure absent from the current run is
counted **FIXED only if its file ran OK**; if the file CRASHED/PARTIAL/TIMEOUT this run it is
listed under **"DID NOT RUN … UNVERIFIED, not fixed"** and excluded from the fixed total.
This kills the flaky-`-j8` trap where pack.t's transient `SIMPLE-FILE-ERROR` (it runs 0
assertions) made all ~89 of its baseline fails look fixed. Regressions (NEW) are unaffected —
a crashed file emits no failures, so it can never manufacture a false regression. (PARTIAL is
treated like CRASH: its post-stop assertions did not run, so a "fix" there is unverifiable
until the early-stop crash is localized — see #3 below.)

### 3. Crash localization (which statement aborted) — planned (falls out for free)
Because `test-ok` prints `ok N` as it goes, the **last logged line + 1 is the crash site**,
with the *name* of the next test. Add an `unwind-protect`/exit marker so a CRASH emits
`# ABORTED after test N (<last-desc>)` automatically. This is the biggest help for the
CRASH/PARTIAL bucket (bop.t, eval.t, caller/length/method/ref/state), which is otherwise
the hardest to debug because you do not even know which statement aborted.

### 4. Root-cause clustering — planned (`tools/triage.pl` on top of the log)
Normalize got/expected (addresses → `ADDR`, numbers → `N`) and group identical shapes
across all files → "this exact mismatch occurs 40× in 9 files" (fix once, clear many).
Also auto-split the fail bucket into **candidate-skips** (match a `not-supported.md`
category but are not registered yet) vs **genuine bugs** (match nothing), so triage stops
being per-file manual grepping.

### How they compound
Current loop: *run file → grep not-ok → re-run from `perl-tests/` for got/expected → fix →
full re-sweep to check regressions.*
Instrumented loop: *one sweep → log → diff/cluster → fix → diff.*
Build order: **#1 + #2 DONE** (session 216 — enabler + regression watchdog), then **#3**
(crash localization), then **#4** (clustering).
