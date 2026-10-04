---
name: project_pcl_measurement_traps
description: "The ways PCL's sweep/companion-suite measurements lie, and the check that catches each one"
metadata: 
  node_type: memory
  type: project
  originSessionId: 50bf0bf5-b10e-4a5a-b5d9-4cf32802f839
  modified: 2026-08-15T17:36:11.901Z
---

Every one of these cost a real session. Full text lives in `docs/DECIDED.md`
(s390, s392, s393, s396); this is the index.

- **A coverage DECREASE can be an assertion becoming HONEST** (s393):
  re/script_run.t "lost" 70 rows because `unlike` had been treating a pattern
  cl-ppcre refused as a PASS. Check whether the assertion GAINED a failure path
  before calling a decrease a regression. All 14 causes:
  `docs/suite-decreases-s393.md`.
- **`sweep-diff`'s FIXED bucket also counts a row that VANISHED** (s396) — and
  LOST cannot catch it either, because LOST reads the PASS baseline and the row
  was never passing. **When a file's FIXED count and its emitted ROW COUNT move
  together, audit the descriptions.**
- **A TIMEOUT row's C_ok is not comparable across `--timeout` values** — it is
  "how far it got", not a result. Re-measure before diffing.
- **Recovering a DROPPED statement RENUMBERS a TAP file**, so number-joined
  baseline rows appear to move. Read **TOTAL passing**.
- **A "one refusal blocks N rows" estimate is an UPPER BOUND until the file has
  RUN** (s396: #325's estimate was 46% wrong — re/opt.t's 639 rows needed
  `re::optimization`, not the refusal that was fixed).
- **A t/ file that measures perl's INTERNALS is not a PCL row count.** Read the
  assertions before sizing a family from its plan: `B::walkoptree`,
  `XS::APItest::sv_count`, `re::optimization` are readouts of one engine's
  state, unreachable by construction.
- **A ZERO from an instrument is worthless until the instrument is
  POSITIVE-CONTROLLED** (s392) — prove the wiring in a path you KNOW runs, and
  write probe events to a FILE, never stderr. Worked example:
  `docs/opus5-review-requests-s393.md` §1.
- **A DEAD-CODE CENSUS lies three ways**: lazily-`require`d modules (BlockAnalyzer
  is LIVE), Moo `is => 'lazy'` implicit builders, and `^sub (\w+)` matching POD.
  The bar is BOTH legs over corpus + gate.
- **`./runpcl` merges stderr into stdout** — separate the streams under sbcl
  before concluding which stream a diagnostic uses.
- **A `Parser2 TODO:` refusal is a compiler GAP, never a blessed non-support.**
- **A HEAD-compare is INVALID if the tree changes mid-run** — a cache-generation
  bump produces phantom diffs; preamble-normalize first. And **never `grep`
  emitted CL, or a `.tsv` under `.faillog/` or `docs/`, without `-a`**: NUL and
  high bytes make grep binary-silent, which is what invented the false #176
  premise.
- **The same binary-silence falsifies a CENSUS — i.e. a claim about SCOPE**
  (s400, three times in two days): "the #323 population is CLOSED, eight files"
  was really ELEVEN (perl's regex `.t` files are full of control bytes, and the
  missed re/pat_advanced.t was the one row that moved in #324's verification),
  and the #278 hard-coded-path survey read 22 hits where there were 31
  (`cl/pcl-pack.lisp`). **Anything that decides scope uses `grep -a` or perl,
  and a guard that greps reads BYTES** (`Pl/t/no-hardcoded-paths-01.t`).
- **A guard row must not make a claim the SORT ALGORITHM answers** — a constant
  non-zero comparator prints perl's mergesort order against the runtime's
  `stable-sort`. Assert what the comparator OBSERVED, not the resulting order.
- **A parallel A/B driver: a forked child's `END` block RUNS TOO** (s398) — the
  worktree-removing END fired in the first child to finish and deleted the ref
  compiler under its siblings: every "ref" transpile came back rc 2 with EMPTY
  stderr and the summary read SAME on empty pairs.  Guard END with
  `$$ == $parent`, and sanity-check that both sides produced BYTES before
  believing an all-SAME verdict.  Same family: **a probe that judges at a
  LATER pass than the thing it measures must replay the passes between**
  (s397: `<tree_val>` at probe time made a claimed term read as a miss).

See also [[project_v2_gate_set_measurement_rules]], [[feedback_check_total_not_just_diff]],
[[feedback_cause_not_count]].

- **`tools/corpus-diff.pl` transpiles via STDIN (`pl2cl < file`), the sweep via FILENAME (`pl2cl file`)** (found s412): with no filename, `require './test.pl'` cannot resolve relative to the source dir, so test.pl's PROTOTYPE FACTS (`is ($$@)`, `ok`, …) never apply in a corpus-diff transpile — they DO in the sweep. corpus-diff therefore cannot prove a change to module/file prototype collection; the oracle for that is `PCL_PROTO_ORACLE=DIR` (per-module JSON dumps of `prototypes` + `export_names`, diffed old vs new — the #391 method). The same difference makes the `for f in perl-tests/*.t; do ./pl2cl < $f` compile-time loop blind to the pre-scan cost: measure compile time with `./pl2cl $f` (s412: 81 s vs 64 s after #391; the stdin loop showed ~55 s both ways).
- **`tail -N` on a gate/prove output can hide the FIRST failing file** (s412: clform-01.t's failure sat above the pclxs rows and was cut off; the commit went in with a red row). Read the summary with `grep -a "Wstat"` — every failing file — never `tail`.

## s415 lessons (moved from MEMORY.md s416)

- **RE-MEASURE THE POPULATION-WIDE INSTRUMENT AFTER TOUCHING A SHARED PREDICATE** (s415): `_ends_term` did not list a REGEXP, so #370's `~~` repair split `/X/ ~~ @a` into two complements — a silent wrong in t/op/smartmatch.t.  corpus-diff is perl-tests only, the gate had no such row, and four hand-picked must-not-fire probes all had a Symbol on the left; **`tools/drop-census.pl` is what caught it**.  Standing lesson: a unit row that builds a PPI doc does NOT run the repairs — the boundary guard has to be END-TO-END.  Related: **a refusal that predates the fix it distrusts is a tax nobody collects** — three of the four refusals cleared this session guarded against a renamer/predicate since made correct, and the comments beside them said so.
- **MEASURE WHERE THE DROPS ARE BEFORE CONVERTING THEM (s415, #371)**: two of Track A's seven rows died on contact with `tools/drop-harvest.pl` — **indirect object** is out (#399: its 2 drops sit in files worth 288 passing rows, and perl still parses it), and **format was INVERTED** (its drops were in productive files, so the mechanism got fixed).  **The `($str_re)|` pass-through in `_preprocess_source` is WRONG, not weak** — any quote imbalance (a `"` in a regex, an apostrophe pair across two comments, a quote in a format picture line) opens a "string" that swallows what follows: t/op/write.t stripped 39 of 104 formats, the hex-float pass rewrote hex text INSIDE string literals (sprintf2.t's EXPECTATIONS became `"-0"`), `CORE::state` never normalised.  Format strip is line-anchored now; the skip pattern eats comments.  **A refusal classifier is ASYMMETRIC: a miss costs nothing, a false positive kills a file** (`~~` only with a TERM before it; `field`/`method`/`ADJUST` only when the file says feature 'class' is on).  Costs recorded, not hidden: #400 (state.t both populations, 158+126 rows), #401 (CORE::state now reaches two v2 state gates, 300 companion rows).

## s421 lessons

- **"PRE-EXISTING" is a verdict about WHEN, not WHY** (s421): s420 verified
  op/gv.t 50/47 → 49/48 on a base worktree and spliced it without a cause;
  one three-way probe (HEAD / post-s419d / pre-s419d worktrees) attributed it
  to the session before (#423).  Splice a companion mover only WITH its cause
  or the named next measurement.  Related: a `cl/` coercion/stringification
  change runs the op/ companion leg — perl-tests has no gv.t, the sweep is
  blind to glob rows.
- **A truncated `diff | head` can invert an A/B verdict** (s421, my own): the
  base-worktree diff of a 9-line probe was cut at 12 lines and read as "base
  gets rows 7-9 right" — it did not; the full outputs were identical.  Compare
  whole outputs (`cmp`), then read the diff.

## Archived from MEMORY.md (s445 compaction)

- **▶ STATE (s444, Fable, 2026-08-24 — ROUND 4 REVIEWED + MERGED; #518 fixed; #73 first cut SHIPPED `81c17ea` (finalize-once guard, 2.2× on a method loop; USER: cache-free first, remainder = NEXT ROUND's spec in task #73 — stash-in-box → fast path → pre-built pl-NAME; per-call-site cache REJECTED); #533 filed; nothing in flight).**  Final tree `c42cc8a`, gen **v2-221** (artifacts regenerated on exactly this tree): cold gate **171/5924** (only the 13 pclxs xs rows), sweep **TOTAL 18313 (+0)** GATE clean drops 5 = census child 9/6, gate-SET scan vs c76875a **638×2 ZERO diff**, companion 528 files (subval 31/4 + leaky-magic 66/5 edited by cause — both s443f; pvbm the EIGHTH time, variables, utf8cache = known noise).  Merged: E #470, G #485+#484(a)+#492, H #516+#515+#511, F #491+#495(a,c) — #495(b) waits for the #266 classifier (probed: widening the ALL-CAPS escapes regresses imported constants).  #518: glob-01.t on the cached-core prefix, row 29 exact; CI half rides the USER's push (week 2026-08-24).  Closed #470 #484 #485 #491 #492 #511 #515 #516; FILED **#519–#532**.  **QUEUE (plan-post-s433 §s444): #502 → I #508–#510+#512+#513 → J #505–#507+#514+#517 → K #463 items 3–5 + #479 compiler half + #478; #504; fillers #519–#532 (#525 one line + sweep; #520 runtime half FIRST).**  Standing: agents run no sweeps; Fable runs ONE sweep + legs over a merged batch, renumbers the generation ONCE above all agent strings, regenerates the artifacts; the scratchpad is SHARED with agents (prefix files).

- **STANDING, learned repeatedly in s438: a shape that occurs in NO corpus is guarded by ROWS + an INVERSE run on a worktree** (s371 — six changes in a row).  **Two predicates answering the same question about the same record and disagreeing IS the bug** (`proto_is_zero_arg`, `proto_text_has_named_params`, `_bareword_fh_p`).  **Two SETS that look alike may answer different questions** — `%PUNCT_ARRAY_CHARS` (which characters EMIT bare) vs the interp subscript set (which SUBSCRIPT in perl); `^` differs.  **The POPULATIONS and the GATE answer different questions**: #435's first version was clean in corpus-diff, the 951-file A/B and the sweep while the gate lost 97 rows.  **A fix that makes values REAL exposes rows that were passing on nothing** (s435 flip ×2, #450 ×1 — always a comparison of two empties).  **A `Pl/t` expectation can encode the OLD bug** (glob-01.t asserted `count:0`); rewrite under the s377 four-conjunct rule.  io/pvbm.t has fooled the #366 serial re-run **SEVEN** times.  **NEVER EDIT THE COMPILER WHILE A MEASUREMENT RUNS.**


- **NEVER ff-merge a compiler or runtime change into main while a measurement of main is RUNNING (s466, Fable's own mistake).**  The companion runner builds its core ONCE at start from `cl/pcl-runtime.lisp` and transpiles each file with the checkout's CURRENT `pl2cl`; merging `p-let` (new emission + new runtime macro) mid-run made every later file run new emission against the OLD core (`(p-let (($x :box …)))` under the legacy box macro → `#S(p-box :value :array)`), so the run's tail was garbage and had to be killed and re-run.  Rule: a merge into main waits for every runner on main to finish, or the runner is pointed at a worktree.  **And `pkill -f PATTERN` matches the shell running it — use a `[b]racket` in the pattern** (the `until ! pgrep -f` rule's sibling; it killed the Bash tool's own shell, exit 144).

- **(s469bi) Never edit `cl/pcl-runtime.lisp` while `tools/prove-core` is running**: the core is content-keyed on the runtime source, so the edit re-keys and PRUNES the core the running gate is using — the gate result is contaminated and must be re-run on a settled tree.

- **(s479) A BENCH ROW'S NOISE FLOOR INCLUDES CODE PLACEMENT — run the PAD PROBE before attributing a ±10 % move to a commit.**  A runtime A/B bisect (`BENCH_RT_B`) over nine runtimes put arrhash-k's +12 % and regexg's +6 % on `72bb6d22`, whose only runtime change is a `let` inside `p-sort` (neither row calls it; `sb-walker:macroexpand-all` of the loop byte-identical).  HEAD + one UNUSED `defun` inserted before `p-sort`, A/B'd against HEAD, moved arrhash-k −10…−12 % and regexg up to −12.5 % on its own.  Recipe: `perl -e '…print "(defun %pad-probe () (list 1..N))" before /^\(defun p-sort/…' > rt-padN.lisp; BENCH_RT_B=rt-padN.lisp perl tools/bench-exec.pl ROWS` for N = 8/40/200.  Check a bisect's culprit against ITS DIFF first.  Record: DECIDED §s479, faster-codegen-suggestions §0.2m.

## s490 (2026-09-18)
- **Run review probes COLD and on MAIN first**: #1844 (script-cache first-run silent wrong) showed only in pass 1 of a fresh `PCL_CACHE_DIR` and healed by run 2; the batch's guards tested script → B, the hole needed script → A → B.
- **A sweep's "0 new" cannot see a FALSE STALE** (a needless re-transpile prints the right answer): the bar for a cache-VALIDITY change is the second-run re-transpile count, base vs tree. And the sweep + `run-dist-t.pl` set `*pcl-skip-cache*`, so they cannot measure a re-transpile at all — drive the population through `pcl`.
- **A batch's one `--all --quick` is a bar only if the tree stood still under it** (t6b's ran as an orphan while commits landed; the owed #1850 splice was missed). Re-run on the final tree at review.
- **Probe any die/warn/stringify change with an overload that COUNTS its calls** — #1878's emptiness test ran an exception object's `""` on every `die $obj` (perl: 0).
- **Don't `cd` into an agent worktree from the main session** — the harness re-homes the session there; use `env -C "$W"` / `git -C "$W"`.
- **The CPAN board is the population no batch re-runs** — between 2026-09-09 and 09-18 six files drifted unseen (a stub's rows "passing on nothing", a file tipping over the 1 GB heap). Before quoting board numbers anywhere, RUN it (`baselines/cpan-board14-fails.tsv` header has the command, ~12 min at --jobs 4) and `--diff` it.
- **A zero-row verdict with rc=0 can be SBCL heap exhaustion** ("Heap exhausted, game over" is not an exit status the runners read) — look at the file's raw output before calling a verdict "unstable" or "load".
- **THIS BOX has `ulimit -n` = 524288, so an fd LEAK is invisible to every suite** (s494: unclosed lexical handles leak one fd each, #2006(b)). Reproduce resource limits as a stock machine has them: `bash -c 'ulimit -n 1024; pcl x.pl'` (macOS default: 256); count fds through `/proc/self/fd` rather than waiting for a failure.
- **No suite sends a signal from OUTSIDE the process** — that is how "only ALRM is wired to %SIG" (#2107) survived 490 sessions. A failure-experience probe needs a runner that waits for the script's "ready" line, then `kill`s it (`~/pcl-agent-scratch/s494/q5/sig/run-sig.pl`).
- **perl judges the PROBE too, even when both sides agree**: a battery row that says "same" can be two wrong answers to a broken probe (s494: `if (!open(my $p, …)) {…} <$p>` — the `my` is out of scope after the `if`; both printed ok=0). Read perl's ANSWER, not only the comparison.
