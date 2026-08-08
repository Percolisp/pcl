# Bug review: what s358 found, and whether PCL needs a big bug hunt (s359, Fable)

The user's question, verbatim: *"make a review of the bugs found — it seems
to be quite a lot. Do we need to do a big bug hunting review of Perl's tests
and CPAN modules (and if so, when?)"*

## 1. The bugs found in the s358 arc

Seven new findings in roughly one session:

| # | Bug | Class | Found by | Status |
|---|-----|-------|----------|--------|
| 1 | Forward `goto` to standalone labels (multi-label, general shape) had no v2 lowering | LOUD gap (hard error post-flip) | Text::Balanced (every t-file) | FIXED (nested catches, defer model) |
| 2 | `_reads_name_rx`: `$name[` counted as a read of scalar `$name` → false self-ref → unbound package-var read emitted | SILENT-WRONG mechanism (crashed only by luck) | Text-Balanced 05_extmul.t | FIXED |
| 3 | Interp subscript CHAINS: `"$_[0]->{k}"` left `->{k}` as literal text | SILENT WRONG | Text-Balanced 04_extdel.t (via infinite recursion in overloaded `""`) | FIXED |
| 4 | Literal `use lib` paths never seeded into the premerge extractor's `inc_paths` → dist-local module prototypes invisible → misparses | SILENT WRONG | prototype-01 port (#255) | FIXED |
| 5 | Module prototype merge OVERWROTE local declarations (`from_module` tag missing; local must win) | SILENT WRONG | prototype-01 port (#255) | FIXED |
| 6 | v2 prototype pre-scan applies prototypes RETROACTIVELY to earlier calls (perl applies only to later-compiled calls) | Real divergence, narrow | prototype-01 port | OPEN — #256 |
| 7 | PPI 1.291 lexer: `for ${*$f} (LIST){}` → "Illegal state in 'for' compound statement" | UPSTREAM | op/for.t triage (#253) | Registered §6 + canary + report file |

(s359 adds one test-infra gotcha, not a product bug: a PPI token array goes
hollow when its `PPI::Document` is garbage-collected — its recursive DESTROY
tears down the tree.  Keep the document alive; noted in
`Pl/t/reduce-term-01.t`.)

## 2. Why so many at once — the pattern, and why it does NOT read as "the compiler is buggy everywhere"

Five of the seven came from ONE mechanism: **code that had always ridden v1
being asserted against v2 for the first time** (the Text::Balanced module
and the 27 v1-era Pl/t files).  Those were dark regions the flip lit up —
the E4.1 flip doing exactly what it was for.  The find-rate is a property
of newly-lit dark regions, not of the compiler at large.

Counter-evidence that the well-covered core is in good shape, measured this
session (s359): the new term-grammar walker (`_term_extent`, #153 step 1)
was diffed against the ~20-year-old hand-derived operand-boundary machinery
across **all 111 corpus files: zero real disagreements** — and the corpus,
gate (132 files / 4717), and sweep stayed green through every s358/s359
change.  Where PCL is exercised, it is solid; the bugs live where nothing
exercises it.

## 3. Do we need a big bug-hunting review? Mostly it already exists — the question is SEQUENCING

The four audit populations (ratified s358) ARE the standing bug hunt:

1. **perl-tests sweep** — 18.5k rows, self-gating, run every few changes.
2. **perl's own t/ suite** companion sweep — 523 files; its biggest dark
   region is exactly the #254 families (~12k rows, 13 files), plan already
   written and awaiting user scope approval.
3. **CPAN 14-dist board** — 1794 target rows; PARTIAL/FAIL families already
   classified (#231, tasks #232–#239).
4. **The fuzzer** (`tools/difftest-ops.pl`) — clean at s336 with 2 blessed
   clusters.

What is genuinely DARK today, in order of expected yield:

- **(a) The #254 families** — the single biggest known dark region, already
  measured and planned.  Nothing new to design; needs the user's scope call.
- **(b) CPAN WIDTH** — 14 dists is thin.  Core-module `.t` suites are the
  best fuzzer we know (`project_cpan_test_suite_strategy`); widening to
  ~30–50 pure-perl dists would find new families the way Text::Balanced did.
- **(c) Fuzzer axes** — the s358 bug classes name two missing axes: interp
  subscript chains, and prototype visibility/ordering (declare-then-use vs
  use-then-declare; local override vs module import).  Half a session to
  add and run.

## 4. Recommendation: schedule the big hunt as the E5 exit gate, not now

- **Pre/during E5 — no new campaign.**  E5 rewrites the statement layer;
  every E5 step already runs the four populations as its verification, so
  E5 *is* continuous hunting on the code that is changing.  Bugs found now
  in that layer would be fixed twice.
- **#254 first, though (if approved): it upgrades the sensors.**  Its ~15k
  dark rows sit in the sweep/suite populations that gate E5 — fixing the
  two families BEFORE deep E5 work turns those rows into live regression
  detectors for the rewrite.  It can run on Opus sessions in parallel with
  Fable's E5.0 work.
- **Cheap now:** the two new fuzzer axes (Opus filler, half a session).
- **The big hunt — POST-E5, pre-R2:** widen the CPAN board to ~30–50 dists,
  full suite pass, fuzzer re-run with the new axes, families triaged the
  #231 way (perl-oracle first).  That is the moment fixes land once, on the
  final compiler shape, and its output becomes the R2 correctness backlog.

## 5. And #153 is indeed the logical next step

Confirmed: E5.0 (`_reduce_term`, Option B) is the scheduled Fable-led item
(queue ruled s349/s353: after E4.1 → STOP → Fable takes #153/E5.0).  It
unblocks #147, the #138 state-init residual, the general bareword rule
(#142's prerequisites), and E5.5 shared predicates.  Steps 1–2 executed
this session — see the session log (s359) and `Pl/t/reduce-term-01.t`.
