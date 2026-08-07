# Opus 5 → Fable: review requests, s352 (E4.1 opened)

Session s352 started E4.1.  **Step 1 is shipped and green** (`ef1b3de`);
step 2 is **blocked on a measurement**, below.

## §1 (ASK, blocking) — the flip's precondition is false: the gate itself runs on v1

**`docs/gate-v1-route-audit-s352.md` has the numbers.**  Short form: the
s342c live-v1 audit measured the perl-tests sweep and the CPAN board, but
never `Pl/t/` — our own gate.  Instrumenting the gate
(`PCL_V2_AUDIT_LOG` + `tools/prove-core`, green run) finds **27 v1 routes in
six TODO families** (plus 5 self-resolving DIE events):

| n | route | source |
|---|-------|--------|
| 15 | `eval-mode multi-segment (top-level package statement)` | Moo / Sub::Quote |
| 2 | `poisoned condition-my … (brace-deref)` | Moo `Method::Generate::{Constructor,Accessor}` |
| 2 | `bare $# magic` | test-generated `.pl` |
| 1 | `my-lexical 'mix' spans a package boundary` | `transpile-test-02.t:572` |
| 1 | `lexical 'x' … captured by nested sub capinner` | `transpile-test-01b.t:501` |
| 1 | `file lexical 'e' captured by sub loop` | `writes-args-01.t:87` |

The three named Pl/t rows are value assertions, so each becomes a gate
failure at the flip, not a silent change.

**The part that needs YOUR ruling, not mine:** `fable-answers-s345.md` §2
recorded the multi-segment refusal as having **ZERO measured events** and
ruled that step 2 rephrases it perl-shaped rather than fixing it.  That
premise does not survive measurement — the 15 events are **Sub::Quote**,
which is how Moo generates every accessor and constructor.  Executing the
ruling as written takes Moo off the supported list at the flip.

Options as I see them, no recommendation implied beyond the ordering cost:

- **(a) Full pre-work.**  Close M1–M6, then flip.  M1 is a compiler feature
  (a `my`-then-`package` eval region — `#226` handles only leading
  `package`), and M4–M6 live in the capture/span machinery that `#153`
  Option B was scoped to make tractable.  Multi-session; possibly wants
  #153 first, which reverses the queue.
- **(b) Narrow M1 only, re-rule the rest.**  Sub::Quote's shape is
  mechanical (`my …; my …; package X; …`); extending `#226`'s collapse to
  "leading `my` declarations then `package X;`" may be small.  M3–M6 then
  need refusal rulings with `docs/not-supported.md` entries, and the three
  Pl/t rows need a call — they cannot be weakened (CLAUDE.md 5) without
  your sign-off.
- **(c) Flip anyway, accept the rows.**  Contradicts §5a.2 ("never an
  acceptable loss") and I have no basis to prefer it; recorded for
  completeness.

I have NOT started any of them — the plan's stop rule (a) says write the
ask rather than spend a session on unplanned structural work.

**Sizing note for (b), from reading the collapse, not from trying it.**
`#226`'s condition (`Pl/Parser2.pm`, the `@segments == 2` branch) requires
segment 0 to be **empty**; Sub::Quote's segment 0 holds the `my` lines, so
that is the only clause that rejects it.  But widening it is not free, and
the trap is the one `#240` step 2 just closed: `#226` emits the package
enter **ahead of the body**, so any leading statement swept into the region
would start resolving its unqualified names in X instead of the caller's
package.  For Sub::Quote itself that is harmless — the initializers are
`${$_[1]->{"…"}}`, no free names — which suggests a narrow, probe-able
predicate ("leading statements are `my` declarations with no free
bareword/sub reference") rather than the general feature (multi-section
eval assembly in `_assemble_eval_mode`).  Whether the narrow predicate is
the right shape or just the next `#240`-style residue is your call; the
same "declare-then-use vs write-only" reasoning that shaped `#240` step 1
applies.

## §2 (FYI, shipped) — E4.1 step 1: bundle mode

`ef1b3de`.  `do_bundle` called `Pl::Parser->parse_file` directly (the one
v1-only bypass, `v2-code-review.md` §4); it now uses
`parse_with_fallback('parse_file', …)`.  Both sub-modes verified against
perl's output on a probe (module `use`, package/OO, `map`/`grep`/`first`,
string `eval`); full quadruple green — corpus identical across 111 files,
gate 131/4652 PASS, sweep 0 new / 0 fixed with TOTAL passing 18498 =
baseline, census 111/111.

The old comment claimed `parse_file` "recursively processes dependencies";
it does not (they load at run time via `p-use`), which is why the port was
a one-liner.

## §3 (FYI, shipped) — F8: a stale eval-mode refusal deleted

The gate instrumentation's first hit was
`Parser2 TODO: eval-mode bareword array subscript (out-of-frame constant)`
(`Pl/Parser2.pm`, added by the E3 commit), live on
`Pl/t/eval-constant-01.t` rows 4–5.  It claimed v2 strings the bareword —
but `PExpr::_bareword_subscript_autoquotes` had already gained the
eval-mode ALL-CAPS carve-out a month earlier (`68ab668`), and Parser2
answers `eval_mode` on both the native and the seam route.

Measured with the gate removed:

| shape inside `eval q{…}` | perl | v2 | v1 |
|---|---|---|---|
| `$self->{PROPS}[P_FOUR]` | `uc` | `uc` ✅ | `uc` |
| `$a[two_idx]` (lowercase) | `a2=lc` | `a0=lc` ❌ | `a0=lc` ❌ |

So the gate was stale for the shape it was written for, and bought nothing
for the other — v1 is wrong identically.  Deleted; `eval-constant-01.t` is
5/5 natively.  The lowercase divergence is **task #246** (pre-existing in
both pipelines, not an E4.1 dependency): perl calls the sub whenever one is
visible at the eval's compile time, and inside a string eval that means
"exists now", so the faithful fix is a runtime resolution rather than a
transpile-time name-shape guess.

## §4 (FYI) — one flaky gate row

First gate run of the session failed `glob-01.t` t29; standalone 36/36 and
it did not reproduce on a full re-run.  That row spawns `pl2cl` + `sbcl`
per assertion, so it is the load-noise family of #180/#215.  Recorded, not
chased.
