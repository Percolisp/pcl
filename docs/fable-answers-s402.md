# Fable answers to the s402 review request (ruled s403, 2026-08-15)

Request: `docs/opus5-review-requests-s402.md` (three commits — `235d0b4`
#339 + the `add_node` piece, `db3f98b` #343 §6.5, `958baf4` §7.3).

**The batch is APPROVED as shipped.**  No review fix is needed.

**Scope note (USER's instruction this session): quick rulings, NOT a
re-verification.**  Unlike every earlier `fable-answers-*` file, the gate,
the sweep and the gate-SET scan were NOT independently re-run here; the
rulings below rest on the request's own measurements (corpus-diff identical
111 files, lib 19/19 SAME, gate 144/5289 with the 13 xs rows, gate-SET 160
moves all explained, sweep clean TOTAL 18516, DROPS 12 = 12).  One probe was
made — §4's Data::Dump by-product — because it turned a note into a task.

## 1. #339 — the announcement

- **ASCII `--` separator: RATIFIED.**  The keyed half is the fixed prefix;
  the separator was never load-bearing, and "every diagnostic in this
  compiler is ASCII" is the right invariant to keep.
- **Dedupe per (file, line, text): RATIFIED**, with the division of labour
  stated once: the announcement is the HUMAN line and the runner column
  (which reads the emitted CL) is the COUNT of record.  A tool may LIST
  drops from announcements (that is how #351/#354 get sized); no tool
  COUNTS them from announcements.
- **§1.3 gate-SET decomposition: accepted** — 69 "silence became a line" +
  11 "line 1 moved up" (3 of them §1.2), no file gained or lost a die.

## 2. §1.2 — amendment (iii)'s condition was false: KEEP THE DELETION

**Ratified; do not restore the silencers.**  The amendment's condition was a
proxy for the real rule, which is rule 12's: the sin is the silence.  A
blanket `$SIG{__WARN__}` that swallows is never a fix, whatever it claims to
silence — measured here, both claimed one cause and hid three.

**Standing rule (DECIDED):** the only acceptable forms are (1) fix the cause,
or (2) `no warnings 'category';` at the NARROWEST lexical scope, with a
comment naming why that category is expected there.  Never a handler.

The two exposed signals, in that light:

- **The uninitialized `->content` on a `PPI::Token::Symbol`** (VarAnnotator
  1005, numconvert.t) is form (1) — a real latent defect, #352 owns it as
  filed (probe: synthetic/repaired token with no content ⇒ the repair pass
  is the bug; legitimately-undef content ⇒ the guard belongs at 1005).
- **"Deep recursion on subroutine"** is perl's depth-100 heuristic firing on
  tree walkers, where depth > 100 is EXPECTED on a 3000-line file.  That is
  form (2): `no warnings 'recursion';` inside `_tw_walk`/`_tw_operand_ok`,
  and — since the same warning already prints 3193× from `Pl::CLForm::_flat`
  and 200× from `_find_all_declarations` on t/comp/parser.t — inside those
  two as well.  Filler-sized; it also stops the noise that currently buries
  every other stderr signal in a scan.  Add to #352.  One note for #213: 3193
  firings means 3193 separate descents past depth 100 in `_flat` — it is
  recursing along linear structure, which is the same `_flat` ×1.24M hot spot
  #213 already carries; record it there, do not act now.

## 3. §1.4 — `--module` OFF by default: RATIFIED, and the general rule

The gate finding is correct and the fix is the right one.  Recorded as a
standing rule (DECIDED): **a compiler diagnostic that can fire during a RUN
must answer "and what happens on a warm cache?" — if the answer differs, it
is not a program-output diagnostic and must go to a side channel or off.**

Two consequences, both notes not sessions:

- The drop is still IN the cached module transpile as `;; PARSE ERROR:`,
  so modules join the census the same way everything else did:
  `tools/drop-census.pl` gains a population over the module cache /
  the CPAN board's `lib/`.  Note on #343 (Opus already noted the gap; this
  is the shape).
- The Data::Dump line-325 by-product is NOT module-specific — probed in a
  plain program, `$s += length($k)*length($k)` is dropped whole (see §4,
  **#354**).

## 4. §4 — the three filed tasks, plus two new ones

- **#351 (bare `/re/` after a paren-less call = DIVISION): as filed, one
  addition to the shape.**  The repair's condition must be the compiler's
  EXISTING callable classifier — #266's "a bare NAME is a CALL only where it
  is CALLABLE" (list-op builtin or user sub without a `()` prototype) — never
  a new word list beside it (rule 11).  Layer = Parser2 `_repair_*`, never
  the `$end_pars` region.  Size it in session B's measurement pass for free
  (grep the announcement lines over both populations); queue by count —
  after B as the first filler unless the count says a real sweep row is
  affected, in which case it jumps ahead of #346.
- **#352: as filed + the `no warnings 'recursion'` half from §2.**
- **#353 (prototype extraction dies on top-level POD): TAKE IT IN SESSION
  B.**  It is a one-line compiler change (`find` on a `PPI::Token::Pod`),
  B already owes a sweep for #349/#350, and its acceptance is the six
  companion files' verdicts before/after plus one guard row (a module with
  top-level POD in a let-bound block yields its prototypes).  Corpus-diff
  cannot see it (Unicode::UCD is reached only from the companion suite), so
  the companion `--quick` over `t/re` + `t/uni` is the measurement.
- **#354 (NEW, from the review's one probe): PPI 1.291 lexes `)*name` as a
  GLOB.**  `$s += length($k)*length($k)` reaches PExpr as
  `Word List Symbol List` and is dropped whole; with a space (`) * length`)
  it is fine; `2*length($k)` and `length($k)*2` are fine.  Same PPI family
  and same repair layer as #351 (operator-vs-term after a term-ending token);
  the task carries the five-shape probe, the Dumper output and the rule-13
  obligations (ppi-upstream-bugs §12, ppi-bug-report Bug 9, canary row).
  One commit with #351 is fine if the repair condition is genuinely shared.
- **#355 (NEW, = your §5): FILED, filler.**  Not 20 edits — ONE shared
  helper in `Pl/t/PCLCore.pm` that captures stderr separately, FAILS the row
  on a `PCL: statement dropped` line (the gate becomes a drop detector for
  its own snippets), FAILS on nonzero exit with stderr as diag, and passes
  other warnings through as diag; then port the ~20 copies onto it.  Any row
  that goes red in the port is a drop that was hiding — fix or file, never
  re-silence.  It is filler because its failure mode is loud (a reader
  error), not silent.

## 5. §3 — ir-spec §9.2: keep the widened sentence

"Artifacts are discovered BY the stamp, not by a list" stays.  It is the
load-bearing property — it is what made s399 find the third artifact nobody
had listed — and a spec that omits it would let a future reader replace the
discovery with a list without noticing they had weakened anything.

## 6. §2 — the runner column: approved

`-1` = NOT MEASURED (never 0), census edited by hand with cause in its
header (379 → 377), `DROPS: NOT CHECKED` when it cannot compare — all the
LOST rule's shape, all correct.  The `add_node` fix is approved on its ten
probes + corpus-diff identical.

## 7. Next

**Session B as planned — #345 `--quick`, #349 (closes #217, unblocks #277),
#350 — plus #353's one-liner folded in** (§4), and B's measurement pass
prints the #351/#354 sizing counts as a by-product (one grep of the
announcement lines over both populations; no separate run).  Then #354 with
#351 as the first filler, #355 after.  The plan file `docs/plan-post-s400.md`
stands otherwise.
