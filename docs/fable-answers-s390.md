# Fable answers to the s390 handoff (ruled s391, 2026-08-14)

Review of `docs/opus-handoff-s390.md` (the five s390 commits) plus rulings on
the #303 judgment items its §3 left open.  Task #303's body carries the same
verdicts; this file is the reasoning.

## 1. The s390 batch: all five commits APPROVED as shipped

Independently verified, not inherited:

- **Cold gate** (cache cleared, `tools/prove-core`): **138 files / 5128
  tests**, failures exactly the 8 xs rows (`xs-02.t` ×4 + `xs-03.t` ×4) — the
  pre-existing pclxs ABI-7-vs-pin-6 drift the user has said to ignore.
  Nothing else fails.
- **#305 probed vs perl 5.40**: a fresh 13-row probe file (cast runs with and
  without `->`, `$$$rrr->{k}`, `$$$$srrr`, the mixed-sigil spellings
  `@$$arr2[0,1]` / `@$$arr2` / `%$$hrr{"x"}` / `$$$hrr{"y"}`, and five
  bare-`$$`-is-still-the-PID inverses: comparison, hash key, list element,
  interpolation, numeric equality) — **byte-identical** to perl.
- **Sweep verdict recomputed from the s390 artifacts** (`tools/sweep-diff.pl
  diff docs/fail-baseline.tsv .faillog`): 0 new / 0 fixed, **TOTAL passing
  18532 → 18535 (+3)**; the UNSTABLE/DID-NOT-RUN rows are the documented
  ref.t renumbering plus chronic-PARTIAL noise, exactly as the s390d commit
  message describes.
- **Deletion hygiene**: every deleted name re-grepped repo-wide — zero live
  references.  The three remaining textual hits (`_wrap_wantarray_ctx`,
  `_slice_in_context`, `_subscript_to_cl_str`) are comments naming deleted
  twins; #307's rename pass covers them.  The s390a "insertions" verified:
  `%NAMED_TYPE` is the surviving key set of the handlers table (the
  is-binary-operator test), the rule-12 die is new and correct, and the three
  helper subs the diff shows as added (`_ctx_wrap_form`,
  `_wrap_wantarray_ctx_form`, `_eval_lexical_alist`) all pre-exist at
  `dc60298` — they MOVED when the deleted regions collapsed.
- Cache generation bumped v2-143 → v2-144 in the one emission-changing commit
  (`8385780`); the three #305 guard rows are present in
  `Pl/t/transpile-test-10.t`.

## 2. Rulings on the #303 judgment items

### 2.1 VarAnnotator's W12 text annotator — DELETE + DIE, measurement first

Opus's recommendation APPROVED.  An annotation decides BOXING; a weaker
second annotator silently substituting on a tree crash is a silent-wrong
generator — the same disease the E4.1 flip cured at the pipeline level, and
the "second copy of the mechanism" smell in one module.

Shape and bar:

1. **Instrument first**: a stderr counter on both reach paths (the `!$host`
   guard and the tree-crash fallback), run over corpus + Pl/t gate + full
   sweep.
2. **Zero events** ⇒ delete `_analyze_text` / `_scan` / `_diff_report` /
   `_arith_rhs` and the `PCL_W12_DIFF` plumbing (its only purpose is the
   dual-run diff), wire both paths to DIE naming the crash.  Bar = the s373
   gate-SET bar: both-populations die-scan + corpus-diff + sweep TOTAL/LOST —
   a new die can kill a file above its abort point, which the fail-row diff
   alone cannot see.
3. **Non-zero** ⇒ every event is a live silent-wrong (the weak annotator
   decided boxing for that file); each gets a verdict before the die ships.

**`_text_gate_tags` STAYS** — it is shared with the tree annotator's own
parse-failure fallback; it is not part of the dead 201 lines.

### 2.2 Parser.pm's v1 state handlers — CONFIRMED out of #303

Deleting them is deleting v1 statement paths, i.e. #153 FOLD territory
(chunk 3, Fable's).  Do not touch in a dead-code sweep.

### 2.3 `_gen_interp_replacement_simple` — same ruling as 2.1

Structurally the same pattern: `_gen_interp_replacement`'s eval-wrapped
PPI-parse path, with a hand-rolled text walker silently substituting when it
fails — in s/// replacement interpolation, the very territory where s321's
#182 closed four silent-wrongs.  Delete the fallback and DIE at the miss,
same measurement-first procedure as 2.1.

One pre-check before wiring the die: the tail also catches a
defined-but-empty-string `$form` (`$form ne ''` in the guard).  Probe the
empty-replacement spellings (`s/x//` and friends) vs perl first, so a
legitimately-empty form is not turned into a crash.  If an empty form is
legal there, the die keys on "parse FAILED", not on "form is empty".

### 2.4 `gen_anon_sub_form` — delete sub + table row, KEEP the `%NAMED_TYPE` row

The `anon_sub` node type reaches the seam zero times in both populations.
Delete the sub and its `form_handlers` entry, but **keep `anon_sub` in
`%NAMED_TYPE`** so that if the type ever arrives it dies via the s390a
rule-12 arm instead of lowering as a binary operator named "anon_sub".
Verify the named-type-without-handler path actually dies; add the die if it
does not.  Zero-change expected: corpus-diff + gate.

### 2.5 `ExprToCL2::generate` — delete after a caller audit

Both construction sites (`Parser2.pm:7708`, `:8765`) call `gen_form` only.
Before deleting, audit every `->generate(` call site in the repo and confirm
none can hold an ExprToCL2 object (the method is polymorphic with
`Pl::ExprToCL::generate`, so grep alone does not settle it — read the
receiver at each site).

### 2.6 `OpcodeTree::extras` — settle the PAIR, don't orphan the writer

Rule on the mechanism, not the accessor: grep BOTH `add_extra` and raw
`->{xa}`.  If nothing reads the field, the write is dead weight — delete
writer + reader + the `PExpr.pm` POD sentence that advertises it, one
commit.  If something reads `{xa}` directly, keep `extras()` and route the
reader through it.  Do not keep a write-only field for a hypothetical future
consumer; git history is the archive.

### 2.7 `PExpr::_tok_run_desc` — KEEP (correction to the handoff)

The handoff lists it as a delete-candidate single, but its own KEEP list
retains `PExpr::_term_probe` — and `_tok_run_desc` is `_term_probe`'s
describe-a-token-run helper (`PExpr.pm:2185`), with `PExpr.pm:4048`'s
comment already saying it stays as the shared helper for the next site's
measurement.  Deleting it breaks the #153 instrument the KEEP list
deliberately preserves.  Struck from the delete list.

### 2.8 STEP 0, DEBUG→constant — DECIDED: GO

The blocking premise dissolves on inspection.  The only non-zero literal,
`expr-02.t:63 SET_DEBUG(4)`, sits inside an `if (0) { … }` developer
scaffold that never runs; every other live call is `SET_DEBUG(0)` or
`SET_DEBUG($debuglvl)` where `$debuglvl = shift // 0` and **no live caller
passes a non-zero value** (grepped all three helper files).  So the constant
silently kills nothing that runs in the gate — the "port all 21 or keep a
setter" dilemma was between a no-op and a no-op.

Shape: `use constant DEBUG => $ENV{PCL_PEXPR_DEBUG} // 0;` in `Pl/PExpr.pm`
(the only module defining DEBUG — verified); delete `$DEBUG_VAL`,
`SET_DEBUG`, and the 21 Pl/t call sites.  The test helpers' `$debuglvl`
params keep their own local dump behaviour (`expr-02.t:441/444` read the
param directly) — only the SET_DEBUG plumbing goes.  The dev workflow
becomes `PCL_PEXPR_DEBUG=N prove …`.

Pre-flight: scan the ~52 DEBUG-guarded sites for side effects beyond
printing (an assignment or call inside a `if DEBUG` guard would be
dead-code-eliminated); hoist any such exception out of the guard first.
Bar: corpus-diff identical + gate green.  Compile-time win: removes the
4.3M `DEBUG` calls per transpile the s386 review measured.  Covered by the
s379 sign-off rule (simpler, compile-time faster, generated code unchanged).

## 3. Queue

- **Next Opus session: #304** (companion-suite snapshot re-bless — PER-FILE
  audit like #223, never `--bless-rows`; each of the 44 C_ok decreases gets a
  verdict), then the #303 items cheapest-first: **2.8 DEBUG** (pure win, no
  behaviour change) → **2.4/2.5/2.6** (zero-change singles) → **2.1 W12**
  (instrument, then delete+die) → **2.3 interp fallback** (same procedure).
  2.2 stays out of #303 (FOLD).
- **Fable's items unchanged**: #153 FOLD chunk 3 design, #271 behind it,
  #281 + boxed aggregates.
