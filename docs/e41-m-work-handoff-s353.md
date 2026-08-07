# E4.1 M-work: s353 state + the Opus 5 worklist to the flip

*Written s353 (Fable), after executing M3, M6, M2 (+two unmasked layers),
and M1 from the `docs/fable-answers-s352.md` ruling.  This file is the
execution handoff: what shipped, what it uncovered, and the exact remaining
queue to E4.1 step 2.  Read the ruling first; this file assumes it.*

## 1. What shipped in s353's fix commit

Fix order followed the ruling (M3 → M6 → M2 → M1); each family was probed
against the perl oracle before and after, and the instrumented gate was
re-run between families.

- **M3 — bare `$#` (2 events → 0).**  v2 already lowered `$#[0]` as element
  access on `@#`; only the forward-declaration was missing.  A `%punct`
  bucket in `_forward_global_decls` now defvars `@#` when the emitted CL
  references it (the `[A-Za-z_]` scan can't match it — same structural gap
  as the caret specials, same fix shape).  Gate deleted.  Both
  `transpile-test-04.t` rows pass natively.

- **M6 — foreach-loop-head shadow (1 event → 0).**  `_block_captures_name`
  now records `for/foreach my $x (LIST) BLOCK` as a shadow declaration
  scoped to the BLOCK (the head `my` is Compound tokens, not a
  `Statement::Variable`, so it was invisible).  A use in the LIST still
  counts as a capture (probed: outer `$e` in the list resolves outer).
  `writes-args-01.t` 15/15; the file now takes the capture-PROMOTION path
  rather than the gate.

- **M2 — poisoned cond-my brace-deref (2 events → 0), in three layers.**
  Clearing each layer unmasked the next; all three are shared-mechanism
  fixes, not spot patches:
  1. `_interp_fixer` learned the braced-interpolation spellings
     (`"${x}"`, `"@{x}[…]"`, `"${x}{k}"`, `"$#{x}"`) in all three sigil
     arms, and the two blockers (`_shadow_rename_blocker`,
     `_state_container_blocker`) now refuse only on a REAL code-level
     brace-deref via the shared PPI-level `_has_code_brace_deref`
     (Cast/`$#`-Magic + Block holding the bare word) instead of a
     whole-content text regex that also tripped on string literals — which
     is what both live Moo events were.
  2. That let Method::Generate::Constructor reach the **self-referential
     my-init** refusal: `my $constructor = defer_sub … => sub { … my
     $constructor = …; … }` — the inner `my` is a SHADOW, not a read of
     the outer.  `_init_reads_scalar` now routes block-contained tokens
     through `_block_captures_name` (the existing shadow-aware primitive);
     a genuine closure capture of the outer name still refuses (probed).
  3. That let the file LOWER, exposing the deepest layer: **the fat-comma
     normalisation in `cleanup_for_parsing` was destructive** —
     `$part->set_content(",")` mutated the shared PPI tree while the key
     stringification stayed local, so any SECOND parse of the same region
     (shadow-rename re-lowering, seam retries) saw `,` + bare Word, which
     strict-subs compiled as a zero-arg CALL: Moo's `{ no_install => 1 }`
     became `(pl-no_install)` — the crash the gate had been masking.  The
     comma is now a fresh local token; the tree keeps `=>`; the pass is
     idempotent.  (The old code carried "XXXX This is destructive, redo
     sometime" since the beginning.)

- **M1 — Sub::Quote leading-`my` regions (15 events → 0), #247.**
  Implemented per the ruling as a WHITELIST:
  - `_eval_safe_leading_stmts`: every leading statement must be a
    single-scalar `my` whose init tokens are only earlier-leading-`my`
    scalars, Magic vars, literals with no live sigil after escape
    stripping, operators/casts/structure.  Anything unrecognized refuses.
    (PPI gotcha for the next reader: `Token::Magic` ISA `Token::Symbol` —
    test Magic FIRST.)
  - TWO collapse arms: the 2-segment `my…; package X; …` spelling, and the
    4-segment FLATTENED spelling — Sub::Quote actually wraps everything in
    one bare block plus a trailing `1;`, which T-A1 flattening renders as
    [empty main | blk leading-`my`s | pkg X (blk) | restore + literal].
    The trailing statements must pass `_eval_literal_only_stmts`.
  - `\&name` where the sub is DEFINED in the eval is deleted from the
    thunk's free-name set (it was becoming a caller-side lookup that bound
    an empty cell over the real `X::sub`).
  - Verified: moo-01.t **15/15 natively, gate audit 15 → 0**; the faithful
    write-through probe (capture = `\$lexical` holding a ref, region does
    `$$q = …`) round-trips both native and fallback.

## 2. Pre-existing divergences uncovered (none caused by this work; all probed at clean HEAD)

- **`${EXPR}` where EXPR yields a LITERAL ref-of-ref (`\\$x`) mis-derefs**
  — `ref()` answers ARRAY, the write-back is lost — in file mode, subs,
  and both eval routes, BOTH pipelines.  This is the documented
  `p-cast-$` residue: PCL's ref model collapses scalar-ref chains and
  needs the **#163 referent-kind box tag** (already APPROVED as the first
  post-R1 runtime item).  Real Sub::Quote is unaffected (its ref-of-ref
  goes through a lexical, which the box model handles — that's the
  faithful probe above).
- **`\&callersub` as an eval free name crashes** (no region needed) —
  same #163 neighborhood, pre-existing.
- **`use constant` inside an eval package region**: `eval 'package X;
  use constant K => 5; K'` dies `X::pl-K undefined` (perl: 5).  Loud, not
  silent; pre-existing #226 residue independent of M1.
- The two refused-inverse shapes (leading `my` with a free global / a
  bareword call) fail under the v1 fallback TODAY (both at HEAD) — no
  coverage change; they become perl-shaped refusals at step 2.

## 2b. M5 — DONE (s353c, Fable), same session

The ruling's bet held: the promotion machinery (`_promote_captured`) was
already extent-scoped and block-capable; the single blocker was the
documented **enclosing-outer-lexical eval refusal** ("string eval names
the lexical" — the site alist's outer let-bound pair would beat the cell).
Closed with a per-site pair riding EXISTING plumbing, no new machinery:

- `_promote_captured` waives that refusal for SCALARS and flags the cell
  (`_eval_block_cells`); containers keep the refusal (the alist carries
  scalar cells only).  The M-F pending backstop is armed the same way and
  cleared by the native decl lowering.
- The flagged decl's `_file_lex_renamed` lowering registers the cell in
  `_let_bound_vars` (block-scoped for free by the existing save/restore)
  and SKIPS the global `p-alias-eval-cell` — a later alias would clobber
  the OUTER promotion's alias, and post-block evals must keep resolving
  the outer cell.
- `_eval_lexical_alist`'s strip rule extends to `__file__N`
  (innermost-first, like `__shadow__N`), so the pair emits under the
  original name ahead of the outer pair; `_lower_sub`'s alist wipe keeps
  flagged cells (defvars — never the unbound-at-call-time crash the wipe
  defends against).

Verified: `transpile-test-01b.t` capinner row native (audit event gone),
write-through + block-end restoration probed (new -09 row "M5: block
static-var + nested sub + evals in both scopes"), container inverse still
refuses (its v1 wrongness is the pre-existing #84 family, byte-identical
at HEAD), and two perl-UNDEFINED-territory divergences recorded, not
chased: a sub whose ONLY mention of the lexical is inside an eval string
never closes over it in perl ("not available"/"will not stay shared"
family — perl's own answer flips on an unrelated `my`), where PCL gives
the consistent shared-cell answer; and the pre-existing print-args eval
timing quirk → task #250.

## 2c. M4 — DONE (s354, Opus 5)

The ruling's §1.6 diagnosis was right and had **two** bare-name sites, not
one; clearing only the first moved the churn rather than removing it.

- **The declaration count.**  `_hard_decl_count($top, $bare)` was called in
  its sigil-CONFLATED mode, so `my %mix` counted as a re-declaration of
  `$mix` → `dc=2` → refuse → `_check_my_spanning` died → whole-file v1.
  Now `'$'`-exact.  This is the last conflated site in the pass: M-F had
  already established that family *uses* are safe here (Symbol rewrites key
  on `->symbol`; the `$x` interp fixer skips `$x[`/`$x{`, which is also
  Perl-correct), so the matching family *decl* must not refuse either.
  (`_promote_captured` keeps its conflated count on purpose — different
  pass, different rule, comment there says why.)
- **The span test.**  `%spanning` is a bare-name TEXT pre-filter, so a
  sibling `my @x` used in a later segment marked `x` spanning and the scalar
  loop renamed `$x` even though the scalar never crossed.  Measured: that
  alone changed `do.t`/`method.t`/`sort.t` emission for no reason.  The
  scalar loop now asks `_canon_refs_in` — the checker's own resolver, same
  shape as the container loop's SPANSCAN — in both the single- and
  multi-instance paths.  The invariant kept is *the rename never refuses a
  name the checker will die on*: both sides now use one predicate.

Verified: `$mix`/`%mix` lowers natively and matches perl; probes for
`$v`+`@v` (`$v[1]`, `$#v`, `"@v"`, `"$v"`, `"$v[2]"`), `%w`+`$w` with
slices, a later-package shadow `my $v`, and a post-block write-through all
native and byte-equal to perl.  corpus-diff: 5 of 111 files change
(`array.t` counter renumbering; `caller.t`/`eval.t` a `my` returning to a
`let` with the eval alist carrying the let-bound box instead of a promoted
global; `each.t`/`vec.t` a dropped forward-decl false positive) — **all 8
files touched across both rounds re-swept identical to
`docs/pass-baseline.tsv`**, row for row.  Gate `131/4656 PASS`, M4's audit
event gone.  Guards: `transpile-test-02.t` — the two stale "must REFUSE /
correct via the v1 fallback" descriptions rewritten (both lower natively
now; assertions unchanged) plus a new INVERSE guard row asserting `$v` and
`@v` keep separate identities across the boundary.

## 3. Remaining queue for Opus 5 (in order)

1. ~~**M4**~~ — DONE s354, see §2c.

2. ~~**M5**~~ — DONE s353c, see §2b.

3. **Three-population zero re-measure** (§5a.2 as amended s353): the
   instrumented gate, the audited sweep, and the CPAN board — all with
   `PCL_V2_AUDIT_LOG`, all showing zero TODO-class events (DIE class
   exempt).  For the board, at minimum re-run the Moo + Role-Tiny dists
   and compare rows against `docs/cpan-module-log.md` baselines (the
   PASS/PARTIAL labels are not the measure — read rows).

   **All three measured s354 (gen v2-110); two clean, one is not:**

   - **Pl/t gate — CLEAN.**  Cold cache, `131/4656 PASS`; 7 audit events =
     5 DIE (exempt) + **2 TODO that are the deliberate inverse-guard rows
     at `transpile-test-09.t:496/499`** (the M1-predicate refusals: leading
     `my` with a free global / a bareword call), already on step 2's
     rephrase list below.  No unaccounted TODO event in `Pl/t/`.
   - **Sweep — CLEAN, and one event BETTER than blessed.**  `--jobs 8`,
     GATE clean, TOTAL passing 18498 = baseline (+0), 0 new / 0 fixed, only
     the two known crash-file UNSTABLE rows.  17 audit events against the
     18 blessed in `docs/DECIDED.md` (F4/#228 ×6, F2 residual ×5, F6 ×1,
     5 DIE): **the sweep's single multi-switch event is GONE**, absorbed by
     M1's widened collapse in s353.
   - **CPAN board — NOT clean: one unnamed TODO family, task #251.**  Moo +
     Role-Tiny, 35 events = 33 DIE (exempt) + 1 TODO true-multi-switch (Moo
     `accessor-weaken-pre-5_8_3.t`'s fresh_perl child — a ruled refusal,
     already on step 2's list) + **1 TODO `re-declaration of 'ISA' after
     in-block our-alias` on `Role-Tiny/t/subclass.t`** (`Parser2.pm:203`),
     which no ruling names.  PRE-EXISTING (verbatim at HEAD, untouched by
     M4) and its board row is byte-identical to the s343 baseline, so
     nothing regressed — but **§5a.2 is unsatisfied until #251 is ruled**
     fix-or-refusal.  Row comparison: Role-Tiny 21/23 identical, 2 changed
     and both already blessed as gains at s346b; **Moo has no blessed
     baseline at all** (`docs/cpan-board14-s343.tsv` does not cover it), so
     its s354 numbers (28 PASS / 5 PARTIAL / 38 FAIL of 71) are a first
     measurement, not a comparison.

4. **#242 — step 2, the flip** (plan §5, guardrails §5a): remove
   `parse_with_fallback`/`PCL_V1`/`PCL_V1_FILES`, purge the consumer list,
   and in the SAME commit: the #228 `[perl #129069]` registration +
   pass-baseline EDIT, and the refusal rephrases — **true multi-switch +
   F6 + the M1-predicate refusals (unsafe leading statements)** — each
   perl-shaped (`PCL: unsupported in string eval: …`) with a
   `docs/not-supported.md` entry naming its owner task.  Note the M1
   refusals' fallback errs today (see §2), so the rephrase is a strict
   improvement in error quality, not a coverage loss.

5. **#243 — step 3 deletion**, then **#244 — step 4 full verification**,
   then STOP (Fable takes #153/E5.0).

## 4. Standing cautions for the executor

- Re-run the instrumented gate after EACH family; a family is closed when
  its events read 0 and its named rows pass with the gate deleted.
- The s353 commit is emission-changing: generation is now **v2-108**, both
  checked-in artifacts regenerated (marker-only diffs).  Any further
  emission change re-bumps.
- `tools/corpus-diff.pl` output for this batch should be reviewed against
  the expectation "fat-comma/interp changes may perturb a few files";
  anything beyond that needs an explanation before commit.
- New probe scratch files from this session (m2/m6/m7/sq/p1–p5/c1/dr) live
  in the session scratchpad only; the durable regression coverage is the
  two new `transpile-test-09.t` rows + the existing files the fixes
  un-gate.
