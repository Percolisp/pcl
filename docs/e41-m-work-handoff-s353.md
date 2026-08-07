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
   - **CPAN board — CLEAN as of s355 (#251/M7).**  At s354 it carried one
     unnamed TODO family, `re-declaration of 'ISA' after in-block our-alias`
     on `Role-Tiny/t/subclass.t`; M7 fixed it (see §2d).  Re-measured:
     36 events = 35 DIE (exempt) + **1 TODO, the ruled true-multi-switch**
     (Moo `accessor-weaken-pre-5_8_3.t`'s fresh_perl child, already on step
     2's list).  Row comparison: Role-Tiny 21/23 identical vs the s343
     baseline, the 2 changed both already blessed as gains at s346b;
     **Moo has no blessed baseline at all** (`docs/cpan-board14-s343.tsv`
     does not cover it), so its numbers are a first measurement, not a
     comparison.

   **§5a.2 is therefore SATISFIED — #242 is unblocked.**

## 2d. M7 — DONE (s355, Opus 5), task #251

`_requalify_block_our_after_pkg_switch` refused whenever the switched region
re-declared the name.  That gated ordinary Perl — one bare block declaring
`our @ISA` in each of four successive packages, which is how you set up an
MI hierarchy compactly (Role-Tiny `subclass.t`; **perl runs it 6/6**, PCL
scored 0).  The rule the pass could not express: **an `our` alias runs to
the end of the block OR to the next declaration of the same name.**  So a
re-declaration ENDS the alias rather than defeating it.

- **Truncate, don't refuse.**  The region stops at a block-level
  `PPI::Statement::Variable` for the name: its binding runs to the block's
  end, so it partitions the block cleanly and the tail gets its own turn in
  the outer loop with its own `decl_pkg`.
- **Sigil-exact re-declaration test** (the M4 lesson again): `foreach my $d`
  binds the SCALAR `$d` and must not end an `@d` alias.  The old
  sigil-blind `[\$\@\%]` refused there, and the v1 fallback it dropped into
  printed the empty list where perl gives `1 2` — probe-verified.
- **The one surviving refusal**: a re-declaration NESTED in an inner block
  or sub.  There the alias RESUMES after the inner scope, which a
  truncation cannot express.  Its v1 fallback is silently wrong today
  (probed), so step 2's rephrase is a strict improvement — **add it to the
  step-2 refusal list below.**

Measured: board `Moo/accessor-default.t` FAIL 0/0/0 → **PASS 40 ok** (a
second file behind the same gate, previously unattributed) and
`Role-Tiny/subclass.t` FAIL 0/0/0 → **PARTIAL 4 ok / 1 not-ok**; no other
board row moved.  corpus emission **identical across 111 files**; gate
131/4658 PASS; sweep GATE clean, TOTAL 18498 = baseline.  Gen v2-111, both
artifacts regenerated marker-only.  Guards: two rows in
`Pl/t/transpile-test-04b.t` beside the existing our-alias row — the
four-package `our @ISA` chain, and an inverse-guard row covering
before/after truncation and the sigil-exact rule.

4. **#242 — step 2, the flip** (plan §5, guardrails §5a): remove
   `parse_with_fallback`/`PCL_V1`/`PCL_V1_FILES`, purge the consumer list,
   and in the SAME commit: the #228 `[perl #129069]` registration +
   pass-baseline EDIT, and the refusal rephrases — **true multi-switch +
   F6 + the M1-predicate refusals (unsafe leading statements) + the M7
   residue (an our-alias re-declaration NESTED in an inner block or sub,
   §2d)** — each perl-shaped (`PCL: unsupported in string eval: …`, or for
   M7 a file-mode equivalent) with a `docs/not-supported.md` entry naming
   its owner task.  Note the M1
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
