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

## 3. Remaining queue for Opus 5 (in order)

1. **M4 — `my-lexical 'mix' spans a package boundary` (1 gate event).**
   Ruling §1.6: the CHECKER (`_check_my_spanning`/`_canon_refs_in`) is
   already canonical via PPI `->symbol`; make the container-spanning
   RENAME's use-collection resolve the same way so `$mix` (scalar) and
   `%mix` (reached via `$mix{k}`) rename independently.  Update the
   guard-edge row's DESCRIPTION (`transpile-test-02.t:572`) in the same
   commit — its "still correct via the v1 fallback" premise is void
   post-flip; the value assertion itself is unchanged.

2. **M5 — block lexical captured by a nested named sub (1 gate event).**
   Ruling §1.7: route through the EXISTING `_file_lex_renamed`
   cell-promotion path extended to block scope; the eval alist must answer
   the inner cell inside the block and the outer lexical after it
   (`transpile-test-01b.t:501` is the probe).  **One-session cap;
   stop-rule: new rename machinery or any PExpr term-region touch → STOP
   and write the ask** — only then is #153-first live.

3. **Three-population zero re-measure** (§5a.2 as amended s353): the
   instrumented gate, the audited sweep, and the CPAN board — all with
   `PCL_V2_AUDIT_LOG`, all showing zero TODO-class events (DIE class
   exempt).  For the board, at minimum re-run the Moo + Role-Tiny dists
   and compare rows against `docs/cpan-module-log.md` baselines (the
   PASS/PARTIAL labels are not the measure — read rows).

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
