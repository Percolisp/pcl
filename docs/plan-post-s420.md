# Plan after s420 (Fable, s421, 2026-08-22) — the queue for the coming sessions

Written after the s420 review (`docs/fable-answers-s420.md`).  It replaces
`docs/plan-post-s408.md` §2 as THE LIVE QUEUE (that plan's H–N items are
done; its §3 Fable queue and §6 guardrails still apply).  Every step names
its bar.  USER decisions are in §4; until they are made, the default below
holds.

## 0. Where the project is (measured s421)

* Gate COLD **155 / 5614** (only the 13 pclxs xs rows); sweep **TOTAL 18364**,
  GATE clean, drops 7 = census; drop census **33 files / 106 drops**
  (39 lvalue exempt, 14 flip-legal, **~53 compiler gaps**, ~40 of those
  B3-shaped — `_reduce_term`); generation v2-163; the three artifacts fresh.
* Release phases 3 + 5 done (s419), phase 4 (the bug hunt) open since s419d
  and continued in s420.  **The push week is 2026-08-24** (USER executes:
  force-push main, keep `snapshot-2026-05`); the first green CI run is #282's
  container half.  The v0.1 tag waits on §4.1.
* The announce→DIE flip is BLOCKED on the unblock list
  (`docs/drop-census-s419-flip-gate.md` §4); after s420 what remains of it is
  dominated by **B3** (#411 8, #374(b)/#365 12, #153-B3 residue 12 + the
  family-4 residue of #410 6, #259 3), then #399 4, #415 9, op/lexsub.t 14,
  re/pat.t 3.  B3 is Fable's (§3).

## 1. Opus queue, in order

Cross-cutting (unchanged): the WHAT-TO-RUN table in CLAUDE.md decides what
runs; every probe-found silent-wrong is FILED with its reproducer; a
review-request doc per session (`docs/opus5-review-requests-sNNN.md`) when a
ruling is needed — s420 had none and the review had to be written from the
commit; write one even if it says "no asks".

**Session O1 — the two biggest single prizes, both one emission rule.**
1. **#419** — one `>0x10FFFF` literal makes the whole emitted file unreadable
   (re/pat.t: 1263 perl rows measure as 0).  Scan string literals at emission
   for a codepoint ≥ `char-code-limit`; emit a form that READS and dies on
   EVALUATION naming the codepoint; `not-supported.md` Unicode entry.  Bar:
   re/pat.t LOADS and its companion row is measured (expect hundreds of rows,
   not 1263 — "an estimate is an upper bound until the file has RUN"); the
   same scan over BOTH populations lists every other file it costs; emission
   A/B shows only files carrying such a literal.
2. **#418 (widened s421)** — pipe-quote ANY emitted symbol whose name carries a
   non-ASCII character (package designators, variables of every sigil, sub
   names); `p-stash`/stash keys stay raw strings.  Bar: the #418 stash probe +
   the s421 `%Ｘ`/`%X` collision probe vs perl; `tools/emission-ab.pl` over the
   populations = every ASCII file byte-identical; uni/ + mro/ companion legs
   A/B against a base worktree (splice with CAUSE).  Grep for the
   `$pkg =~ /::/ ? "|$pkg|" : $pkg` copies first and fold them into one helper
   (rule 11) — that helper is where the rule lives.

**Session O2 — the glob-value row and the interpolation family.**
3. **#423** — measure first (task text): is `\*FOO` structurally different from
   `*FOO` in a box?  If yes: one `box-sv` branch, `ref(\$a)` follows, op/gv.t
   back to ≥ 50, `"$glob"` interpolation fixed as a bonus.  If no: re-raise
   the s335 no-ref-kind ruling with the measurement (a Fable ask, §3) and
   record the divergence in `not-supported.md` with its row cost; do NOT add a
   slot silently.  Bar either way: the probe table vs perl + the op/ leg.
4. **#388 consumer 3** (StringInterpolation on InterpScan's `scan_one` events)
   with **#420** (`"$$r[1]"` family), **#422.1** (`"@{^CAPTURE}"`) and the #390
   shape as its ACCEPTANCE SET.  The s379 standing rule ("no new scanner
   fixes") routes #420 here rather than into another branch of the hand-rolled
   scanner; if the port slips a session, #420's local fix is allowed by the
   s366 filler rule ONLY if it reuses the `$name[` continuation (one path, not
   a copy).  Bar: the ten-shape probe vs perl (both tasks), corpus-diff
   IDENTICAL outside the named shapes, the sweep (StringInterpolation is
   reached by every file).

**Session O3 — fillers, measured small.**
5. **#415** items (two already measured: `@?` needs token repair + emission +
   defvar like `@#`; `<~>` is a CRASH, a dead file), **#421** (prototype table
   keyed by (package, bare) with bare fallback), **#422.2** (`snext_sibling`
   in `_reclass_subscripts_after`; probe `for my $Ｘ (…) {` untouched), the
   #403-family filetest fidelity if a row depends on it.  Bar: s366 filler
   rule (same mechanism + gate-SET measured + new axes filed).
6. **#399** (indirect object: 4 drops in files worth 288 rows; #381 `h F` is
   the crash twin) — needs a USER/Fable call (§4.3) before any code; Opus
   measures the shapes in both populations and brings the count.

**Then the release gate** — push (USER) → first CI run (#282 container half)
→ the tag decision (§4.1) → tag.  #359 stays behind the release; #221 first
post-release; #409 (server RSS) and #326 (the hang set, ~7500 rows) are
infrastructure items to schedule when a session has nothing rows-shaped.

## 2. What a Fable review session checks (unchanged, restated)

Cold gate + full sweep + the census when drops moved + probes vs perl of
every shape the commit names + an A/B on a base worktree for every
companion mover before it is called pre-existing — AND its cause (s421
ruling).  A `--quick` companion run when the batch touched name resolution,
the harness, or a `cl/` coercion/stringification path.

## 3. Fable queue

1. **B3 — `_reduce_term` / the operand-fallback collapse (#153)** — now the
   flip's long pole (~40 of the remaining ~53 compiler-gap drops and three
   filed tasks — #411, #374(b)/#365, #259 — wait on it).  Method is already
   ruled: s363's argument-plus-measurement — list the operand fallback branches,
   measure which are REACHABLE over both populations (gate-SET scan + the
   census as instruments), delete the unreachable ones, then take the B3
   residue list in #153 + #411 as the acceptance set.  Do NOT rewrite
   `parse()`'s main loop.  Bar: the s373 three-leg bar + sweep TOTAL/LOST.
   **Recommended as the NEXT Fable session** (§4.2).
2. Rule the asks as they come (O1–O3 above each end with a review-request
   doc); the #423 representation question if O2 measures "identical".
3. **#281** (v0.1 IR pass, in_progress) — finish with the tag.
4. Post-v0.1: boxed aggregates (design, do not start before), #221 (warnings
   model), E5.3 `local`.

## 4. USER decisions (DECIDED s425, 2026-08-22 — the USER took 3 and delegated 1 and 2 to Fable)

1. **Does the v0.1 tag DECOUPLE from the flip?  YES** (Fable, delegated).  Tag
   after the first green CI run + O1 (#419 merged s421; #418 merging s425).
   The flip's remaining blocker is B3, an open-ended Fable design; the drops
   are ANNOUNCED (loud), counted, each with an owning task, and
   `docs/STATUS.md` already words them as the sharp edge.  The flip becomes
   v0.2's headline.
2. **Is the next Fable session B3?  YES** (Fable, delegated) — the queue's
   critical path is Fable-bound; every Opus item above runs in parallel with
   it (s425 launched O2 + O3 + the #418 finish as parallel agents).
3. **Indirect object syntax (#399/#381): MAYBE LATER** (USER) — registered in
   `docs/not-supported.md` ("Indirect object syntax with a SCALAR invocant"),
   row in `docs/STATUS.md`.  Measured s425 before writing it: the CLASS-NAME
   spellings (`new Foo`, `new Foo LIST`, `new Foo(LIST)`, `ref(new Foo)`)
   already WORK through the PExpr pre-pass, and every corpus occurrence of the
   constructor shape (cpan-tests 3 lines, perl t/ 3, perl-tests 0, lib 0) is
   one of those; what is missing is the scalar-invocant spelling (2 loud
   census drops: ref.t:334, method.t:72 + twins) and #381's `h F` mis-read.
   Not refused (would cost ref.t + method.t = 288 passing rows), not
   scheduled; cheapest after B3 (a `WORD $scalar TERM` extent question).
   #399 CLOSED; #381 stays open as the mis-compile.  O3.6 (the count) is
   therefore DONE and dropped from the O3 agent's list.

## 5. Guardrails (the ones this review added; the rest are DECIDED s401–s420)

* "PRE-EXISTING" is a verdict about WHEN, not WHY — splice a companion mover
  only with its cause or the named next measurement.
* A `cl/` coercion/stringification change runs the op/ companion leg (the
  sweep has no gv.t).
* A review session writes a review-request doc even when it has no asks.
