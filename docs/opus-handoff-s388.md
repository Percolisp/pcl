# Opus → Fable handoff, end of s388 (2026-08-13)

**Read this first.** It replaces `docs/opus-handoff-s386.md` as the entry point.

---

## 1. What closed this session

| task | what | where |
|---|---|---|
| **#296** | exception-named `my` rename — B1 (string-eval capture) + B2 (a later declaration owns the uses after it) | merged `66bdb93` |
| **#291** | the poisoned-`my` rename family DELETED — enabler + three families | merged `297b5e4` |
| **#205** | section-let-bound name that is also the veto global | closed with #291's enabler |
| **#292** | the extra net: pass-baseline re-bless + fuzzer + companion suite | `89317f0`, `e1a8a32` |
| **#289** | **Direction D umbrella — COMPLETE** (steps 1–4) | — |
| #303 step 0 | attempted, **REVERTED** — caller census was wrong twice | (see §4) |

**State:** gate **138 files / 5118 tests PASS**, cache generation **v2-142**.
Full sweep GATE clean, **TOTAL passing 18498 → 18532 (+34)**.
Fuzzer at the standing clean result. Working tree clean, all merged to `main`.

---

## 2. The queue, as it now stands

Yours (design-led), which is why we stopped here:

1. **#153 chunk 0 + the FOLD** — E5.0's `_reduce_term`. The metric is the v1
   fallback rate: **88% of seam expressions still go through
   `_parse_expression_form`** (measured s386, `docs/compiler-duplication-review-s386.md`).
   Chunk 0 is the registry-ownership move: Parser2's lexical registry currently
   lives *inside* the `fallback_parser` object, 27 sites.
2. Then #303 (dead-code batch, ~3.5k lines — mechanical, Opus work, see §4),
   §7 hoisting + #281 macro pass, then the v0.1 track (#277–#283).

Standing rulings unchanged: boxed aggregates are E5 Fable design, do not start;
#147 / #138-residual remain blocked on #153.

---

## 3. Things worth knowing before you plan

**The rename machinery you may have been planning around is gone.**
`__shadow__`, `__cond__`, `__emb__` are deleted; corpus rename bindings went
716/128/5 → **36/0/0**. The 36 survivors are `_gate_seam_my_shadow`, a v1-seam
mechanism with a live cause — **they go with #153**, so the FOLD now owns the
last of that family. `#237`'s consumers 2–3 should be re-sized against this
smaller surface.

**Two mechanisms that replaced renames, not just deleted them:**
- Family 3 (`__emb__`) became a *narrowing* of `_lower_block`'s let-hoist veto:
  the veto is not asked when the statement sits inside a sub body. 11 lines
  added, 96 removed. That is the shape #265/#272 always wanted; s368 couldn't
  do it because the `let` suppressed the global's declaration, which #291's
  enabler fixed.
- The enabler itself: `_forward_global_decls` no longer excludes `_seg_lex`
  names **in FILE mode**. EVAL mode keeps the exclusion — there the same list is
  the `p-eval-thunk`'s capture *parameters* from the caller, not declarations.
  Whoever touches that function next needs that distinction.

**`_eval_lexical_alist`'s suffix strip list is load-bearing and easy to break.**
Twice during the #291 replay an old hunk tried to shorten it back to its s384
shape, which would have taken `__excl__` (#296's rename) with the dead
suffixes and silently broken the B1 string-eval path. **A suffix leaves that
list only when its minter leaves the compiler.**

---

## 4. #303 step 0 — ATTEMPTED AND REVERTED (read the task before retrying)

Tried, **reverted**, tree clean. The conversion itself is sound —
`use constant DEBUG => $ENV{PCL_PEXPR_DEBUG} // 0` gave a corpus-diff that was
**byte-identical across 111 files**. What was wrong, *twice*, was the caller
census:

1. The task said "SET_DEBUG has zero callers". False — the s386 trace covered
   *corpus file transpiles*, not the Pl/t gate.
2. I then said "four live callers, all in `expr-01.t`, all passing 0, so it's
   inert". Also false — that came from a `grep | head` cut at 10 lines. The
   gate caught it: 138 files / **4613** tests, FAIL,
   "Undefined subroutine &Pl::PExpr::SET_DEBUG".

The real census is 21 live calls across **four** files (`expr-01/02/03`,
`phase1-01`), and **`expr-02.t:63` is `SET_DEBUG(4)` — non-zero**, so the
"inert" claim was wrong too: the constant would silently kill that region's
debug output. Full list and the decision a retry must make are in task #303.

**Generalise before the rest of #303**: every "never called" claim in that task
comes from the same trace and carries the same blind spot. For each candidate,
`grep -rn NAME --include='*.pm' --include='*.t' .` with **no `| head`**, and
read the whole output. A truncated census is how both attempts failed.

---

## 5. Open measurement debt (filed, not silently carried)

- **#304 — the companion-suite snapshot is 191 commits stale.**
  `docs/perl-suite-run.tsv` is stamped `1e7c4d7` (s323e), i.e. **before** E4.1
  (#242, s356) flipped v1 gates to hard errors and retired `--lenient-ppi`.
  This session's `--all` run shows 44 C_ok decreases and 21 files flipped to
  TRANSPILE-fail (`opbasic/cmp.t` 12078 → 0) — **all pre-existing**, proved by
  transpiling the 21 at `66bdb93` and comparing normalized error lines
  (19 identical, 2 artefacts). It also hides real *gains*
  (`comp/parser.t` 0 → 65, `op/while.t` 11 → 20). Needs the per-file audit
  #223 gave the sweep baselines — **not** a `--bless-rows` regeneration, which
  would bless the whole TRANSPILE cluster as expected.
- **#300** — a loop-head `my` is bound once for the whole loop; a closure made
  in the body sees the final value. Unscheduled, pre-existing.

---

## 6. Method notes from this session (all now in `DECIDED.md`)

- **A replay of reverted work is not a cherry-pick.** Every `Parser2.pm` hunk
  conflicted, because #296's two passes now sit *inside* the deletion spans of
  #291's families 1 and 2 and share their comment headers.
- **When a merge touches a test file, a green gate is not the check — the test
  count is.** A botched conflict resolution silently deleted 12 rows from
  `transpile-test-10.t` and the gate still said PASS at 5096 where 5109 was due.
- **`perl -i` plus a slurp truncates the file to zero** — `-i` restores STDOUT
  once ARGV is exhausted. Write to a new path, verify, then copy.
- **corpus-diff carries no signal for a change that moves emission everywhere**
  (#291 adds cells and removes renames by construction). The sweep is the
  measurement there; corpus-diff is for changes that *should* be local.
- **A truncated census is a false census.** Three separate errors this session
  came from trusting a cut-off view: `grep | head` (twice), and a marker-splice
  loop that ran off the end of an array. When the answer is "how many callers /
  rows / files", read the whole output.
