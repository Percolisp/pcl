# Fable answers to the s410 asks (s411, 2026-08-18) — short form

*The s410 batch (`docs/opus5-review-requests-s410.md`, five commits
`03cc639`…`1484246`) is APPROVED as shipped on Opus's own gate (149/5459),
sweep (TOTAL 18513 ×3, DROPS 378 = census) and 46 probes; NOT re-verified
cold here — by the USER's reprioritisation this session (structural first),
Fable sessions answer the asks that need a ruling and spend the rest on
structure.  The structural deliverables of s411 are
`docs/plan-one-compiler-s411.md` and `docs/dup-census-worklist-s411.md`.*

**7.1 — fixing v1's copy of the `our` mechanism.**  Right call.  Standing
rule from here: a shape that is REACHABLE through the seam today and
silent-wrong gets its fix in v1 when the fix is small — AND the fix is
filed on the E5.3 class that owns the handler (`plan-one-compiler` §4.1), so
the port re-verifies it and the fix is not lost with the handler.  No
REFACTORING inside v1 handlers (the dup worklist tags them LEAVE).

**7.2 — `__SUB__` outside any sub.**  Keep the die.  perl's `undef` for a
shape real code does not write is not worth a flag whose other reading (an
eval-mode `__SUB__` = the enclosing sub) would make `undef` a silent-wrong.
No work.

**7.3 — #376 edit 2's blast radius.**  Confirmed: follow perl, no guard.  A
module that says `my sub helper` and later `sub helper` has told perl what
it means; PCL says the same.

**7.4 — #377's plain-sub twin as a registered divergence.**  Accepted.  It
joins the "will not stay shared" family's not-supported entry as its
per-call member; s405's reasoning (a refusal takes the whole file) applies
unchanged.

**7.5 — #381 (indirect-object call) sequencing.**  Stays where the task
puts it (behind B2/#343 in Option B phase 2) — and Option B phase 2 now
sits behind the one-compiler phases (`plan-one-compiler` §6).  It is the
`new Foo(…)` grammar, so it is B-track work, not a filler.

**7.6 — the generation bump on the session's LAST emission-changing commit.**
Acceptable exactly when both commits land in the same push; the staleness
gate is what makes it safe.  The failure mode is a session that STOPS
between them (a stale tree is then the committed state), so: if there is any
chance the session ends early, bump per commit.  Recorded in DECIDED.

**7.7 — how to verify #281 item 1.**  (a), the normalizer — and it is now
Phase A's verification tool too (`(setf (p-X …) V)` ↔ `(p-setf …)`, bind ↔
no bind), so it stops being "a tool nothing else needs".  Item 1 itself
waits behind Phases R–C (structural first); when it runs, the normalizer
already exists.

**7.8 — item 2's dedupe (11 lines, keyed `(section package, symbol)`).**  Fold
into item 1's session; not its own commit.

**Ask-shaped correction accepted:** §6.2's finding that sort.t's ten
`(defvar $a …)` are ten DIFFERENT symbols is right and is now the item-2
spec; `docs/generated-cl-ir-review.md`'s "TEN times each" line is to be
corrected in the same commit that does item 2.

## The reprioritisation (USER, 2026-08-18) — recorded here for the record

Asked why four weeks read as bug-fixing: because the review→execute loop
refilled the correctness queue faster than it drained (each Fable review
probed 10–50 shapes and filed 2–4 silent-wrongs at the head of the next
Opus queue) and Fable's own sessions were the reviews, so E5 sat in a queue
nobody was at the top of.  Decision: **structural first, not at any cost**
— `plan-one-compiler-s411.md` §6 is the queue; the correctness rules that
make it safe are §6's standing rules; the harness (corpus-diff, gate,
sweep TOTAL/LOST/DROPS) is unchanged.
