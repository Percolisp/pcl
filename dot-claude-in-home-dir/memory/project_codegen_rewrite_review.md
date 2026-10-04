---
name: project-codegen-rewrite-review
description: 2026-07-02 go/no-go review of the compiler rewrite plan — verdict PROCEED with R1/R2/R3 additions; docs/codegen-rewrite-review.md is the decision doc
metadata: 
  node_type: memory
  type: project
  originSessionId: 657169b8-1488-43d3-ad7f-5e5daa0e21a9
---

**`docs/codegen-rewrite-review.md` (2026-07-02) is the decision basis for the
compiler rewrite** (user: "generated code must beat native Perl; compiler must
be simple and easy to extend; basis of rewrite-or-give-up").

Verdict: **proceed**, but the plan-as-written misses the bar by its own numbers
(projects ~2–3× slower than Perl blended). Three additions close the gap:
- **R1** inline fast-path operators (`(declaim (inline p-+))` + numberp guard;
  today every `p-+` allocs a closure via `%pcl-ieee-arith` AND runs
  `with-float-traps-masked` per op — verified, not in any doc). Phase-1-grade.
- **R2** calling convention both halves: callee lambda lists (spec #3) + caller
  `*wantarray*` elision via per-sub context-insensitivity bit (recommend
  dynamic-var-kept design (b), not hidden-ctx-arg).
- **R3** structured s-expression emission (forms not text; printer at end) —
  scheduled explicitly (spec never scheduled the IR the goto doc demands).

Correctness holes found (review §4, must amend docs before Phase 2): eager
to-string unsound for opaque/overloaded sources; string-eval lexical capture
must disqualify unboxing of visible lexicals; foreach var aliasing; `m//g`/pos
box magic missing from Gate 1; two-phase Step 6 `_infer_type` unsound —
superseded; ast-annotation D3 `do{}` capture claim wrong.

**Flag measurements (§3.2b) + speed menu**: no-overload flag = only ~4% at
runtime (probes already fast-bail); the per-op `%pcl-ieee-arith` closure +
`with-float-traps-masked` = **7.4×** (!); unboxing a further 13× (6.5× faster
than perl). **VERIFIED: `(sb-int:set-floating-point-modes :traps nil)` once at
startup = Perl's model (Inf/NaN, no per-op cost) → delete %pcl-ieee-arith
entirely = week-one fix.** Sealed-subs: unneeded for plain calls (symbol-cell
dispatch is redefinition-safe); unlocks inlining/A4/devirtualization; method
inline caches (PIC) get most of it without sealing. Both flags inferable
automatically (PCL sees all source; string eval = spoiler).
`docs/where-the-time-goes.md` = layman deep-dive + extended menu (FPU modes,
dynamic-extent @_, sort-comparator recognition, PICs, map/grep fusion, PGO,
aggregate element repr); user decided SPEED > readable Lisp
([[feedback-speed-over-readable-lisp]], CLAUDE.md §2 amended).

**Boxing vs sub calls (measured, `tools/box-survey.pl`, 9093ba8)**: sub-call
sources do NOT force boxing (box = the variable's cell; args are copies since
@_-aliasing unsupported). Real-CPAN survey (~1400 my-scalars, 10 modules):
~11% Gate-1 disqualified locally (true 15–25% w/ closure/foreach cases), ~11%
call-sourced. **Sealed-subs adds ~zero unboxing** — only Gate-2 guard
elimination (≤1.6× on arithmetic, few % blended); sealing pays in OO
dispatch/inlining instead. GOTCHA: §4.2 eval-string disqualifier must be
per-scope not per-file (2/10 modules have string eval → would lose ALL
unboxing under blanket rule). Saved in `where-the-time-goes.md` §6.

Regex is the one honest "can't promise faster" (cl-ppcre ~3.7×; PCRE2 bridge =
separate decision after Phase-1 measurement of plumbing-vs-engine split).

Checkpoints: after Phase 1+R1 expect ≥3× on intmath/fib or re-derive cost
model; after Phase 4, arithmetic+call code faster than Perl, strings/hashes
parity, regex ≤1.5×. Re-verified 2026-07-02: fib 5×/intmath 7.5× slower;
Tier-3 #9–#12 all still open; VOID_CTX body-wrap regression visible in output.
Related: [[project_wantarray_followup]], [[project_intra_sub_goto]].
