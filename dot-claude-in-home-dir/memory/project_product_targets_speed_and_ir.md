---
name: project_product_targets_speed_and_ir
description: "The two user-set product targets (2026-07-20) — general speed must BEAT perl (slack only for regex engine + pack oracle), and the generated IR must be clear with obvious macros for all CL-specific machinery"
metadata: 
  node_type: memory
  type: project
  originSessionId: ec671759-b221-4b2b-a23e-bc2dd08f5df4
  modified: 2026-07-20T06:22:58.245Z
---

**Set by the user 2026-07-20 (s301). Written up as `docs/v2-endgame-plan.md` §6 ("P-phase") — that section is the authority; this memory is the pointer.**

**Target A — speed:** general program speed must beat Perl itself. A couple of areas may stay much slower (granted slack: the regex *engine* (cl-ppcre, unless [[project_parser2_prototype]] #71 PCRE2 lands) and the pack/unpack *oracle* rows). Slack ≠ don't-fix. Scoreboard = `perl tools/bench-exec.pl` (geomean of non-slack rows < 1.0×). Measured worklist = `docs/faster-codegen-suggestions.md` (per-item variant timings, §11 before/after shapes, §12 priority; also lists measured DEAD ENDS — native `+`, hash-value unboxing, const-key hashing — do not spend sessions there). Tier 1 filed: task #62 (S1 string-append buffer ~2400× + N1 raw-numeric verdict ~13× + N2 rider), #73 (M1 method inline cache ~15× — biggest OO/CPAN lever), #74 (P1 pack/sprintf template memoize). All independent of E2–E5; can interleave.

**Target B — IR clarity:** the generated IR must be clear; every CL-specific mechanism behind an obviously-named macro from the closed `ir-spec.md` vocabulary (p-scalar-ctx not bare `*wantarray*` lets, p-new-av/p-vlist not raw make-array, p-esc, structured regex literals, no raw seam text). Worklist = `generated-cl-ir-review.md` §4 items 2–6 + the E2.final/E5 seam retirement; filed as task #75 (flag-day post-E5, one pipeline = cheap).

**Why:** no conflict with speed-wins (CLAUDE.md §2) — macros are free at runtime; they expand to the fast shape. **Rule: every new fast shape from Target A ships pre-wrapped in its named macro** (p-append!, p-call-cached, …) so the targets converge.

**Release roadmap (user, 2026-07-20, same session — `v2-endgame-plan.md` §7 is the authority):** **R1 (correctness release)** = rewrite finished (E2–E5, v1 deleted) + re-run remaining internal perl t/ dirs (t/mro, t/class never surveyed) + CPAN module suites vs baselines — that verification = task #25 (now the R1 gate); speed is NOT an R1 gate. **R2 (speed release)** = Target A acceptance; may ship with one or two documented perf TODOs — named candidate: string concat/append (S1), recorded in `docs/todo-features.md` §Perf. **N1 (numeric loops) is NOT slippable for R2.** Post-R2: slipped TODOs, Tier-2/3 perf, #71 PCRE2, XS bridge.
