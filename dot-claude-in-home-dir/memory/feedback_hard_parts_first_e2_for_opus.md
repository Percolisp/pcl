---
name: feedback-hard-parts-first-e2-for-opus
description: Division of labor — Fable owns the PCL v2 compiler rewrite (hard parts first); Opus runs pclxs from its repo docs
metadata: 
  node_type: memory
  type: feedback
  originSessionId: 47b2d0e1-8fed-425b-befb-8741039299d4
  modified: 2026-07-25T06:08:23.263Z
---

**Do the hard parts first. E2 emitter→CLForm conversions are now mechanical
(scaffold + recipe proven in s293/s293b) — leave them for Opus 4.8.**
(User, 2026-07-17, superseding the plan's "alternate E1/E2 sessions" cadence
for Fable sessions.)

**Why:** The capable model makes the most difference on the semantically hard
gates (E1 M-F eval family, closure per-iteration binding, dynamic goto #63,
state-in-closure), not on byte-parity conversions that follow a fixed recipe
(exec-plan §E2.0).

**How to apply:** In a Fable session, pick from the hard E1 remainder (or
other hard open items: #63 dynamic goto, #64, method.t stop@157) — not from
`%FUNCALL_FORM_DECLINES` or the emitter frontier. E2 steps are Opus 4.8
session material. Related: [[project_parser2_prototype]].

**Update 2026-07-25 (user):** the split is now per-project. **Fable stays on
the PCL v2 compiler rewrite** (the E-plan, this repo); **Opus 5 executes
pclxs** in `~/pclxs`, working from that repo's own CLAUDE.md +
`docs/plan.md` (Fable's s6 review added a "Next, in order" header, the
pre-push gate, and R18/R19 there — the plan is written to be
self-sufficient for a fresh Opus 5 session). Fable's pclxs role is
planning/review passes from outside, not implementation. Pattern confirmed
working by the user: Fable plans/reviews, Opus 5 does.

**Why (user, 2026-07-25):** getting the v2 PCL compiler *correct* is the
critical path — it must get good enough that people want to use it.
pclxs is experimental; roadblocks and rewrites are expected there, and its
transferable assets (conformance suite, census, R-rules) survive a rewrite
anyway. So Fable's depth goes where a subtle miscompile costs adoption,
not where a rewrite is cheap.
Related: [[project_xs_shim_design]].
