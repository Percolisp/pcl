---
name: feedback_read_not_supported_first
description: "Before triaging a perl-suite divergence, grep docs/not-supported.md — the answer is often already a blessed decision (user, PCL 2026-08-01)"
metadata: 
  node_type: memory
  type: feedback
  originSessionId: 1511008a-7c9e-41d8-9abe-ad6dadcc48fa
  modified: 2026-08-01T11:33:51.304Z
---

**Before probing a perl-suite divergence, grep `docs/not-supported.md`.**

**Why:** s316v, I triaged op/localref.t t64 by writing four probes to
establish that PCL never fires `DESTROY` (lexical scope exit, `undef`,
reassignment, sub return — perl fires in all four, PCL in none). All of it
was already written down at `not-supported.md` §846 "DESTROY called by
garbage collector", with the rationale (CL's GC gives no finalizer order or
timing). The user pushed back: *"DESTROY is not supported. That is
documented. Didn't you read about what we don't support?"* — correctly.
`docs/not-supported.md` is listed in CLAUDE.md's "Key Files to Read".

**How to apply:**
- Triage order for a suite/sweep divergence: read the failing test → grep
  `docs/not-supported.md` for the feature → *then* probe, only if it is not
  already a blessed decision.
- If it IS blessed, the work is not a bug report: it is a row in
  `baselines/perl-suite-expected.tsv` (per-file, reason must cite the
  not-supported section) or `cl/skip-registry.lisp` (per-test). Register it
  only when the file's failures are FULLY explained — op/bless.t had 6
  failures of which only 2 were DESTROY, so it stays a triage target.
- The related docs worth the same reflex: `docs/test-debugging-runbook.md`
  (the FIX-vs-REGISTER decision tree) and `docs/sweep-bug-catalog.md`.
