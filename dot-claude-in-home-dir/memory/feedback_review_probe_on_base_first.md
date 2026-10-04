---
name: feedback-review-probe-on-base-first
description: A review probe must be run on the BASE tree first; a probe the base passes has not reached the bug, so its "identical" on the fixed tree proves nothing
metadata:
  type: feedback
---

When reviewing an agent's fix, run each review probe on the BASE (main) before the agent's tree. If the base already matches perl, the probe never reached the mechanism and "identical on the tree" is meaningless.

**Why:** s491 — the first #1921 probe (a rename truncating PPI full-quote tokens) read identical to perl on main too; the rename only fires with BOTH an inner-block redeclaration of the name and a sub reading the file-level one. With the trigger added, main mangled 7 of 15 quote classes. Same session: probes written before the report found ten pre-existing bugs on main (#1914–#1919, #1990–#1992), because they were run on main first.

**How to apply:** write probes from the TASK RECORDS before reading the agent's report; run perl → base → tree; require "base shows the bug, tree == perl". Also from s491: an agent's claimed bar must be ON DISK (else re-run gate + sweep on the sha), and a batch cut off before its final bar may FAIL the gate (s491a did: stale `docs/ir-op-inventory.tsv`). Related: [[feedback_probe_the_breaking_case]], [[feedback_cause_not_count]].
