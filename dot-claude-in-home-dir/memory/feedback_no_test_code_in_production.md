---
name: feedback_no_test_code_in_production
description: "Keep test/benchmark harness code out of production files — one-off drivers go in the scratchpad, reusable ones in tools/"
metadata: 
  node_type: memory
  type: feedback
  originSessionId: 7cc61f2e-06d2-4e82-aef4-c7a8bd586021
---

Test or comparison harness code must never be added to production files
(`cl/pack-impl.pl`, `Pl/*.pm`, `cl/pcl-runtime.lisp`, `lib/`). The user
checked in s289 ("you are not putting test code into production code
again, right?") when I built the pack-oracle harness — implying this had
happened before.

**Why:** Production files are transpiled/loaded by every user of the
pipeline; embedded harness code changes emission, load time, and the
artifact-review surface (the s288b pcl-pack.lisp review already flagged
artifact hygiene).

**How to apply:** One-off drivers/probes → session scratchpad. Reusable
verification tools → `tools/` (like [[project_parser2_prototype]]'s
`tools/corpus-diff.pl`). Overriding builtins for oracle comparisons →
`CORE::GLOBAL::*` in the *driver*, never edits to the oracle itself.
