---
name: feedback_reuse_dont_duplicate
description: "When fixing a bug, find the existing mechanism for sibling cases and route through it; don't copy a special-case branch."
metadata: 
  node_type: memory
  type: feedback
  originSessionId: fbb309c9-0152-4b66-a4f9-16528986a343
---

When fixing a bug, do NOT add a special-case branch that duplicates logic
already handled for a sibling case. Find the existing mechanism first and route
the new input through it.

**Why:** Perl features come in families (named-unary `$_`-default
`uc`/`lc`/`length`; list-op filehandle `print`/`say`; block-arg prototype
`grep`/`map`/`first`). A per-case copy misses the other parse paths the same
input flows through and drifts over time. The user explicitly asked for a rule
to prevent this (2026-06-23).

**How to apply:**
- Grep for the data table/helper/marker that drives the working sibling (e.g.
  the `[1,-2]` "defaults to `$_`" spec, `_is_print_term_start`,
  `add_implicit_default_param`); read a sibling end-to-end.
- Prefer a single pre-pass that *normalises* the odd input into the shape the
  generic machinery already consumes, over branching beside it.
- Count the parse paths the input can arrive via (single-element dispatch,
  operator loop, funcall args, block body) — a per-path fix is a smell; push the
  fix to the one upstream point they all pass through.
- Hard stop: if the diff adds the same logic twice or copies a branch with one
  token changed, find the shared upstream point.

Worked example (2026-06-23): bare filetest `-e` is tokenised as an Operator (not
a Word), so it never hit the `$_`-default machinery. Fix = insert a `$_` token
after a bare filetest in one `_default_filetest_operand` pass, so both the
single-element and operator-precedence paths handle it with zero new special
cases. Codified as CLAUDE.md Design Principle 11.

Related: [[feedback_fix_at_right_layer]] (the layer to put a fix); this is about
not duplicating once you're at the right layer.
