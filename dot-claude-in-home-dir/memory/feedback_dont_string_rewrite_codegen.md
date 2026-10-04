---
name: feedback_dont_string_rewrite_codegen
description: "Don't disambiguate by pattern-matching GENERATED codegen strings — inspect the codegen for the SIBLING interpretation first; verify-before-commit"
metadata: 
  node_type: memory
  type: feedback
  originSessionId: 61f1f17b-6e7e-4006-a2c0-97c32e5d76c5
---

When a construct misbehaves, do NOT fix it by regex-rewriting the **generated
CL string** in `ExprToCL` to "mean" one interpretation. Two source constructs
often compile to the SAME shape, so a string rewrite that helps one silently
breaks its sibling.

**Why:** Session 245, `@{EXPR}[slice]`. I rewrote `(p-cast-@ (p-aref-box E I))`
→ `(p-aslice E I)` to make symbolic-ref slices work. But `@{$a[0]}` / `@{$h{a}}`
(array **deref of a container element**) compile to the *identical* shape, so the
rewrite turned them into slices → `scalar @{$a[0]}` returned 1 not 3. A
**committed** regression (`2bc25da`), caught only later by the fuzzer. The user:
"too basic an error for this late stage … we need to look at crashes directly."

**How to apply:**
1. Before committing a codegen change, **generate the codegen (`./clt` / `./pl2cl`)
   for BOTH the target case AND its structural siblings** — the other source
   construct that could produce the same tree. `@{$scalar}[..]` (slice),
   `@{$h{k}}` (deref), `@{"name"}[..]` (symbolic slice) all look alike post-parse.
   Eyeball the generated forms; run both vs real `perl`.
2. Prefer fixing at the **AST/parser layer** (where the bracket nesting that
   distinguishes "subscript OF a deref" from "deref of a subscript" is still
   visible) over post-hoc string-munging the generated output. String-sniffing
   the base (`_is_symbolic_name`) is a smell — it needs a new special-case per
   new shape.
3. **Look at the crash directly**: read the generated `.lisp` and the SBCL
   backtrace for the exact failing form, and the codegen of the nearby
   *non-crashing* forms, before reaching for a rewrite.

See [[project_symbolic_ref_slice_decision]], [[feedback_ast_vs_string_matching]]
(same principle on the input side: inspect AST nodes, don't match strings).
