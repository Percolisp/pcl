---
name: prefer-ast-level-checks-over-generated-string-pattern-matching
description: "When deciding code generation behavior based on expression type, check the AST node structure, not the generated CL string"
metadata: 
  node_type: memory
  type: feedback
  originSessionId: 455cf5ab-ec41-4e06-8da2-a5b5fc63df3d
---

When code generation logic needs to distinguish "this expression returns a list" from "this returns a scalar", check the AST node type/structure BEFORE code generation — not by pattern-matching the generated CL string afterward.

**Why:** String-matching on generated CL is fragile. The generated string changes as we add features. AST-level checks are stable and clearly express intent.

**How to apply:** Use `get_a_node()` / `is_internal_node_type()` / `node->{type}` to inspect the child node before generating. Write a helper like `_child_is_list_expr()` that checks node types. The `set_metadata`/`get_metadata` API on OpcodeTree can also store boolean flags on nodes set during parsing.

**Example:** In `gen_tree_val`, when a single-child paren expr is in list context:
- Wrong: check if generated string matches `^\(let \(\(\*wantarray\* t\)\) \(p-map\b`
- Right: call `_child_is_list_expr($kids->[0])` which checks `$node->{type} eq 'funcall'` and the function name

**Also applies to COLLECTING facts, not just checking them** (user, strong, 2026-06-24): do not *discover* data (e.g. which variables need a forward defvar) by regex-scanning the generated CL text. Record it in a built-up data structure when codegen EMITS the construct. Established pattern: `environment->caret_globals` / `expression_our_vars` / `punct_globals` — codegen calls `register_*` at the emit site, and `_insert_variable_forward_declarations` consumes the set. (`punct_globals` added for `@#`/`%#` from `$#[idx]`.) The legacy `@all_lines` regex scan inside that sub is the anti-pattern to migrate AWAY from, not to extend.
