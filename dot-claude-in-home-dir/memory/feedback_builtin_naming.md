---
name: p- prefix applies only to runtime-defined names
description: Clarification on Option A builtin naming fix scope
type: feedback
---

Only functions/macros defined in `cl/pcl-runtime.lisp` get the `p-` prefix rename.
User-defined Perl subs in generated code keep the `pl-` prefix.

**Why:** The collision is between pcl runtime built-ins and user methods. Renaming only the runtime side (`pl-push` → `p-push`) is sufficient — user methods keep `pl-PUSH`, so `P-PUSH ≠ PL-PUSH`.

**How to apply:** In `cl_name()` in ExprToCL.pm, use a static `%RUNTIME_NAMES` set (all names exported from pcl-runtime.lisp). If the name is in that set → `"p-$name"`. Otherwise → `"pl-$name"` (user function). The `OP_EXCEPTIONS` values that reference runtime functions also change from `pl-` to `p-`.
