---
name: feedback_cl_indentation_depth
description: CL code must use indentation that makes paren depth visually obvious — 2 spaces per level
metadata: 
  node_type: memory
  type: feedback
  originSessionId: aafba459-cc38-48d6-8388-b89615aaa56f
---

Use exactly 2 spaces per paren level when writing CL code. A line's column ÷ 2 = its paren depth. If the indentation looks wrong, the parens are wrong.

**Why:** Two sessions were spent counting parens in pcl-pack.lisp because deep nesting (~d22) was written without indentation discipline. The buggy simple paren checker (which doesn't handle `#\(`) then gave false "depth 0" and masked structural bugs. Visual indentation would have made the misplaced closings obvious immediately.

**How to apply:** When writing any `.lisp` block — especially nested `let*`, `labels`, `flet`, `loop` bodies — indent every level by exactly 2 spaces. A closing `)` should always sit at the same column as the `(` that opened it (or on the same line as its last argument). Never write a `)` that is indented deeper than the opening form.
