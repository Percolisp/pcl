---
name: project-direction-d-globals
description: Direction D global-variable representation facts (symbol-macro cells, defglobal dead, measured costs) — do not re-derive
metadata:
  type: project
---

**Direction D (don't re-derive; plan + #290)**: symbol-macro over a cell, `defglobal` is DEAD; reads −20%, my-shadows −36%; `local` of an ordinary global ~41ns vs ~4.6ns (hot locals are magic vars, still defvar).  Partition = `Pl::GlobalPartition`, ONE function both emitters ask.  **`p-defcell` MUST be define-once** (guard `Pl/t/global-cell-01.t`).  **A `let` of a symbol-macro name is LEGAL CL and SHADOWS it** — that IS the my-shadow mechanism.

Related: [[project-parser2-prototype]]
