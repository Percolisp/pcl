---
name: feedback_dead_cell_not_neutral
description: "In PCL, emitting an \"unused\" global declaration is not semantically neutral — several sites read \"this name has a cell\" as \"this name is a global\""
metadata: 
  node_type: memory
  type: feedback
  originSessionId: 21d44c05-772f-4643-9732-37032ce74bef
  modified: 2026-08-11T22:39:46.938Z
---

Since the direction-D flip (#290) an ordinary package global and a lexical are
spelled the SAME CL symbol, so various sites answer "is this a global?" by
asking whether the symbol has a `p-defcell` (a global symbol macro). Adding a
declaration for a name that is only ever lexical therefore CHANGES BEHAVIOUR.

**Why:** measured twice in s384. `%p-cell-loop-var-p` made `foreach my $x`
localize a cell instead of binding a lexical (#294, fixed by emitting `:my t`).
And the #291 enabler, whose only effect on closure.t was 47 added `p-defcell`
lines, cost 5 rows of that file (#299, cause still open).

**How to apply:** before widening what gets a declaration, grep for every site —
compiler and runtime — that treats "has a cell" as a proxy for "is a global",
and make the compiler STATE the fact instead of letting the runtime guess. See
also [[project_v2_session_state]] and `docs/DECIDED.md` s384b.
