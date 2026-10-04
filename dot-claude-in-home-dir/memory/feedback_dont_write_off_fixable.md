---
name: feedback_dont_write_off_fixable
description: "Don't mark things not-supported before checking they're actually unfixable — test the primitive first"
metadata: 
  node_type: memory
  type: feedback
  originSessionId: 25716126-f270-4bcd-b656-dcd66ed21e5a
---

Don't be too fast to write off a feature as "not supported" when it may be
fixable. The user pushed back twice on a hasty `fork` not-supported call.

**Why:** PCL's whole point is CPAN compatibility (CLAUDE.md principle 4: "No Easy
Write-Offs"). A wrong not-supported claim permanently buries a fixable feature and
its dependent tests. The bar for "not supported" is *demonstrated* infeasibility,
not a plausible-sounding rationale.

**How to apply:** Before documenting anything as not-supported, spend the 2 minutes
to actually TEST the underlying primitive. Worked example: I claimed `fork` was
impossible ("SBCL multithread image can't fork safely") — but a 6-line
`(sb-posix:fork)` probe showed the child runs Lisp, prints, and exits with a
reapable status. fork/exec/wait/waitpid/getppid/kill were all then implementable
(c9f53fb). The real caveats turned out narrow (no fork after CL threads; a
fork-then-continue child is an SBCL process that catches signals) — document the
*actual* limits, not a guessed blanket one. When you must write a not-supported
entry, state the concrete thing you tried that failed. See [[feedback_fix_at_right_layer]].

**Recurred s278c (2026-07-07):** I called the v2 W10 `eval_unsafe` gate a
"correctness wall — impossible to support" for bop.t/sprintf2.t. WRONG — the user
pushed back ("why is it impossible?") and on actually tracing it, the blocker was
just the rename's *unconditional name-mangling* (`$x`→`$x__file__N`), which a
dynamic `eval $var` referencing the bare name can't see. Fix: when the spanning
name has NO other lexical binding in the file, rename to the plain `$Pkg::name`
global (no mangle) — eval'd code in package Pkg then resolves `$name` to the same
cell, so the guard is unnecessary. **Do NOT assume a gate is fundamental just
because the current implementation trips on it — trace WHY the specific mechanism
fails before labeling it impossible.**
