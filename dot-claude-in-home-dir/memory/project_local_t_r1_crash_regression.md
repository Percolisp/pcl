---
name: project-local-t-r1-crash-regression
description: "FIXED 2026-07-04 (37fcf8f): local.t R1 crash was compiler OOM — top-level `local` giant let + inlined ops blew up constraint propagation; capped inlining in top-level local bodies"
metadata: 
  node_type: memory
  type: project
  originSessionId: 1a509f1b-73fe-40a8-88a3-63f0e922b002
---

**FIXED 2026-07-04, commit `37fcf8f`.** local.t back to **302/319 no-crash** on
the default heap (pre-R1 state). Full Pl/t gate 3758/3758 pass.

**Root cause (NOT a runtime crash — SBCL COMPILER OOM):**
"Heap exhausted, game over" in `SB-C::CONSTRAINT-PROPAGATE`. A direct top-level
`local $m = 5;` has dynamic scope to EOF, so PCL wraps the *entire program
remainder* in one `(let (($m ...)) …)` — in local.t that's 1973 lines / 67 KB,
one enormous function. R1 declaims the fast-path ops `inline`; inlining even a
few type-dispatch diamonds into a function that large makes constraint
propagation blow up **superlinearly**: measured **1.2 GB to compile that one
form** (`notinline` → 95 MB). The sweep + production `pcl` use SBCL's ~1 GB
default heap → OOM. `runt` escaped only because it passes
`--dynamic-space-size 4096`. So it was a REAL regression (a big real file with a
top-level `local` would crash too), not a harness artifact.

**Suspect #3 (FPU traps) and the speed-2 declaim were RED HERRINGS** — speed 1
gave identical 1.2 GB; debug 1 still crashed. It was the **inlining**, not the
policy level.

**Fix:** `Pl/Parser.pm` `_process_local_declaration` emits
`(declare (notinline pcl::p-+ … pcl::%pcl-nan-p))` at the head of a top-level
local's let body (helper `_notinline_ops_decl`; list MUST match the runtime's
`(declaim (inline …))`). Gated on **`in_subroutine == 0 && indent_level == 0`**
captured BEFORE the indent++ — the precise discriminator for scope-to-EOF
locals. A `local` nested in a top-level loop/if is indented (scope bounded,
possibly-hot small body) → keeps inlining; subs keep inlining. R1 wins
preserved. Also a backstop `_cap_inlining_if_huge` wraps any >20 KB single
runtime expression form in `(locally (declare (notinline …)) …)` (skips
eval-when/p-sub/defvar — would break compile-time visibility). Guard:
`Pl/t/transpile-test-02.t` (top-level local emits declare; local-in-sub does
not).

**Sweep baseline still stale** (the 54 "new" diff fails were verified identical
pre-R1 = drift, not R1). Re-bless `baselines/fail-baseline.tsv` is still TODO but no
longer blocked by this crash.
