---
name: Guard fully-passing file count against regressions
description: After every sweep, check if the fully-passing file count dropped; if so, fix the regression immediately before continuing.
type: feedback
originSessionId: 9c4f9114-22a3-4799-9738-4a58b4135d9d
---
After every full sweep (`perl sweep-perl-tests.pl --jobs 8`), compare the **"Fully passing (N)"** count to the last recorded value.

**If the count drops**, immediately identify which file(s) regressed (was fully passing before, now has failures), diagnose the cause, and fix it before moving on to new work.

**Why:** The fully-passing count is the clearest health signal for PCL. It should only ever go up (or stay flat). A drop means a previously-correct file now has new failures introduced by our changes — that's a regression and must be fixed.

**How to apply:**
- After each sweep, note the fully-passing count.
- Compare to the value stored in MEMORY.md / session log.
- If lower, run `./runt <regressed-file>` to find the new failure, then fix it.
- Do not commit or proceed with new work until the count is restored.
