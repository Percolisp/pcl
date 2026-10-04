---
name: feedback-no-unless-in-perl
description: "User prefers `if (! ...)` over `unless` in Perl code Claude writes"
metadata: 
  node_type: memory
  type: feedback
  originSessionId: a1578bfb-39cf-4dd3-9220-142eb0e4c044
  modified: 2026-07-20T16:00:05.795Z
---

Avoid `unless` in Perl code; write `if (! ...)` instead (user, 2026-07-20).

**Why:** Stylistic preference of the user.

**How to apply:** In any new or edited Perl code (Pl/*.pm, tools/, one-liners),
use `if (!COND)` / `if (! ...)` rather than `unless (COND)` or statement-modifier
`... unless COND`. Do not mass-rewrite existing `unless` usages in untouched
code — apply the preference to lines Claude writes or already touches.
