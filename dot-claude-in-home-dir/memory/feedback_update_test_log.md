---
name: Keep test-failures-categorized.md current
description: Rule to update docs/test-failures-categorized.md and MEMORY.md whenever the status of a perl-tests/ file is discovered to have changed
type: feedback
originSessionId: e7e95a20-4011-4421-b1fb-9ae38cb22fa8
---
Whenever a test file's status is checked (via `./runt`, sweep, or manual inspection) and it differs from what `docs/test-failures-categorized.md` or MEMORY.md says, update those docs immediately — in the same session, before moving on.

**Why:** Stale entries waste time re-investigating files that are already fixed or already known to be blocked. In session 146 we spent investigation time on defins.t (already fully passing since session 130) and bless.t (no longer crashing) because the categorization doc was out of date.

**How to apply:**
- After `./runt <file>` or a sweep run, compare the output against the entry in `docs/test-failures-categorized.md`.
- If pass count, crash status, or root cause has changed: edit the table row in place.
- Add a note in `docs/session-log.md` under the current session listing what changed and why.
- Do NOT write per-file status details into MEMORY.md — it gets too large. MEMORY.md is an index only; details go in `docs/`.
