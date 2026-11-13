---
name: feedback-claude-md-size-limit
description: CLAUDE.md is limited to 40,000 characters and MEMORY.md to 24,400 bytes; how to keep them there (USER ask, s541, 2026-11-13)
metadata:
  type: feedback
---

`CLAUDE.md` must stay under 40,000 characters and the memory index `MEMORY.md` under 24,400 bytes (lines past the limit are not loaded). In s541 the USER asked for CLAUDE.md to be checked against its maximum size and reorganized without losing what matters while working; it had reached 194,000 bytes.

**Why:** it grew by one habit -- every merge PREPENDED a narrative to the "CURRENT NUMBERS" line (58 KB in one line) and every review round added a "2b000..." paragraph under Key Files. Past the limit the file is flagged as hurting performance and a rule in the middle is not reliably applied.

**How to apply:**
- CLAUDE.md states each rule shortly. It has exactly TWO companion files, never more: `docs/claude-md/REFERENCE.md` (the full text of the working sections under the SAME HEADINGS) and `docs/claude-md/HISTORY.md` (old numbers, the old review record). The USER warned in the same session that notes split into pieces have been a problem with Fable and Opus before: subtasks must still FIND the information. So: never add a third file, never rename a section or renumber a principle (briefs and code comments cite them by name), and when a rule changes edit CLAUDE.md and REFERENCE.md together.
- At a merge, REPLACE the "Current numbers" paragraph in place and move the old paragraph to the top of `docs/claude-md/HISTORY.md` part 1. The narrative goes in `docs/session-log.md` and `docs/DECIDED.md` as before.
- A new doc = one line in the deep-dive index + its description in `REFERENCE.md` under the same heading.
- The MEMORY.md STATE line is rewritten in place and kept short; session detail goes in `briefs-and-rules-for-claude-subagents/sNNN/PAUSE-sNNN.md`.
- The guard is `Pl/t/claude-md-size-01.t` (in the gate). When it fails, move text out; never delete a rule to make room.
- Related: [[feedback-no-session-log-in-memory]].
