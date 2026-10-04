---
name: Write session state when instructed
description: User explicitly asks to write down state for next session — do it immediately, then stop
type: feedback
originSessionId: f10a5022-34bd-4b7e-8bbe-ba08f2434078
---
When the user says "end of session, run tests and write up" — do exactly that and nothing more.

**Why:** Twice now I kept debugging after being told to stop. Once the user asks to end the session, the job is: run suite, run sweep, write docs. No more fixes.

**How to apply:**
- "End of session" = stop all investigation immediately. Run tests. Write docs. Done.
- Session history (what happened, what was tried, what failed) → `docs/session-log.md` ONLY
- MEMORY.md = durable rules and current state pointer. NOT a session log. Never paste old session statuses in there — they bloat it past the 200-line truncation limit, hiding the feedback rules that matter most.
- When the task changes mid-session (new findings, user redirects), update the relevant memory file immediately — don't wait until "end of session"
- Write the specific next action, not just the background analysis
