---
name: feedback_no_session_log_in_memory
description: Never accumulate per-session log entries in MEMORY.md — it grows unbounded; keep only current state + pointers
metadata: 
  node_type: memory
  type: feedback
  originSessionId: 933125d5-e552-4dc7-b751-e3cfa1e44b67
---

Do not keep a session log in MEMORY.md (user, 2026-07-11).

**Why:** per-session entries (s278b/s280/s281 style) accumulate every session
and MEMORY.md is loaded into context whole — it becomes too big.

**How to apply:** MEMORY.md holds only *current state* (one STATE line with
the latest census/gate/gen numbers and NEXT items) plus pointers.  Session
narratives go to `docs/session-log.md` (the repo log); v2 per-session ledger
detail goes to [[project_parser2_prototype]] (or the relevant topic memory
file).  At the end of a session: update the STATE line in place (replace, not
append), append the ledger entry to the topic file, write the full entry in
`docs/session-log.md`.
