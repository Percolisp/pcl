---
name: feedback-check-queue-on-repeated-waiting-notices
description: "When a batch agent sends repeated \"waiting on its own background work\" notices, look at the heavy-leg queue and the leg's log before passing the notice on -- a starved or dead leg looks exactly like a slow one"
metadata:
  node_type: memory
  type: feedback
  originSessionId: 13519802-16f6-43ca-9ead-366645d032cf
  modified: 2026-11-22T15:22:14.248Z
---

When a subagent's interim notice says it is "waiting on its own background work" for the SECOND time in a row on the same leg, run one command before reporting anything: the leg's `waiting since` line in `~/pcl-agent-scratch/s513/HEAVY.queue`, `HEAVY.holder`, and the last line + mtime of the leg's newest log.  Tell the USER what the leg is actually doing and how long it has waited.

**Why:** s543, 2026-11-22: s543h's exclusive bench leg was starved in the queue for 36 minutes (see [[project-heavy-slots-memory-pressure]] and task #5191: `heavy.sh` announces an exclusive leg only once it holds slot 1), then died at once of a non-executable script (rc=126), then waited again.  The reviewing session passed six "interim, nothing to act on" notices to the USER without looking.  The USER concluded it was hanging, said something was broken, and ended the session.  It was not a hang, but the USER was right that something was broken, and found it before the reviewing session did.

**How to apply:** an interim notice is a prompt to CHECK, not a status to relay.  A leg waiting more than ~15 minutes for a slot, a log with an `END rc=` other than 0, or a log that has not grown in five minutes is a finding: say it, and act (message the agent, or fix the queue).  With three batches on the box, exclusive legs (bench, everyday, `--record`) are the ones that starve; near the end of several batches at once, consider telling them to take their exclusive legs in a stated order.  (My reading of the event; the USER's words were that the task had waited on the same process for over 1.5 hours and was obviously hanging.)
