---
name: feedback_no_redundant_bg_wait
description: "Don't poll-wait on a background task you started — the harness notifies you on completion"
metadata: 
  node_type: memory
  type: feedback
  originSessionId: f539b425-9a61-434d-80b0-ac173933ac13
  modified: 2026-08-15T21:21:47.650Z
---

When I launch a long command with `run_in_background: true` (e.g. the full `prove -j8`
gate or the perl-tests sweep), the harness **re-invokes me automatically when it
completes**. I do NOT need to (and must not) follow it with a separate polling
`until grep ...; do sleep 10; done` wait loop — that is redundant, and the harness
blocks foreground `sleep`/sleep-chains, so it can hang the turn. The user flagged this
twice in session 238 ("Your last wait command hanged.").

**Why:** background tasks are harness-tracked → completion notification is guaranteed.
Polling burns a tool call, risks the sleep-block, and provides nothing the notification
doesn't.

**How to apply:** start the background task, then end the turn (or do unrelated work).
Read the task's output file only AFTER the completion notification arrives. Use a wait
loop ONLY for external state the harness can't see (a remote CI run, a deploy) — never
for a Bash background job I started here. See [[feedback_debugging_hangs]].

**s404, what it actually costs (measured, not theoretical):** I used
`until ! pgrep -f 'tools/prove-core' ...` a dozen times in one session. `pgrep -f`
matches the WAITER's own command line, so every one of those loops was unkillable-by-
condition and ran until I killed it by hand — and while they piled up, a second
`tools/prove-core` was started concurrently, overwriting the first gate's output file.
Two gate runs, ~20 wasted minutes, and for a while I could not tell which run's numbers
I was reading. **If a bounded wait is genuinely needed, poll the OUTPUT FILE for a
terminal marker** (`for i in $(seq 1 40); do grep -q '^Result:' out.txt && break; sleep
15; done`) — never `pgrep -f` on a pattern the waiter itself contains.

**s463f addendum (2026-09-01): `pkill -f PATTERN` / `for p in $(pgrep -f
PATTERN)` KILLS THE CALLING SHELL (exit 144) whenever the same Bash command
ALSO contains the literal text the pattern matches — a heredoc, a `setsid
bash -c "... tools/prove-core ..."` string, an echo.  The `patter[n]`
bracket trick only protects against the pattern's OWN text, not against a
second literal mention.  Rule: put a long chain in a SCRIPT FILE, launch it
with `setsid /path/legs.sh &` from a command that names only the script,
and do process cleanup in a SEPARATE command that contains no other mention
of the process name.**

**s480 (2026-09-09):** the s473w agent left ELEVEN `until ! pgrep -f X; do sleep N; done` loops running 2.5 h after it finished — each matched its own command line.  End-of-session check: `pgrep -x sleep` → kill the parents; the rule is already in `~/pcl-agent-scratch/s473/COMMON.md` and agents still write it, so the check is Fable's.
