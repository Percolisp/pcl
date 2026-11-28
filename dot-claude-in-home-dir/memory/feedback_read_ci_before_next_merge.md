---
name: feedback_read_ci_before_next_merge
description: After a merge push, read that push's CI run before the next merge -- s543 (2026-11-27/28) stacked four merges on a red CI because the local gate was green
metadata:
  type: feedback
---

Read the `CI` run of a merge push (`gh run list --limit 3`) BEFORE the next merge is fast-forwarded -- a green local
gate is not CI green.

**Why:** in s543 (2026-11-27 .. 28) Fable merged s543n (MAIN-READY-111), then s543o, s543m, s543l and two census
record merges, each with a PASSING full gate on merged main, and looked at CI only five pushes later: it had been red
since -111.  The cause was one test row that depends on the ENVIRONMENT (`Pl/t/system-block-01.t` row 51: a GitHub
runner starts jobs with SIGPIPE ignored, an ignored disposition survives `exec`, so `yes` into a closed pipe exits 1
there -- in perl too; PCL = perl in both environments).  Nothing in the local gate can show that.  The observation is
Fable's own; the USER had not yet commented when this was written.

**How to apply:** at every merge: push, then check the previous push's CI conclusion before starting the next merge's
fast-forward (a CI run takes 11-19 minutes; the next merge's review probes fit in that time).  A red run is diagnosed
before anything else is merged (`gh run view <id> --log-failed`).  To reproduce a runner-only failure locally, think
of what the runner's environment differs in: SIGPIPE ignored (`trap '' PIPE` in the parent shell), no TTY, a slower
box (timed rows are relative to perl: CLAUDE.md rule 6), a stock perl without extra modules ([[project_ci_stock_machine]]).
A `Pl/t` row that asserts a child's death by a signal sets that signal's disposition itself.
