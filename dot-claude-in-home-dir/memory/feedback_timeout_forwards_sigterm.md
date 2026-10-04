---
name: feedback-timeout-forwards-sigterm
description: "Killing a `timeout N cmd` wrapper KILLS the command — timeout(1) forwards SIGTERM to its child; never 'un-cap' a long run that way"
metadata: 
  node_type: memory
  type: feedback
  originSessionId: 66d9b237-b64d-46e1-981b-5f3faca5611a
  modified: 2026-08-01T20:34:47.177Z
---

**s321, self-inflicted, cost ~80 minutes of a release-gate run.** A per-dir
`timeout 5400 perl tools/run-perl-suite.pl --dir op …` was 7 minutes from
expiring on a run that needed longer. I tried to "remove the cap" by killing
the `timeout` process itself, expecting the child to be orphaned and continue.

**It does not work: `timeout(1)` installs a signal handler and FORWARDS the
signal to the command it manages.** Killing the wrapper is exactly equivalent
to letting the timeout fire — the run dies immediately.

**What to do instead**, in order of preference:
1. **Size the cap correctly up front.** For a full `--dir op` suite run at
   `--jobs 3-4`, budget hours, not 90 minutes (58 of 218 files took 83 min).
2. If a cap is already ticking and the work must survive: there is no safe
   in-flight fix. Let it expire, then re-run the affected chunk. The
   run-perl-suite runner is crash-honest (task #157), so a killed run writes
   every row as KILLED/NOT-RUN with a nonzero exit — you lose the time, not
   the evidence.
3. Prefer relying on the tool's OWN per-file timeout (`--timeout N`) plus its
   straggler killer, and use an external `timeout` only as a very generous
   backstop — or omit it.

**Related, same session:** the runner CLEARS its `--faillog` dir per
invocation, so per-dir chunks must each pass their own `--faillog DIR` or only
the last dir's per-test triage survives.
