# briefs-and-rules-for-claude-subagents

Much of PCL is written by background agents: a reviewing Claude session
designs a batch of work, writes a *brief* for it, and launches an agent that
carries the batch out in its own git worktree and reports back; the reviewing
session then probes the result against perl, runs the gate and the sweep
itself, and merges.  This directory holds the documents those agents are
handed — the standing rules, the per-session traffic plan, and the briefs.

On the development machine these files are reached through
`~/pcl-agent-scratch/`, where each one is a **symbolic link** into this
directory (the layout below mirrors that directory, so
`~/pcl-agent-scratch/s473/COMMON.md` is `s473/COMMON.md` here).  The rest of
`~/pcl-agent-scratch/` is measurement output — logs, probe results, extracted
trees, gigabytes of it — and stays outside the repository.

| File | What it is |
|---|---|
| `s473/COMMON.md` | **The rulebook.**  The rules every agent reads before its own brief: work only in your own worktree, keep a state file so you can be resumed, read `CLAUDE.md` first, never push or merge, how to file a task, every fix needs a test row that fails on the old code, baselines are edited row by row, where your records go, report the everyday number before and after. |
| `sNNN/SHARED-BOX.md` | **The protocol of one session.**  Which batches are running, the commit they start from and its numbers, the merge order, and how they share one machine: one heavy run (gate, sweep, companion suite) at a time, how to wait for a turn, how a benchmark reserves a quiet box, what a batch re-runs after rebasing across another's merge.  The newest one is the template for the next session. |
| `sNNN/<label>-prompt.md` | **A brief**: one batch of work — the tasks, the design as ruled, the order of the members, the bars it must pass, the form of the final report. |
| `sNNN/resume-<label>.md` | **A resume brief**: how a batch that was stopped part-way is taken up again, with the reviewer's findings so far. |
| `sNNN/PAUSE-sNNN.md` | **The reviewing session's running notes**, written as the session goes so that a session cut off in the middle can be taken up from them.  What matters from them is condensed into [`docs/session-log.md`](../docs/session-log.md) and [`docs/DECIDED.md`](../docs/DECIDED.md) at the end of the session; those two are the record. |
| `s473/s473t6/plan.md` | **A plan** that is designed and not yet launched. |

This is a working set, not an archive: the rulebook, the current protocol, the
briefs of batches in flight or just merged (they are also the examples of how
a brief is written), and plans waiting their turn.  The history of earlier
sessions is in `docs/session-log.md`.

## Rules for whoever writes here

- **This directory is published with the repository** — technical content and
  work decisions only, as for [`dot-claude-in-home-dir/`](../dot-claude-in-home-dir/README.md).
- Edit the file **here**.  The `~/pcl-agent-scratch/...` path is a link for
  readers; a tool that replaces a file instead of writing into it (`perl -pi`,
  `sed -i`) turns the link back into a separate file without saying so.
- A new session's protocol and briefs are written here, under `sNNN/`, and
  linked from `~/pcl-agent-scratch/sNNN/` when a brief cites that path.
- An agent reads these through the link or through the main checkout's path,
  never from the copy inside its own worktree (that copy is as old as the
  commit the worktree started from).

## On another machine

Nothing needs linking to read or use these files: point an agent at the path
in your checkout.  Briefs written before the move cite
`~/pcl-agent-scratch/...`; read that prefix as this directory.
