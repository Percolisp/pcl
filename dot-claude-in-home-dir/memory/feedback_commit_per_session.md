---
name: feedback-commit-per-session
description: "Commit each session's finished work before moving on — don't let uncommitted work pile up and entangle across sessions"
metadata: 
  node_type: memory
  type: feedback
  originSessionId: d55506d2-cff6-4073-a8c1-ff09659b85bf
---

Commit each session's finished, tested work at the end of that session, in
focused per-feature commits — do not let uncommitted work accumulate across
multiple sessions.

**Why:** In session 222 the working tree had three sessions of uncommitted work
(220 pipe/alarm, 221 the `pcl` command, 222 sprintf). `cl/pcl-runtime.lisp` held
both session 220's pipe/alarm and session 222's sprintf changes, so they could
not be separated into clean commits (interactive `git add -p`/`-i` are blocked in
this harness — a single dirty file can only go into one commit as a whole). The
result was a bundled, less-accurate commit. The user called this out as sloppy.

**How to apply:**
- When a session's change is finished and the gate is green, commit it before
  starting unrelated work — especially before touching `cl/pcl-runtime.lisp`
  again, since everything funnels through that file.
- One commit per logical feature/fix; don't mix unrelated features in one commit.
- If the tree is already entangled (multiple features dirtying one file), say so
  and commit in the cleanest grouping possible rather than pretending it's clean.
- Leave obvious scratch/WIP files (e.g. `README-temp.md`, `*-draft.org`)
  untracked; don't sweep them into a feature commit.
- This repo is solo-dev and commits directly to `main` (every prior commit is on
  main) — follow that convention; don't branch unless asked. See [[feedback-write-session-state]].
