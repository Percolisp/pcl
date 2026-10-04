---
name: feedback_no_stash_when_stash_exists
description: "Don't use git stash push/pop for HEAD-vs-change comparisons when a stash already exists"
metadata: 
  node_type: memory
  type: feedback
  originSessionId: e90f4b5d-790f-4422-9adc-e1cdc9a106c5
---

For "compare behavior at HEAD vs my uncommitted changes" experiments, do NOT use
`git stash push -- <files>` / `git stash pop`. In s258 this was repeated many
times and one conflicted pop leaked a PRE-EXISTING `pack-P WIP` stash into the
working tree (`UU cl/pcl-pack.lisp` + staged pack files that weren't touched at
session start). Restored by `git checkout HEAD -- <files>`; the WIP stayed safe
in `stash@{0}`.

**Why:** stacked stashes + repeated pops are easy to mis-balance, and a
conflicted pop neither drops the stash nor cleanly restores — it silently
intermixes unrelated stashed content with the working tree.

**How to apply:** when a stash already exists (`git stash list` non-empty), use a
**git worktree** (`EnterWorktree` / `git worktree add`) or copy the few files to
`/tmp` and diff against them. Reserve stash for when the stash list is empty.
Always check `git status` after the experiment to confirm only intended files
changed. Related: [[feedback_commit_per_session]].
