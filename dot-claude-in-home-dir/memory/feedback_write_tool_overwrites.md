---
name: feedback_write_tool_overwrites
description: "A plan calling a file \"new\" is not evidence it doesn't exist — check before Write, and read a falling gate row count as a finding"
metadata: 
  node_type: memory
  type: feedback
  originSessionId: 481f601f-5ed6-43d0-98df-b41d70e42e21
  modified: 2026-08-20T20:16:55.095Z
---

`docs/b1-operand-grammar-s416.md` said its guard rows land in
`Pl/t/reduce-term-01.t`, "new file, it does not exist yet".  It did exist —
Option B phase 1's `_term_extent` unit tests, 127 rows — and Write silently
replaced it.  Its "File updated successfully" reads the same for a create and
an overwrite.

**Why:** a design doc is written at a point in time and the tree moves under
it; and in this repo a clobbered test file is invisible to every green signal
except the row count, because the replacement file passes.

**How to apply:** before Write on a path a plan calls new, run `ls` and
`git ls-files` on it — `git status` also distinguishes `M` from `??` in one
character.  And treat **a gate row count that FALLS as a finding, never
noise**: the tell here was `151 files / 5451 rows` where the previous session
left `151/5566`, and the 127-row hole was exactly the clobbered file.  Restore
from `git show HEAD:<path>` and give the new rows their own topic-named file.

Related: [[feedback_no_stash_when_stash_exists]], [[feedback_check_total_not_just_diff]].
