---
name: reference_agent_worktree_base_is_session_start
description: "The Agent tool's isolation:worktree branches from the SESSION-START commit, not from main's HEAD at launch — every agent must be told its real base and to rebase onto main first"
metadata:
  type: reference
---

Observed s463f (2026-09-01): main had been fast-forwarded to `6e6f191` in
the session, then two Opus agents were launched with `isolation: "worktree"`;
`git worktree list` showed BOTH new worktrees at `12eacb5` — the commit the
session STARTED on (the gitStatus snapshot), three commits behind main.

**Why it matters:** an agent whose task builds on the just-merged round
(e.g. #960 on #934's `%p-overload-fallback-of`) works against code that is
not there, and its inverse-guard "base" is wrong.

**How to apply:** in every agent brief say "check `git log --oneline -1`;
if HEAD is not <main sha>, `git rebase main` before any work" — and after
launching, run `git worktree list` and SendMessage a correction if needed.
The pre-existing rule "rebase onto main before the final gate" still stands.
Related: [[project_s421_opus_agents_inflight]], [[feedback_subagents_must_pin_model]].
