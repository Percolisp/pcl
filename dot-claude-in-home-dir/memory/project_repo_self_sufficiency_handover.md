---
name: project_repo_self_sufficiency_handover
description: USER goal — the repo must be self-sufficient so someone else can take PCL over; Claude's out-of-repo stores live in `<repo>/dot-claude-in-home-dir/` behind symlinks (task store moved s507, 2026-10-04; memory + agent briefs: see body) — task #1833
metadata:
  type: project
  modified: 2026-10-04T18:24:57.103Z
---

**The fact.** On 2026-09-16 the USER asked whether a GitHub clone is enough to
work on PCL with Claude Code, and said it would be good to have everything
ready for someone else.  Measured that day: the TESTS are reproducible from a
clone (CI runs the installer + `tools/prove-core`; `.claude/settings.json` +
hooks are tracked), the PROCESS was not — the task store, the agent rulebook +
briefs + plans + PAUSE files (`~/pcl-agent-scratch/`, Markdown; the gigabytes
there are measurement output) and this memory dir all lived outside the repo.

**USER, 2026-10-04 (s507): do it now** — "be careful when moving the files and
creating the link", name the destination pedagogically
(**`dot-claude-in-home-dir/`** at the repo root, the USER's own suggestion), and
check that nothing embarrassing is in it before it is published.

**State (update in place):**
- **Task store: MOVED s507.**  `~/.claude/tasks/pcl` is a SYMLINK to
  `/home/bernt/pcl/dot-claude-in-home-dir/tasks/pcl` (copy verified by checksum,
  then an atomic swap; the pre-move original is kept at
  `~/pcl-agent-scratch/s507/tasks-pcl-before-move`).  Every reader and writer
  keeps its old path.  An agent in a worktree writes through the link into
  MAIN's working tree — never read `$W/dot-claude-in-home-dir/` in a worktree,
  it is a stale checkout; task changes are committed on main by Fable.
- **Memory dir: MOVED s507** the same way
  (`~/.claude/projects/-home-bernt-pcl/memory` → `dot-claude-in-home-dir/memory`).
- **Agent rulebook / protocol / briefs: MOVED s507 (lean set)** into
  **`~/pcl/briefs-and-rules-for-claude-subagents/`** (layout mirrors
  `~/pcl-agent-scratch/`; `s473/COMMON.md`, `sNNN/SHARED-BOX.md`, briefs, the
  current `PAUSE-sNNN.md`).  The old paths are FILE-level symlinks.  WRITE THE
  REPO PATH: the Edit/Write tools refuse a symlinked file, and `perl -pi` /
  `sed -i` on the old path would silently replace the link with a copy.  A new
  session's SHARED-BOX, briefs and PAUSE file are written there under `sNNN/`;
  `~/pcl-agent-scratch/` keeps only measurement output.  The session HISTORY
  is `docs/session-log.md` — old briefs/PAUSE files are not copied in.
- `find ~/.claude/tasks/pcl -name …` does NOT follow the link (add a trailing
  slash or use a glob); `ls`, globs and `grep -r` do.
- **CONFIRMED 2026-10-05 (USER): a new Claude Code session loads `MEMORY.md`
  and the tasks through the symlinks.**  Left in #1833: only a parse / id test
  row for the task store (task files 407–409 have an empty `id`).

**How to apply:**
- Anything a successor would need goes into the repo (DECIDED, docs,
  CONTRIBUTING, `dot-claude-in-home-dir/README.md`) — never only into memory or
  `~/pcl-agent-scratch`.
- These directories are PUBLISHED with the repo: write tasks and memory notes
  as if a stranger reads them — nothing personal about the USER, no quotes
  beyond work directives.
- Related: [[feedback_no_session_log_in_memory]], [[project_ci_stock_machine]].
