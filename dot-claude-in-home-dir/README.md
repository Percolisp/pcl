# dot-claude-in-home-dir

PCL is developed with [Claude Code](https://claude.com/claude-code).  Claude
Code keeps some of a project's working state under **`~/.claude/`** in the
developer's home directory, outside the repository.  For PCL that state is
large and the documentation points into it everywhere (`#2680` in a doc is
task 2680), so it lives **here**, in the repository, and the home-directory
paths are symbolic links into this directory.  A clone is then the whole
project: the code, the documentation, the task list and the notes.

| In the home directory | is a symlink to | What it is |
|---|---|---|
| `~/.claude/tasks/pcl` | `dot-claude-in-home-dir/tasks/pcl/` | **The task store.**  One JSON file per task, `NNNN.json`.  Every `#NNNN` in `docs/`, `baselines/` and the commit messages is a file here. |
| `~/.claude/projects/-home-USER-pcl/memory` | `dot-claude-in-home-dir/memory/` | **Claude's notes between sessions.**  `MEMORY.md` is the index Claude Code loads at the start of every session; each other file is one note. |

Nothing else from `~/.claude/` is here, on purpose: no session transcripts, no
credentials, no personal settings.  (The project's own Claude Code settings and
hooks are in the tracked [`.claude/`](../.claude/) directory, as usual.)

## Reading it without Claude Code

A task is plain JSON with `id`, `subject`, `description`, `status`:

```sh
perl -MJSON::PP -0777 -ne 'my $t = decode_json($_); print "$t->{subject}\n\n$t->{description}\n"' \
    dot-claude-in-home-dir/tasks/pcl/2680.json
grep -l 'use constant' dot-claude-in-home-dir/tasks/pcl/*.json     # which tasks mention something
```

A task's description usually carries its reproducer, what was measured, what
was tried and what killed the attempt — the project's rule is that a task says
what NOT to retry.  `status` is `pending`, `in_progress`, `completed` (older
files also say `open` / `done`).

The notes in `memory/` are Markdown with a small front-matter block.  They are
Claude's working memory: rules the developer gave, measurement traps, pointers
to where things are.  The settled rulings themselves are in
[`docs/DECIDED.md`](../docs/DECIDED.md) and the history in
[`docs/session-log.md`](../docs/session-log.md); when a note and those two
disagree, the docs win.

## Taking the project over on another machine

After cloning, link the two directories into your own `~/.claude/` so that
Claude Code reads and writes them in the repository:

```sh
cd /path/to/pcl
mkdir -p ~/.claude/tasks
ln -s "$PWD/dot-claude-in-home-dir/tasks/pcl" ~/.claude/tasks/pcl

# Claude Code names a project's directory after the checkout's absolute path,
# with every "/" turned into "-":  /home/alice/src/pcl -> -home-alice-src-pcl
proj=~/.claude/projects/$(pwd | tr / -)
mkdir -p "$proj"
ln -s "$PWD/dot-claude-in-home-dir/memory" "$proj/memory"

export CLAUDE_CODE_TASK_LIST_ID=pcl     # in the environment Claude Code starts in
```

If either path already exists as a real directory, move it aside first — `ln -s`
into an existing directory creates the link *inside* it.

## Rules for whoever writes here (human or model)

- **This directory is published with the repository.**  Write a task or a note
  as if a stranger reads it: technical content and work decisions only.
- Task files are written with `JSON::PP->new->utf8` to a `:raw` handle; one file
  per task, never renumbered; the next free id is the highest id plus one.
- A background agent working in its own git worktree still writes tasks through
  `~/.claude/tasks/pcl`, that is, into the **main** checkout's copy of this
  directory.  The copy inside a worktree is a stale checkout — do not read it.
  Task and note changes are committed on `main` by the reviewing session.
- `find ~/.claude/tasks/pcl -name '*.json'` does not follow the symlink (add a
  trailing slash, or use a glob); `ls`, shell globs and `grep -r` do.
