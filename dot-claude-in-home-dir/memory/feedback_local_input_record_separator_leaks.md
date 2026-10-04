---
name: feedback_local_input_record_separator_leaks
description: "In a multi-read perl one-liner, `local $/` leaks past the open it was written for — scope it with a do-block or a later read slurps the whole file as one line"
metadata: 
  node_type: memory
  type: feedback
  originSessionId: bde72691-cee1-4c12-a674-8952ce26933e
  modified: 2026-08-08T16:10:28.729Z
---

`local $/;` is scoped to the enclosing BLOCK, not to the `open`/read it sits
next to. In a one-liner with no enclosing block it stays undef for the rest
of the program, so a LATER `my @lines = <$fh>;` returns the whole file as a
single element.

**Why:** s361 lost `~/.claude/projects/-home-bernt-pcl/memory/MEMORY.md` this
way. The script slurped a replacement line (`local $/; my $line = <$f>;`),
then read MEMORY.md into `@l` intending one element per line — but `$/` was
still undef, so `@l` had ONE element (the entire file), the "find the STATE
line" loop matched index 0, and the assignment replaced the whole file with
one line. The file is outside the repo, so there was no `git checkout` to
undo it; it was recoverable only because the full contents were in the
session's context.

**How to apply:** scope the slurp — `my $text = do { local $/; <$fh> };` —
or set `$/` back explicitly before the next read. And before overwriting any
file that is not in git, either copy it aside first or verify the parse
(`scalar @l > 1`) before writing. Related: [[feedback_no_stash_when_stash_exists]].
