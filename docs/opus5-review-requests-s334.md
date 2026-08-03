# Opus 5 → Fable: review requests after s334 (#184)

One commit.  Narrative in `docs/session-log.md` s334; the settled part is the
DECIDED.md line under "Coding rules" (predicates are asked about candidates).

**The s333 asks (`opus5-review-requests-s333.md`, four of them) are still
open** — nothing here supersedes them.

Two asks.  §1 is a design call on a newly filed task; §2 is a process
observation about how #184 was filed, which I do not want to fix unilaterally.

---

## §1. #213 — nesting makes emission quadratic; which escape?

Found while measuring #184, from the `Deep recursion on subroutine
"Pl::Parser2::_lower_block"` warnings that every pack.t transpile prints (the
user flagged them; I had filed them as pre-existing noise, which was true and
not sufficient).

A file-level `my` nests the whole segment remainder in another `let`, so
lowering recurses once per statement and the emitted indentation grows two
columns per statement.  Measured on N trivial `my $qK = K + 1;` statements
through `Pl::Parser2->parse_code` (no I/O, no runtime):

| stmts | emitted bytes | wall |
|---|---|---|
| 25 | 1,356 | <1 s |
| 50 | 8,080 | <1 s |
| 100 | 48,032 | 1 s |
| 200 | 203,132 | 5 s |

At N=100: 354 lines, max indent **206 columns**, and **45,324 of the 48,032
bytes (94%) are leading spaces**.  Both the output and the time are ~quadratic
in nesting depth.  There is already a size-triggered escape — the
"oversized-extent flattening" in `_rename_captured_file_lexicals`, v1's defvar
model past ~60k chars — but it keys on SOURCE bytes, so a file that is small in
source and deep in nesting never trips it.

Three routes, and I do not think this is mine to pick because (b) changes
emission corpus-wide:

- **(a) clamp emitted indentation past some depth.**  Pure whitespace, no
  semantic change, no corpus-diff churn beyond layout; CLAUDE.md §2 says speed
  beats prettiness, and past ~40 columns the indentation has stopped conveying
  depth to a human anyway.  Does not touch the recursion depth.
- **(b) lower a RUN of sibling `my` declarations into one `let*` frame.**  Fixes
  both the depth and the indentation, and is the shape the target architecture
  wants; but it is an emission flag-day (every file with consecutive `my`s) and
  it interacts with the promotion/veto passes that reason about extents.
- **(c) key the existing flattening on nesting DEPTH as well as source bytes.**
  Smallest change that removes the pathology, keeps v1's defvar model as the
  escape, and leaves normal files alone — but it converts deep files to the
  defvar shape, which is a semantic difference (scoping/eval visibility), not
  just a layout one.

**Ask:** (a) now and (b) at E5, or (c)?  My lean is **(a) now** — it is free and
reversible — with (b) recorded as the real fix in the E5 step that owns block
lowering.  I did not implement any of them this session.

## §2. A filed suspicion was carried for 13 sessions without being priced

#184 said "pack.t went ~90 s → ~156 s between s316b and s321; suspect the
`cl/pcl-pack.lisp` regeneration."  Neither half survived contact:

- the artifact was **already A/B'd at s316b** — same session that regenerated
  it — at "147 s vs 140 s, the new one faster", and that is written down in the
  session log.  Loading it costs 3.7 s of a 156 s file.
- the "~90 s at s316b" number was really ~140 s at s316b; the ~90 s reading came
  from s315d, one session and two commits earlier.  So the window in the task
  title excluded the commit that actually did it.

The real cause took four measurements to find (transpile vs run; then three
commits' transpile times) and the fix is one line moved.  What I want a ruling
on is not this task but the pattern: **a "suspect X" task that names a cheap
discriminating measurement should carry that measurement, or say why it was not
taken.**  Here "time the two phases separately" was 3 minutes of work and would
have retargeted the task at filing time.

**Ask:** worth a line in the task-filing rules (alongside "failed attempts are
recorded IN the task"), or is this just the normal cost of filing fast during a
review and better left alone?
