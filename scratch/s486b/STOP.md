# s486b — STOP/RESUME state

Worktree: `/home/bernt/pcl/.claude/worktrees/agent-a6c7caf694eb1f8c2`
Branch `worktree-agent-a6c7caf694eb1f8c2`, based on main `4429126a` (rebased at start).
Task #1787.  **Do not merge into main — Fable merges after review.**

## DONE (committed)

- `f28a688a` **step 1 — the instrument**.  `cl/pcl-test.lisp` emits
  `ok N # skip [registry] <reason>`; `tools/sweep-perl-tests.pl` counts
  `registry_skips` + `registry_stale`, prints a `Reg` column, the new TOTAL
  line and a `REGISTRY-STALE:` line, and writes both as the LAST TWO columns
  of `.faillog/_status.tsv` (after the tab-scrubbed `note`);
  `tools/sweep-diff.pl` prints one `REGISTRY:` line (or `NOT COUNTED`).
  Paren check on `cl/pcl-test.lisp`: **balanced**.
  `prove tools/t/audit-instruments.t`: **PASS (29 rows)**.

## OWED

- step 2: the REGISTRY-STALE cleanup (per-file before/after tuples).
- step 3: docs (`docs/test-skip-registry.md` "How skips are counted",
  `docs/DECIDED.md` `## s486b` verbatim ruling, `docs/session-log.md`,
  task #1787 JSON).
- bars: `prove tools/t/audit-instruments.t tools/t/tap-align.t
  tools/t/sbcl-prefix.t`, `prove Pl/t/license-tag-01.t
  Pl/t/no-hardcoded-paths-01.t`, the FULL sweep, `tools/prove-core`.

## FINDING SO FAR (contradicts the brief's table — measured, not inferred)

The brief's per-file table said lex.t had **0 registry / 8 REGISTRY-STALE**
and join.t **0 / 2**.  Measured with the marker (the whole point of the
task — the reason TEXT was never a reliable classifier):
`lex.t pass=45 fail=0 skip=8 registry=8 stale=0`,
`join.t 41/0/2 registry=2 stale=0`, `crypt.t 5/0/1 registry=1 stale=0`.
So those 10 rows are live registry skips, not stale entries.  The real stale
set is whatever the 24-file sweep reports (`scratch/s486b/reg24.log`).

## RESUME COMMANDS

```bash
cd /home/bernt/pcl/.claude/worktrees/agent-a6c7caf694eb1f8c2
git log --oneline -3

# one file, the sweep's own loader (tools/runt's plain --load aborts
# tr/ref/readline/substr/state early — do not measure with it):
perl scratch/s486b/measure.pl ref.t          # prints (pass,fail,skip,registry,stale) + the STALE lines
PCL_REUSE_CL=1 perl scratch/s486b/measure.pl ref.t   # skip the transpile

# the 24 registry files:
perl tools/sweep-perl-tests.pl --jobs 4 $(perl -ne 'chomp; print "perl-tests/$_ "' scratch/s486b/registry-files.txt)

# the bars:
sbcl --script tools/check-parens.lisp cl/pcl-test.lisp cl/skip-registry.lisp
prove tools/t/audit-instruments.t tools/t/tap-align.t tools/t/sbcl-prefix.t
prove Pl/t/license-tag-01.t Pl/t/no-hardcoded-paths-01.t
pgrep -f run-perl-suite ; perl tools/sweep-perl-tests.pl --jobs 8   # FULL, last
tools/prove-core
```
