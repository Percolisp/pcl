# s486b — STOP/RESUME state

Worktree: `/home/bernt/pcl/.claude/worktrees/agent-a6c7caf694eb1f8c2`
Branch `worktree-agent-a6c7caf694eb1f8c2`, based on main `4429126a` (rebased at start).
Task #1787.  **Do not merge into main — Fable merges after review.**

## MERGE-READY SHA

`d6382c0d` (three commits on top of `4429126a`) — **plus whatever the two
remaining bars force**; see OWED.

```
d6382c0d s486b step 3: docs + task record -- the ruling recorded verbatim
de0b7b7d s486b step 2: the REGISTRY-STALE entries narrowed or dropped -- 7 of them, in 4 files
f28a688a s486b step 1: the registry's skip line is marked [registry] and the sweep COUNTS it
```

## DONE

- **The instrument.**  `cl/pcl-test.lisp` emits `ok N # skip [registry] <reason>`;
  `tools/sweep-perl-tests.pl` counts `registry_skips` + `registry_stale`, prints a
  `Reg` column, `TOTAL: … K skipped (R by the registry) …` and
  `REGISTRY-STALE: S entries in T files`, and writes both as the **last two
  columns** of `.faillog/_status.tsv` (after the tab-scrubbed `note`);
  `tools/sweep-diff.pl` prints one `REGISTRY:` line, or `NOT COUNTED`.
- **The stale cleanup**: 7 entries in 4 files (NOT the brief's 19 — see FINDING).
- **Docs**: `docs/test-skip-registry.md` "How skips are counted", `docs/DECIDED.md`
  `## s486b` (ruling verbatim), `docs/session-log.md` session 486b, task #1787 JSON.
- The registry STAYS; no row migrated; nothing changed about what counts as
  pass or fail.

## BARS

| bar | result |
|---|---|
| `sbcl --script tools/check-parens.lisp cl/pcl-test.lisp` | **balanced** |
| `sbcl --script tools/check-parens.lisp cl/skip-registry.lisp` | **balanced** |
| `prove tools/t/audit-instruments.t tools/t/tap-align.t tools/t/sbcl-prefix.t` | **PASS, 81 rows** |
| `prove Pl/t/license-tag-01.t Pl/t/no-hardcoded-paths-01.t` | **PASS, 16 rows** |
| stale cleanup, per-file before/after | **all four identical except stale → 0** (table below) |
| FULL sweep `--jobs 8` | **RUNNING** → `scratch/s486b/full-sweep.log` |
| `tools/prove-core` | **OWED** (run after the sweep) |
| companion | **NOT OWED** — the companion loads `cl/pcl-test.lisp` but never `cl/skip-registry.lisp`, so `%skip-registry-lookup` always answers nil there and the changed branch cannot fire |

### the stale cleanup, measured — `(pass, fail, skip, registry, stale)`

| file | before | after |
|---|---|---|
| ref.t | (199, 12, 31, 25, **4**) | (199, 12, 31, 25, **0**) |
| array.t | (171, 15, 9, 9, **1**) | (171, 15, 9, 9, **0**) |
| chop.t | (144, 0, 4, 4, **1**) | (144, 0, 4, 4, **0**) |
| state.t | (88, 0, 4, 4, **1**) | (88, 0, 4, 4, **0**) |

What changed: ref.t `^(Scalar|Array|Hash|Code|Glob) dereference$` →
`^(Scalar|Array|Glob) dereference$` + integer keys 38/39 (the `foreach $ref
(*STDOUT{IO}, *STDERR{FORMAT})` loop emits the same four descriptions twice and
only the FORMAT iteration's `%$ref`/`&$ref` pass now — a description cannot
separate them); ref.t's UTF8 pattern → `via the correct name works`; array.t
`\@_ alias to nonexistent` → `…nonexistent neg index`; chop.t `chomp @a
when.*eq 0 and` and state.t `^Reference to state variable$` DROPPED (one row
each, both passing).

## FINDING — the brief's 19 was 7 (measured)

The brief's per-file table classified a skip by matching its REASON text
against the registry file.  With the `[registry]` marker: **lex.t registry=8
stale=0**, **join.t registry=2 stale=0**, sub.t likewise — those 12 are LIVE
registry skips, not stale entries.  The real stale set was ref.t 4 + array.t 1
+ chop.t 1 + state.t 1 = **7 in 4 files**.  That is the case for the marker in
one measurement, and it is why the reason text must never be the classifier.

## OWED

1. Read `scratch/s486b/full-sweep.log` when it finishes.  Expect: GATE clean
   (0 new / 0 fixed / 0 LOST), **TOTAL passing 18676** (the baseline), drops 5
   = census, `REGISTRY: ~198 rows relabelled in ~24 files`,
   **REGISTRY-STALE: 0 entries in 0 files**.  A changed pass/fail count in ANY
   file is a FINDING, not noise — compare file by file against
   `baselines/pass-baseline.tsv`.  NB another agent's `Pl/t` gate was on the
   box when this run started (load ~7), so a LOST report may be load noise —
   the sweep re-runs a LOST file serially by itself and prints both verdicts.
2. `tools/prove-core` ONCE (`cl/pcl-test.lisp` is loaded by gate files).
   Expect `Result: PASS`, 242 files / 8212 rows (the three xs files parked).
3. Write both results into this file and into the final report.

## RESUME COMMANDS

```bash
cd /home/bernt/pcl/.claude/worktrees/agent-a6c7caf694eb1f8c2
git log --oneline -4

grep -a 'TOTAL:\|REGISTRY\|summary:\|LOST\|DROPS' scratch/s486b/full-sweep.log
# if the sweep needs re-running (quiet box first):
ps -eo comm,args --no-headers | awk '$1=="perl"||$1=="sbcl"' | grep -v claude
perl tools/sweep-perl-tests.pl --jobs 8 > scratch/s486b/full-sweep.log 2>&1

tools/prove-core 2>&1 | tail -20

# one file, the sweep's own loader (tools/runt's plain --load aborts
# tr/ref/readline/substr/state early — do not measure with it):
perl scratch/s486b/measure.pl ref.t                    # (pass,fail,skip,registry,stale)
PCL_REUSE_CL=1 perl scratch/s486b/measure.pl ref.t     # reuse the transpile
PCL_NO_REGISTRY=1 PCL_REUSE_CL=1 perl scratch/s486b/measure.pl ref.t   # descriptions + raw verdicts
# the 24 registry files:
perl tools/sweep-perl-tests.pl --jobs 4 $(perl -ne 'chomp; print "perl-tests/$_ "' scratch/s486b/registry-files.txt)
```

## OPEN USER QUESTION (for Fable to carry, not for a session to decide)

**Retire the registry in favour of the cause column, or keep both?**  Fable
recommends retiring, after the tag.  Retiring moves the headline (649 → ~830
fails), so it is a USER decision.  Until then both stand and every statement of
the fail count carries the registry count beside it.
