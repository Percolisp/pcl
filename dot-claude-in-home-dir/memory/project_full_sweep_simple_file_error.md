---
name: project-full-sweep-simple-file-error
description: "RESOLVED (session 219): full-sweep SB-INT:SIMPLE-FILE-ERROR mass-crash was a relative PCL_TEST_LOG_DIR breaking after a test chdir; fixed by absolutizing + crash-proofing the logger"
metadata: 
  node_type: memory
  type: project
  originSessionId: db64d4e3-8b50-4087-8c99-87730f92a3f4
---

**RESOLVED in session 219.** For a while the full `perl-tests` sweep marked ~36
partial/failing files as "Crashed (SBCL)" with `Unhandled SB-INT:SIMPLE-FILE-ERROR`
(pass total collapsed to ~7993, looked like mass crashes; memory called it "flaky -j8"
since s216). It was **not** parallelism, disk, FDs, tmpfs, or the fasl cache.

**Root cause:** the failure-log writer `%test-log-stream` (`cl/pcl-test.lisp`) opened
`PCL_TEST_LOG_DIR/<file>.fails.tsv` via a **relative** path. SBCL is run from
`perl-tests/` and many tests do `chdir 't'`; so when a *failing* test fired **after** the
chdir, the relative `.faillog/` dir didn't exist in the new cwd → `OPEN` (`:if-does-not-
exist :create` makes the file, not the dir) → unhandled file error killed the whole SBCL
process. It only triggered when the failing test was GC-nondeterministic (e.g. array.t 83
"freed array" passes alone but fails under full-run memory pressure) — hence "flaky."

**Fix (commit a00c241):**
1. `sweep-perl-tests.pl`: absolutize `$log_dir` (`relative → $project_root/$log_dir`) so a
   test's `chdir` can't break it. NB the default (no `PCL_TEST_LOG_DIR` set) was already
   absolute; passing `PCL_TEST_LOG_DIR=.faillog` (the documented form) was the trap.
2. `%test-log-stream`: `ensure-directories-exist` + wrap the open in `ignore-errors` — a
   diagnostic side-channel must NEVER crash a run (returns NIL → logging silently off).

**Healthy baseline after the fix (session 219):** **16849 pass / 770 fail / 11881 skip,
63 fully passing** (honest registry-era counter; the old ~28604 scored skips as pass).
**Only 2 genuine crashes remain** — bop.t (`pack "P"` ~496) and eval.t (`die if $@` ~29
after a string-eval lexical-scope failure) — both not-supported, both need the deferred
per-statement `handler-case` wrapper (`docs/test-skip-registry.md` §3.1) to recover the
tests after the crash point. `sweep-diff` is reliable again; baseline 506 keys.

**Lesson (user feedback):** when a sweep records a failure, read `.faillog` first
(`grep -v $'\tOK\t' .faillog/_status.tsv | cut -f1,6`; per-file `.faillog/<file>.tsv` has
got/expected) instead of hand-re-running SBCL. The full crash backtrace lives in the SBCL
output the sweep captures — reading it (here: the OPEN of a relative path) pinpointed it.
