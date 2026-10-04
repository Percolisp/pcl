---
name: project_sweep_flakiness_investigation
description: "QUEUED investigation — parallel sweep is flaky (bop.t crash-point shifts, spurious SIMPLE-FILE-ERROR); costing real time on every regression check"
metadata: 
  node_type: memory
  type: project
  originSessionId: c5866a4a-7729-47d1-9351-8e8409ee9b09
  modified: 2026-08-02T23:29:27.055Z
---

**QUEUED (user-requested 2026-06-06, session 236d): find & fix the parallel-sweep flakiness.**

**Why:** Every regression check goes: run `perl sweep-perl-tests.pl --jobs 8`, then
`tools/sweep-diff.pl`. The parallel sweep is non-deterministic — it reports spurious "new
failures" that don't reproduce under `--jobs 1`. Each false alarm forces a manual
stash/compare cycle to rule out a real regression. Across sessions this is **costing a lot of
time**. The user explicitly asked to put this on the stack and fix it after the current work.

**Symptoms seen (session 236d):** after an unrelated change (string-interp + grep/map/sort
boxing), the `--jobs 8` sweep-diff reported **11 new bop.t failures** + "2 baseline fail(s)
absent — PARTIAL". Ran bop.t under `--jobs 1` with my changes vs `git stash` (HEAD): **identical**
(446 pass / 49 fail, ran 495 of 510, same last-test). So the 11 were pure parallel flakiness —
bop.t's crash point (it dies at `pack "P"`, ~line 636) shifts run-to-run, changing which tests
emit before the crash, so sweep-diff sees different (file,desc) keys each time.

**Prior knowledge (MEMORY.md):** "parallel sweep flaky `SIMPLE-FILE-ERROR` crashes (join/anonsub
seen once) — re-bless baseline only from clean sweeps." Session 219 fixed one cause (relative
`PCL_TEST_LOG_DIR` breaking after a test `chdir` → absolutized `$log_dir` + crash-proofed
`%test-log-stream`). So one class is fixed; another remains.

**Hypotheses to check:**
1. **Crash-file partial output is inherently nondeterministic** — bop.t/eval.t die mid-file;
   under parallel load the exact crash point varies (timing/GC/buffer flush). Fix idea: the
   deferred per-statement `handler-case` wrapper (test-skip-registry.md §3.1) would turn the
   `pack "P"` die into one `not ok` instead of aborting the file → deterministic. That's the
   already-planned fix for bop.t/eval.t crashes anyway.
2. **Shared mutable state across parallel workers** — the persistent `pl2cl --server`, FASL
   cache (`~/.pcl-cache/`), or `.faillog` writes racing between workers. Check whether each
   `--jobs` worker gets an isolated cache/server or they contend. A per-worker cache collision
   would produce `SIMPLE-FILE-ERROR`.
3. **`.faillog` write races** — multiple workers appending to the same status/fail files.

**Approach when picked up:** run the full `--jobs 8` sweep N times back-to-back on an unchanged
tree, diff the N `.faillog`s against each other (not against baseline) to enumerate exactly which
(file,desc) keys flap. Then localize: are flappers ONLY in crash/PARTIAL files (→ hypothesis 1,
fix via handler-case wrapper) or also in clean files (→ hypothesis 2/3, a real race)?

**Diagnostic done (session 236d, at user's request — "comment out that test in bop.t to see if
it really was the crash"):** YES, confirmed bop.t has real, DETERMINISTIC abort sites under
`--jobs 1`:
- As-is: stops at **test 495** (`pack "P"` pointer-pack, line 636 — `unpack("P2", pack "P", ...)`).
- With the pack-P block (lines 633-638) commented out: advances to **test 507**, new stop at
  `formline` (line 702, `[perl #17844]`). So ≥2 abort sites; each Perl `die`/unsupported-op
  ABORTS THE WHOLE FILE instead of emitting one `not ok` and continuing.
- `--jobs 1` is fully deterministic (always 446 pass / 49 fail / ran 495) — **identical** with my
  session-236d changes and on HEAD. The flapping is **parallel-only**.

**Refined conclusion:** two independent problems.
1. **File-abort-on-die** (hypothesis 1): real, deterministic, makes bop.t/eval.t PARTIAL. The
   per-statement `handler-case` wrapper (test-skip-registry.md §3.1) converts each die→`not ok`
   →continue, so the file runs all 510 every time. This is the high-value fix and is already
   planned for the bop.t/eval.t crashes anyway.
2. **Parallel-only variation** (hypothesis 2/3): the BASELINE (`baselines/fail-baseline.tsv`) records
   only **2** bop.t keys, but a real run records ~49 — so the baseline was blessed from a run
   where bop.t aborted MUCH earlier than test 495. Either an old/bad bless, or a parallel race
   (SIMPLE-FILE-ERROR in harness log writing, or `~/.pcl-cache/` / `pl2cl --server` contention)
   aborts bop.t early & nondeterministically under `--jobs 8`. A `handler-case` wrapper does NOT
   catch a harness-level CL file-IO crash, so #2 needs separate work: isolate per-worker cache +
   log files, and re-bless the baseline from a clean run AFTER #1 lands.

**Net for picking up:** do #1 first (handler-case wrapper → bop.t/eval.t deterministic & full),
then re-measure whether any parallel flapping remains; if so chase the per-worker file race (#2).

---

## INVESTIGATED & PARTIALLY FIXED (session 237, 2026-06-07)

**Measured it.** Ran the full `--jobs 8` sweep **3× back-to-back into separate log dirs** on an
unchanged tree → **byte-identical**: 0 (file,desc,num) key differences across all pairs, 784 raw
lines / 449 deduped keys each, same TOTAL (18046/784/11959), same 69 fully-passing, same 8 PARTIAL
files at the SAME counts (bop.t 446+49/510 every run, = `--jobs 1`). **So the parallel sweep is
deterministic — the flakiness is NOT run-to-run nondeterminism, and it's RARE (didn't reproduce in
3 runs).** The 236d "11 new bop.t" was a one-off rare crash-under-load in a known CRASH/PARTIAL file.

**Ruled out:** faillog write-race (each child opens its OWN `<file>.fails.tsv`, `:supersede`,
`force-output` per line — no contention). `/tmp/pcl-sweep-$$.out` is PID-unique. Module-cache:
FASL mode (default) uses PID-temp + atomic rename (safe); only the non-default Lisp-mode branch
(`p-load-module-cached`, line ~7530) does a racy direct `:supersede` write to the shared cache-path
— a latent bug but not the sweep's path, and bop.t uses only pragmas (no module load). `*pcl-skip-
cache*` only forces a cache *miss* (re-transpile); it still WRITES the cache.

**Root of the false-alarm cost:** sweep-diff keys on (file, **description**); many crash-file fails
have EMPTY descriptions (collapse to one key) but some are described. A crash file's abort point
shifting (rarely, under load; or after a real fix lets it run further) changes its set of *described*
fails above the abort → sweep-diff flagged them as "NEW failures (regressions)" → forced stash/compare.

**FIX SHIPPED (commit 6f0cb18):** `tools/sweep-diff.pl` now segregates a NEW failure whose file is
CRASH/PARTIAL/TIMEOUT this run into **"UNSTABLE (crash-file noise)"** — shown but NOT counted as a
regression and NOT setting the nonzero exit. This is the symmetric twin of the existing `ran_clean`
guard on the FIXED side. Verified: a real regression in an OK file still gate-fails (exit 1); the
236d bop.t scenario now exits 0. Also **re-blessed `baselines/fail-baseline.tsv`** from a deterministic
run (450→449 keys, captures the concat2 RT#132385 fix); runs 2 & 3 diff 0/0.

**STILL OPEN (the real cure, not yet done):** the per-statement `handler-case` wrapper (test-skip-
registry.md §3.1) so bop.t/eval.t/etc. run to COMPLETION every time (each `die`→`not ok`→continue,
finer than p-load-with-recovery's per-top-level-form granularity — which is why bop.t still stops at
495: the missing ~15 tests live inside loops/blocks that abort partway). That would (a) make crash
files deterministic & fully-counted, (b) let regressions INSIDE them be detected (the UNSTABLE guard
currently can't), (c) likely eliminate the rare under-load flapping entirely. `p-load-with-recovery`
(`cl/pcl-test.lisp:620`) catches CL `error` per top-level form; note it does NOT catch non-`error`
conditions (e.g. `storage-condition` from `--control-stack-size 512` exhaustion) — a candidate for
the rare crash. Worth widening the handler to `serious-condition` there too.

Related: [[feedback_fully_passing_regression]] (the guard that triggers these checks).

---

## THE RARE "UNDER-LOAD" FLAP HAS A NAME NOW: SYSTEM-WIDE OOM (session 333, 2026-08-03)

The 237 measurement stands — the sweep IS deterministic on a quiet box (3 identical runs).
The rare crash-under-load is **memory pressure from the whole desktop, not the sweep**.
Measured after the user suggested it (they called it "a repeat problem"):

- A full sweep failed #204's gate with `LOST ref.t -5 (184 -> 179)`, TOTAL -1, exit 1.
  An immediate re-run of the SAME TREE was clean, ref.t 186+18.
- `journalctl` for the window: `02:04:43 kernel: invoked oom-killer` →
  `Out of memory: Killed process (Isolated Web Co) anon-rss:1054148kB`.
  **System-wide, no cgroup cap.** Run 1 was executing ref.t at that moment
  (file 84/108, 91 s); run 2 started after that ~1 GB was freed.
- CONTROL: 8 heavy files incl. ref.t at `--jobs 8` **with headroom** → ref.t 186+18,
  **8.9 GB minimum available**. Concurrency alone does not reproduce it.
- Box is a **13 GB desktop shared with Firefox** (~5 GB across processes when open).

**Why ref.t specifically:** it forks **23 `fresh_perl_is`/`runperl` children**, each a fresh
transpile + SBCL. Under memory pressure those get starved and fail ROWS **without aborting
the file** — the one shape that slips past the UNSTABLE crash-file filter and lands in LOST.

**PROCEDURE before believing any sweep/runner verdict on this box:**
1. `journalctl -k --since "<run start>" | grep -i oom-killer` — one grep, decides it.
2. Watch `MemAvailable` during the run (`free -m`); a real regression does not need headroom.
3. Check for a stray `pl2cl --server` ([[project_v2_session_state]] task #128, caught at
   4.95 GB) — same failure mode with a cause inside our own tree.
4. Re-run the file at `--jobs 1`: it is deterministic there (ref.t 186 twice, HEAD 184).

Recorded on task #180 and in `docs/opus5-review-requests-s333.md` §4, which asks whether the
gate should sample MemAvailable and print it with the verdict, and/or re-run a LOST file once
at `--jobs 1` before failing (the #176 TIMEOUT-retry rule, reused).
