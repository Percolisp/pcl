---
name: current-session-state
description: "The detailed resume recipe for the checkpointed PCL agent batches (s506f perl switches, s502e everyday singles 3) plus the standing USER directives that the MEMORY.md index only names"
metadata:
  node_type: memory
  type: project
  originSessionId: e5efc7ca-7bfd-47de-90dd-4c30746b5c62
  modified: 2026-10-03T07:09:38.675Z
---

**Where the LIVE state is:** `~/pcl/briefs-and-rules-for-claude-subagents/s507/PAUSE-s507.md` (the running log of the last session; its LAST block is the resume recipe).  Read it first at a session start; this file is the stable background.  Since s507 the session protocol, the briefs and the running notes are written in that repo directory (`sNNN/`), not in `~/pcl-agent-scratch/`.

**s507 ENDED 2026-10-05 ~03:30 (USER: "After you merge that, we end the session"): main = origin = `fee16466` + a records commit.**  Merged in s507: s502e (`d23b5927`), s506f perl's switches in `pcl` (`08de9e4f`), s507d the `pcl` documentation incl. README (`faeb6792`), #2688 (`ac2fec1e`), s507b the fix batch (`fee16466`).  Gen v2-4080, gate 278/9740, sweep 18731 pass / 647 fail, **EVERYDAY 114 of 122 (93.4 %)**.  TWO batches CHECKPOINTED and NOT REVIEWED: **s507c** (`agent-a5c8a09980aa19ae8`, `0229e0ec`: part one done — #2690 CHECK ordering / `pcl -c`, #2692 `try` under `-E`, three small ones; part two owed) and **s507p** perf round 39 (`agent-a55609aa75faa5780`, `fcfd7144`: #2539 #2637 #2111 built, #2115 phase (a) WIP, NO bench).  Resume with fresh pinned Opus agents WITHOUT `isolation`.

## s507 findings that outlive the session
- **The reviewer's quick companion run on a batch's final sha is part of the merge legs whenever `cl/` changes** — s502e's `p-cast-$` arm broke `$$globref` (#2688) and only the NEXT batch's companion leg showed it.  Run it in the batch's OWN worktree: a brand-new worktree's first runs flake (#2689).
- **A legs launch is ONE script path plus arguments** — a command line that names a heavy tool makes `fable-leg.sh`'s waitbox wait on its own launcher; `pgrep -f X | kill` from a line containing X kills the shell.
- **`perl -pi` inserting a wide character re-encodes the whole file**; the Edit / Write tools refuse a symlinked FILE (write the target).
- **SendMessage resumes a finished agent with its context intact within the SAME session** (used after the usage limit: both agents had checkpointed on a message and picked up from one).
- **`isolation: worktree` confines an agent to its launch worktree**: new batch = with it; resume of an existing worktree = without it.
- Everything below this line is the s506 state, kept as history.

**s506 ENDED 2026-10-04 ~18:15 (USER: "Time to pause the subjobs, finish up the session please. We continue later."): main = origin = `2e71d4aa`** = s501t (#155 tie on ARRAY/HASH, `618db500`) + s504c (#2633 file-private cells, `ddb959d2`), both REVIEWED and MERGED in s506, + one docs-only records commit.  Gen v2-3780, gate 276/9576, sweep 18728 pass / 650 fail, **EVERYDAY 110 of 122 (90.2 %)**.  CI green on 618db500; ddb959d2 / 2e71d4aa to read.  **TWO batches CHECKPOINTED and NOT YET REVIEWED — resume each with a FRESH pinned Opus 5.5 agent in its EXISTING worktree from `$W/scratch/<label>/STOP.md` + its brief (a new session cannot SendMessage the old agent ids):**
- **s506f** (perl's command-line switches in `pcl`: #2097 + #1702 + parts of #1709 — the USER asked for it in s506 and RAISED its priority; `agent-a6a0e67984b1d959d`, HEAD `516850b3`, 12 commits on 618db500; brief `s506/s506f-prompt.md` with the ruled design D1–D5).  All six members built and guarded; one WIP commit edits sweep baselines row by row (18728 -> 18731) and its confirming sweep is not re-run.  Measured: one-liner battery 147 of 152, `t/run/switches.t` 29 -> 125 of 140, corpus-diff = the 27 `-w` files.  OWED: every heavy bar (gate, sweep, `--all --quick` companion, everyday, gate-set-scan, installer test, compile-time), records.  At its rebase keep BOTH its `text` option and s504c's `module_unit` in `pl2cl`.  `-v` becomes perl's -v (version); TELL the USER the `-w` decision it measured.
- **s502e** (everyday singles 3: #2537 #2538 #2056 #2559 #2084(1); `agent-aefe0849c7b133e5f`, HEAD `2a698dc9`, 9 commits on 618db500; brief `s506/s502e-prompt.md`).  All five members built; its own tree reads EVERYDAY 113 of 122, sweep 18728.  OWED: the full gate (NEVER run), rebase onto main, gen renumber to v2-3880, artifacts, corpus-diff, everyday, records; the #2056 bench RE-TAKEN on a quiet box (taken at load 0.9–3).  Filed #2610 #2611.
- **USER-authorized follow-up (s506): after s506f is MERGED, launch ONE Opus docs agent for the `pcl` command documentation** (`docs/pcl-commands.md` switch reference, `pcl --help`, cross-references; README only if asked).
- **OWED first, on a quiet box before launching agents:** the FULL companion run (stamps older than two code merges).  ASK the subjob cap: "three at a time" and "don't start more subjobs (tokens)" were s506-only; default TWO.
- Fable review material: `s506/review/` (r506-nested2.pl + out/, fable-leg.sh = cold gate + sweep on a worktree), `s505/review/` (the tie probe set + NOTES.md).  Fable free task IDs 2680+.

## s506 findings that outlive the session
- **#2602: a gate file that HANGS after a cold cache on a tree older than `618db500` is the gate transpile server holding its spawner's stderr pipe** (fixed in s501t: `pl2cl` `_xc_spawn` reopens STDERR) — rebase first, do not investigate.
- **A second batch's rebase-independent work can run BEFORE the first merge** (s504c phase 2A: the ruled change + guard + probes, then it ends; continued by SendMessage for the rebase + bars) — it merged ~2.5 h after the first without idle waiting.
- **Foreground `sleep` is blocked in this harness** — wait on a background command's completion notification, never a sleep-poll.
- Filed from review probes (pre-existing, plain containers): **#2638** nested element in ARGUMENT position not vivified, write through `@_` lost; #1190 annotated (`local $h{r}{k} = V` reads undef even when the intermediate exists); #2639 (`scalar(%tied)` without SCALAR: 0 vs "").  Accepted flag: #2647 module-mode compile +22 %.

## History: the s505 state (superseded)
**s505 ENDED 2026-10-03 ~10:12 (USER: "End of session soon, don't start new tasks"): main = origin = `e5d72352` (= a33ce274 + docs-only s505 records; its CI is TO READ), NOTHING MERGED.  TWO batches CHECKPOINTED — resume each with a FRESH pinned Opus 5.5 agent in its EXISTING worktree from `$W/scratch/<label>/STOP.md` + its commits (a new session cannot SendMessage the old agent ids):**
- **s501t** (#155 tie; `agent-a267e3a9772561eda`; HEAD `3b90d0aa`, 27 commits on a33ce274).  DONE on that commit: Fable's review findings F1–F3 fixed with guard rows (guard file 128 rows), Fable probes == perl, bench re-taken (no row above the band), 18-file companion 0/0/0, sweep GATE clean TOTAL 18728.  OWED, in order: full gate (the exact command is in its STOP.md), corpus-diff, emission-ab, gate-set-scan both populations, ir-conform, ir-host-leak, tag-license, check-parens, artifact-staleness, everyday 108 -> 110 expected (re-check Sys-Hostname-Env-English after the path_sep fix), records, `--record` last, MERGE-READY.  Brief `s505/resume-s501t.md`.  Then Fable: re-run `s505/review/run.pl $W/pcl treeN` against `s505/review/NOTES.md`, cold gate + sweep legs (`s503/review/fable-leg.sh`), ff-merge, push, CI.
- **s504c** (#2633 file-private cells; `agent-a41c97e5832f266e1`; PHASE-1 DONE `0e5baf50`, 4 commits on 4ea2915c).  PHASE 2 only AFTER s501t is merged: rebase, seam probes (tie x file-private cells), corpus-diff, sweep, gate, everyday `--record`; brief `s505/resume-s504c.md` **plus the Fable ruling: widen `_unit_cell_base` from ~30 to ~50 digest bits**.  Flagged: module-mode compile +17 % (#2647).
- Protocol `s505/SHARED-BOX.md`.  Both rebase over e5d72352 (docs-only; DECIDED / session-log / not-supported touched — keep both sides).

**USER s503 (2026-10-01): "Don`t start new subjobs."** was for s503; in s504 the USER approved s501t and then s504c when ASKED.  Standing: ASK before launching anything new (everyday singles 3 is still unlaunched).

## In-flight batches (as of s503 start, 2026-10-01 08:19)
main = origin = `b7ec0093` (s503, 2026-10-01 ~10:30: s501b `2284d58d` + s501q `6757ddfe` merged, then docs/statistics); gen v2-3480, gate 274/9428, sweep 18714, **EVERYDAY 108 of 122**, next goal 110 (> 90 %).  NO agent running.  **s503 ENDED ~10:50 with CI GREEN on `b7ec0093`.**

Each checkpointed batch is resumed with a FRESH Opus 5.5 agent (model pin "opus"; it writes its model id to `scratch/<label>/MODEL.txt` first) in its EXISTING worktree, from `$W/scratch/<label>/STOP.md` + a resume brief `~/pcl-agent-scratch/s503/resume-<label>.md` + the protocol `s503/SHARED-BOX.md` (heavy legs serialize; the BENCH-WANTED reservation file; a docs-only rebase re-opens no bar).

- **s501b** everyday singles 2 — **MERGED s503 as `2284d58d`** (pushed; gen v2-3380, gate 273/9328, sweep 18714, EVERYDAY 108 of 122; platform-touching, macOS NOT TESTED #2196; scratch archived `~/pcl-agent-scratch/s503/s501b-agent-a8afc5670788453d5/`).  Member 5 (#2056) stays out, patch `~/pcl-agent-scratch/s502/s501b-m5-2056.patch`.  Review residue filed: #2630.
- **s501q** perf round 38 — **MERGED s503 as `6757ddfe`** (sigarith -80 %, passarr -37 %, listdeclcat -51 %; real programs -23..-30 %; FLAGGED fibret +4..5 % cause not found; #2535 short-string path removed, #2575 shared ref box — both TOLD to the USER in the s503 report; scratch archived `~/pcl-agent-scratch/s503/s501q-agent-a54e7f657b5c4898e/`).  Review residues filed: #2631 (list declaration reads the new variables), #2632 (`&name;` does not share @_).
- **s501t** #155 tie on ARRAY/HASH — worktree `agent-a267e3a9772561eda`, HEAD `f027754a` (14 commits, gen v2-3580).  ALL THREE PHASES BUILT; sweep 18714 → 18728; tiearray.t 70/5.  FIRST at resume: bisect op/tiehandle.t 11/32 → TIMEOUT in `6c708f98..5bf2e561`.  Its transpile-test-07.t rewrite `19adda84` was ACCEPTED by Fable in s503 (four conjuncts verified vs perl).  NOT resumed in s503 (USER: no new subjobs); resume brief `~/pcl-agent-scratch/s503/resume-s501t.md`; main has moved across TWO merges since its base (s501b, s501q) — the brief names the interactions to re-check after its rebase.
- **everyday singles 3** (not launched) — brief `~/pcl-agent-scratch/s502/s502e/prompt.md`: #2537 #2538 #2056 #2559 #2084(1); gen v2-3680; IDs 2610–2629; fill with `s502/fill.pl`.

Merge = Fable review: probes (`~/pcl-agent-scratch/s501/review/run.pl <W>/pcl tree-<label>` vs NOTES.md, perl → base → tree), `s503/review/fable-leg.sh <W> <label> both` (cold gate + sweep on the sha), ff-merge, push, read CI.

Filed s502 from review probes (pre-existing): #2537 list-valued `use constant` returns its LAST element; #2538 `Scalar::Util::set_prototype` is a stub; #2539 perf — unguarded pos() remhash on the general store path (round-39 candidate); #2560 Math::BigRat->new("49/4") = 1.  OWED: #2389 quiet companion run (not with agents up).  Fable free task IDs 2633+ (s503 filed #2630 #2631 #2632 from review probes).

## Standing USER rulings that shape every session
- **Subjob cap** = whatever the USER states per session; default TWO (s502: "Do 3 subjobs this session" was for that session only).  Ask at the start without blocking the resumes.
- **s499:** start-up / first-run build / string-eval compile time are DOCUMENTED speed problems, NOT optimized now (#2420–#2423 parked); optimizations are PRIORITIZED BY MEASURED GAIN.
- **s498 ("close enough to Perl now"):** speed work looks at where REAL PERL PROGRAMS spend time (everyday corpus + Rosetta), reviewed by Fable and ASKED before launching; open no new correctness batch unasked.  The review happened in s499 (faster-codegen §0.2r/§0.2s); perf rounds 36–38 follow it.  #155 tie and everyday singles 3 were explicitly approved by the USER in s502.
- **s494:** stop opening new test angles, burn down the briefed queue; steer by the EVERYDAY number (`tools/everyday-smoke.pl`, goal > 90 %), not the suite pass rate.
- **s488:** skip registry RETIRED after the next tag (#1836); #233 caller() = bug queue; README companion figures at tag time; cache prune = miss-only, once a day.
- **Before s486:** op/ census rounds t6a–t6c DESIGNED (`~/pcl-agent-scratch/s473/s473t6/plan.md`), not launched — the USER decides; USER owes the GitHub checklist, P4 docs split, P5.

Related: [[project_everyday_battery]], [[feedback_two_agents_at_a_time]], [[feedback_subagents_must_pin_model]], [[feedback_put_live_before_new_work]], [[reference_agent_worktree_base_is_session_start]].

## s504 findings that outlive the session
- **The FULL companion form must run when its stamps are older than the last code merge** (it had not run 2026-09-13 .. 10-02; #2633 sat unseen six days because `--quick` skips the regexp wrappers).  Do it at session start on the quiet box, BEFORE launching agents, `< /dev/null`.
- **#2633** a promoted file lexical is a package cell shared ACROSS compilation units (do FILE / eval string / second file of a package); the eight re/regexp.t wrappers read 0 rows since aa657c6b (7,752 rows) and are deliberately NOT spliced in the snapshot.  Filed with it: #2634 (`%{Foo::}` dies under strict), #2635 (inline `(?p)` flag rejected).  #2389 CLOSED.
- Fable free task IDs: 2638–2639, then 2660+ (s501t has 2590–2609, used through 2601; s504c 2640–2659, used through 2647).

## s505 findings that outlive the session
- **Run the review probes EARLY** — written before the agent reports, run on its tree as soon as the code is built (not after MERGE-READY): s501t's three defects reached the agent while its bars were still ahead of it.
- **Read the BENCH-WANTED file in its OWN step before any run of mine** — a check chained in front of the run does not gate it (I perturbed an agent's bench window this way).
- **A batch that merges second works in TWO PHASES** (phase 1 light, ends; phase 2 after the first merge) — no sleep loop waiting for a merge.
- Filed: **#2636** (a sub parameter passed to a method / qualified / coderef call is copied and the backstop is silent — Tie::File STORE drops the record separator), **#2637** (one live container tie costs untied containers ~40 %; one-macro candidate).  An agent's default cwd is the MAIN checkout: output files of a command run without `env -C "$W"` land there.
