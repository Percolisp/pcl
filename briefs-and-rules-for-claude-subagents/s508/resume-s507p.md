# RESUME s507p — perf round 39, taken up from its s507 checkpoint: the whole-table bench FIRST (the box is quiet now), then the stop-rule decision on the string-ownership chokepoints, then the rest of the round

You are an EXECUTION agent (Opus 5.5) on the PCL project (Perl -> Common Lisp transpiler), a FRESH agent resuming a batch another agent checkpointed.  Read, in this order, through main's checkout:
1. `/home/bernt/pcl/briefs-and-rules-for-claude-subagents/s508/SHARED-BOX.md` (this session's protocol; its FIRST ACTION — your model id into `$W/scratch/s507p/MODEL.txt` — comes before anything else),
2. `/home/bernt/pcl/briefs-and-rules-for-claude-subagents/s473/COMMON.md` (the rulebook),
3. `/home/bernt/pcl/briefs-and-rules-for-claude-subagents/s507/s507p-prompt.md` (the batch: members, standing rulings, bars, the form of the final report — all still in force),
4. `$W/scratch/s507p/STOP.md` (what is built, what is owed, the exact commands).

`W=/home/bernt/pcl/.claude/worktrees/agent-a55609aa75faa5780`, HEAD `fcfd7144` = 4 commits on main `6bb94178` (code `08de9e4f`): M1 #2539 `60d9c367`, M2 #2637 `a2d6f1d8`, M3 #2111 `b6690b6b`, M4(a) #2115 WIP `fcfd7144`.  NO bench and NO full bar has been taken.  Task IDs 2720–2739.  You run NOTHING in `/home/bernt/pcl`.

## Step 1 — NOW: the whole-table bench, exactly as STOP.md "Owed 1" designs it
The box was rebooted minutes ago and is idle; the other agent (s507c) does single-process light work only while your reservation stands.  Do NOT rebase first: the variants are built on the `08de9e4f` extraction and the A/B is only valid on one base.
- Write `s507p <HH:MM> 100` to `~/pcl-agent-scratch/s508/BENCH-WANTED` (the s508 directory — not s507's), then `perl "$W/scratch/s507p/waitquiet.pl" 20`.
- BEFORE timing anything: the reboot emptied nothing on disk, but check that every variant STARTS from a saved core (one trivial run per variant with `PCL_SHOW_SBCL=1`, or whatever `bench-multi.pl` does to warm them) — a variant that falls back to source mode, or builds its core inside a timed run, poisons its row.  Say in STOP.md how you checked.
- Run the bench in ONE background command (`< /dev/null`, log `$W/scratch/s507p/bench-all-1.log`, `uptime` before and after), then the three interleaved everyday wall-time runs and the interleaved `cost-onetie.pl` pair.  Delete BENCH-WANTED the moment the last of them ends.
- While it runs: NO other process of yours beyond reading files (no proves, no probes).  Use the time to read tasks #2115 / #2098 for step 3 and to prepare the table script.

## Step 2 — the table, written where Fable can read it at once
Write `$W/scratch/s507p/BENCH-TABLE.md` (Fable waits on that file): one row per bench row with base, ctl (the control band of this run), m12, m3, m4a, each change in %, `uptime` before / after; the everyday wall times; the `cost-onetie` ratio base -> m12.  Then, under a heading `## STOP RULE`, the verdict for M4(a) in one line: PASSES (no common row — strcat, textproc, moo-objs, arrhash, hashcopy, the sub-call rows — worse than 2 % outside the control band, m4a vs m3) or TRIGGERS (name the rows and their numbers).  Under `## M1`, `## M2`, `## M3`: is each lever's gain outside the band, and does any row lose.
- TRIGGERS: `git -C "$W" branch s507p-m4a fcfd7144 && git -C "$W" reset --hard b6690b6b`; M4 stops here; do not start (b) or (c).  Whether a quadratic-to-linear gain on general appends is worth a measured loss on every string store is the USER's decision; Fable carries your table to them.  Continue with step 4.
- PASSES: continue with step 3.

## Step 3 — only if the stop rule PASSES
First rebase (step 4's first bullet), THEN (b) the audit mode and (c) the in-place append on the general `p-.=` path, as the brief's member 4 rules them.  The audit's runs over the gate, the sweep and the everyday corpus are HEAVY legs: they serialize, and s507c's bars and Fable's merge legs go first when they are waiting.
**M3's flagged cost has a candidate cure that exists only if (a) ships — evaluate it under the audit:** today the `:memfh` cell's getter hands every READER a fresh snapshot, so print-then-read per iteration copies the whole buffer each time (your predecessor measured 20k iterations: 0.004 s -> 0.185 s, perl 0.002 s).  With (a)'s chokepoints in place a RETAINING store snapshots for itself, so the getter could hand a non-retaining reader (`length`, a comparison, a match, a print) the handle's live buffer and copy nothing.  Decide by measurement: is every consumer of the getter's value either non-retaining or behind one of (a)'s chokepoints (the audit of (b) is exactly the tool that answers this)?  If yes: ship it as part of member 4, with `probes/m3-memfh.pl` still identical to perl in all 14 shapes and `probes/m3-time.pl` re-measured.  If no, or if the stop rule triggered: M3 ships as it is — it repairs a silent wrong and a hash-table corruption — and its cost is FLAGGED with numbers that say what a user would see: besides the 20k figure, the bounded-buffer shape (print a line, test `length($buf) > 65536`, flush and reset when true; 200k lines) on base, tree and perl.

## Step 4 — the rest of the round, on CURRENT main
- `git -C "$W" rebase main` (main = `16cf9642`, code `fee16466`, gen v2-4080), KEEPING BOTH sides.  The fix batch s507b changed code your members sit beside: `local` on a tied scalar goes through the tie (near `box-set`'s general arm — your M1 and M4(a) edit that function), `lib/English.pm` now TIES the separator aliases and `$|` (SCALAR ties, live in every program that says `use English`), `p-split`, `close ARGV`.  After the rebase: re-run its guards (`prove Pl/t/english-01.t Pl/t/transpile-test-10.t Pl/t/script-cache-01.t`) with yours, and answer one question for M2 by measurement: **does a live SCALAR tie (i.e. `use English;` alone) make an untied CONTAINER pay the census** — `cost-onetie.pl`'s loop with `use English;` and no container tie, on `fee16466` and on your tree.  If it does on your tree, M2's refusal must cover it; if it does on the base only, say so (it is part of M2's gain).
- `tools/bench-exec.pl` has a new row on main (`localvar`); the final whole-table bench includes it.
- M5 (the fibret flag): one bounded hour, as the brief says.
- Records (`## s507p` in docs/DECIDED.md and `## Session s507p` in docs/session-log.md, placed as s508's SHARED-BOX says; the round's section in `docs/faster-codegen-suggestions.md`; ir-spec for M3's and — if it ships — member 4's normative statements).
- The full bars of the brief on the final rebased tree (`corpus-diff` / `emission-ab` reference `fee16466`), and the final whole-table bench on that tree under a new BENCH-WANTED reservation.
- If `~/pcl-agent-scratch/s508/MAIN-READY-1` appears (s507c's PART ONE merged: it adds a UNIT notion to the runtime's load path and touches the phase model), rebase across it as SHARED-BOX says before your final bars.

## Box etiquette for this session
After step 1 RELEASE the box and leave it for about 75 minutes of other people's heavy legs (s507c's PART ONE bars, then Fable's merge legs) before you reserve it again; steps 2–4's light work (the table, the rebase, single proves, probes, the audit-mode code) proceeds meanwhile.  Keep `$W/scratch/s507p/STOP.md` current at every step and commit a WIP checkpoint after every member — the session can be cut at any time.

## Final report (SHORT)
As the original brief's "Final report", with: `MERGE-READY: <sha>` (or `NOT MERGE-READY` + the exact reason) as the first line of STOP.md; your model id; the stop-rule verdict and its rows; what was done about M3's flag and the numbers; the answer to the `use English` question; everything FLAGGED; anything you could NOT do, said plainly.
