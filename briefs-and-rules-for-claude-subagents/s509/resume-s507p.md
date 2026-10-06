# RESUME s507p (s509) — perf round 39: every member is built; the in-place `.=` cell is RATIFIED; owed are the bars, the final whole-table bench, the fibret hour and the records

You are an EXECUTION agent (Opus 5.5) on the PCL project (Perl -> Common Lisp transpiler), a FRESH agent resuming a batch that two earlier agents checkpointed.  Read, in this order, through main's checkout:
1. `/home/bernt/pcl/briefs-and-rules-for-claude-subagents/s509/SHARED-BOX.md` (this session's protocol; its FIRST ACTION — your model id into `$W/scratch/s507p/MODEL.txt` — comes before anything else; heavy legs and benches go through `heavy.sh`),
2. `/home/bernt/pcl/briefs-and-rules-for-claude-subagents/s473/COMMON.md` (the rulebook),
3. `/home/bernt/pcl/briefs-and-rules-for-claude-subagents/s507/s507p-prompt.md` (the batch: members, the stop rule, bars, the form of the final report) and `s508/resume-s507p.md`,
4. `$W/scratch/s507p/STOP.md` — its top block ("s507p RESUME RECIPE" … "Probes") is the state; everything below "previous state" is history.  `$W/scratch/s507p/BENCH-TABLE.md` has the round's table and the stop-rule verdict (PASSES, final).

`W=/home/bernt/pcl/.claude/worktrees/agent-a55609aa75faa5780`, HEAD `c22b831b` = `16cf9642` + 16 commits, code final at `b74d4938`.  Main is `4fb7fa26` = `16cf9642` + NON-CODE commits.  Task IDs 2720–2739 (2720–2722 used).  Runtime-only batch: no generation string.  You run NOTHING in `/home/bernt/pcl`.

## RULED (Fable, s509): M4(c)'s magic cell is RATIFIED
The growable buffer stays behind the `:strbuf` magic cell (M3's mechanism), not as a growable string in the slot: the read side already has ONE chokepoint (get-magic, which a tied scalar needs anyway) and the store side has none (`my $t = $g` is a direct assignment), so the cell is the design that can be correct.  Measured by the reviewing session on your tree `b74d4938` against main and perl (`~/pcl-agent-scratch/s509/review/sb/sb.pl SECTION N`, driver `time.pl 30000 perl=perl main=… tree=…`; N = 30000 appends to a hash element, wall seconds incl. start-up):

    section       perl    main    tree
    build         0.01    1.58    0.05     append only
    match         0.01    1.60    0.06     build, then a //g loop over it
    substr        0.01    1.61    0.06     build, then 30000 substr reads
    index         0.01    1.61    0.05
    copy          0.02    1.68    0.05     build, then 2000 x `my $t = $h{s}`
    split         0.01    1.10    0.06
    eq            0.01    1.86    0.06
    print         0.04    2.48    0.86     2000 prints of the 289 kB string
    app-len       0.01    1.99    1.66     append + length($h{u}) per iteration
    app-substr    0.01    1.88    1.53     append + substr($h{u}, -2, 1) per iteration
    app-match     0.09   11.05   10.80     append + =~ /9\n\z/ per iteration

No shape is slower than main; a read after the build is one cached snapshot.  Two things follow:
- **ONE BOUNDED ADDITION (one hour, its own commit, a guard row that fails before it):** `app-len` — `length` of an ELEMENT cell still pays a snapshot per append, although `85060938` cured `length` of a SCALAR cell.  If the element fetch that `length(ELEMENT)` compiles to can reach the same live-length arm with one more clause (no new mechanism, nothing on the path of a plain element), add it and show the row; if it needs more than that, stop and write what it needs into #2722.  `app-substr` is #2722 as filed — do not start it.
- `app-match` is NOT yours: 11 s on main and on the tree is the regex engine scanning a growing string from its start for a pattern anchored at its end (perl 0.09 s).  File it in your ID range with the reproducer (`sb.pl app-match 30000`) and the measurement that discriminates it (the same loop with `substr($h{u}, -2) eq "9\n"` in place of the match) — one task, no work.

## Owed, in order (your STOP.md's list, with this session's protocol)
1. `git -C "$W" rebase main` (no code change); `env -C "$W" prove Pl/t/perf-levers-08.t Pl/t/pcl-doc-examples-01.t`.
2. The bounded `length(ELEMENT)` addition above (or its note) — BEFORE the bars, so the bars run once on the final code.
3. The bars on the final tree, each heavy one through `heavy.sh s507p leg <log> …` (your `bars.sh` waits by itself: take its `leg` lines apart and queue them one by one; `waitheavy.pl` is retired).  The gate and corpus-diff logs on `b74d4938` stand ONLY if step 2 changed no code — otherwise re-take both.  Owed regardless: emission-ab, the sweep (GATE clean; TOTAL against 18731; drops 5), ir-conform, the CPAN board before / after (`cpan-scoreboard.pl --diff …` as your STOP.md spells it), the quick companion (`--all --quick --jobs 4`; splice and explain every mover), everyday BEFORE (main's 114 of 122) → AFTER.  Run the gate and the sweep with `PCL_STRBUF_AUDIT=<file>` and READ every audit log: a line is a producer handing out a non-simple string — fix it at the producer.
4. The FINAL whole-table bench on the final tree and `fibret.sh` (M5), both as `heavy.sh s507p bench <log> <script>` (about 11 + 10 minutes; `uptime` beside every number, the control band stated).  Write `docs/faster-codegen-suggestions.md` §0.2w "Round 39 movers" (base → tree per row; what shipped; flags #2720, #2722 and the new regex task).
5. Records: `## Session s507p` and `## s507p` (the draft's audit line corrected to what the logs say; the ruling above; the review table), ir-spec as already written; close #2115 with a DONE section; `tools/everyday-smoke.pl --record` LAST (through heavy.sh), committed.
6. `MERGE-READY: <sha>` as the FIRST line of STOP.md only when every bar's log is on disk and newer than the last code commit.  If you have a SendMessage tool, send one line to `main`: `s507p MERGE-READY <sha>`.

## The reviewing session's probes still to come
Before the merge Fable probes the reused line buffer (two handles read interleaved, a line kept in an array across later reads, a read inside a sort / map block that itself reads, lines longer than 64k, `$/` undef / "" / a reference / a multi-character string, list-context readline, chomp and `.=` on a line just read) and the in-place `.=` (the aliasing shapes of the brief: a copy taken before and after the cell is created, foreach / map / sub-argument aliases, references, `local`, a string eval's capture, a tied target, `substr` / `vec` / 4-argument substr lvalues, `pos`, sort keys, hash keys).  A finding reaches you as a message naming a probe file; your own `probes/c-alias.pl` (50 rows) is the starting point, not a substitute.

## Beside you
**s507c** (the phase model: `%p-load-unit`, feature bundles, the ARGV family, `$^X` at start-up) runs now and merges its PART ONE first — a small runtime diff; rebase across it when `~/pcl-agent-scratch/s509/MAIN-READY-<N>` appears, KEEPING BOTH sides, and re-take only the gate, the sweep, corpus-diff, everyday and the final bench.  **s508a** takes the next free slot.

## Final report (SHORT)
As the original brief's "Final report", with: STOP.md's first line; your model id; the final table's movers with the control band; the `length(ELEMENT)` outcome; every audit log's line count; everything FLAGGED (added complexity, a slow-down, a row that got worse); anything you could NOT do, said plainly.
