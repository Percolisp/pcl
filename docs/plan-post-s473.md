# Plan: the queue after the s472 restart (s473, Fable, 2026-09-06)

**USER (2026-09-06): "Just keep starting new subjobs for the tasks and plan for them."**
Standing rules it runs under: TWO execution subjobs at a time (USER 2026-09-05); put things
LIVE before new work; Fable never executes what an Opus agent can do from a written design;
every round = one perf agent + one or two correctness agents (plan-post-s433 §s452).

Briefs live in `~/pcl-agent-scratch/s473/<label>/prompt.md` (the standing rules for all of them:
`~/pcl-agent-scratch/s473/COMMON.md`); the s470-era briefs still to launch are under
`~/pcl-agent-scratch/s470/` (BU, BZ, s1262).  A brief's `__MAIN__`/`__GEN__`/`__MYGEN__` are
filled at launch into `prompt.launch.md`.  An agent reports by writing `MERGE-READY: <sha>` as the
first line of its `scratch/<label>/STOP.md`; Fable gates the sha in a throwaway worktree
(`git worktree add --detach`), reads the hunks that matter, fast-forwards main, pushes, launches the
successor.

## 1. Where the tree is (2026-09-06 evening)
- main `21f6622c` (gen v2-840) = s470bt (B6 `tools/ir-conform` + the 345-case corpus + B7) and
  s470bx phase 1 (#1042 #1236 #1043 #1231 #1044) merged this session; CI green through `2e39cc67`.
- MERGED after this plan was written: **BX phase 2** (#1237 — the read family's errnos; corpus rows 122-io + 123-io left known-fail at the merge).  In flight: **BU** (perf round 29, `agent-a4ae344882cebc0bc`); **BY** (#1300 #1303 #682 + the
- **s474 (2026-09-06 night, Fable): BY MERGED `0d783d24`** (#1300 #1303 #682 #1338 — the cache surface; `PCL_CACHE_DIR` is a process fact, LAST-USE prune with a daily stamp, 0700 root refused loudly, `pcl --version/--cache-info/--no-cache`, ONE `%p-mtime`; gate 216/7517 xs-only with ZERO write-date signatures = #1338 closed) and **BU MERGED `8a879a57`** (perf round 29: #1250 per-SITE regex/subst/tr op, #1251 the s/// compiled record — json-rt +99 %, subste +62 %, regexg +32 %, textproc +30 %, runtime-only, no gen bump).  In flight: **s473t1** (`agent-af5e2bdefd8089352`) + **s473t2** (`agent-a66c9ca1780b383d5`) — the two slots (t1 was the USER's one-off third while BY/BU finished).
  absorbed **#1338**, `agent-a5d566deb35707e17`).
- Part B of plan-speed-and-ir-s470 is COMPLETE (B1–B7).  Part A continues by rounds.
- The ir-conform corpus's 58 known fails are the correctness worklist's new metric
  (`tools/ir-conform --strict`, the number to watch shrink); their owners by row count:
  #1249 10, #1240 8, #1241 7, #1248 5, #1245 5, #1244 4, #1243 4, #1242 4, #1247 3, #1246 3,
  #1190 3, #221 1, #155 1.
- Found this session: **#1338**, the shared-cache prune race — `p-cleanup-old-cache` stats a file
  a sibling process (or the gate's own `prove -j8` siblings) has just replaced; two Fable gates
  lost 17 rows to it (15/15 and 8/8 alone).  Absorbed by BY.

## 2. The queue (launch order; two slots)
| # | label | what | gen | after |
|---|---|---|---|---|
| 0a | **s473t1** (IN FLIGHT s474, gen v2-960) | TRIAGE (USER, 2026-09-06 "soon"): the 13 PARTIAL files that stop early UNEXPLAINED — one cause per file, fix fillers, file the rest (`s473/s473t1`) | if a fix | a slot, FIRST after the in-flight pair |
| 0b | **s473t2** (IN FLIGHT s474, gen v2-970) | TRIAGE (USER, "soon"): the 105 blessed failure keys with no cause (pack.t's 47 EXCLUDED — USER: skip pack.t) + the 60 bug-notes without a task — cluster, probe, attribute, fix ≤1h clusters (`s473/s473t2`) | if a fix | a slot, right after s473t1 |
| 1 | **BU** | perf round 29 — **MERGED s474 `8a879a57`** (#1250 + #1251; two levers, round closed early: put things LIVE) | none (runtime-only) | done |
| 2 | **BZ** | #1302 + #1304 install layout + installer tests, podman legs (`s470/s470bz`) | none | BY merged |
| 3 | **#1262** | `docs/caching.md` for users — Sonnet, `model: "sonnet"` (`s470/s1262`) | none | BY merged |
| 4 | **s473a** | the `local` family: #1243(a)(b)(c) = #1192 + #1260, #1190 — 7 corpus rows | v2-870 | a slot |
| 5 | **s473b** | the nested-element family: #1241 = #1150 (read vivifies), #1151, #1152, #1010 — 7 rows; bench-gated | v2-880 | a slot |
| 6 | **s473p** | perf round 30: #1187 (the non-simple operand in the regex scan), #1200 (string evals on disk), the single-array foreach lever | v2-890 | BU merged |
| 7 | **s473c** | #1240 die LOCATION — sized first (register vs cold-branch constant), shipped by the ≤1 %/≤3 % rule — 8 rows | v2-900 | s473b merged (both touch the accessor emission) |
| 8 | **s473d** | numeric representation: the CRASH pair #1245 + #1230, #1248(a)=#1191, #1248(b), #1012, #1248(c) — 10 rows | v2-910 | a slot |
| 9 | **s473e** | loop control: #1244(a)=#1161, (b), (c), #1164 — 4 rows | v2-920 | a slot |
| 10 | **s473i** | product bugs: #1060 `--executable`, #1284 `-I` to the module transpile, #1119 content-keyed module cache | v2-930 | BZ + s473p merged (cache + `pcl` regions) |
| 11 | **s473h** | #1249's seven singletons (+ #1290), and #1242/#1117 SIZED (not designed) | v2-940 | a slot |
| 12 | **s473f** | the handle family: #1309 #1246 #1307 #1308 (representation measured first) #1233 #1220 #1074 #1247(b) — 4 rows | v2-950 | a slot |
| — | Fable designs | #1117 typeglob slots (from s473h's sizing); #582 `@ISA`-write invalidation (USER: box magic?); #71 PCRE2 (from s473p's measurement); #1202 extension fasl (USER-parked with pack) | | |

Task ID ranges reserved: BZ 1325–1334, #1262 1335–1337, s473a 1339–1348, s473b 1349–1358,
s473c 1359–1368, s473d 1369–1378, s473e 1379–1388, s473h 1389–1398, s473i 1399–1408,
s473p 1409–1418, s473f 1419–1428, BU 1250–1259 (old range), **s473t1 1431–1445, s473t2 1446–1460**.  Next free after these: **1461**.

## 3. Standing decisions made this session (also DECIDED §s473)
- A baseline `.tsv` is BINARY: edit through `:raw` handles only.  s470bx's row-removal script
  read the companion fail baseline through a UTF-8 layer and rewrote 25 unrelated rows' Latin-1
  bytes as U+FFFD — the descriptions are join keys, so those rows would have become unpairable.
  Caught by `git diff main -- baselines/*.tsv | grep -a '^[-+]' | cut -f1 | sort | uniq -c`
  (a file that both loses and gains the SAME count of rows is the signature) and a byte `cmp`
  of the untouched rows against main.  Both are now part of Fable's merge read.
- A MERGE-READY sha is gated in a throwaway detached worktree when the agent keeps working in
  its own (a gate over a tree being edited is neither a gate nor safe).
- Rows that die with `Failed to find the WRITE-DATE of ~/.pcl-cache/modules/…` are #1338, not a
  regression: re-run alone, cite the task.  Until BY lands, every gate on this box may lose a
  file to it.
- The perf line's next rounds are ordered by §A.4.1/§A.4.3, not by A.2's guesses (s470bn's
  finding); the correctness line's metric is the corpus's known-fail count plus the sweep and
  companion totals.

## 4. Owed to the box (Fable, once quiet — which under continuous subjobs means "in a lull")
- `tools/run-perl-suite.pl --all --quick --jobs 1` over BV + s471a + BT: STARTED this session on
  main `637fc58e` (log in the session scratchpad); read its NOT-RUN and ROW DIFF blocks.
- The quiet-box bench re-measure (BN/BP rows + #1188 constants).
- A full `--all --jobs 4 --bless-stamps` once per round (I4).
