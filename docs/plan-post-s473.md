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
| 0a | **s473t1** — **MERGED s474 `9e938813`** (gen v2-960; three compiler fixes, five baselines edited row by row, #1431–#1445) | TRIAGE (USER, 2026-09-06 "soon"): the 13 PARTIAL files that stop early UNEXPLAINED — one cause per file, fix fillers, file the rest (`s473/s473t1`) | if a fix | a slot, FIRST after the in-flight pair |
| 0b | **s473t2** — **MERGED s474 `278d5eb8`** (baselines+docs only; 152 causeless rows attributed, #1446–#1460; CAUSES 0) | TRIAGE (USER, "soon"): the 105 blessed failure keys with no cause (pack.t's 47 EXCLUDED — USER: skip pack.t) + the 60 bug-notes without a task — cluster, probe, attribute, fix ≤1h clusters (`s473/s473t2`) | if a fix | a slot, right after s473t1 |
| 1 | **BU** | perf round 29 — **MERGED s474 `8a879a57`** (#1250 + #1251; two levers, round closed early: put things LIVE) | none (runtime-only) | done |
| 2 | **BZ** (IN FLIGHT s474b, launched 2026-09-07 07:30, worktree `agent-a18594e226bec1266`) | #1302 + #1304 install layout + installer tests, podman legs (`s470/s470bz`) | none | BY merged |
| 3 | **#1262** | `docs/caching.md` for users — Sonnet, `model: "sonnet"` (`s470/s1262`) | none | BY merged |
| 4 | **s473a** — **MERGED s475 `bb057f33`** (gen v2-1020; member 0 = #1513 CLOSED: the 64-bit boundary DOCUMENTED, numconvert.t registered XDIFF, guard int-boundary-01.t; #1243(a) `local($x) = LIST` is a LIST assignment, narrowed to scalar targets; #1260 a symbolic `local` assigns its initializer; #1192 + #1190 NOT attempted, each now carries a measured design (#1190 blocked on #1150 = s473b's #1241); filed #1339–#1341) | the `local` family: #1243(a)(b)(c) = #1192 + #1260, #1190 — 7 corpus rows | v2-870 | a slot |
| 5 | **s473b** — **MERGED s477 `3b153d7f` — ALL FOUR MEMBERS SHIPPED** (gen v2-1030; #1151 the nested `=` writes THROUGH the slot; #1241 = #1150 the READ path vivifies every intermediate via the `p-viv-container` marker + the existing chain walker, closing #1456 and the 7 ir-conform autoviv rows; #1152 an element compound assign evaluates container and KEY once, +0.55 % on its own bench row; #1010 a SLICE vivifies where its CONSUMER aliases it, ONE rule and five consumers incl. the `p-viv-slice` marker.  Sweep 0/0/0 TOTAL 18673, for.t 131 → 130 by EDIT because perl's own RT #2166 TODO row now fails as it does under perl.  Filed #1349–#1353) | the nested-element family: #1241 = #1150 (read vivifies), #1151, #1152, #1010 — 7 rows; bench-gated | v2-880 | a slot |
| 6 | **s473p** — **MERGED s475 `bfa11c72`** (gen v2-990; #1200 the string-eval DISK cache: `use JSON::PP; print 1` 1.13 → 0.41 s; #1409 ONE bare array is a foreach run: feread 0.47× → 0.30×; #1182 → `%p-array-grow-discarding`: listcopy +27 %, slices +20 %; #1411 filed; #995 open with one sized lever) | perf round 30 (USER 2026-09-07 "other speed tests first"): #1200, the single-array foreach lever, #995+#1056 loop-body box reuse, #1182 storage adoption — #1187 HELD | v2-890 | BU merged; the next perf slot |
| 7 | **s473c** | #1240 die LOCATION — sized first (register vs cold-branch constant), shipped by the ≤1 %/≤3 % rule — 8 rows | v2-900 | s473b merged (both touch the accessor emission) |
| 8 | **s473d** — **MERGED s477 `0c90e681`** (RUNTIME-ONLY, **no gen bump** — corpus-diff identical over 111; all five members shipped: #1230+#1245 the :DUAL marker, #1191=#1248(a) pp_modulo's two regimes, #1248(b) pp_pow's branch rule + the pack-impl `2**N` dependency, #1012 the %.15g rounded-exponent switch + sprintf's `%g` sibling, #1248(c) read-only storage = adjustable-without-fill-pointer; ten ir-conform rows left known-fail BY EDIT; filed #1369 the IV/NV residue and #1370 the string-side scalar collapse) | numeric representation: the CRASH pair #1245 + #1230, #1248(a)=#1191, #1248(b), #1012, #1248(c) — 10 rows | none | done |
| 9 | **s473e** | loop control: #1244(a)=#1161, (b), (c), #1164 — 4 rows | v2-920 | a slot |
| 10 | **s473i** | product bugs: #1060 `--executable`, #1284 `-I` to the module transpile, #1119 content-keyed module cache | v2-930 | BZ + s473p merged (cache + `pcl` regions) |
| 11 | **s473h** | #1249's seven singletons (+ #1290), and #1242/#1117 SIZED (not designed) | v2-940 | a slot |
| 12 | **s473f** | the handle family: #1309 #1246 #1307 #1308 (representation measured first) #1233 #1220 #1074 #1247(b) — 4 rows | v2-950 | a slot |
| 13 | **s473q** — **MERGED s476 `c4304834`** (docs-only: `docs/pcre2-spike-s473q.md`; the stop rule KEPT cl-ppcre — without JIT PCRE2 loses `subste` 1.188×, with JIT it loses `regexg` 1.134×; the strong case is PARITY +311/−7 re_tests rows and PCRE2 answers #196's 18 hangs; BLOCKER = the four images ship PCRE2 10.39/10.42/10.42/10.46 and there is no macOS CI leg → #71 stays PARKED, a USER decision; #1187 CLOSED unbuilt — no non-simple operand, cl-ppcre declaims `*string*` as the union `simple-string`; #1461 filed and RULED s476: a 256-way HASHED BMH skip table, version-pinned shadow, loud refusal; Fable filed #1528 = translator-vs-engine census of the 311) | THE REGEX ENGINE SPIKE #71 (PCRE2 via sb-alien: measurement + binding stop rule + portability record; USER: approved, NOT high priority) | v2-980 (only if member 2 ships) | AFTER s473p and one more non-engine perf round (the remaining S/M levers) |
| 13a | **s473r** — **MERGED s475 `5d59e447`** (perf round 31, RUNTIME-ONLY, no gen bump: #1514 the 68 ns were `int()`/`/`/`%` not `+` — `arith` −53 %, `use integer` −76 % (now FASTER than the plain loop), collatz −26 %; #1189 the package preamble stops re-running `defpackage` on an existing package — moo-objs 2.00 → 1.22 s, 54× → 29.6× perl; #986 SETTLED = #900's overload guard, 15.1 %, filed #1516; #883's arm re-measured 8.5 ns/elem → #1517; #1142 PROMOTED (a read-only grep costs the raw run 30.6 %); #1141 closed by a 65-file scan; #1518 the defclass half; #1515 the >2**53 divergences; #813/#924 closed) | perf round 31 = the LAST non-engine round before s473q: #1514 both halves (`use integer` as a FACT; the 73-vs-5 ns op-dispatch profile), #986 step 2 (the rounds-15–18 bisect), #1141+#1142, #883's open arm, #1189's breakdown instrument → A.2 row 7 sized or filed; #813 + #924 CLOSED as measured-declined | v2-1030 | the PERF slot when s473p merges; s473q after it |
| 13b | **s473s** (brief `s473/s473s/prompt.md`, written s476) | perf round 32 = THE METHOD-CALL ROUND (USER s476: method calls stay visible on the speed table, #1527): `methret` 1.51× profiled FIRST, then the lever the profile names — the OWN-CLASS half of #582 (an EQ-keyed per-class cache stamped by a global sub-generation; the inherited half stays blocked on the `@ISA` marker) or a fixed-arity `p-method-call-N` Kind-A entry (the per-call-SITE cache stays REJECTED, s444); #1518 the `p-defclass` no-op (moo-objs); #1461 as ruled; #1516 if time | v2-1060 if an emission changes | the PERF slot: the next free slot after s473d launched (s476) |
| 13b | **#1262** — **MERGED s475 `7c4f302f`** (Sonnet; `docs/caching.md`, the USER-facing page on the four cache layers incl. the s473p eval disk cache; filed #1335 = `pcl --cache-info` does not list `evals/`) | prose | done |
| 0c | **s473t3** | FILLER: the 79 blessed rows whose only cause is a `CATALOG` note get TASKS (one perl-probed reproducer per cluster, ~25 clusters; ≤2 one-hour fixes may ship) — `s473/s473t3/prompt.md`; brief WRITTEN s474b, launches on the next free slot | v2-1000 only if a fix ships | a slot |
| 0d | **#1501** (label s473t4; brief NOT yet written — Fable writes it when the s473a–f rounds have consumed #1431–#1460, USER 2026-09-07 "after those bugs are fixed") | the companion suite gets the sweep's two triage checks: one cause per early-stopping file (188 files produce fewer rows than perl) + a CAUSE column on `baselines/perl-suite-fails.tsv` (18,340 rows / 265 files, none attributed); first brief = the cluster census + per-directory split; re/ after s473q | if a fix | after s473a…f; re/ after s473q |
| 0e | **#1061** — **MERGED s475 `46ede39d`** (label s1061; gen v2-1010; #1505 `import Foo::Bar` bare designator = a whole file unreadable, #1508 `$ref->{k} =~ s///` wrote to a VALUE; nine down-movers each bisected, #1506–#1512 + #1525 filed; board `cpan-board14-s474.tsv` 79/54/50, 2190/346, exactly four SLU rows moved vs s467, all explained) | FILLER (USER 2026-09-07 "Sounds good"): the CPAN board's nine unattributed DOWN-movers bisected + a fresh board snapshot on main with every mover since s467 attributed | if a fix | the slot after s473t3 |
| 0e′ | **#1513** | FILLER (USER 2026-09-07 "Sounds good"): DOCUMENT the missing 64-bit integer boundary as ONE not-supported entry (folds two old ones), register `op/numconvert.t` as an expected divergence (1446 rows leave UNEXPLAINED), ir-spec paragraph, guard row asserting the documented answers | none | after #1061 (same filler slot; ~1 h) |
| 0f | **#1502** | the board gets the sweep's treatment: a ROW-level fail file with a CAUSE column, one probe per cluster, ≤1 h clusters ship, CAUSES line in the board diff | if a fix | #1061 merged |
| 0g | **#1503** | SCORE `cpan-tests/modules/` (Test-Simple, Text-CSV, YAML-Tiny, easy-targets) — a drop-census population today, never scored; decide Test-Simple's standing first | if a fix | any slot (filler) |
| 0h | **#1504** | WIDEN the board with on-disk pure-Perl dists in rounds of 2–3 (Moo + Getopt-Long first; Path-Tiny, Test-Deep; Pod-Simple, Math-BigInt, Memoize, HTTP-Tiny/-Message) + App-cpanminus as a real-program target | if a fix | #1502 merged; one round per slot |
| — | Fable designs | #1117 typeglob slots (from s473h's sizing); #582 `@ISA`-write invalidation (USER: box magic?); #71 PCRE2 (from s473p's measurement); #1202 extension fasl (USER-parked with pack) | | |

Task ID ranges reserved: BZ 1325–1334, #1262 1335–1337, s473a 1339–1348, s473b 1349–1358,
s473c 1359–1368, s473d 1369–1378, s473e 1379–1388, s473h 1389–1398, s473i 1399–1408,
s473p 1409–1418, s473f 1419–1428, BU 1250–1259 (old range), **s473t1 1431–1445, s473t2 1446–1460, s473q 1461–1470, s473t3 1471–1500, #1501 = the companion triage (s473t4), #1502–#1504 = the board work, s1061 1505–1512, #1513–#1514 filed s474b, s473r 1515–1524 (used 1515–1518; s473r bumped NO gen — v2-1030 is free for s473b), #1525 filed by s1061, #1526 filed s475; s473a used 1339–1341; #1527–#1529 filed s476; s473s 1529 was taken by #1529 → s473s uses 1530–1539**.  Next free after these: **1540** (#1526 = the element-access predicate filler, filed s475; #1527 = USER ask s476: the `methret` + `moo-objs` rows join the README speed table at its next quiet-box refresh; #1528 = the translator-vs-engine census of the 311 parity rows, for the #71 decision; #1529 = USER ask s476: `$ENV{_PCL_RUNTIME_}` so Perl code can detect PCL, synthetic in %ENV so children never inherit it, documented — appended to the s473h brief).

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
- **s474 (USER): ONE regex engine at run time, never two; the #71 spike BEFORE #1187; approved 2026-09-07 ("Make it so") but NOT high priority — "do other speed tests first" (s473q after s473p + one more perf round); and a platform-touching change (an engine, a foreign library, an installer step) ships only with the install matrix green WITH it plus a macOS leg — "we are not writing software for our laptop".**  Rulings in DECIDED §s474.
  finding); the correctness line's metric is the corpus's known-fail count plus the sweep and
  companion totals.

## 4. Owed to the box (Fable, once quiet — which under continuous subjobs means "in a lull")
- `tools/run-perl-suite.pl --all --quick --jobs 1` over BV + s471a + BT: STARTED this session on
  main `637fc58e` (log in the session scratchpad); read its NOT-RUN and ROW DIFF blocks.
- The quiet-box bench re-measure (BN/BP rows + #1188 constants).
- A full `--all --jobs 4 --bless-stamps` once per round (I4).
