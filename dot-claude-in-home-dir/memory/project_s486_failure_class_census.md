---
name: project_s486_failure_class_census
description: "s486 (2026-09-16) — the \"how many fails are not-supported\" question, the measured split per population, the skip-registry duplication, and the three agents launched on it"
metadata: 
  node_type: memory
  type: project
  originSessionId: c1ffe70f-09c3-4286-a64e-45293270c704
  modified: 2026-09-16T07:50:01.226Z
---

**The USER's question (2026-09-16, README `Measured` review):** how many of the
failing rows (sweep / perl's t/ / CPAN board) are from what PCL deliberately
does not support.  Answer: known PER ROW (every blessed fail carries a CAUSE,
`tools/lib/PCLCauses.pm`), never SUMMED — nothing in STATUS.md or any runner
printed the split until s486a.

**Measured by Fable from the checked-in baselines (sweep log `.faillog` dated Sep 6, 642 fails):**
- sweep: 267/642 cite a not-supported.md section (`NS:` anywhere), 69 PARKED pack/unpack, 232 task-only, 67 other (DECIDED notes), 7 rows absent from the baseline.
- companion `perl-suite-fails.tsv`: 4,661/11,984 NS, 87 parked, 4,874 task, 2,362 causeless (2,335 op/).  XDIFF 105 files / 2,517 rows are NS by construction.  Shortfall t/ half: 28 files 51,890 rows NS; 47 files 522 rows UNEXPLAINED.
- board: 88/389 NS, 294 task (109 = #233 caller), 7 PERL-SKIP.
- **The skip registry (`cl/skip-registry.lisp`, 103 patterns / 24 files) relabels ≈180 rows per run as `ok # skip` — OUTSIDE the 649 — and no run records that count; 19 REGISTRY-STALE entries had accumulated.**  Two mechanisms (registry vs `NS:` cause) own one fact.

**Ruling (s486, in the briefs `~/pcl-agent-scratch/s486/s486{a,b}/prompt.md`, recorded by the agents in DECIDED `## s486a/b`):** six cause classes decided by ONE function `PCLCauses::cause_class` (`NS:` anywhere ⇒ not-supported even with a task; PARKED separate; PERL-SKIP out of the denominator; `other` = hygiene list); a registry-relabelled row is a not-supported FAILURE the fail count omits, so the sweep gets a measured `registry_skips` column and prints it beside the fail count; the registry STAYS until the USER decides.

**OPEN USER QUESTIONS — ASK AT THE START OF THE NEXT SESSION (USER 2026-09-16):** (1) retire the registry in favour of the cause column (Fable: yes, after the tag; headline 649 → ~830)?  (2) Is #233 caller fidelity not-supported or queue?  (3) README companion figures are Sep 4 (Sep 14 snapshot: 107 identical / 105 registered / 264 queue).  (4) USER: "is the 30-day cache prune expensive and run at every startup?" — answer ready: NO, it runs only on a cache MISS and scans at most once a day (`.last-prune` stamp), one walk of modules/+evals/+proto/ (~1,200 files); one utime per entry per day keeps last-use fresh (`cl/pcl-runtime.lisp` ~19884, ~21093).

**s486a SHIPPED (main `13ce14ed`, 2026-09-16):** `PCLCauses::cause_class` (the rule, DECIDED §s486a), `tools/cause-census.pl --markdown|--hygiene`, `docs/failure-cause-classes.md`, the table in STATUS.md.  First measurement: sweep 635 rows = 268 NS / 69 parked / 291 bug / 7 other (53.1 % NS+parked); companion 4,661 / 88 / 4,873 / 2,362 unexplained (39.6 %); board 88 / 0 / 294 / 7 perl-skip (23.0 %).  The registry column reads NOT COUNTED until s486b's sweep column exists — re-run after a sweep on the merged tree.

**How to apply:** the number the README may cite comes from `tools/cause-census.pl` — never from a hand count; STATUS.md carries the breakdown, README.md/README.proposed.md are the USER's.  Related: [[project_pcl_measurement_traps]], [[feedback_check_for_a_second_copy]].
