# E4.1 step 4 — full verification (#244), measured s357

Verification of the shipped flip (`1e891d0`, #242) at HEAD `c250ab6`,
gen v2-112.  Plan: `docs/v2-endgame-plan.md` §5 step 4; guardrails
`docs/v2-opus5-execution-plan.md` §5a.8.

## 1. The quadruple — GREEN

| check | result |
|---|---|
| `tools/corpus-diff.pl` | emission **identical to HEAD across 111 files** |
| `tools/prove-core` | **131 files / 4658 tests, PASS** |
| full sweep + `tools/sweep-diff.pl` | **GATE clean** — 0 new / 0 fixed; TOTAL passing **18499 = baseline (+0)**; current 677 fails vs 679 blessed |
| `perl tools/v2-census.pl` | **111 v2-native / 0 gated** |

Sweep detail: 2 UNSTABLE (postfixderef.t, ref.t — new fails *above* their
abort points) + 4 unverified (did not run) are the standing crash-file
noise `CLAUDE.md` already documents.  min MemAvailable 6.1 GB, so no
load-noise caveat (#215).

## 2. The full perl suite — the finding: 28 files lost to the flip

Run per-dir (`tools/run-perl-suite.pl --dir D --jobs 8`, 11 dirs,
523 rows — the same file set as `baselines/perl-suite-run.tsv`).

**93 rows moved vs the snapshot.**  Most is 13 sessions of ordinary drift
(the snapshot is `# taken-at: 1e7c4d7`, s323e/s325, 2026-08-02), including
three GAINS — `op/push.t`, `op/splice.t`, `op/unshift.t` all DIFF → **OK**.

The one that matters is a new status: **29 files are now TRANSPILE-FAIL**,
and **28 of them transpiled at the pre-flip commit via v1**.

### 2.1 The attribution is measured, not inferred

A `git worktree` at `26ce393` (s355, the commit before the flip) was asked
for the pipeline marker of all 29:

```
$ ./pl2cl t/io/through.t | head -1
;;; pcl: pipeline=v1 gen=v2-111
```

**28 of 29 print `pipeline=v1`.**  They were live v1 file-level routes; the
flip removed the fallback, so each is now a hard `Parser2 TODO:` error.
That is the flip working as designed — a v2 gap is no longer a silent
re-transpile through a second compiler — but in a population that was never
audited.

The 29th, **`op/for.t`, is NOT a flip loss**: pre-flip it already produced
no output (`Failed to parse file: … Pl/Parser.pm line 786` — a PPI parse
failure).  It regressed somewhere between the snapshot and s355 and is a
separate, pre-existing item.

### 2.2 Why the suite was missed

`fable-answers-s352.md` §1 amended §5a.2 to define the audit populations as
**sweep + CPAN board + the Pl/t gate**, and the step-2 precondition as zero
TODO-class events across those three.  That amendment was itself written
because "the s342c audit measured sweep + board and never `Pl/t/`".

The perl suite (`tools/run-perl-suite.pl` over the perl build's `t/`) is a
**fourth** population and was not in the list.  The same class of miss, one
population further out.  Nothing was violated — the precondition as written
was met — but the list was incomplete.

### 2.3 Cost

**15,129 previously-passing PCL rows** across the 28 (sum of the snapshot's
`C_ok`; they also carried 1,307 not-ok, and perl runs 20,431 rows in them).
The single largest is `io/through.t`, which was **OK at 942/0**.

### 2.4 The 28, by family

Every reason is the first line of `./pl2cl <file>` at HEAD.

| # | family | files |
|---|---|---|
| 9 | **capture / package-spanning lexical** | `file lexical captured by sub`: io/through.t, op/attrproto.t, op/getppid.t · `my-lexical spans a package boundary`: io/shm.t, op/exec.t, op/sub_lval.t, op/svleak.t, op/taint.t · `possibly captured by nested sub`: re/reg_eval_scope.t |
| 4 | **poisoned condition-`my`** | op/my.t, op/while.t, re/pat_advanced.t, re/regexp_unicode_prop.t |
| 4 | **unsupported `our` declaration** | comp/our.t (`our` shadows a my-lexical), op/inccode.t (`our $count++`), op/repeat.t (`our $Tiecount++`), re/opt.t (`our \$TODO = \$::TODO`) |
| 3 | **unsupported `my` declaration** | op/attrs.t + uni/attrs.t (`my ($c, @g, %b) : attr = …`), opbasic/cmp.t (`my @raw, @upgraded, @utf8;` — the #138 depth-0 comma-tail shape) |
| 2 | **self-referential my-init** | re/pat_special_cc.t, run/runenv.t |
| 2 | **`foreach` without list** | op/const-optree.t, op/for-many.t |
| 2 | **state** | op/coresubs.t (state in named sub, string eval), op/lvref.t (non-scalar state outside a named sub) |
| 2 | **singles** | op/try.t (`compound 'try'` — an unimplemented feature, not a gap) · op/lexsub.t (dies in v1's still-live *expression seam*: `Negative repeat count does nothing at Pl/Parser.pm line 8642` — the file-level fallback used to absorb it) |

The top two families (13 of 28) are the capture/spanning and poisoned-`my`
machinery — the same E1-era M-work areas, on shapes the `perl-tests/`
copies do not contain.

## 3. The CPAN board — Text-Balanced collapsed, same cause

14-dist board, same command as `cpan-board14-survey-s343.md` §Reproduce.

**62 PASS / 60 PARTIAL / 61 FAIL** (baseline 65/65/53), and rows
**1794 ok → 1028 ok**.

Only two things moved:

- **GAINS, all expected** — `Role-Tiny/subclass.t` FAIL 0/0 → **PARTIAL 4/1**
  (that is #251/M7 landing), `role-basic-exceptions.t` 2/0 → 4/0,
  `role-basic-composition.t` PASS 8/0 → PARTIAL 10/3 (the s346b
  "read ROWS, not labels" case — +5 rows of coverage), plus the three
  pre-existing #208 drifts (`class_inspector` 53→55, `140-lvalue` 4→6,
  `reduce` 21/11→23/9).
- **ONE LOSS, and it is the whole story**: all **9 runnable
  `Text-Balanced` files went PASS/PARTIAL → FAIL 0/0**, costing
  **766 passing rows**.

The cause is not in the t-files. The dist's own
`lib/Text/Balanced.pm` now hard-errors:

```
Parser2 TODO: forward goto to a standalone label
```

and at `26ce393` (pre-flip) that same file transpiled `pipeline=v1`.
Every t-file `use`s the module, so one module-level v1 route takes the
whole dist down. Same flip mechanism as §2, one layer up.

### 3.1 Why the board audit did not catch it — the method lesson

The board **was** one of the three audited populations, and s355 measured
zero events. The mechanism that explains the gap: `PCL_V2_AUDIT_LOG`
records events at **transpile** time, and a module already resident in
`~/.pcl-cache` is never re-transpiled — a cache hit produces no event.
The cache is keyed by `*pcl-cache-generation*`, so a run at the same
generation as a previous one is warm for every module it touches.

I cannot re-measure s355's cache state (the flip commit deleted the
instrumentation), so this is the mechanism, not a reconstructed fact.
But the durable rule follows either way, and the project already knows the
shape of it — `CLAUDE.md` records the s341 sweep as "re-measured on a COLD
cache" precisely because warm/cold is material:

> **A live-v1 audit must run on a COLD cache** (`rm -rf ~/.pcl-cache/*`
> first, or bump the generation). A warm cache makes the audit report
> zero for exactly the modules that are already compiled — which is the
> set most likely to be load-bearing.

That belongs beside the existing `PCL_V2_AUDIT_LOG` note in memory.

## 4. Also surfaced by the suite run

- **`mro/inconsistent_c3_utf8.t` is STALE** — a blessed expected-divergence
  row now PASSES, so it must come out of `baselines/perl-suite-expected.tsv`.
  The runner already fails the run for this (by design, #185).
- **`comp/hints.t` and `mro/inconsistent_c3.t` XDIFF → DIFF** — their
  blessed row sets grew rows that are not in
  `baselines/perl-suite-expected-rows.tsv`.

## 5. The verdict, and the ask

**The quadruple passes; the two wider populations do not.**  Step 4 is what
the plan put here to catch exactly this, and it caught it:

| population | verdict |
|---|---|
| corpus emission / gate / sweep / census | **green** |
| perl suite | **28 files** newly TRANSPILE-FAIL, **~15,129 rows** |
| CPAN board | **9 files** (one dist) newly FAIL, **766 rows** |

Every one is the same mechanism: a v2 gap that used to be a silent v1
re-transpile is now a hard error, in code that was never audited for live
v1 routes. **Nothing is silently wrong** — the flip's actual goal — but
~15.9k rows of coverage went dark, and CLAUDE.md principle 4 says that is
not something to write off quietly.

Fixing 29 files' worth of v2 gaps is an M-work campaign on the scale of the
one that preceded step 2 — multi-session, and squarely the kind of scope the
user decides.  Options:

- **(a) Fix the families.** 8 suite families + Text::Balanced's
  `forward goto to a standalone label`.  Restores the ~15.9k rows.
  Several sessions.
- **(b) Accept and register.** Both are *bug-finding* populations, not
  shipped-correctness gates; register the 29 with their families and
  re-bless the two snapshots.  Cheap, but it writes off the coverage.
- **(c) Split.** Fix the highest-yield few now — Text::Balanced's single
  goto shape (766 rows, one file) and the two large suite families
  (capture/spanning + poisoned `my`, 13 files); register the rest with
  their causes.

**Recommendation: (c)**, and start with Text::Balanced: it is *one* gap in
*one* module worth 766 rows and a whole dist, the best ratio on the board.
The two large suite families are existing machinery rather than new
mechanisms.  The residue is genuinely varied (an unimplemented `try`, an
attribute-declaration form, a v1-seam die) and is better handled as named
items than as one campaign.

Whatever is chosen, two durable amendments:

1. **The audited populations must include the perl suite** — a fourth
   population, after the same miss happened at Pl/t (s352 §1).
2. **The audit must run on a COLD cache** (§3.1) — otherwise it reports
   zero for precisely the already-compiled modules that matter most.
