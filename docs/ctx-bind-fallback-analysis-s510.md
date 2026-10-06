# The list-context bind around built-ins that never read their context (task #2775) — analysis

s510, agent a2775 (Opus 5.5), 2026-10-06, on main `597a2bd4` (generation v2-4380).  Analysis only:
nothing under `Pl/`, `cl/`, `lib/`, `tools/` or `baselines/` was changed.  Every number below names
its evidence file under the agent's scratch directory, written `scratch/a2775/` here (in the worktree
`.claude/worktrees/agent-a24e9cbaab6227172/`; not committed).

## 1. Verdict

**Remove the fallback, after one fix in the same commit.  Keep `join`'s bind until #2803 is fixed.**

The table of context-sensitive built-ins (`%WANTARRAY_SENSITIVE`) is missing eight built-ins that read
the context.  Today they work in a list slot only because the list-only fallback binds them.  Add the
eight to the table, then remove the fallback.  The addition also fixes five rows that are wrong today
(task #2801).

`join`'s call-wide bind is **not** redundant, whatever its comment says.  It hides a bug that exists
today: a hash assignment whose right-hand side is not a literal list has no context bind of its own
(task #2803).

- Removing the fallback **without** the table addition breaks 9 of 13 list-slot probe rows
  (`probe-a5.pl`).
- The trial removed the fallback and `join`'s bind and added the eight names (variant 3):
  - **Gate:** no behaviour row breaks.  Three rows assert the wrapper text.  One row's expected
    answer was the old wrong one, and perl agrees with the trial.
  - **Everyday:** 114 of 122, unchanged.
  - **Sweep:** one NEW row and one LOST row.  Both are the same `join` row, caused by #2803.
- What it buys:
  - **Reading.** The context wrappers drop from 11.1 to 7.1 per 100 emitted lines on the everyday
    corpus, and from 20.1 to 16.3 on the perl-tests corpus.  Most built-in calls in argument
    position lose their `(p-list-ctx …)`.
  - **Speed.** 2-7 % on inner loops dense with built-in calls in argument position.  Nothing
    measurable on the bench board, whose hot loops contain no such bind (section E).

## 2. The exact change (for the later batch)

In `Pl/ExprToCL.pm`, in one commit:

1. **Extend the table.**  Add `getpwnam getpwuid getpwent getgrnam getgrgid getgrent glob readline`
   to `%WANTARRAY_SENSITIVE` (~line 222).  Its INVARIANT comment then becomes load-bearing: the
   table is the only thing that binds a built-in's context.  Say so in the comment.  `readline` and
   `glob` here mean the *call* spellings `readline($fh)` and `glob("…")`; the `<FH>` and `<pat>` node
   types keep their own wrappers.
2. **Keep `join`'s call-wide bind** (~lines 2977-2988) for now.  Correct its comment: the bind is
   load-bearing for `join ':', %h = (1) x 8`.  Delete it only after #2803 gives every hash
   assignment its own context bind.  Measured: with the bind removed, that row answers 8 where perl
   gives `1:1` (the sweep's only NEW row).  With the bind kept (variant 1), the row is right.
3. **Delete the fallback.**  The end of `gen_funcall_form` becomes `return $call;`.  Its comment
   becomes: "a built-in outside the table never observes its context: it is emitted bare in every
   slot".
4. **`docs/ir-spec.md` §"the context protocol"** (~line 1786, "Call sites bind it where the callee is
   context-sensitive").  Add one sentence: a built-in call is bound only when it is in
   `%WANTARRAY_SENSITIVE`, which must list every runtime built-in that reads `*wantarray*`.
5. **Add `DECIDED.md` lines** for the ruling and for task #2801.

Bump the generation and regenerate the three compiler-built artifacts.  `cl/pcl-pack.lisp` loses 3 of
its 22 `p-list-ctx`, `cl/pcl-mro.lisp` loses 2 of 3, and `cl/pcl-warnings.lisp` loses its only one
(the trial did all three; `rebuild-pack.log`).

The bar, from CLAUDE.md's WHAT TO RUN WHEN, row "`Pl/**`, corpus-diff shows diffs":

- `tools/prove-core`
- `tools/corpus-diff.pl`, with every diff explained (section D gives the explanation mechanically)
- `tools/emission-ab.pl` over `lib/**`
- the full sweep
- the companion `--quick` over the directories whose files carry the shape.  That is all of them,
  so run `--all --quick` once.
- `tools/ir-host-leak.pl`
- `tools/everyday-smoke.pl` before → after
- guard rows for #2801: the scalar-slot rows of `probe-a4.pl` and the list-slot rows of
  `probe-a5.pl`.  Both are inverse-checkable on main: the first set fails there today, the second
  fails on a fallback-only removal.

**Test churn: four gate rows change.**

- **Three rows assert the wrapper text:**
  - `Pl/t/case-regime-01.t` row 6 expects `(p-list-ctx (p-lc $b))`.
  - `Pl/t/list-arg-context-01.t` row 12 expects `p-reverse (p-list-ctx (`.  Its INTENT is "reverse's
    first argument runs in list context", and its callee `sort` never reads that context.  Rewrite
    it with a callee in the table (e.g. `reverse localtime`) so it still tests something.
  - `Pl/t/clform-01.t` row 14 is the join-bind row.  It stays while join's bind stays.
- **One row's expected answer is wrong:** `Pl/t/punct-array-glob-01.t` row 31 expects `0:0` from
  `print scalar(glob("")), ":", …`.
  - perl 5.40.3 prints `:0`, and so does the trial.
  - The row encodes #2801: `glob` ran under the list bind inside `scalar(…)`.
  - Edit it under the four-conjunct expectation rule (s377), with the perl probe cited.

**Not part of the change:**
- A short NEGATIVE list.  Nothing needs one: section A found no built-in that needs a list bind and
  does not belong in the table.
- Any per-built-in "context-blind" list.  Rule 11: the table of sensitive built-ins already exists.

## 3. Sections A–F

### A. Is the table complete?  No — eight built-ins are missing

**Evidence:**
- `A-wantarray-mentions.txt`: every line in `cl/pcl-runtime.lisp`, `cl/pcl-pack.lisp` and
  `cl/pcl-xs.lisp` that mentions `*wantarray*` or `*pcl-caller-wantarray*`, with its enclosing
  definition.  That is 124 lines; `cl/pcl-mro.lisp` and `cl/pcl-warnings.lisp` have none.
- `A-crosscheck.pl` / `A-crosscheck.txt`: every `%RUNTIME_NAMES` built-in's emitted head checked
  for a read, directly or through a helper one level down.
- Probes: `probe-a4.pl`, `probe-a5.pl`, `probe-pw.pl`, `emit1.pl`.

Each reader falls into one of four classes:

| class | readers (runtime definition → Perl construct) |
|---|---|
| (1) in the table | `p-each` `%p-ta-each` `%p-th-each` (each), `p-localtime` `p-gmtime`, `p-reverse`, `%p-readdir-impl`, `%p-splice-result` `%p-ta-splice`, `%p-stat-answer` (stat/lstat), `%p-ent-answer` via `%p-protocol/service/host/net-result` (getproto*/getserv*/gethost*/getnet*), `%p-select-4arg` (select), `p-caller`, `p-backtick` (readpipe; `` `…` `` is its own node), `p-unpack` (pcl-pack.lisp) |
| (2) a non-funcall node with its own wrapper | `p-readline` for `<FH>`, `p-glob` for `<pat>`, `do-regex-match` (`=~`), `p-array-=` / `p-hash-=` / `p-list-=` (assignment), `p-return` / `p-return-empty` / `p-return-value` / `%p-leavesub` / `p-caller-ctx` (return protocol), `p-eval-block` (eval BLOCK, `$wrap`), `p-do` (do FILE, its own `_ctx_wrap_form` arm), `p-slice-result` (only reached in INHERIT context) |
| (3) a user-sub call path (always bound) | `p-sub` (sub entry), `%p-const-answer` (constant subs), `p-xs-invoke` / `%xs-ensure-package` (XS subs), `%p-re-regexp-pattern` (`re::regexp_pattern`, installed as `PL-REGEXP_PATTERN`), `%pcl-no-op-import-result` (`import` method calls), the transpiled subs inside `cl/pcl-pack.lisp`, `p-cloned-sub` |
| binds, does not read | `p-map` (binds t around the block), `p-grep` (binds nil), `%p-tie-call` / `%p-tie-call-list` / `%p-tie-aggregate` / `%p-ta-push` (bind around tie methods), `%pcl-user-property-test`, `p-goto-sub`, `%p-eval-1`, `%expand-foreach*` |
| **(4) passes today only because of the fallback** | **`p-getpwnam` `p-getpwuid` `p-getpwent` `p-getgrnam` `p-getgrgid` `p-getgrent`** (`&key (wantarray (eq *wantarray* t))`), **`p-glob`** for the call spelling `glob("…")`, **`p-readline`** for the call spelling `readline($fh)`.  `p-setpwent`, `p-endpwent`, `p-setgrent` and `p-endgrent` take the same key but `(declare (ignore wantarray))`, so they need nothing. |

**The probes, perl 5.40.3 against main and against the trial variants:**

| probe | perl | main | fallback removed (v1) | v1 + table addition (v3) |
|---|---|---|---|---|
| list slot with no enclosing macro: `cnt(getpwuid 0)` in scalar context, `push @p, getpwuid 0`, `[getgrnam "root"]`, `(k => [glob …])`, `cnt(readline $h)` (`probe-a5.pl`, 13 rows) | 9, 4, 5, many … | same as perl except the one row in note (a) | **9 rows broken** (each gives 1 / "one") | same as main |
| scalar slot inside the argument list of a list-context call: `id("" . getpwuid 0)` and the same for getpwnam, getgrgid, glob and readline (`probe-a4.pl` S-* rows) | root, 0, root, /etc/passwd, root: | **ARRAY(0x1)…ARRAY — wrong today** | same as main | **matches perl** |
| list slot inside a sub called in scalar context, and the reverse (`probe-a4.pl` SUBS-* and SUBL-* rows) | — | same as perl, except the eval STRING rows | same | same, except one glob row (note (b)) |

Notes on the table:
- (a) The one row that differs on every PCL side is `printarg-pwuid`: the password field, `x`
  against perl's `!*`.  It has nothing to do with context; filed as #2802.
- (b) `SUBL-S-glob` moves from `/etc/passwd` to `/etc/passwd-`.  This is #489 (glob's scalar
  iterator is keyed by the PATTERN): the earlier `S-glob` row now really runs in scalar context, so
  it advances the shared iterator.

`probe-a4.pl` covers every slot listed in the brief: list slot, scalar slot, and called from inside a
sub that was itself called in the other context.

**A second gap, separate from the fallback (task #2800).**  `eval STRING` never gets a context bind.
The `eval` arm returns `_gen_eval_string_form(...)` (~lines 2433 and 2439) before the context rule is
reached:
- `print 1, eval "(3,4)", 2` prints `142`; perl prints `1342`.
- `my $x = eval "(5,6,7)"` inside a sub called in list context gives `ARRAY(0x…)`; perl gives 7.

Removing the fallback does not change this either way.

### B. User code run from inside a built-in outside the table — unaffected

**Evidence:** `probe-b.pl`; `.perl.out`, `.pcl.out`, `.v1.out`, `.v3.out`.

Each case records `wantarray` inside the user code, at top level and in subs called in list context
and in scalar context.  The cases:

- overload `abs`, `""` and `0+` handlers, reached through `abs $o`, `lc $o`, `sprintf("%s", $o)`
  and `join("", $o)`
- tie `FETCH`, reached through `length $t`
- `sort SUBNAME`, `sort $name` and `sort { by_num() }`
- `$SIG{__WARN__}`, reached through `warn` in a list slot
- `DESTROY`, reached through `undef $d` in a list slot
- the four override spellings, in list, scalar and void slots:
  - `BEGIN { *CORE::GLOBAL::hex = sub … }`
  - `BEGIN { package Other; *main::oct = sub … }`
  - `use subs qw(chr)`
  - an import list (`use MyOrd qw(ord)`)

Results:

- **v1 and v3 print byte-for-byte what main prints.**
- The overload, tie, sort and warn handlers see scalar context on every side, as in perl.  Each one
  gets its own bind: overload calls, `%p-tie-call`, the comparator, the handler.  The fallback never
  reaches them.
- A replaced built-in is emitted as a USER-sub call (`pl-hex` …), so it takes the user-sub path,
  which always binds.  The fallback is not what gives it list context.

Two differences from perl exist today.  Removing the fallback does not widen either:
- A replaced built-in in a SCALAR slot sees `L`; perl gives `S`.  This holds for all four spellings,
  and it is already in #2779 ("a CORE::GLOBAL replacement reads wantarray wrongly in a scalar slot"),
  so no new task.  The ordering of the probe's list rows also differs, because of #2779's parse gap 1.
- `DESTROY` does not run on `undef $d`.  That is the blessed non-support "DESTROY called by garbage
  collector" in `docs/not-supported.md`.

### C. How much of the emission it is

**Evidence:** `C-counts.txt`, made by `count-c.pl` over `emit-v0/` (main's emission with every bind
marked by kind: `/fb` = the fallback, `/join` = join's bind, `/tab` = a table addition) and
`emit-v2/` / `emit-v3/`.  The split between built-in and user sub comes from the compiler's own
branch (the mark is set inside the fallback, which is reached only when the name is in
`%RUNTIME_NAMES`), not from the eye.

Before the mark was trusted, it was checked: a marked emission of the everyday corpus, with the
marks folded back, is identical to main's own `./pl2cl` in 122 of 122 files.  Three compiler
consumers look at the wrapper's head and look through it:
- `Pl::ClassicSort` `%TRANSPARENT` / `_fresh_arg`
- `Pl::ExprToCL` `%CTX_WRAP`
- `Pl::Parser2` `_auto_defined_call`

The scratch copy taught them the marked spellings.  They also handle a bare call, so removing the
wrapper changes nothing for them.

| population | files | lines | all ctx wrappers | p-list-ctx | fallback | join | per 100 lines: main → v2 (both removed) → v3 (+ table) |
|---|---|---|---|---|---|---|---|
| bench board | 61 | 2,716 | 84 | 36 | 25 | 6 | 3.09 → 1.96 → 2.00 |
| everyday | 122 | 10,101 | 1,125 | 662 | 310 | 118 | 11.14 → 7.08 → 7.11 |
| lib/**/*.pm | 24 | 13,419 | 1,001 | 174 | 63 | 9 | 7.46 → 6.95 → 6.95 |
| perl-tests (111) | 111 | 72,228 | 14,524 | 4,631 | 2,614 | 465 | 20.11 → 16.25 → 16.27 |

In all, 3,012 fallback binds and 598 `join` binds over 318 files, around 69 distinct heads.  None of
the heads is a user sub.  Top 30 by count:

`p-undef` 520, `p-scalar` 352, `p-map` 248, `p-substr` 219, `p-length` 145, `p-sort` 133, `p-pack` 132,
`%p-sort-classic` 130, `p-chr` 121, `p-sprintf` 119, `p-keys` 98, `p-ord` 94, `p-prototype` 87,
`p-ref` 44, `p-index` 38, `p-grep` 36, `p-uc` 35, `p-defined` 33, `p-lc` 31, `p-quotemeta` 29,
`p-rindex` 26, `p-int` 25, `p-shift` 24, `p-chop` 22, `p-ucfirst` 21, `p-values` 20, `p-lcfirst` 19,
`p-vec` 19, `p-fc` 15, `p-pos` 15.

The table additions bind 21 sites in v3, in both contexts.  Most are in perl-tests:
`p-readline` 10, `p-glob` 6, `p-getgr*` 5.

### D. The trial removal

**Evidence:**
- The emission lab is `trial/`: a `git archive` extraction of main.  `patch-trial.pl` makes its
  variant switchable by environment variable; `patch-marks.pl` makes the wrapper consumers accept
  the marks.
- The heavy legs ran on `trial3/`: variant 3 hard-coded, generation `v2-4380-a2775`, all three
  artifacts regenerated, its own `PCL_CACHE_DIR`.

**Emission, proved mechanically** (`cmp-norm.pl`; `D-proof-v1.txt`, `D-proof-v2.txt`).  The script
starts from main's emission, removes every fallback wrapper (and, for v2, every `join` wrapper) with a
balanced-paren unwrapper, collapses whitespace and compares with the variant's emission:

- **317 of 318 files are then identical**, for both v1 and v2.  The removal is nothing but lost
  wrappers.
- The one exception is `perl-tests/sort.t`.  One top-level form shrinks below `Pl::Parser`'s
  `$HUGE_FORM_CHARS` (20,000 characters), so it loses its
  `(locally (declare (notinline …)))` cap.  That form gets the normal inlining back.  It is a
  compile-memory guard, not semantics; the sweep below covers sort.t.
- v3 differs from v2 only by the 21 new table binds.

This lab compares emission whole-population.  `tools/corpus-diff.pl` itself needs a git work tree
for the trial, which an extraction is not, so it was not run.  It covers the same 111 files.

**The gate** (`gate-v3.log`, `tools/prove-core` on trial3): 281 files, 9,832 rows, `Result: FAIL`
on 6 rows.

| kind | rows |
|---|---|
| trial artifacts (would not occur in a real commit) | `manifest-01.t` #3: the trial's generation string `v2-4380-a2775` does not match `^v2-\d+$`.  `pcl-runtime-env-01.t` #1: the extraction is not a git checkout. |
| assert the wrapper text | `case-regime-01.t` #6, `list-arg-context-01.t` #12, `clform-01.t` #14 (see §2) |
| behaviour, and the trial is RIGHT | `punct-array-glob-01.t` #31: expects `0:0`, perl and the trial print `:0` (#2801) |

No behaviour row breaks.

**The sweep** (`sweep-v3.log`, `--jobs 4`): **NOT clean** — 1 NEW, 0 FIXED, 1 LOST.  TOTAL passing is
18,734 against the baseline's 18,735.

- The NEW and the LOST row are the same one: `hashassign.t` "hash assignment in list context removes
  duplicates", `join ':', %h = (1) x 8`, which got `8` where perl expects `1:1`.
  - The cause is the removal of `join`'s bind, not of the fallback.
  - `pj.pl` is right under variant 1, the fallback alone.
  - The underlying bug exists today outside `join`: `print %h = (1) x 8` prints 8, perl prints 11
    (`pj2.pl`).  Task #2803.
- 4 UNSTABLE rows sit above the abort point of files that were already PARTIAL (`magic.t`,
  `method.t`, `yadayada.t`).  The tool counts them as crash-file noise.  They were not
  re-attributed one by one.
- Drops 5 = census.

**Everyday** (`everyday-v3.log`): `EVERYDAY: 114 of 122 identical to perl (93.4 %)`, the same as main.
Buckets NEW 0, FIXED 0, MOVED 0, UNEXPLAINED 0, STALE 0.

**The companion:** not run.  The brief allows it only when the gate and the sweep are clean of
behaviour rows, and the sweep is not (one row, explained above).

**Not run:** the gate, sweep and everyday legs on variant 1, which keeps `join`'s bind.  The only
behaviour difference between the variants that the probes found is the `join` row.  Variant 1 is the
recommended change, so it still owes its own bar.

### E. What it buys at run time

**Evidence:** `bench-e.log`, made by `bench-e.pl`.

- Each program was transpiled once by main's `pl2cl` and once by trial3's, and both `.lisp` files
  ran under main's cached runtime core.  So only the emission differs.
- The runs were interleaved, best of K=5, two rounds, as one `heavy.sh a2775 bench` leg.  The
  1-minute load was 1.96 at the start.
- **None of the bench board's programs has a fallback bind in its hot loop**, so the timed programs
  are this analysis's own loops (P1-P5).  The controls are the board's `fibret`, `methret` and
  `intloop`, whose emission does not change.

| program | round 1 main → trial | round 2 main → trial |
|---|---|---|
| C-fibret (control) | 0.481 → 0.482 s (+0.2 %) | 0.476 → 0.479 (+0.7 %) |
| C-methret (control) | 0.122 → 0.121 (−1.5 %) | 0.119 → 0.120 (+0.6 %) |
| C-intloop (control) | 0.032 → 0.032 (−0.2 %) | 0.032 → 0.032 (−0.9 %) |
| P1: #2775's loop (5 binds per iteration) | 0.685 → 0.635 (**−7.4 %**) | 0.670 → 0.634 (**−5.3 %**) |
| P2: `push @o, lc $s, uc $s, length $s` | 0.277 → 0.269 (−2.8 %) | 0.277 → 0.267 (−3.7 %) |
| P3: `g(length $s, ord $s, abs $i)` | 0.272 → 0.266 (−2.3 %) | 0.272 → 0.264 (−2.9 %) |
| P4: `length join ",", map {…} @a` | 0.296 → 0.295 (−0.3 %) | 0.295 → 0.296 (+0.5 %) |
| P5: `@k = (sprintf …, substr …, chr …)` | 0.480 → 0.475 (−1.2 %) | 0.483 → 0.479 (−0.9 %) |

**The control band is −1.5 % to +0.7 %.**

- P1, P2 and P3 gain 2-7 % in both rounds, outside the band.  That is roughly 1.5-2 ns per removed
  bind, in line with #2775's hand measurement.
- **P4 and P5 show no measurable gain** (inside the band).  In P5 the binds sit inside a list
  assignment that `p-array-=` already binds.
- Real programs carry far fewer binds in hot code.  None of the board's 61 rows has one in its loop,
  so **on the bench board the change buys nothing measurable.**  The run-time value is "a few
  percent on argument-dense inner loops"; the readability value (section F) is the larger one.

### F. What it buys the reader

**Evidence:** `F-bench-json-rt.txt`, `F-everyday-lc-uc-etc.txt`, `F-lib-List-Util.txt` (made by
`f-show.pl`: main against v3, whole diff).

| file | context wrappers main → v3 | lines main → v3 |
|---|---|---|
| bench `json-rt` | 9 → 4 | 98 → 92 |
| everyday `index/lc-uc-etc.pl` | 37 → 2 | 111 → 101 |
| lib `List/Util.pm` | 109 → 93 | 805 → 791 |

A typical hunk, from `lc-uc-etc.pl`.  Main:

```lisp
(p-print (p-list-ctx (p-ucfirst (p-lc "HELLO wORLD"))) …)
(p-list-ctx
  (p-join ","
    (p-list-ctx
      (p-map (lambda ($_) (p-list-ctx (p-sprintf "%s" $_)))
        (p-flatten-args
          (list (p-list-ctx (p-abs -3)) (p-list-ctx (p-int -3.7)) (p-list-ctx (p-sqrt 16)) …))))))
```

v3:

```lisp
(p-print (p-ucfirst (p-lc "HELLO wORLD")) …)
(p-join ","
  (p-map (lambda ($_) (p-sprintf "%s" $_))
    (p-flatten-args
      (list (p-abs -3) (p-int -3.7) (p-sqrt 16) …))))
```

## 4. What could not be established

- **The bench board.**  None of the board's own programs has a fallback bind inside its timed loop.
  All 31 of the board's binds sit in setup code or the final `print` (`C-counts.txt`; the per-row
  sites are listed in the agent's notes).  So the board cannot show this change, and E uses its own
  loops.
- **`tools/corpus-diff.pl` as such** was not run, because the trial is an extraction, not a git work
  tree.  The section-D lab covers the same 111 files and three more populations, with a stronger
  check.
- **The heavy legs ran on variant 3 only** (fallback and `join`'s bind removed, table extended).  The
  recommended change is variant 1 plus the table addition.  Its gate and sweep are expected to equal
  variant 3's minus the `join` row and the `clform-01.t` #14 shape row, but that was not measured.
- **The companion suite** was not run (see D).
- **The 4 UNSTABLE sweep rows** in already-PARTIAL files were not attributed one by one.
