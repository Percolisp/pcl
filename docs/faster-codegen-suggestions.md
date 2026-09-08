# Generating faster CL — measured variants, per category

**What this is.** A catalogue of PCL's hot constructs.  For each one: the
Common Lisp PCL emits today, an alternative shape that computes the identical
result, and a head-to-head timing of the two.  Every entry says concretely
*"emit shape B instead of shape A, it is N× faster"*.

It is the measured basis of **Target A** — general program speed must beat
Perl (`v2-endgame-plan.md` §6 holds the acceptance criteria and sequencing).

**Written** 2026-07-19 against the v2 pipeline; §0.1 re-measured 2026-08-25; §0.2i is the current board (2026-09-04, quiet box).

## Where this stands (2026-08-25)

> **The current measured board is [§0.2k](#02k-the-board-on-a-quiet-box-s475-2026-09-07-main-047cc249-gen-v2-1020) (2026-09-07, quiet box: fourteen of the nineteen original rows beat perl, `listcopy` 0.36×, `symref` 0.38×, `slices` 1.65×); §0.2i (2026-09-04, quiet box): ten of nineteen rows beat perl, `arrhash` 0.60×, `slices` 2.60×, `symref` 1.37×.**  The table below is the 2026-08-25 reading and is kept as the record of what each tier delivered.  **Round 32 (the method-call round) is [§0.2l](#02l-round-32-movers-2026-09-08); the rows that moved before it — rounds 29–31 (regex ops once per site, the single-array foreach run, `int()`/`/`, the package preamble; `use JSON::PP` load 0.41 s) — are [§0.2j](#02j-rounds-2931-movers-2026-09-07); the next quiet-box board supersedes its derived ratios.**

Every shipped transform is a **named, switchable emission** in the
optimization registry [`Pl/Passes.pm`](../Pl/Passes.pm) (`PCL_OPT`):
`raw-slot`, `raw-numeric`, `str-buffer`, `foreach-range`, `insensitive-call`,
`elem-setf`, and since s456af the four verdict-COVERAGE names
`raw-block-eval`, `raw-op-family`, `raw-closure-capture`, `raw-topic`
([§13.1](#131-all-four-shipped-s456af-round-13--and-what-they-cost)).
Tier 1 is half done:

| tier-1 item | task | state |
|---|---|---|
| **S1 + N1** — string buffer, raw numerics | #62 | **done.**  `str-buffer` killed the O(n²) append class (`strcat` 756× → 5.7×); `raw-numeric`/`raw-slot` put the counting loops and `collatz` *ahead of perl*.  See the re-measured [§0.1 table](#01-re-measured-baseline-2026-08-25-after-62--the-73-first-cut). |
| **verdict COVERAGE** — where N1's machinery fires | #758–#761 | **done (s456af).**  Not new shapes: four vetoes narrowed, so real code reaches the raw machinery.  **Both losing loop rows now beat perl** — `intloop+=` 2.02× → **0.28×**, `intloop=` 4.83× → **0.29×**, `arrhash` 2.17× → **1.45×**.  [§13.1](#131-all-four-shipped-s456af-round-13--and-what-they-cost) and the [§0.2b table](#02b-after-the-verdict-coverage-work-s456af-758761). |
| **M1** — method dispatch | #73 | **done, cache-free.**  The per-call-site inline cache was *rejected* (USER, s444): profiling showed the ~15× was mostly `finalize-inheritance` running on every call, not the lookup.  The own-package fast path plus the stash/`pl-NAME` memos took a monomorphic loop to **2.62× of perl** and an inherited one to **4.74×** (`ovlsub` 3.44×).  Two further steps measured 7 % and 0 % and were closed unshipped — [§7](#7-object-handling--method-dispatch-is-15-a-plain-call-biggest-oo-lever) has the table.  One lever remains: a per-CLASS cache (#582), blocked on `@ISA`-write invalidation. |
| **P1** — `sprintf` / `pack` | #74 | **open**, and now the largest single loss on the board. |

## How the numbers were taken

1. **Whole-program:** `perl tools/bench-exec.pl` — execution only, startup
   subtracted, best-of-5, against perl.  That is the [§0](#0-whole-program-baseline-vs-perl-this-session)
   and [§0.1](#01-re-measured-baseline-2026-08-25-after-62--the-73-first-cut) tables.
2. **Variant experiments:** for each hot category, the *actual emitted CL* was
   taken, alternative CL computing the identical result was hand-written, and
   the two were timed head-to-head against a fresh runtime core (same
   big-N−small-N, best-of-5 method).  That is [§2](#2-arithmetic--operators--the-p--pipeline-is-already-at-the-sound-ceiling)–[§8](#8-io--regex--pack--io-is-syscall-bound-the-other-two-re-parse-constants).
   The harness and every variant `.lisp` file are reproducible —
   [§9](#9-how-to-reproduce--extend-the-variant-experiments) is the recipe.

**Policy** (CLAUDE.md §2): speed wins over readable CL.  Every change proposed
here is an opt-in narrowing with a **boxed fallback** — if the analysis cannot
prove its precondition, today's code is emitted unchanged, so a wrong analysis
costs a speed-up and never correctness.  Every new fast shape ships wrapped in
its named macro (the Target-B rule).

**Companions — the *why* and the soundness proofs:**
[`where-the-time-goes.md`](where-the-time-goes.md) (the four taxes),
[`raw-numeric-verdict.md`](raw-numeric-verdict.md) (use-proven eager
coercion), [`bench-exec-investigation.md`](bench-exec-investigation.md) (the
counting loop), [`ir-spec.md`](ir-spec.md) §2.2 (the box/raw invariant).

## Contents

* **Baselines** — [§0 whole-program vs perl](#0-whole-program-baseline-vs-perl-this-session) · [§0.1 re-measured, 2026-08-25](#01-re-measured-baseline-2026-08-25-after-62--the-73-first-cut) · [§0.2 re-measured, 2026-08-30 + the #680 m//g result](#02-re-measured-baseline-2026-08-30-round-12-perf-agent-s454ac) · [§0.2a the two load-suspect rows on a quiet box + the `symref` bisection](#02a-the-two-load-suspect-rows-re-measured-on-a-quiet-box-s456af) · [§0.2b after #758–#761 — both intloop rows beat perl](#02b-after-the-verdict-coverage-work-s456af-758761) · [§0.2c after the boxed-aggregates flip](#02c-after-the-boxed-aggregates-flip-s457ai-phases-03-task-816) · [§0.2d after the accessor-dispatch work](#02d-after-the-accessor-dispatch-work-s458ak-phase-4s-runtime-half) · [§0.5 headline results](#05-headline-results-what-the-experiments-proved) · [§0.2k the board on a quiet box, 2026-09-07](#02k-the-board-on-a-quiet-box-s475-2026-09-07-main-047cc249-gen-v2-1020) · [§0.2j rounds 29–31 movers](#02j-rounds-2931-movers-2026-09-07)
* **Verdict coverage** — [§13 the s453 review](#13-s453-review--the-unclaimed-speed-is-in-verdict-coverage-not-new-shapes-probes-on-head-a2b2eb5-tasks-758761) · [§13.1 all four shipped, s456af](#131-all-four-shipped-s456af-round-13--and-what-they-cost)
* **Per category** — [§1 loops](#1-loops) · [§2 arithmetic](#2-arithmetic--operators--the-p--pipeline-is-already-at-the-sound-ceiling) · [§3 boxed accumulator](#3-boxed-accumulator--raw-slot-is-13-the-intloop-tax) · [§4 strings](#4-strings--fill-pointer-buffer-is-2400-the-single-biggest-win) · [§5 aggregates](#5-aggregates--the-value-box-is-not-the-cost-keys--lookups-are) · [§6 calls and recursion](#6-function-calls--recursion--already-winning-keep-it) · [§7 objects and dispatch](#7-object-handling--method-dispatch-is-15-a-plain-call-biggest-oo-lever) · [§8 I/O, regex, pack](#8-io--regex--pack--io-is-syscall-bound-the-other-two-re-parse-constants)
* **Working with this catalogue** — [§9 reproduce or extend the experiments](#9-how-to-reproduce--extend-the-variant-experiments) · [§10 microbench → whole-program impact](#10-expected-wins--microbench-speedup--whole-program-impact) · [§11 before/after listings](#11-before--after--perl--current-cl--proposed-cl) · [§12 priority, win ÷ effort](#12-priority-by-measured-win--effort) · [§13 s453 verdict-coverage review, #758–#761](#13-s453-review--the-unclaimed-speed-is-in-verdict-coverage-not-new-shapes-probes-on-head-a2b2eb5-tasks-758761)

---

## 0. Whole-program baseline vs Perl (this session)

```
bench          perl(s)     pcl(s)  pcl/perl
intloop+=       0.0679     0.2112     3.11x    for(1..n){ $s += $_ }
intloop=        0.0664     0.2848     4.29x    for(1..n){ $s = $s + $_ }
cfor            0.1046     0.1585     1.52x    C-style counter
arrhash         0.1299     0.2683     2.07x    $h{x}=..; $a[3]=..
fib(27)x        1.4767     0.3664     0.25x    ← 4.0x FASTER
gcdrec          0.1907     0.0850     0.45x    ← 2.2x FASTER
collatz         1.9535     3.8381     1.96x    while + % / *
strcat          0.0021     1.6046   755.88x    $s .= 'x'  (O(n^2))
pack            0.0040     4.6642  1175.46x    transpiled oracle
packunpk        0.0030     4.6899  1587.26x    transpiled oracle
```

Recursion/calls already beat Perl. The losses are four specific things, and
the variant experiments below pin the cause and the fix for each.

## 0.1 Re-measured baseline (2026-08-25, after #62 + the #73 first cut)

Same instrument (`perl tools/bench-exec.pl`, best-of-5, startup subtracted);
the suite has grown five rows since §0 (arrfill, slices, sliceasgn, ovlsub,
symref).

```
bench          perl(s)     pcl(s)  pcl/perl
intloop+=       0.0666     0.1377     2.07x    (was 3.11x)
intloop=        0.0662     0.3218     4.86x
cfor            0.1064     0.0257     0.24x    <- was 1.52x SLOWER; now 4.1x FASTER
arrhash         0.1327     0.2816     2.12x
fib(27)x        1.4728     0.4439     0.30x    <- 3.3x FASTER
gcdrec          0.1945     0.0952     0.49x    <- 2.0x FASTER
collatz         1.9829     0.7987     0.40x    <- was 1.96x SLOWER; now 2.5x FASTER
strcat          0.0012     0.0068     5.71x    (was 756x -- S1 killed the O(n^2) class)
pack            0.0035     5.0354  1432.92x    (P1 #74 -- still the big loss)
packunpk        0.0040     5.1136  1270.47x    (P1 #74)
arrfill         0.0500     0.1972     3.94x
slices          0.0704     0.3276     4.65x
sliceasgn       0.0264     0.0760     2.88x
ovlsub          0.0417     0.2078     4.98x    (was 7.27x before the #73 finalize-once guard)
symref          0.0215     0.1601     7.44x
```

What moved and why: `raw-numeric`/`raw-slot` (#62/N1) fixed the counting
loops and `collatz`; `str-buffer` (#62/S1) removed the append complexity
class; the s444 finalize-once guard took the first bite out of dispatch.

> **`ovlsub` moved again in s446m: 4.98× → 3.44×** (0.2078 → 0.1620 s; the
> other rows re-measured within noise on the same run — intloop+= 2.07×,
> arrhash 2.20×, fib 0.29×).  That is #73's cache-free remainder, §7.

Left, in order of size: the pack/unpack template re-parse (#74), method
dispatch's remainder (#73, cache-free plan in the task), aggregate/slice
traffic (boxed-aggregate design, post-v0.1), symbolic refs.

---

## 0.2 Re-measured baseline (2026-08-30, round-12 perf agent, s454ac)

Same instrument (`perl tools/bench-exec.pl`, best-of-5, startup subtracted),
taken on the round-12 tree BEFORE this session's #680 regex work (no bench
row exercises m//g, so that work does not move this table; `regexg` below is
the new row that will track it from round 13).

```
bench          perl(s)     pcl(s)  pcl/perl
intloop+=       0.0657     0.1328     2.02x
intloop=        0.0650     0.3141     4.83x    (#759 + #761)
cfor            0.1053     0.0265     0.25x    4x FASTER
arrhash         0.1294     0.2805     2.17x
fib(27)x        1.4483     0.4175     0.29x    3.5x FASTER
gcdrec          0.1956     0.1007     0.52x    1.9x FASTER
collatz         1.9259     0.7738     0.40x    2.5x FASTER
strcat          0.0016     0.0057     3.50x
pack            0.0042     5.1287  1219.05x    (P1 #74 -- still the big loss)
packunpk        0.0046     5.6759  1222.47x    (P1 #74)
arrfill         0.0737     0.2915     3.96x
slices          0.1001     0.4757     4.75x
sliceasgn       0.0322     0.0900     2.80x
ovlsub          0.0489     0.1867     3.82x    (s446m said 3.44x -- see note)
symref          0.0257     0.2397     9.33x    (§0.1 said 7.44x -- see note)
```

Notes: taken while sibling agents were active on the box, so treat rows that
moved AGAINST the record (ovlsub 3.44→3.82, symref 7.44→9.33) as
load-suspect until the round-13 agent re-measures on a quiet machine; no
change since s446m touched either path.  The stable story matches §0.1:
counting loops and recursion beat perl, `intloop=`/`intloop+=` wait on the
verdict-coverage tasks (#758–#761, §13), pack/unpack wait on #74.

### 0.2a The two load-suspect rows, re-measured on a quiet box (s456af)

Verdict on the note above: **`ovlsub` was load noise, `symref` was a real
+34 % regression** — and telling them apart needed the ABSOLUTE PCL time,
not the ratio.  Two independent best-of-5 runs at 1-min load 0.1–0.2, agreeing
within 0.5 %:

| row | pcl(s) now | pcl(s) record | perl(s) now | perl(s) then | ratio now | verdict |
|---|---:|---:|---:|---:|---:|---|
| `ovlsub` | 0.1477 | 0.1620 (s446m) | 0.0388 | 0.0471 | 3.80x | **no regression** — PCL is 9 % FASTER than the record; the ratio rose only because perl ran faster too |
| `symref` | 0.2155 | 0.1601 (§0.1) | 0.0210 | 0.0215 | 10.26x | **real, PCL-side**: perl is unchanged, PCL is 34 % slower |

> **Reading rule this establishes:** a `pcl/perl` ratio compares two
> measurements taken in the same machine state, so it is only comparable
> across sessions when the perl column moved too.  Compare the **PCL absolute
> seconds** first, and look at the perl column before calling a ratio a
> regression.

`symref` BISECTED (the bench row is `${'main::g'} + ${'g'} + scalar(@{'main::ga'})`
in a 200k loop; each step a `BENCH_K=3` run on that commit):

| commit | pcl(s) | what landed |
|---|---:|---|
| `99b003d` | 0.1578 | (pre-#525 baseline) |
| `91633d6` | 0.1739 | **#525** — an unqualified symbolic name resolves in the perl-level current package (+10 %) |
| `c8a8311` | 0.1737 | round 6 |
| `f937afd` | 0.1804 | round 8 |
| `80a11a7` | 0.1844 | round 9 + s451x |
| `7ca3da4` | **0.2150** | **#685** — a foreign-qualified symbolic name never reaches main's magic (**+16.6 %**) |
| `0dd7434` | 0.2247 | round 10 |
| `83b335f` | 0.2155 | HEAD |

So it is not a mystery and not a bug: it is the accumulated price of the
round-5–10 correctness work on the symbolic-name resolver, two thirds of it
in the two commits that made resolution package-aware.  Whether any of it is
recoverable (a per-call-site memo on the resolved cell, keyed for
invalidation the way #582 would need) is **task #812** — filed, not
scheduled.

### 0.2b After the verdict-coverage work (s456af, #758–#761)

```
bench          perl(s)     pcl(s)  pcl/perl   was
intloop+=       0.0647     0.0179     0.28x   2.02x  <- 3.6x FASTER than perl
intloop=        0.0652     0.0190     0.29x   4.83x  <- 3.4x FASTER than perl
cfor            0.1062     0.0242     0.23x   0.25x
arrhash         0.1303     0.1891     1.45x   2.17x
fib(27)x        1.4773     0.4437     0.30x   0.29x
gcdrec          0.1909     0.0947     0.50x   0.52x
collatz         1.9626     0.7839     0.40x   0.40x
strcat          0.0017     0.0040     2.33x   3.50x  (0.004 s: noise-dominated)
pack            0.0031     4.5134  1472.04x          (P1 #74)
packunpk        0.0038     4.6277  1203.57x          (P1 #74)
arrfill         0.0810     0.3049     3.77x   3.96x
slices          0.1105     0.5251     4.75x   4.75x
sliceasgn       0.0414     0.1128     2.73x   2.80x
ovlsub          0.0677     0.2505     3.70x   3.82x
symref          0.0400     0.3639     9.10x   9.33x
regexg          0.1039     0.0012     0.01x          (#680, s454ac)
```

**Both losing loop rows now beat perl**, and they decompose exactly as §13
predicted: `intloop=` needed #759 (4.83x → 3.01x) and then #761 (→ 0.29x);
`intloop+=` needed #761 alone.  Two rows §13 did not name moved with them
because they are topic loops too — `arrhash` 2.17x → 1.45x and `strcat`.

This table's TAIL (arrfill through symref) was taken with a sibling agent
active — its perl column reads high — so those rows are for the shape of the
board, not for comparison; the §0.2a rows above are the quiet-box numbers.
What is left, in order of size: pack/unpack (#74, still the whole board),
aggregate/slice traffic (boxed-aggregate design), `symref` (#812),
dispatch's remainder (#73/#582).

**m//g (task #680, fixed this session, runtime-only):** the qp6 shape
`while ($x =~ /./g) {}` went from ~1.7 µs/match to **~0.21 µs/match**
(perl 0.09 µs — ~19× → **~2.4×**); 1M chars 1.88 s → 0.40 s wall via runpcl.
sb-sprof named the cost: 66 % was `p-regex` RE-PARSING the pattern text per
ITERATION (emission calls `(p-regex "/./g")` inside the loop condition —
eight `regex-replace-all` passes per match), ~8 % the scanner-cache FORMAT
key + \\G strip + options plist per call, the rest the 20-write capture
clear, the @-/@+ fresh-box rebuild, and the CLOS dispatch on
`cl-ppcre:scan`.  All five are gone: `p-regex`/`p-regex-from-parts` memoize
on source text, the compiled scanner lives in the struct, the capture clear
is high-water-marked, @-/@+ element boxes mutate in place (which also fixed
a probed divergence — perl's `\\$-[0]` reads the CURRENT match), and scan is
a direct funcall.  What remains is ~irreducible from PCL's side: the
cl-ppcre engine itself (§8's ~3.7× note; the PCRE2-FFI future item is the
next lever on this shape).

### 0.2c After the BOXED-AGGREGATES flip (s457ai, phases 0–3, task #816)

Array/hash ELEMENTS are no longer boxed unconditionally: a slot holds a raw
value until something takes an alias to it, and is then promoted to a box IN
PLACE (`docs/boxed-aggregates-design-s455.md`).  A/B on the same quiet box, in
one sitting, both sides `perl tools/bench-exec.pl` best-of-5: the BASE column
is a `git archive` of main at `07f2df0` (the rebase base), the FLIP column is
this tree.  An earlier A/B against `0237940` agreed within 3 % on every row.

```
bench          base pcl(s)  flip pcl(s)   base      flip
intloop+=         0.0177       0.0182     0.26x    0.28x
intloop=          0.0184       0.0184     0.28x    0.28x
cfor              0.0262       0.0269     0.25x    0.26x
arrhash           0.1929       0.1594     1.48x    1.23x   <- -17 %
fib(27)x          0.4260       0.4196     0.29x    0.29x
gcdrec            0.0977       0.1003     0.51x    0.52x
collatz           0.7873       0.7878     0.40x    0.40x
strcat            0.0040       0.0034     1.83x    1.54x   (abs. 4 ms — noise)
pack              4.4936       4.5099        —        —    (#74, untouched)
packunpk          4.6371       4.6165        —        —    (#74, untouched)
arrfill           0.1920       0.1454     3.94x    2.99x   <- -24 %
slices            0.3219       0.3465     4.77x    5.07x   <- +8 % WORSE
sliceasgn         0.0702       0.0727     2.65x    2.88x   (+4 %, see below)
ovlsub            0.1510       0.1494     3.84x    3.83x
symref            0.2195       0.2179     9.72x    9.74x
regexg            0.0012       0.0005        —        —
```

**What moved, and why.**  `arrfill` (`@a = (1..20, $_)` 200 k times) and
`arrhash` (`$h{x}=$_; $a[3]=$_+1` 2 M times) are the two rows whose inner loop
allocated a box per element store; both now store the raw scalar.  **Neither
reached the design's target** (arrhash ≤1.0×, arrfill ~1×) because what is left
in those loops is the ACCESSOR dispatch, not the allocation — `p-aref`'s
`to-number` + negative-index handling, `p-gethash`'s stringify + lookup.  That
is a different axis and a fair phase-4 item.

**The two rows that went the wrong way are both understood and both owned.**
`sliceasgn` pays for the correctness half of this work: perl evaluates a list
assignment's whole RHS before its first store, and PCL now does too
(`%p-assign-snapshot`, without which `@a[0,1] = @a[1,0]` reads source 1 after
store 0 overwrote it — the sweep caught exactly that).  `slices` pays for
promotion in a COPY position: `my @v = @a[1..5]` takes the container's own
element cells because a slice is a list of ALIASES in perl, so the first pass
promotes every visited slot.  The design's phase 4 already names the fix — the
"proven arm": when the annotator can show the slice's consumer only READS,
bind raw values and skip promotion entirely.  That is the same two-arm shape
§4.4 gives `foreach`, and `slices` is the row that will measure it.

**Nothing already won regressed**: every counting/recursion row is within the
7–10 % noise band this machine shows (measured by running the base twice), and
four of them are nominally faster.

### 0.2d After the ACCESSOR-DISPATCH work (s458ak, phase 4's runtime half)

The §0.2c note said what was left in `arrhash`/`arrfill` "is the ACCESSOR
dispatch, not the allocation".  This session profiled that claim and acted on
it — RUNTIME ONLY, no emission change, so the corpus, the generation string
and the three artifacts are untouched.  A/B on a quiet box in one sitting,
both sides `perl tools/bench-exec.pl` best-of-5; the BASE column is a
`git archive` of main at `8e38d79`.  Re-confirmed after rebasing onto
`0d5b923` (the sibling agent's emission work landed first): every row within
noise of the column below — `arrhash` 0.0870, `arrfill` 0.0710, `slices`
0.2135, `sliceasgn` 0.0539, `collatz` 0.7681.

```
bench          base pcl(s)  s458ak pcl(s)    base    s458ak
intloop+=         0.0176       0.0174       0.27x    0.27x
intloop=          0.0185       0.0185       0.29x    0.29x
cfor              0.0269       0.0261       0.26x    0.25x
arrhash           0.1624       0.0854       1.28x    0.67x   <- -47 %, TARGET (<=1.0x) MET
fib(27)x          0.4178       0.4196       0.29x    0.29x
gcdrec            0.0980       0.0942       0.53x    0.50x
collatz           0.7765       0.7731       0.40x    0.40x
strcat            0.0029       0.0037          —        —    (3 ms: noise-dominated)
pack              4.5078       4.2968          —        —    -4.7 %  (#815)
packunpk          4.6182       4.4649          —        —    -3.3 %  (#815)
arrfill           0.1449       0.0719       3.09x    1.48x   <- -50 %
slices            0.3437       0.2135       5.13x    3.18x   <- -38 %
sliceasgn         0.0720       0.0554       2.78x    2.19x   <- -23 %
ovlsub            0.1500       0.1329       3.76x    3.39x   <- -11 %  (#815)
symref            0.2159       0.2160       9.76x    9.80x
regexg            0.0001       0.0000          —        —
```

Plus, off the board: a **boxed** scalar accumulator (`my $s=0; my $r=\$s;
for (1..5e6) { $s = $s + $_ }` — the escaping reference defeats the raw-slot
verdict, so every write is a `box-set`) **0.2521 s → 0.1734 s, −31 %**
(#811 + #815 together).

**What sb-sprof actually blamed, and what each fix was.**  Six findings, in
descending size; none of them was allocation.

| finding | cost, where measured | fix |
|---|---|---|
| the generic `aref` on an adjustable vector | 44 % of `arrfill`, 18 % of `arrhash` | `%p-vec-data` hands back the backing SIMPLE-VECTOR so a hot loop indexes it directly (a hand copy loop beat `aref` 3.3×).  Used by `p-aref`, the element write rule, the array-fill walker and the RHS snapshot |
| number → string | 24 % of `slices` (its hash keys are numbers) | `%p-fixnum-string`: a digit loop instead of the printer, **3× faster** (0.21 s → 0.064 s for 2 M conversions).  Everything numeric that is printed, interpolated or used as a hash key pays this |
| the overload NEGATIVE path (#815) | 14 % of the `pack` loop | one inline predicate `%p-no-overload-possible-p`, asked AT THE CALL SITE by the seven hot askers, not only inside `p-find-overload` |
| `box-set`'s store dispatch (#811) | 1.54× a plain slot write | a fast path for a raw scalar into an untied, unmagic, plain-valued box; `pos()`'s `remhash` only when `*p-match-pos*` is non-empty |
| `vector-push-extend` | 18 % of `arrfill`, 15 % of `slices` | `%p-vpush` (write + fill-pointer bump when capacity allows) and preallocating the slice result to its known length instead of growing it from 0 |
| two per-ELEMENT predicates compiled as full CALLS | 4.9 % of `arrfill` | `p-flatten-marker-p` and `%p-hash-marker-p` were DEFINED BELOW their hottest callers, so neither the struct predicate nor the `inline` proclamation was in force there.  Moving the definitions up is the whole fix |

> **The ordering trap, worth more than any single fix.**  The first version of
> `%p-no-overload-possible-p` asked "is the overload table empty?" FIRST,
> reasoning that one slot read settles every program that never says
> `use overload`.  It cost **`collatz` 19 %** (0.776 → 0.926 s, reproduced in
> three runs), because `HASH-TABLE-COUNT` is an out-of-line call and
> `%pcl-raw-coerce-check` asks the predicate twice per iteration on a RAW
> number — where the predecessor exited on a single `p-box-p` test.  Reordered
> (box test → payload test → table read), `collatz` is back to 0.773 s.  A
> "cheap global short-circuit" is only cheap if it is cheaper than the test it
> jumped in front of; sb-sprof said so in one run.

**Still open on this axis**, both re-sized by measurement in the same session
(task **#862**): the design's phase-4 EMISSION arms.  The read-only verdict for
a `foreach`-LIST loop variable is worth ~40 % of a read-only foreach
(a 30 M-iteration read loop over a 1000-element array runs in ~0.48 s where the
never-promoting index spelling runs in ~0.29 s; perl 0.616 s / 0.708 s) and
also closes #810.  The slice COPY-position arm is now the SMALLER prize:
after this session `slices` no longer shows `make-p-box` or the promotion arm
anywhere in its top 19 — the remainder is equal-hash lookups (21 %) and
`p-array-fill`'s own construction.

### 0.2e After the FOREACH read-only arm (s459am, phase 4's emission half, #862 ARM A)

`for my $v (@a)` aliases each element, which under raw element storage
PROMOTES every slot to a box — once, but promotion is MONOTONE, so the array
then pays box indirection on every later read of it forever.  When the
annotator can prove the loop variable read-only the identity is never used, so
the loop lowers to `p-foreach-raw`, which binds the slot as it stands.  Design
`docs/boxed-aggregates-design-s455.md` §4.4 + P4b; Kind-A gate `foreach-raw`.

A NEW BENCH ROW, `feread`, is this arm's own metric — the exact program #862
quotes: `my @a=(1..1000); for (1..30000) { for my $x (@a) { $s += $x } }`.
A/B in one session through the arm's own gate (BENCH_K=3):

| row | arm OFF | arm ON | |
|---|---:|---:|---|
| `feread` | 0.3970 s / **0.71×** | 0.2746 s / **0.47×** | −31 %, the predicted ~40 % of the loop |
| `slices` | 0.3033 s / 3.02× | 0.2101 s / 3.06× | **unmoved** (the two runs' perl sides differ more than the arm does) |

**`slices` not moving is the result #862 asked for before anyone spent on ARM
B**, and it confirms §0.2d's profile reading from the other direction: the
promotion arm contributes ~nothing there.  ARM B as scoped — "skip the
promotion in a slice COPY position" — is optimizing something that is not in
the profile; **recommended for closure, task #882**.  `slices`' remaining ~3×
is the equal-hash lookups, `p-array-fill`'s construction and the key
stringify, which is a different piece of work.

**And the `regexg` row was never measuring anything** (task #814, fixed here):
`tools/bench-exec.pl` hand-wrote its own SBCL command line instead of using
`tools/lib/PCLSbcl.pm`, so it ran PCL on SBCL's 2 MB DEFAULT control stack,
where the row's `'a' x 200000` died before the loop ran — identically at N=5
and N=0, and `2>&1 >/dev/null` hid it, so the subtraction of two crashes
printed 0.0009 s / **0.01×**.  This is #324 exactly, one runner later.  Fixed
three ways: the command line now comes from PCLSbcl; **every row is VERIFIED
before it is timed** (both engines' stdout must match and PCL must exit 0, or
the row prints BROKEN and is not timed — a crashed run is fast, so without
this the table's most attractive number is its least trustworthy one); and the
underlying `p-str-x` stack blowup is task #880.  `regexg` now reads **2.19×**,
and agrees with itself at two N.  **Read no bench row whose signal is under
the ~1 s constant term both runs pay** — `strcat` is the next one, task #881.

### 0.2f The board re-tabulated (s463av, round 20) — the first full table since §0.2e

Rounds 16–19 moved five rows whose numbers lived only in `docs/session-log.md`
sections.  This is the whole board again, on the tree round 20 delivers, with
the task that moved each row.

```
perl tools/bench-exec.pl          # on the s463av tree; BENCH_K=5 is the default
```

Median of three best-of-5 runs; the `pcl(s) spread` column is that row's
min..max across the three, so a row whose spread is wider than the move it is
being read for is a row that has not been measured yet.  **1-minute load
1.3–7.3** — a sibling correctness agent and the reviewer were live on the same
box all session — so read the **PCL absolute seconds against the perl column**
(the §0.2a rule), never the ratio alone.

```
bench          perl(s)     pcl(s)  pcl/perl   pcl(s) spread    last recorded
intloop+=       0.0667     0.0224     0.34x   0.0207..0.0254   0.27x  §0.2d
intloop=        0.0663     0.0205     0.31x   0.0192..0.0219   0.29x  §0.2d
cfor            0.1083     0.0302     0.28x   0.0298..0.0385   0.25x  §0.2d
arrhash         0.1364     0.0835     0.61x   0.0827..0.1077   0.67x  §0.2d
fib(27)x        1.5083     0.4327     0.29x   0.4315..0.4503   0.29x  §0.2d
gcdrec          0.1930     0.0952     0.49x   0.0949..0.0981   0.50x  §0.2d
collatz         1.9404     0.5104     0.26x   0.5079..0.5464   0.25x  s460ao
strcat          0.2997     0.6492     2.17x   0.6429..0.7025   2.20x  s461aq
pack            0.0038     3.4051   896.08x   3.4025..3.5687      —   §0.2d
packunpk        0.0042     3.5817   852.79x   3.4246..3.6291      —   §0.2d
arrfill         0.0491     0.0711     1.45x   0.0710..0.0748   1.48x  §0.2d
slices          0.0686     0.2071     3.02x   0.2031..0.2184   3.18x  §0.2d
sliceasgn       0.0266     0.0534     2.01x   0.0531..0.0547   2.19x  §0.2d
listcopy        0.5126     0.4771     0.93x   0.4769..0.4830    NEW   #981
feread          0.4266     0.1943     0.46x   0.1933..0.1946   0.47x  §0.2e
feread2         0.4272     0.5404     1.26x   0.5387..0.5413      —   #883
ovlsub          0.0398     0.1307     3.28x   0.1291..0.1406   3.39x  §0.2d
symref          0.0228     0.0324     1.42x   0.0310..0.0339   1.02x  s461aq (quiet box)
regexg          0.3706     0.8059     2.17x   0.7847..0.8256   2.19x  §0.2e
```

**Re-verified after the rebase** onto main `aafb02c` (the sibling agent's
emission work, gen v2-581), one best-of-5 run at 1-min load **1.24**, the
quietest the box got all session: every row within noise of the table above,
and several PCL absolutes at or just under their medians — `intloop=` 0.0177,
`cfor` 0.0291, `arrhash` 0.0800, `slices` 0.2052, `listcopy` 0.4599 (0.90×),
`symref` 0.0316, `regexg` 0.7754.  Nothing in that merge moves this board.


**Ten of nineteen rows beat perl.**  What moved each, and who owns what is
left:

| row | what moved it | what is left |
|---|---|---|
| `intloop+=` `intloop=` | #759 + #761 (s456af) took them from 2.02×/4.83× to under 0.30× | nothing; they are 3× faster than perl |
| `cfor` | the counting-loop lowering (`p-foreach-range-raw`) | — |
| `arrhash` | #816's raw elements (−17 %) then s458ak's accessor dispatch, 1.28× → 0.67×; #950 a further −5.2 % | beats perl |
| `fib(27)x` `gcdrec` | unmoved for six rounds; the sub-call path is settled | — |
| `collatz` | **#890** (s460ao): the raw-numeric freeze DECLINES instead of dying, 0.40× → 0.25× | — |
| `strcat` | **#881** (s461aq) raised N_big to 20 M — before that the row was two noise samples and printed 1.00×, 1.64× and 1.79× on the same two trees | 2.17×: PCL's fill-pointer buffer against perl's realloc-in-place |
| `pack` `packunpk` | **#74** owns the whole ratio: PCL runs the transpiled pure-Perl oracle (`cl/pack-impl.pl`) and re-parses the template per call, where perl runs `pp_pack.c` | a plan/executor restructure, a session of its own |
| `arrfill` | #816 (−24 %) then s458ak, 3.09× → 1.45× | the remainder is the per-element store rule |
| `slices` | s458ak 5.13× → 3.02×; #922 −4.3 %; #981 +0.9 % | **#982** key stringify ~9 %, **#985** the slice-argument list ~5 %, and ~21 % equal-hash lookups that are the hash table itself |
| `sliceasgn` | s458ak −23 %; it also PAYS for `%p-assign-snapshot`, the correctness half of #816 (perl evaluates a list assignment's whole RHS before its first store) | — |
| `listcopy` | **NEW, #981** — `my @c = @src` at 50 elements.  The shape had no row, which is why a 20 % win in it was invisible in the two slice rows (they assign 5 and 10) | beats perl at 0.93× |
| `feread` | **#862 ARM A** (s459am): a read-only foreach binds the slot as it stands, 0.71× → 0.47×; #950 a further −7.2 %, the largest of that change | beats perl |
| `feread2` | **#883** measured and DECLINED; #923's sized flattener | 1.26× — the multi-array flatten still promotes |
| `ovlsub` | s458ak −11 % (#815) | **#582**, blocked on @ISA-write invalidation |
| `symref` | **#812** (s461aq): the name → SYMBOL memo, 9.80× → **1.02× on a quiet box** | 1.42× here is this box, not a regression — see below |
| `regexg` | **#680** (s454ac) 30× → ~2.4×, and **#814** (s459am), which found the row had been timing a CRASH | the cl-ppcre engine itself; **#71** (PCRE2 FFI) is the next lever |

**Two rows read higher here than in their record, and neither is a
regression.**  `symref` 1.02× → 1.42×: perl's column is 0.0228 s against
s461aq's 0.032 s on its quiet box, so *perl* got faster here and the ratio
moved with it — the PCL absolute (0.0324 s) is within 1 % of the record's
0.0327 s.  `intloop+=` and `cfor` sit ~16–20 % above §0.2d's quiet-box
absolutes (0.0209 vs 0.0174; 0.0302 vs 0.0261) while `intloop=` matches its
record exactly (0.0182 vs 0.0185).  **That was probed rather than left as a
suspicion**: five interleaved A/B runs of this tree's runtime against round
18's (`4c354bc`) give `cfor` **+9.3 %, all five samples positive** — round 18
is *slower* — and `intloop+=` +2.8 % with mixed signs.  So rounds 18 and 19 did
not cause it, and the gap is either older or is this machine.  **Settling it
needs a quiet box, and it is a ten-minute job there**: task **#986** has the
numbers and the bisection recipe.

**SETTLED (s473r, round 31) — #986 IS CLOSED, and the answer is "intended".**
It is not this machine: a `git archive f49a34df` extraction (s458ak, the tree
§0.2d was measured on) hits §0.2d's numbers to the fourth decimal on this box
in a quiet window (`intloop+=` 0.0175 against the record 0.0174, `cfor`
0.0253, `intloop=` 0.0175), with perl flat across both trees — so the record
REPRODUCES and today's tree is +17.7 % / +5.1 % / +2.3 % against it.  The
cause is `bfa170d9` (round 18, task **#900**): `p-incf-raw`'s body became
`%compound-arith-form`'s guarded form, i.e. perl's `+=`/`++` overload
dispatch, which PCL had been skipping.  The row's EMISSION is byte-identical
between the two trees, which is what localises it without any timing, and the
guard is worth **15.1 %** of `intloop+=` by hand-replacement
(`(p-incf-raw $s $_)` against `… :numeric`, one core, interleaved,
best-of-7).  DECIDED §s470 already ruled the correctness fix stays; the
guard-free path for a slot the emitter can prove is task **#1516**.  The
remaining few % — `cfor`'s 5.1 %, `intloop=`'s 2.3 % — is the CORE BEING
RE-LAID-OUT: a runtime built from the base plus two NEVER-CALLED functions of
the same size moves `cfor` −3.6 % and `intloop+=` −5.6 %, which is the cheap
discriminator to build before attributing anything in that band.

**AND TWO ROWS MOVED IN ROUND 31 that this table predates** (#1514, runtime
only): `collatz` 0.5305 → 0.4005 s, and the new `arith` / `useint` pair
0.1109 → 0.0515 and 0.1262 → 0.0300 — `int()`, `/` and `%`, not `+`, which
hand-replacement shows was already open-coded (+0.8 %).  See the plan's
§A.4 ROUND 31 verdict.

**What the board says as a whole.**  The counting, recursion, foreach and
whole-array-copy shapes beat perl, several of them by 3×.  Everything still
behind perl is behind for a *named* reason with an owner: an oracle that
re-parses (`pack`, #74), an engine PCL does not own (`regexg`, #71), a dispatch
cache that needs an invalidation story (`ovlsub`, #582), a string buffer versus
realloc (`strcat`), and the hash/slice traffic (`slices`, `sliceasgn`, #982 and
#985).  There is no row left whose gap is unexplained.

### 0.2g `slices` after the slice-argument flattener (s464ax, round 22, #985)

`%p-flatten-slice-args` — the ONE flattener the eight slice functions share —
answered a LIST, so `@a[1..5]` and `@h{@k}`, which hand it a single vector,
allocated the index list twice (`coerce … 'list`, then LOOP `APPEND`'s copy).
It now answers a SIMPLE-VECTOR and a COUNT, and in that single-vector case the
vector is the argument's own backing store, so nothing is allocated at all.

**sb-sprof, N = 1.5e6, this row's program, same box, before and after:**

| | before | after |
|---|---|---|
| total sampling time | 0.686 s (1372 samples) | **0.654 s** (1308) |
| `sb-kernel:vector-to-list` | 1.7 % self / 2.0 % total | **absent** |
| `sb-impl::copy-list-to` | 2.6 % self / 2.8 % | **absent** |
| `LISTIFY-&REST` + `listify_rest_arg` | — | 0.0 % self / 0.1 % total |

That last row answers #985's second question by measurement: the `&rest` list
the eight functions cons is **1 sample of 1308** — it is not the allocation
that dominates, so no `dynamic-extent` declaration was added.

**Bench, interleaved best-of-5 A/B (`BENCH_RT_B`), A = this tree, B = main
`80b715c`; positive = B slower = the change helps.  The CONTROL is A against a
BYTE-IDENTICAL COPY of A, run alternately with the A/B so both see the same
box (1-min load 0.1–1.0 throughout):**

```
row          control (3 runs)          A/B (4 runs)              verdict
slices       -0.6 -1.7 +0.1            +5.8 +5.8 +6.4 +5.0       -5.5 %, every run outside the band
sliceasgn    +3.2 -1.3 -2.0            -1.0 +0.2 +0.3 -0.0       inside the band (it takes the LHS-macro path)
arrhash      -1.3 +0.4 -1.1            +0.5 -0.5 +1.0 +2.0       inside
arrfill      -1.0 +1.5 -1.7            +0.3 -0.7 +0.4 -0.7       inside
listcopy     +0.0 +4.2 +0.1            -0.1 +0.2 -0.2 -0.0       inside
feread       +1.7 +0.3 +0.3            +2.2 +12.6 +0.8 +2.4      inside but for one outlier (this row has no slice)
feread2      +1.5 -1.1 +1.5            +1.7 +3.2 +1.3 +1.5       inside
```

PCL absolute seconds for `slices`: **0.1878–0.1924 (A) against 0.2002–0.2012
(B)**, perl 0.0656–0.0681 — so the row reads **2.78–2.92×** here against
§0.2f's 3.02×.  `sliceasgn` does not move because a list-assign LHS slice goes
through `%p-slice-args-vector`, which must still hand back a length-exact
vector.

### 0.2h `slices` after the small-fixnum string table (s464ax, round 22, #982)

Since the boxed-aggregates flip an array ELEMENT holds a RAW value until
something aliases it, so `@h{@k}` re-stringifies the same ten integers on
every iteration with **no box and therefore no SV cache anywhere** to hold the
answer — #922 closed with exactly that note.  `%p-fixnum-string` now answers
small non-negative values out of a pre-built 1024-entry table (~32 KB) built
BY the digit loop, which is unchanged and renamed `%p-fixnum-string-digits`.

**The entries are SHARED, and that was the gating question, answered twice.**
Every in-place string writer in `cl/` was read and none writes into a string
it did not make (the list is in the runtime comment beside the table).  Then it
was *measured*: a 16-route probe — a hash key handed back by `keys`/`each`/a
foreach over `keys`, interpolation, `join`, `sprintf`, an array element, each
followed by chop / substr-lvalue / vec-lvalue / an in-memory filehandle / `++`
/ `s///` / `tr///` / a str-buffer append — run with the entries deliberately
built **adjustable with a fill-pointer**, i.e. mutable in place: all 16 routes
still read correctly.  With entry 5 poisoned to `"ZZ"` the first field reads
`key:ZZ`, which is what makes the negative result mean something.  Guard rows:
`Pl/t/misc-fixes-02.t` (#982 ×2).

**The instrument.**  `bench-exec` on a shared box read ±8–15 % on `slices`
that afternoon, so the primary measurement is an interleaved wall-time A/B of
the two SAVED CORES on one emitted program (startup subtracted, best-of-K),
with a same-core CONTROL run alternately:

```
program            control (A vs A)        A/B (A = table, B = 0e54656)
slices, N=1.5e6    +1.6 % +0.7 %           +20.6 %  +16.9 %  +17.4 %
                   (B vs B) +0.2 %
numstr-small       -0.8 %                  +20.7 %    "" . ($i % 1000), 4e6
numstr-big         +2.9 %                  +0.1 %     "" . ($i + 1e6),  4e6
numstr-neg         -0.6 %                  +0.4 %     "" . -$i,         4e6
```

**~15 % of the `slices` program and ~17 % of a small-integer stringify loop;
the non-table paths are unchanged** — the extra `TYPEP` costs nothing
measurable.  Interleaved sb-sprof of the same two cores, three rounds each:
A 1472/1500/1480 samples (0.736/0.750/0.740 s CPU) against B
1754/1761/1737 (0.877/0.881/0.869); `%p-fixnum-string` is 5.5/5.1/4.5 % self
in B and absent from A.  Note the profile's own attribution UNDERSTATES the
win — the 15 M short-lived strings the digit loop allocated cost more than the
time spent inside it.

On the board, `bench-exec` A/B twice: `slices` **+13.5 % and +13.4 %** (base
slower) in both, every other row sign-mixed and inside a control band that was
±7 % on `intloop+=` and ±33 % on `pack` that afternoon.  `slices` reads
**2.49x / 2.51x**.

**Where `slices` stands after round 22: 3.02x -> ~2.5x** (#985 then #982).
Both commits together against main `80b715c`, one interleaved best-of-5 run at
1-min load 3.6:

```
bench          perl(s)    pclA(s)    pclB(s)       B/A    A/perl
slices          0.0685     0.1699     0.2036    +19.9%     2.48x   <- the round
arrhash         0.1302     0.0810     0.0840     +3.8%     0.62x
gcdrec          0.1890     0.0935     0.0955     +2.1%     0.49x
feread2         0.4241     0.5429     0.5538     +2.0%     1.28x
arrfill         0.0489     0.0701     0.0705     +0.6%     1.43x
listcopy        0.5131     0.4720     0.4738     +0.4%     0.92x
feread          0.4186     0.1954     0.1958     +0.2%     0.47x
collatz         1.9328     0.5225     0.5223     -0.0%     0.27x
sliceasgn       0.0252     0.0526     0.0524     -0.3%     2.09x
```

What is left on the row is the ~23 % that is the `equal` hash table itself.

### 0.2i The board on a QUIET box (s467, 2026-09-04, main `bc9aa4a`, gen v2-611)

Taken for the README refresh: nothing else was running (the gate had just
finished, the CPAN board started only after this run), one `perl
tools/bench-exec.pl` at the default best-of-5.  This is the first full table
on a quiet machine since §0.2f, and the first with rounds 21–23 in the tree
(#964 the return protocol, #994 tail-return, #985/#982 the two `slices`
changes, #1028 bitwise, #1035 `p-let`).

```
bench          perl(s)     pcl(s)  pcl/perl   §0.2f      note
intloop+=       0.0652     0.0230     0.35x    0.34x
intloop=        0.0680     0.0196     0.29x    0.31x
cfor            0.1145     0.0293     0.26x    0.28x
arrhash         0.1364     0.0819     0.60x    0.61x
fib(27)x        1.4670     0.4211     0.29x    0.29x
gcdrec          0.1928     0.0998     0.52x    0.49x
collatz         1.9518     0.5064     0.26x    0.26x
strcat          0.3168     0.6786     2.14x    2.17x
pack            0.0029     3.3648  1174.03x    896x     perl's column moved (0.0038 -> 0.0029); PCL 3.41 -> 3.36 s
packunpk        0.0041     3.4902   857.54x    853x
arrfill         0.0488     0.0710     1.46x    1.45x
slices          0.0678     0.1760     2.60x    3.02x    #985 + #982 (§0.2g/h predicted ~2.5x)
sliceasgn       0.0266     0.0530     1.99x    2.01x
listcopy        0.5129     0.4802     0.94x    0.93x
feread          0.4090     0.1920     0.47x    0.46x
feread2         0.4168     0.5486     1.32x    1.26x    PCL 0.5404 -> 0.5486 s, inside §0.2f's spread
ovlsub          0.0399     0.1381     3.46x    3.28x    PCL 0.1307 -> 0.1381 s, inside §0.2f's spread (0.1291..0.1406)
symref          0.0222     0.0304     1.37x    1.42x
regexg          0.3685     0.8043     2.18x    2.17x
```

Read against §0.2f by the PCL absolute seconds (the §0.2a rule): every row
is inside that table's recorded spread except `slices`, which is the two
round-22 changes landing as predicted.  The `pack` ratio rose because
*perl* ran faster on this machine today; PCL's own time is unchanged.
**Ten of nineteen rows beat perl**, the same ten as §0.2f.

### 0.2m The board on a QUIET box (s479, 2026-09-08, main `a9f2a264`, gen v2-1080)

Taken for the README refresh (#1527: the method-call rows join the
front-page table), at the start of s479 while the two execution agents were
still in their reading phase: 1-min load 0.86 at the start, 1.20 at the end
(§0.2k ran at 0.46 → 1.15), one `perl tools/bench-exec.pl` at the default
best-of-5, four minutes.  The first table with round 32 (s473s: the own-class
method cache, `p-defclass`, the hashed-BMH literal-prefix scan) and the
s473b/c/d/e correctness batches in the tree.  All 34 rows:

```
bench          perl(s)     pcl(s)  pcl/perl
----------- ---------- ---------- ---------
intloop+=       0.0647     0.0207     0.32x
intloop=        0.0644     0.0189     0.29x
cfor            0.1053     0.0258     0.24x
arith           0.1465     0.0369     0.25x
useint          0.0961     0.0240     0.25x
arrhash         0.1293     0.0811     0.63x
arrhash-k       0.0557     0.0662     1.19x
fib(27)x        1.4443     0.4168     0.29x
gcdrec          0.1908     0.0993     0.52x
fibret          1.4460     0.4198     0.29x
gcdret          0.1888     0.0867     0.46x
subret          0.2024     0.0822     0.41x
methret         0.0884     0.0937     1.06x
collatz         1.9412     0.3435     0.18x
strcat          0.2932     0.6322     2.16x
pack            0.0035     3.8032  1094.78x
packunpk        0.0038     3.8136  1014.55x
arrfill         0.0479     0.0285     0.60x
slices          0.0689     0.1131     1.64x
sliceasgn       0.0258     0.0293     1.14x
listcopy        0.5095     0.1714     0.34x
pushloc         0.1024     0.0286     0.28x
sortnum         0.0251     0.0668     2.66x
sortstr         0.0690     0.1107     1.60x
feread          0.4211     0.1217     0.29x
feread2         0.4120     0.1247     0.30x
feread3         0.4265     0.1185     0.28x
ovlsub          0.0395     0.1315     3.33x
symref          0.0222     0.0092     0.41x
json-rt         0.8756     1.5407     1.76x
moo-objs        0.0394     1.2727    32.29x
textproc        0.4374     1.4406     3.29x
regexg          0.3711     0.7679     2.07x
subste          0.0551     0.2565     4.65x
```

**Against §0.2k.**  The two levers round 32 shipped read as predicted:
`methret` 1.51× → **1.06×** (0.1336 → 0.0937 s, #582's own-class half) and
`textproc` 4.58× → **3.29×** (1.9873 → 1.4406 s, #1461).  Every other row is
inside its spread except three that moved the WRONG way, and all three were
run down before this table was written, because a board taken beside two
agents is exactly the shape that hides a regression:

* `arrhash-k` 1.04× → 1.19× (0.0592 → 0.0662 s; re-timed 0.0662 / 0.0702 /
  0.0679) and `regexg` 1.98× → 2.07× (0.7265 → 0.7679 s; re-timed 0.7668 /
  0.7684, perl's column unchanged).  A runtime A/B (`BENCH_RT_B`: one
  emission, two cores, interleaved) against the runtimes of 047cc249 (§0.2k),
  post-s473d, post-s473b, post-s473c, post-s473s and every `cl/` commit of
  s473s and s473e put the step for BOTH rows on `72bb6d22` (s473e member 4,
  #1164): every earlier runtime 5–10 % faster on arrhash-k, that one −0.5 %.
  Its whole runtime change is one `let` of `*p-dyn-loop-frames*` inside
  `p-sort`, which neither row calls, and `sb-walker:macroexpand-all` of the
  arrhash-k loop is byte-identical under the post-s473s and HEAD runtimes.
  **The discriminator was a PADDING-ONLY runtime — HEAD plus one unused
  `defun` inserted before `p-sort`, A/B'd against HEAD**: arrhash-k −11.6 /
  −9.8 / −11.7 % for a defun of 8 / 40 / 200 list elements, regexg −0.1 /
  −1.6 / −12.5 %.  A semantically empty change moves both rows by as much as
  the "regression", in the faster direction: it is CODE PLACEMENT (SBCL's
  function alignment downstream of the changed function), the effect s473c
  met when one store read +5.5 % on `intloop=` and 0 % on `intloop+=`.  The
  rows stand as measured; nothing is owed on them.
* `moo-objs` 29.45× → 32.29× (1.1629 → 1.2727 s).  It cannot be runtime-A/B'd
  before s473s (the emission needs the new macros); post-s473s vs HEAD reads
  −0.3 %.  Alternating the whole tree twice — HEAD 1.2483 / 1.1739 s, a
  worktree at 047cc249 1.1599 / 1.1904 s — overlaps, so the board reading was
  the load (the row is compile-dominated, #1189).

**The rule this leaves (DECIDED §s479): a bench row's noise floor includes
code PLACEMENT, and it is measured with a padding-only runtime before a
±10 % move is attributed to a commit.**  A runtime bisect always finds a
"culprit" — the first commit whose function sizes shift the hot loop — and
that culprit's diff is the tell: when it touches nothing the row calls, run
the pad probe.  Raw output: `~/pcl-agent-scratch/s479/bench-*-s479.txt`.

### 0.2l Round 32 movers (2026-09-08) — the method-call round

**Not a re-run of the board.**  Each line is the round's own interleaved A/B
(one transpiled program, N runtimes on their own cores, all series round-robin
in ONE window, best-of-K, **column 2 a byte-identical COPY of column 1 as the
control**, `uptime` printed beside every number — the §0.5 method).  The box
was shared with a sibling agent's gate for part of the round; where the
control column moved more than ~2 % the row was re-measured and the second
reading is the one quoted.  Tree: main `c709804a`, generation v2-1060.

```
row        change      control   lever
methret    -27.5 %     +1.4 %    #582 own-class half — p-method-call caches
                                 (class, method) -> the SYMBOL
ovlsub      -5.8 %     -0.2 %    same
json-rt     -4.8 %     -1.4 %    same
methinh      0.0 %     +0.4 %    same — pure INHERITED dispatch pays the failed
                                 probe and gains nothing; it must not regress
textproc   -24.5 %     +2.9 %    #1461 — literal-prefix scanning is BMH with a
                                 256-way HASHED skip table
json-rt     -1.4 %     +0.1 %    #1461
subste      -0.3 %     +1.9 %    #1461
regexg      +5.9 %     +1.1 %    #1461 — no literal prefix, so nothing to gain
moo-objs    +2.3 %     n/a       #1518 p-defclass — NOISE, and zero by
                                 construction: the guard never fires (see below)
```

**#1518 is a measured ZERO, recorded so it is not re-derived.**  The task
predicted ~35 % of `moo-objs` from re-run `ensure-class` calls, by analogy
with the `p-defpackage` half (#1189).  Counting both readiness predicates on
the same program says the analogy was false: `moo-objs` runs
`%p-package-ready-p` 6096 times at N=500 and 24096 at N=2000 (12 per
iteration, #1189's own figure) and `%p-class-ready-p` **20 times, at every N,
with zero hits**.  The preamble a string eval's program carries declares its
PACKAGE and not its class.  `p-defclass` is kept for the IR (the emitted file
no longer writes a bare host `defclass`), not for a number.

**#1461's memory result is the reason it is the hashed table and not the
flag.**  Twenty literal-prefix scanners: 14.8 MB → **184.8 MB** with
cl-ppcre's dense skip table (8.5 MB each), 14.8 MB → **14.9 MB** with the
256-way one.  `Pl/t/bmh-scan-01.t` asserts the second, so a regression to the
first cannot pass silently.

**And three of #1461s four rows CANNOT move**, which is how the `regexg`
reading is settled without arguing about noise: counting the BMH matchers each
row builds says `textproc` builds exactly ONE (its `tag=t` literal prefix) and
`regexg`, `json-rt` and `subste` build ZERO.  A row that builds none runs the
same code either way -- the DENSE column reads +3.9 % on `regexg` for the same
reason -- and every row reports one extra, the install self-tests own probe
scanner.  Peak RSS on `textproc`: base 99.9 MB, hashed 98.2 MB, dense 106.6 MB.

### 0.2k The board on a QUIET box (s475, 2026-09-07, main `047cc249`, gen v2-1020)

Taken for the README refresh the USER asked for, right after the session's
last merge: nothing else running (1-min load 0.46 at the start, 1.15 at the
end), one `perl tools/bench-exec.pl` at the default best-of-5, four minutes.
The first full table since §0.2i (2026-09-04) and the first with rounds
27–31 in the tree: #1180 `symref-const`, the round-27 bulk fill and
`numeric-slot`, `foreach-arrays` (#1184, #1409), the round-28 aggregate
family, #1250/#1251 (regex ops once per site), #1200 (the eval disk cache —
not in these rows, they subtract startup), #1182 (`%p-array-grow-discarding`),
#1514 (`int()`, `/`), #1189 (the package preamble).  All 34 rows, incl. the
three macro rows and the two new arithmetic rows:

```
bench          perl(s)     pcl(s)  pcl/perl   §0.2i    note
intloop+=        0.0647     0.0202     0.31x   0.35x  
intloop=         0.0640     0.0178     0.28x   0.29x  
cfor             0.1183     0.0256     0.22x   0.26x  
arith            0.1482     0.0430     0.29x   (new)  
useint           0.0965     0.0247     0.26x   (new)  
arrhash          0.1285     0.0808     0.63x   0.60x  
arrhash-k        0.0570     0.0592     1.04x   (new)  
fib(27)x         1.4492     0.4294     0.30x   0.29x  
gcdrec           0.1867     0.0941     0.50x   0.52x  
fibret           1.4516     0.4324     0.30x   (new)  
gcdret           0.1866     0.0852     0.46x   (new)  
subret           0.2016     0.0833     0.41x   (new)  
methret          0.0883     0.1336     1.51x   (new)  
collatz          1.9257     0.3514     0.18x   0.26x  
strcat           0.2915     0.6288     2.16x   2.14x  
pack             0.0037     3.6440   995.37x   1174x  
packunpk         0.0033     3.6053  1107.34x   858x   
arrfill          0.0478     0.0282     0.59x   1.46x  
slices           0.0681     0.1124     1.65x   2.60x  
sliceasgn        0.0261     0.0296     1.13x   1.99x  
listcopy         0.5058     0.1811     0.36x   0.94x  
pushloc          0.1014     0.0282     0.28x   (new)  
sortnum          0.0246     0.0689     2.80x   (new)  
sortstr          0.0674     0.1124     1.67x   (new)  
feread           0.4223     0.1220     0.29x   0.47x  
feread2          0.4259     0.1295     0.30x   1.32x  
feread3          0.4095     0.1198     0.29x   (new)  
ovlsub           0.0388     0.1392     3.58x   3.46x  
symref           0.0222     0.0084     0.38x   1.37x  
json-rt          0.8764     1.5881     1.81x   (new)  
moo-objs         0.0395     1.1629    29.45x   (new)  
textproc         0.4338     1.9873     4.58x   (new)  
regexg           0.3662     0.7265     1.98x   2.18x  
subste           0.0572     0.2528     4.42x   (new)  
```

**What it says against §0.2j's derived figures — the board is the measure,
the derivations were not.**  Where a lever's own A/B was the only number,
the derived ratio was right within noise for `feread` (0.30 → 0.29 measured)
and `collatz` (0.19 → 0.18), but `listcopy` came in at **0.36×**, not the
derived 0.69× — #1182's −27 % was measured under load beside a sibling's
sweep, and the quiet box shows the lever is worth far more — and `slices`
at 1.65× (derived 2.1×) likewise; `regexg` at 1.98× is less than the derived
1.65× (round 29's +32 % was taken at a different N).  Two rows §0.2j did not
list moved because rounds 27–28 landed after §0.2i too: `symref` 1.37× →
**0.38×** (#1180 `symref-const`: a constant name resolves once per site) and
`arrfill` 1.46× → **0.59×** (the round-27 bulk fill), with `sliceasgn`
1.99× → 1.13× from the round-28 aggregate family.  **Fourteen of the
nineteen §0.2i rows now beat perl** (ten did then); the five that do not are
`sliceasgn`, `slices`, `regexg`, `strcat`, `ovlsub` and the two `pack` rows.
Of the rows added since: `arith`/`useint` 0.29×/0.26× (#1514 — the pragma
is now the FASTER of the two), `pushloc` 0.28×, `subret` 0.41×, `arrhash-k`
1.04×, `methret` 1.51×, `sortnum` 2.80×, `sortstr` 1.67×, `subste` 4.42×;
the macro rows `json-rt` 1.81×, `textproc` 4.58×, `moo-objs` 29.45×.

This table supersedes §0.2j's derived ratios; §0.2j stays as the record of
which lever moved which row.  Raw output: the s475 scratch
(`bench-board-s475.txt`).

### 0.2j Rounds 29–31 movers (2026-09-07)

**Not a re-run of the board.**  Each line is the round's own interleaved A/B
(two `.lisp` files on ONE core, best-of-K, a byte-identical control pair timed
in the same window, `uptime` printed beside it — the §0.5 method) on the row
the lever changed; the rounds' control rows stayed inside §0.2i's recorded
spread.  A ratio marked *derived* is §0.2i's ratio scaled by the measured
change in PCL's own time, i.e. it assumes perl's column did not move; the
next quiet-box board (the §0.2i recipe) supersedes every derived figure.
Tree at the last lever: main `5d59e447`, generation v2-1010.

```
row        §0.2i     now           PCL time                     round  lever
regexg     2.18x     ~1.65x drv    -24 % (speedup +32 %)        29     #1250 a regex/subst/tr LITERAL builds its op once per SITE
subste     (new)     --            -43.5 %                      29     #1251 s/// keeps its compiled record (many short s///)
feread     0.47x     0.30x MEAS    0.1938 -> 0.1230 s (-36 %)   30     #1409 ONE bare array is a foreach RUN (svref over the store)
listcopy   0.94x     ~0.69x drv    -27 %                        30     #1182 growth installs a FRESH store (%p-array-grow-discarding)
slices     2.60x     ~2.1x  drv    -20 %                        30     #1182 same
collatz    0.26x     ~0.19x drv    0.5457 -> 0.3902 s (-26 %)   31     #1514 int() inline; `/` without a RATIO inside 2**53
arith      (new)     --            0.0999 -> 0.0455 s (-53 %)   31     #1514 `$s = ($s*3 + int($i/7)) % 1000003` -- the loop the task was filed on
useint     (new)     --            0.1182 -> 0.0270 s (-76 %)   31     #1514 the same under `use integer`: was 1.5x SLOWER than `arith`, now 1.7x faster
```

`+`/`*` were already open-coded: replacing `p-+`/`p-*` by CL `+`/`*` in the
emitted `arith` loop moved it 0.8 % (s473r hand-replacement) — the 68 ns the
#1514 filing attributed to "op dispatch" were `int()`, `/` and `%`.

**Whole-program constants** (plan-speed-and-ir-s470.md §A.4.1: one program,
wall clock, nothing subtracted — what a user waits for):

```
program                        s470bn (2026-09-05)   #1188 fasl cache    #1200 eval disk cache (round 30)
use JSON::PP; print 1  (warm)  6.44 s                1.43 s              0.41 s        (cold, no cache at all: 13.42 s)
use Moo; print 1       (warm)  3.50 s                0.343 s             0.201 s
eval "1"; print 1              0.292 s               0.304 s             0.172 s
```

```
macro row     before      after         round  cause
moo-objs      54x perl    29.6x perl    31     #1189 the emitted package preamble re-ran `defpackage` on an EXISTING package
                                               (12x per iteration: Moo's Sub::Quote eval package); 2.00 -> 1.22 s
json-rt       --          +99 % faster  29     #1250/#1251 (the regex op built once per site; s/// compiled record)
textproc      --          +30 % faster  29     same
```

Records: DECIDED §s470bu (round 29), §s473p (round 30), §s473r (round 31);
the ROUND 29/30/31 verdict paragraphs under plan-speed-and-ir-s470.md §A.4.3.
Open on the perf line: #1516 (the `+=` overload guard, 15 % of `intloop+=`),
#1517 (the flattener's 8.5 ns/element), #1518 (the preamble's `defclass`
half), #1142 (a read-only `grep` costs the raw foreach run 30 %), #71 (the
regex engine — brief s473q).


---

## 0.5 Headline results (what the experiments proved)

Every row is a head-to-head timing of two CL programs that compute the
**byte-identical** result (§9 lists the files; numbers are best-of-5,
startup-subtracted, against the runtime core).

| category | emit THIS instead of today's shape | measured | precondition |
|---|---|---:|---|
| **String append** | fill-pointer buffer (`vector-push-extend`) instead of copy-`.=` | **~2400×** | ref doesn't escape, string-only use |
| **Method dispatch** | monomorphic inline cache / direct call instead of `p-method-call` string walk | **~15×** | per-call-site class guard |
| **Boxed accumulator** | raw slot instead of `make-p-box`-per-write | **~13×** | raw-numeric verdict |
| **Numeric-string scalar** | number in slot instead of re-numifying `"42"` per use | **~8.5×** | raw-numeric verdict |
| **`push @a, x`** | `vector-push-extend` instead of `p-push-impl` | **~7×** | non-escaping array + plain element |
| **`sort {$a<=>$b}`** | native `(sort v #'<)` on unboxed keys instead of a boxed generic comparator funcall | **~6×** | recognized comparator idiom |
| **`sprintf` (const fmt)** | pre-compiled formatter instead of re-parsing the template per call | **~5×** | literal format string |
| **Array element read** | `(aref v i)` raw instead of `(p-aref …)` boxed | **~3.5×** | raw-element array |
| **Box write** | mutate `p-box-value` in place instead of re-allocating a box | **~1.3×** | any boxed write that keeps the box |
| **Native fixnum add** | `(the fixnum (+ …))` under `(safety 0)` | ~10× | **range proof** (unsound without it) |
| **Arithmetic op** | *(nothing — `p-+` on raw slots already equals native `+`)* | **~1.0×** | — |
| **Hash incr (const key)** | *(nothing — already ~5 ns/iter)* | ~1.0× | — |

The three surprises that correct the intuition (and my own first draft):
**native `+` is not a win** (R1's `p-+` already open-codes the fast path);
**a hash *value* box is not the cost** — the key stringification and lookup
are; and the fixnum win is real but **gated on a range proof** — the
bignum-correct typed variants gain nothing over `p-+`. Chase the top rows
(boxing removal, buffered append, dispatch caching, oracle re-parse), not the
operator pipeline.

---

## 1. Loops

Already fast: `for my $i (A..B)` → `p-foreach-range-raw` (counting loop, raw
var) is **2.8× faster than Perl** (shipped). The residual loss on the
`for(1..n){ $s += $_ }` idiom is *not* the loop — it is the boxed accumulator
(§3) and the boxed implicit `$_`. Nothing loop-specific remains except
extending the counting-loop lowering to **postfix** `EXPR for A..B` (still the
old materializing path).

---

## 2. Arithmetic / operators — *the p-+ pipeline is already at the sound ceiling*

Emitted for `for my $i (1..n){ $s = $s + $i }` (raw slots):
```lisp
(let (($s 0)) (p-foreach-range-raw ($i 1 n) (setf $s (p-+ $s $i))))
```

Variants, sum 1..N, **N = 5,000,000**:

| variant | CL | exec(s) |
|---|---|---:|
| `a0_current` | `(setf $s (p-+ $s $i))` | **0.0161** |
| `a1_native`  | `(setf s (+ s (1+ i)))` — generic CL `+` | 0.0184 |
| `a3_integer` | as a1 under `(speed 3)`, `(type integer)` | 0.0191 |
| `a4_guarded` | fixnum-if-fixnum-else-generic | 0.0183 |
| `a2_typed`   | `(the fixnum (+ …))` under `(safety 0)` | **0.0015** |

**Findings.**
1. **`p-+` on raw slots is already as fast as a bare CL `+`** (a0 ≈ a1). R1's
   inline fast path open-codes the add; replacing `p-+` with `+` buys nothing.
   *Do not* spend effort emitting generic native ops — it is not a lever.
2. The only real arithmetic win (10×, a2) needs **`(declare (fixnum))` +
   `(safety 0)`**, which is **unsound** for Perl's overflow-to-float semantics
   unless the value's range is proven. The bignum-correct typed variants (a3,
   a4) are **not faster** than a0 — the type check / bignum branch costs what
   `p-+` already costs.

### Suggestion
- **O1. Fixnum specialization ONLY behind a range proof.** For a loop counter
  bounded by a proven-fixnum endpoint (`for my $i (1..$n)` where `$n` is
  raw-numeric fixnum) and an accumulator whose running value is provably
  fixnum-bounded, emit `(the fixnum …)` in a `(safety 0)` region. Where the
  range can't be bounded, keep `p-+` — it is already optimal for sound code.
  This is a *narrow, hard* analysis with a *modest* payoff; schedule it low.
- **O2. Constant-fold literal operands** (`$i * 3`, `$m % 2`) into the fast
  path so the constant isn't re-dispatched each iteration. Cheap, local.

---

## 3. Boxed accumulator — *raw slot is 13× (the intloop+= tax)*

The `for(1..n){ $s += $_ }` idiom keeps `$s` boxed and re-allocates the box on
every write (`p-my-=` → `make-p-box`). Variants, N = 5,000,000:

| variant | CL (inner) | exec(s) |
|---|---|---:|
| `acc0_boxed`  | `(setf $s (make-p-box (p-+ (unbox $s) $_)))` | **0.2237** |
| `acc2_boxmut` | `(setf (p-box-value $s) (p-+ (p-box-value $s) $_))` | 0.1706 |
| `a0_current`  | raw slot `(setf $s (p-+ $s $i))` | **0.0160** |

**Findings.** The boxed form is **~13× slower** than the raw slot. Of that,
re-allocating the box each write (acc0→acc2) is ~23%; the rest is box-slot
indirection plus the boxed loop var. (acc0/acc2 use the boxed-`$_`
`p-foreach-range`; a0 uses raw `$i` — so the 13× is the *combined* accumulator
+ loop-var boxing that the real idiom pays.)

### Suggestions
- **N1. `raw-numeric` verdict (task #62)** — the direct fix: a `my $s` whose
  every use is numeric and whose writes are arith-shaped becomes a raw slot.
  Extend "arith write" to cover `+=`/`-=`/`*=` (today only `$s = $s + X`
  qualifies). Empirical ceiling on this shape: **13×**. The same verdict covers
  the **numeric-string** case (`my $n = "42"` or `$ENV{N}` used only
  numerically): a string slot re-numifies every use — measured **~8.5×** slower
  than freezing the number into the slot once (`num0` 0.82s vs `num1` 0.10s @
  5M). This is the `cfor`/`$n=$ENV{N}` bound tax from §2/O4.
- ~~**N2. In-place box write**~~ — **STRUCK s456af, premise false.**  It asked
  for `p-my-=`/`p-scalar-=` to mutate `(p-box-value)` instead of allocating a
  fresh box; `p-my-=` has always expanded to `box-set`, which does exactly
  that.  Re-measured, today's real emission (`acc_today`, 0.2880 s) is SLOWER
  than the `acc0_boxed` variant N2 wanted to replace, because the cost is
  `box-set`'s store-semantics DISPATCH, not allocation — 1.54× over a plain
  slot mutation.  The real item is **task #811** (a no-tie/no-magic/simple-
  scalar fast path in `box-set`); the table and the reasoning are in
  [§13.1](#131-all-four-shipped-s456af-round-13--and-what-they-cost).

---

## 4. Strings — *fill-pointer buffer is ~2400× (the single biggest win)*

`$s .= 'x'` emits `(p-.= $s "x")`, which **copies the whole string every
append** → O(n²). Variants, N = 100,000 appends:

| variant | CL | exec(s) |
|---|---|---:|
| `s0_copy`    | `(setf s (concatenate 'string s "x"))` (models `p-.=`) | **1.4689** |
| `s1_fillptr` | `(vector-push-extend #\x s)` on an adjustable string | **0.0006** |
| `s2_wots`    | `(write-char #\x o)` in `with-output-to-string` | 0.0009 |

**Finding.** ~**2400×**, and it *grows with N* (complexity class, not a
constant). This is the highest-value codegen change in the whole catalogue.

### Suggestion
- **S1. `raw-string` append buffer (W15.8).** When a `my $s` is string-only-used
  (see `raw-numeric-verdict.md` §use-sets) and every write is `.=`-shaped,
  represent it as an adjustable `:fill-pointer` character vector and compile
  `.=` to `vector-push-extend` (or accumulate in a `with-output-to-string` when
  the value is only read once at the end). The `raw-string` verdict is the
  enabling analysis; the append transform rides on it. Turn O(n²) into O(n).
- **S2. Fold wholly-constant interpolation** to one literal at compile time;
  under `raw-string`, parts already in string slots skip the `to-string`
  coercion in `p-string-concat`.
- **S3. Zero-copy substring *scanning* (narrow, sound).** A read-only rvalue
  `substr`/`index` used only to *inspect* a slice (a tokenizer walking a buffer,
  never retaining the substring) can pass `(start,end)` index pairs into the
  consumer instead of materializing the substring — no copy. This is the *only*
  sound residue of the "displaced-array substr" idea (assessed in
  `advice-from-gemini.md`): a `:displaced-to` view is **rejected** for general
  substr because rvalue `substr` returns an independent copy in Perl (a live
  view aliases the parent → wrong results) and displaced arrays are non-simple
  strings that lose SBCL's simple-string fast paths downstream. Only the
  index-pair form (no view object, no aliasing) is safe, and only when the
  slice is provably not retained. Niche; the append buffer (S1) is the real
  string win.

---

## 5. Aggregates — *the value box is NOT the cost; keys & lookups are*

Emitted for `$h{$w}++`:
```lisp
(p-post++ (p-gethash-box %h $w))
```

Variants, N appends/increments:

| experiment | variant | exec(s) | N |
|---|---|---:|---:|
| dynamic key | `h0_boxed` (box value, 1 lookup) | 0.1337 | 2M |
| dynamic key | `h1_raw` (raw value via `(incf (gethash k h 0))`, 2 lookups) | 0.1721 | 2M |
| const key | `h3_incf` `(incf (gethash "x" h 0))` | 0.0266 | 5M |
| const key | `h2_single` (explicit get-then-set) | 0.1228 | 5M |

**Findings — these correct the naive "unbox hash elements" intuition.**
1. **A boxed value is not the bottleneck.** `h0_boxed` (box, single lookup)
   *beat* `h1_raw` (raw value, but `incf`'s setf-expander does **two**
   lookups). The lookup count dominates, not the box.
2. **Constant-key increment is already ~5 ns/iter** (`h3_incf`, 0.0266s / 5M).
   Hashing a constant key is cheap; don't optimize it.
3. For dynamic keys, the **key stringification** (`write-to-string`/
   number→string) is a large share of both variants.

**Arrays — measured (these ARE big wins, unlike the hash value box):**

| experiment | current | fast variant | speedup | N |
|---|---|---|---:|---:|
| `push @a, x` | `(p-push-impl o (* i 2))` | `(vector-push-extend (* i 2) o)` | **~7×** | 2M |
| element read | `(p-+ $s (p-aref a i))` (boxed) | `(+ s (aref v i))` (raw) | **~3.5×** | 5M |
| `sort {$a<=>$b}` | `(p-sort (lambda ($a $b) (p-<=> $a $b)) v)` | `(sort (copy-seq v) #'<)` | **~6×** | 50 elems ×100k |

### Suggestions
- **A1. Single-lookup hash update.** The real hash win is *one* probe per
  `++`/`+=`, not raw values. `p-gethash-box` already returns a place in one
  probe — keep that; make sure `+=`/`=`-into-element don't compile to a
  read-probe then a separate write-probe.
- **A2. Don't re-stringify a stable key.** For `$h{$w}` where `$w` is
  loop-invariant-typed, cache the stringified key (under `raw-string` `$w` is
  already a string slot). Only pays when it also removes a lookup (finding 2).
- **A3. `push @out, X`** on a non-escaping local `@out` → `vector-push-extend`
  on a `:fill-pointer` vector instead of `p-push-impl` (**~7×**; `p-push-impl`
  pays arg-flattening + box handling per call). Pre-size when the final length
  is known (`(0) x N`, `$a[$big]=…`) to a `simple-vector`.
- **A4. Raw array elements** (`5.7`) → `(aref v i)` on a non-escaping,
  never-referenced `my @a` (**~3.5×** vs `p-aref`).
- **A5. Recognize classic sort comparators** (`5.5`). `{ $a <=> $b }`,
  `{ $b <=> $a }`, `{ $a cmp $b }`, the key-extractor `{ $a->{k} <=> $b->{k} }`
  and Schwartzian forms cover ~95% of real code → emit `(sort v #'<)` /
  `(sort v #'string<)` on unboxed keys instead of a per-comparison generic
  funcall through boxes (**~6×**). A day of pattern-matching in codegen.

---

## 6. Function calls & recursion — *already winning; keep it*

`fib` 0.25× and `gcdrec` 0.45× beat Perl. The residual per-call cost is the
`@_` parcel + the `*wantarray*` bind. Suggestions (from
`where-the-time-goes.md`, unchanged, all sound with the current convention):
- **F1. `dynamic-extent @_`** when the parcel provably doesn't escape → stack
  allocation, zero per-call garbage. Nearly free.
- **F2. Real `&optional`/`&rest` lambda lists** for `my (…) = @_;` prefixes →
  arguments in registers, no parcel.
- **F3. Elide the `*wantarray*` bind** for context-insensitive callees (per-sub
  bit). Note the bind wraps *every* call site (`(let ((*wantarray* nil)) …)`).

---

## 7. Object handling — *method dispatch is ~15× a plain call (biggest OO lever)*

Real transpiled loops, identical work (`$o->v()` vs `getv($o)`), N = 2,000,000:

| variant | inner form | exec(s) |
|---|---|---:|
| `m_method`  | `(p-method-call $o "v")` | **2.6158** |
| `m_subcall` | `(pl-getv $o)` | **0.1757** |

**Finding.** `p-method-call` (string-keyed package/@ISA walk + `*wantarray*`
bind, every call) is **~15× slower than the equivalent plain sub call** — far
more than the "2–5×" folklore. This is the dominant cost in Moo/Moose CPAN
code and the highest-value OO change.

> **Superseded (s444, 2026-08-24).** `sb-sprof` on the 2M-call loop showed
> the ~15× was **not lookup-dominated**: ~45% was `p-method-call` calling
> `sb-mop:finalize-inheritance` on every call, ~15–20% per-call string
> manufacture, only ~10–15% the package walk.  The finalize-once guard
> shipped (2.2× on the loop); the USER ruled **cache-free first** and
> rejected the per-call-site cell below.  M1 as written below is kept for
> the record only.
>
> **DONE, cache-free (s446m, 2026-08-25).** Measured on the same instrument
> (2M calls, startup+compile subtracted, best-of-5; perl ≈ 0.145 s):
>
> | loop | s444 (after finalize-once) | s446m | of perl |
> |---|---:|---:|---|
> | monomorphic `$o->v()` | 1.2537 | **0.3802** | 9.05× → **2.62×** |
> | inherited through `@ISA` | 1.8567 | **0.7115** | 13.29× → **4.74×** |
> | the same call as `C::v($o)` | 0.2091 | 0.2020 | control, unchanged |
>
> What did it, in order of size: the **own-package fast path** (a plain
> method name found in the invocant's own class package returns at once —
> the same function both slow paths would reach, since each starts its walk
> at the class itself); **the stash table** (`%pcl-find-package` memoized —
> perl resolves a stash by name through one hash, `gv_stashpv`, and only
> successful resolutions are recorded, so no entry can go stale);
> **`%pcl-cl-sub-name` memoized and hoisted** (M2, generalised: the
> `pl-NAME` string was rebuilt for every class a walk visited);
> `(declare (type string method-name))` — 12 % on its own; the `@ISA` walk
> **starting at the parents** when the fast path already missed; and three
> per-call allocations removed (the `SUPER::` prefix `subseq`, the
> qualified-name `search` on names with no colon, the `plc-NAME` symbol
> built when the CLOS branch cannot be taken).
>
> **Steps (2) and (4) of task #73 are closed as not worth it, by
> measurement.** Making *both* remaining lookups free — the stash pointer in
> the box, and codegen passing the pre-built `pl-NAME` — is worth
> 0.3802 → 0.3534 s on the monomorphic loop (**7 %**) and nothing measurable
> on the inherited one (bounded with a one-element eq cache in front of each,
> applied to the tree and reverted).  Neither earns a box-representation
> change (~40 class-slot reads woven through `ref()`/stringification) or an
> emission change (a new IR shape + generation bump + three artifacts).
> **The one remaining lever on inherited dispatch is a per-CLASS method
> cache — task #582, whose blocker is invalidation on `@ISA` writes, not the
> cache.**

### Suggestions
- **M1. Polymorphic inline cache.** At each `$o->m` call site, cache
  last-seen-class → resolved function in a 2-slot cell patched at runtime; a
  matching class = one pointer compare + direct call. Monomorphic sites (the
  vast majority) collapse toward the `m_subcall` number (~15× faster here). The
  class guard makes runtime redefinition safe automatically (cache miss).
  Pure runtime+codegen, no whole-program analysis, no sealed world. **Do this.**
- **M2. Hoist `%pcl-cl-sub-name` out of the MRO walk** (existing TODO) — a
  smaller constant-factor lift for every dispatch even before M1 lands.
- **M3. Devirtualize under the closed-world flag** (no `*foo=`/AUTOLOAD/
  `local *foo`/string-eval) — resolve a known class's method to a direct call
  at compile time.

---

## 8. IO / regex / pack — I/O is syscall-bound; the other two re-parse constants

- **IO.** `(p-open …)`/`(p-print :fh …)`/`(p-close …)` are thin over CL
  streams; syscalls dominate, codegen wins are marginal. Only ensure a tight
  `print $fh` loop isn't flushing per line unless `$|` is set, and that a
  `while (<$fh>)` loop binds `*wantarray*` once and reuses one `$_` box. **Do
  not over-engineer IO.**
- **Regex (`5.13`).** cl-ppcre is ~3.7× behind Perl's engine and no codegen
  change touches the engine. The one pure codegen win: **compile every constant
  pattern once at load time** (`load-time-value` scanner) instead of per match;
  then measure how much of the gap is PCL plumbing (capture boxing, `=~`
  `*wantarray*` wrap) vs the engine before considering a PCRE2 FFI.
  - **DONE at the runtime layer instead (#680, s454ac; see §0.2):** memoizing
    `p-regex`/`p-regex-from-parts` on the source text + caching the compiled
    scanner in the op struct gets the same effect as the load-time-value
    emission with NO emission change, and it covers the interpolated-pattern
    spelling too.  The measured plumbing share is now small: a scalar m//g
    step is ~2.4× perl, i.e. inside the engine gap — the next lever on regex
    IS the PCRE2 FFI below, not more PCL plumbing.
  - **PARKED — PCRE2 (USER 2026-09-07, s476, after the s473q spike: "Sad, but good
    review.  Please park PCRE2").**  The spike (`docs/pcre2-spike-s473q.md`, task
    #71) bound `libpcre2-32` through `sb-alien` (zero-copy pinned subjects, offsets =
    char indexes — the marshalling budgeted below does not exist on the 32-bit
    path) and swapped ONE runtime function; the stop rule kept cl-ppcre: without
    PCRE2's JIT it LOSES the many-tiny-matches row `subste` 1.188× (control spread
    0.6 %), with JIT it wins `subste` 0.972× / `textproc` 0.880× / `json-rt` 0.915×
    and still loses the long-scan row `regexg` 1.134×.  **The strong case is PARITY,
    not speed: +311 / −7 rows of perl's own `re_tests` and PCRE2 answers the 18 rows
    where cl-ppcre HANGS (#196).**  The blocker is portability: the four
    install-matrix images ship PCRE2 10.39 / 10.42 / 10.42 / 10.46 against gains
    measured on 10.46, and CI has no macOS leg.  **A POSSIBLE FUTURE, re-opened only
    by one of:** (a) every supported image supplies PCRE2 ≥ 10.46 from its own
    repositories (ubuntu:22.04 and debian:12 leave the matrix, or the matrix moves);
    (b) a decision to BUILD/VENDOR PCRE2 at install time on every platform incl. a
    macOS CI leg; (c) #1528's census showing the 311 misses are mostly the ENGINE's
    (a translator-owned majority is fixed without any library).  Until then
    cl-ppcre is the ONE engine, its levers are #1461 (hashed BMH) and the parity
    fixes #1528 files, and the regex `not-supported.md` entries that name #71 stay.
  - **FUTURE ITEM — PCRE2 via `sb-alien` (not CFFI).** Investigated 2026-07-19.  **(The 2026-07-19 sizing; SUPERSEDED by the spike — kept as the record.)**
    Feasibility is good; it's scoped as a separate, well-contained project.
    Findings: `libpcre2-8/16/32.so.0` are already present on the dev box (no
    `-dev` headers needed — FFI declares its own signatures, the `.so` links at
    runtime); CFFI is *not* installed and shouldn't be added — PCL already
    bridges C with SBCL-native `sb-alien` (the `crypt()`→`libcrypt.so.1` path),
    so hand-bind the handful of PCRE2 entry points (`pcre2_compile_8`,
    `pcre2_match_8`, `pcre2_get_ovector_pointer_8`, `pcre2_code_free_8`) the same
    way. **The FFI is the easy part; budget the effort for two things:** (1)
    string marshalling — SBCL strings are UCS-4, the 8-bit lib matches over a
    UTF-8 code-unit buffer, so encode the subject once (cache it per subject) and
    **map returned byte offsets back to char indices** for `$&`/`pos`/captures;
    (2) compiled-pattern lifetime — wrap the `pcre2_code` so GC finalizes it, and
    compile constant patterns once (`load-time-value`). **Bonus argument
    stronger than the ~3.7× speed:** PCRE2 is literally Perl-Compatible, so it
    would likely *close* correctness gaps (`/n`, `(?{…})`, Unicode property
    classes — see `not-supported.md`) as a side effect. Do the plumbing/capture
    measurement above FIRST to confirm the engine (not PCL's own wrapping)
    dominates before committing.
- **pack/unpack (1175–1587×).** ~~The transpiled pure-Perl oracle **re-parses
  the template string every call**. **P1: memoize the template parse**~~
  **PREMISE CORRECTED s455c (sb-sprof, steady state after warming the lazy
  extension compile): there is NO separated parse to memoize** —
  `_pack_tmpl`/`_unpack_tmpl` walk the template WHILE packing, and the
  profile is FLAT (`%make-p-box` 14 %, `p-find-overload` 14 % → **#815**,
  box-set 6 %, class checks 6 %, `%p-flatten-list` 8 %, `p-substr` 5 %):
  ~21 µs per bench call-pair vs perl's ~0.17 µs is the boxed interpreter,
  not a parse.  The in-PCL fix is a **plan/executor restructure** (parse
  once → compact op list; raw-CL executor) — a real session; the USER-ruled
  pclxs/`pp_pack.c` route (#74) stands as the plan.  P2 (literal-template
  specialized packer) survives as the restructure's second phase.
- **sprintf (measured ~5×).** `p-sprintf` re-parses the format each call:
  `sp0` `(p-sprintf "%05d-%s" i "x")` 0.129s vs a pre-compiled CL formatter
  `sp1` 0.025s @ 200k. Same "hoist the constant parse out of the hot loop"
  pattern — for a **literal** format string, compile the field plan once at
  load time (`load-time-value`) and reuse it. Applies to constant `join`
  separators too.

---

## 9. How to reproduce / extend the variant experiments

The method that produced §2–§7 (recommend promoting it to `tools/`):

1. Build a runtime core once:
   `sbcl --noinform --non-interactive --load cl/pcl-runtime.lisp
   --eval '(sb-ext:save-lisp-and-die "rt.core")'`.
2. Write each variant as a standalone `.lisp` in `(in-package :pcl)` that reads
   `N` at runtime: `(parse-integer (sb-ext:posix-getenv "N"))` — so the compiled
   code is identical across the big/small timing runs and startup cancels.
3. Time `exec = t(N=big) − t(N=small)`, best-of-5, against the core (the
   `vbench.pl` harness used here: `vbench.pl CORE big small f1.lisp f2.lisp …`).
4. To get the *real* emitted shape for a category, transpile a tiny Perl
   program with `./pl2cl` and copy the inner form; write the faster variant by
   hand; confirm both print the same result at small N before timing.

**Rule:** a variant only counts if it computes the byte-identical result; the
speedup is only bankable behind an analysis precondition with a boxed fallback,
and every eager freeze obeys the checked-coercion discipline
(`raw-numeric-verdict.md` §"Checked coercion") — die loud on a violated
assumption, never silently corrupt.

---

## 10. Expected wins — microbench speedup → whole-program impact

A microbench speedup is the ceiling for the *fraction of runtime* a program
spends in that construct; a program is a mix, so translate carefully. The
table gives the measured per-construct factor, the whole-program bench it
moves (from §0), and a realistic expectation.

| change | construct speedup | moves bench(es) | expected program impact |
|---|---:|---|---|
| **S1** raw-string append | ~2400× | `strcat` 755× → ~1× | Decisive for any string-building code (templating, serializers, report gen). Removes an O(n²) class — the win *grows* with input size. |
| **M1** method inline cache | ~15× | (OO not in suite) | 2–15× on Moo/Moose/OO-heavy CPAN; the dominant real-world OO cost. Monomorphic sites (most) approach plain-call speed. |
| **N1** raw-numeric verdict | ~13× (accum), ~8.5× (numstr) | `intloop+=` 3.1×→~1×, `intloop=` 4.3×→~1×, `cfor` 1.5×→~1×, `collatz` 1.96×→~1× | Broad: every counting/accumulating loop and element-seeded numeric scalar. One design fixes four benches to ≈parity-or-better. |
| **A5** sort comparators | ~6× | (sort not in suite) | Order-of-magnitude on sort-heavy code; ~95% of comparators are the recognized idioms. |
| **A3** push buffer | ~7× | contributes to `arrhash` | Large on list-building loops (`push @out, …` is ubiquitous CPAN style). |
| **P1** pack/sprintf memoize | pack oracle, sprintf ~5× | `pack` 1175×→?, `packunpk` 1587×→? | Decisive for pack/unpack/sprintf-in-loop code; local to the oracle/formatter. |
| **A4** raw array elements | ~3.5× | `arrhash` 2.07×→~1× | Recovers array-traffic loops; needs the Phase-4 element machinery. |
| **F1** dynamic-extent `@_` | (GC, not timed here) | helps `fib`/`gcdrec` further | Cuts per-call garbage → less GC on call-heavy code; keeps PCL's existing call advantage. |
| ~~**N2** in-place box write~~ | — | — | **STRUCK s456af**: `p-my-=` already mutates in place. Superseded by **#811** — the tax is `box-set`'s store-semantics dispatch, **1.54×** over a plain slot mutation, on every still-boxed write. See §13.1. |
| **X1** block-compile runtime | 1.2–2× (est.) | everything | Broad baseline lift; free once the load-time cost is managed. |
| **O1** fixnum specialization | ~10× | tightest numeric loops | Only behind a range proof; narrow applicability, hard soundness. Low priority (§2). |

**Honest translation caveat.** None of these makes an *arbitrary* program N×
faster — each helps the fraction of runtime in its construct. The reason the
list still matters: real CPAN hot loops concentrate in exactly these
constructs (string building, OO dispatch, numeric accumulation, list/sort
traffic). A program that is 60% method dispatch gets most of M1's 15×; a pure
regex program gets none of it (§8). Always re-measure with
`tools/bench-exec.pl` after a change — the bench is the only scoreboard.

---

## 11. Before / after — Perl → current CL → proposed CL

Concrete codegen targets for the top items. "Current" is the real emitted
shape (via `./pl2cl`); "proposed" is what the analysis+codegen change should
emit when its precondition holds (else fall back to current).

### 11.1 String append (S1) — ~2400×, the biggest win

```perl
# Perl
my $s = '';
for (1..$n) { $s .= 'x' }
```
```lisp
;; Current — O(n^2): p-.= copies the whole string every append
(let (($s (make-p-box "")))
  (p-foreach-range ($_ 1 $n) (p-.= $s "x")))
```
```lisp
;; Proposed — raw-string append buffer (O(n)); $s is string-only-used and
;; its ref never escapes, so represent it as an adjustable fill-pointer string
(let (($s (make-array 0 :element-type 'character :adjustable t :fill-pointer 0)))
  (p-foreach-range ($_ 1 $n) (vector-push-extend #\x $s)))
;; …and any later read of $s uses it directly (it IS a CL string).
```
**How:** VarAnnotator classifies `$s` as `raw-string` (all uses string; all
writes `.=`-shaped → `raw-string-append` sub-verdict). Codegen emits the
fill-pointer init + `vector-push-extend`; a non-`.=` string write falls back to
a normal string set. See `raw-numeric-verdict.md` §"raw-string".

### 11.2 Numeric accumulator (N1) — ~13×

```perl
# Perl
my $s = 0;
for (1..$n) { $s += $_ }
```
```lisp
;; Current — $s boxed, box re-allocated per write (implicit $_ also boxed)
(let (($s (make-p-box 0)))
  (p-foreach-range ($_ 1 $n) (p-my-= $s (p-+ (unbox $s) $_))))
```
```lisp
;; Proposed — raw slot (raw-numeric verdict; += counts as an arith write)
(let (($s 0))
  (p-foreach-range-raw ($_ 1 $n) (setf $s (p-+ $s $_))))
;; (p-+ on the raw fixnum slot is already at the native-add ceiling — §2.)
```
**How:** add `+=`/`-=`/`*=` to the annotator's "arith write" set so `$s`
qualifies for the existing raw-slot verdict; separately let `$_` go raw when
the body neither `s///`-es nor `chomp`s it.

### 11.3 Numeric-string scalar (N1) — ~8.5×

```perl
# Perl
my $n = $ENV{SIZE};      # a string like "42"
for (...) { ... $n ... } # every use numeric
```
```lisp
;; Current — string slot, re-numified (%pcl-to-number) on every numeric use
(let (($n (p-gethash %ENV "SIZE")))   ; slot holds "42"
  … (p-+ $acc $n) …)                  ; re-parses "42" each iteration
```
```lisp
;; Proposed — raw-numeric: freeze the conversion into the slot once at the write
(let (($n (%pcl-to-number-strict (p-gethash %ENV "SIZE"))))  ; slot holds 42
  … (p-+ $acc $n) …)
```
**How:** raw-numeric verdict when every use of `$n` is numeric and the
no-overload flag holds; the write wrapper is the **strict** coercion that dies
on an overloaded ref / dualvar (`raw-numeric-verdict.md` §"Checked coercion").

### 11.4 Method dispatch (M1) — ~15×

```perl
# Perl
my $x = $obj->value();
```
```lisp
;; Current — string-keyed package/@ISA walk + *wantarray* bind, every call
(p-my-= $x (let ((*wantarray* nil)) (p-method-call $obj "value")))
```
```lisp
;; Proposed — per-call-site monomorphic inline cache (one cell per site)
(p-my-= $x
  (let ((*wantarray* nil))
    (p-call-cached #1=#.(cons nil nil) $obj "value")))
;; p-call-cached: if (car cell) eq (class-of $obj) → (funcall (cdr cell) $obj)
;;                else resolve, patch the cell, call.  Guard = one pointer compare.
```
**How:** codegen mints a fresh cons/2-slot vector per call site (a
`load-time-value`); `p-call-cached` compares the invocant's class to the cached
class and either direct-calls the cached function or falls back to
`p-method-call` and patches the cell. No analysis, no sealed world; runtime
redefinition just misses the cache. (Illustrative — real cell/guard shape is an
implementation choice.)

### 11.5 sort comparator (A5) — ~6×

```perl
# Perl
my @y = sort { $a <=> $b } @x;
```
```lisp
;; Current — generic comparator funcall per comparison, through $a/$b boxes
(p-sort (lambda ($a $b) (p-<=> $a $b)) @x)
```
```lisp
;; Proposed — recognized idiom → native predicate on unboxed numeric keys
(p-sort-native @x #'< :key #'to-number)   ; or (sort (copy-seq @x) #'<) when
                                          ; elements are already raw numbers
```
**How:** pattern-match the block AST for the handful of idioms
(`{$a<=>$b}`, `{$b<=>$a}`, `{$a cmp $b}`, `{$b cmp $a}`, key-extractor
`{$a->{k} <=> $b->{k}}`); emit a native `sort`/`stable-sort` with the CL
predicate and an unboxing `:key`, bypassing the per-comparison funcall.

### 11.6 push (A3) — ~7×

```perl
# Perl
push @out, $x;
```
```lisp
;; Current — p-push-impl (arg-flatten + box handling) on an adjustable vector
(p-push @out $x)
```
```lisp
;; Proposed — direct vector-push-extend when @out is a non-escaping local
(vector-push-extend $x @out)
```
**How:** when BlockAnalyzer proves `@out`'s ref never escapes (not returned,
not aliased, not `\@out`), lower `push` to `vector-push-extend` and skip the
flatten/box path.

### 11.7 sprintf / pack (P1) — ~5× / oracle

```perl
# Perl
my $s = sprintf("%05d-%s", $i, $name);   # constant format in a loop
```
```lisp
;; Current — p-sprintf re-parses "%05d-%s" every call
(p-sprintf "%05d-%s" $i $name)
```
```lisp
;; Proposed — compile the field plan once at load time, reuse per call
(p-sprintf-compiled
  (load-time-value (%pcl-compile-format "%05d-%s")) $i $name)
```
**How:** when the format/template is a string literal, hoist its parse into a
`load-time-value` and call a plan-driven formatter; identical mechanism for
`pack`/`unpack` constant templates.

---

## 12. Priority (by measured win ÷ effort)

**Tier 1 — measured huge, mostly local:**
1. **S1 `raw-string` append buffer** — ~2400× on `.=` (a complexity class).
   **DONE (#62, the `str-buffer` pass).**
2. **M1 method inline cache** — ~15× on OO dispatch; pure runtime, no analysis.
   **Superseded s444 — see §7: cache-free plan in task #73; first cut
   (finalize-once) shipped.**
3. **N1 `raw-numeric` verdict** (+`+=` as arith write) — ~13× on boxed-accum
   loops; fixes `intloop+=`/`intloop=`/`collatz`/`cfor` from one design.
   **DONE (#62, the `raw-numeric`/`raw-slot` passes).**
4. **P1 pack template memoization** — the 1000×+ oracle rows.  **OPEN (#74) —
   now the largest single loss in §0.1.**

**Tier 2 — needs the type-flow / Phase-4 spine:**
5. **A1/A3 single-lookup + fill-pointer aggregates** (arrhash, push loops).
6. **F1 `dynamic-extent @_`**, then **F2/F3**.
7. ~~**N2 in-place box write**~~ — **STRUCK s456af** (already done); read
   **#811** instead: a fast path in `box-set` for a simple scalar into an
   untied, unmagic box, worth **1.54×** on every still-boxed write (§13.1).
7b. **T1 return-family transfer through sub_info** (task #77, user-approved
   idea s303, **scheduled AFTER E2–E4**): per named sub, classify every
   `return`/tail expression with `_tw_shape_ok`'s family oracle in the
   existing sub_info pre-pass; a call site `my $x = f()` with
   `returns => 'num'/'str'` becomes a PROVEN family write — the slot goes
   plain raw with NO strict-freeze wrapper (better than the B-verdict).
   Simple-case boundary: all returns operator-coerced/literal, else record
   nothing.  Adds NO new soundness assumptions — same closed-world rules as
   direct calls (no methods/coderefs/AUTOLOAD, bail on glob redefinition).
   Second phase (larger): caller→callee param use-class transfer so `f($q)`
   need not be an opaque use of `$q`.

**Tier 3 — free riders / long tail:**
8. **X1 block-compile the runtime** (broad 1.2–2×, ship anytime — watch the
   SBCL 2.6.0 inline+ftype ICE / load-time cost, `parser2-prototype.md`).
9. **R1 compile constant regex once**, **O2 constant-fold operands**.
10. **O1 fixnum specialization behind a range proof** — only ~10× *and* hard to
    prove sound; explicitly low priority because `p-+` is already at the sound
    ceiling (§2).

**Deliberately NOT worth it (measured):** replacing `p-+` with native `+`
(0%); unboxing hash *values* without also removing a lookup (can be net
negative); optimizing constant-key hashing (~5 ns already); micro-tuning IO
codegen (syscall-bound).

---

## 13. s453 review — the unclaimed speed is in VERDICT COVERAGE, not new shapes (probes on HEAD `a2b2eb5`; tasks #758–#761)

The USER asked whether more speed can be squeezed out of box elision.  Answer:
yes, and almost none of it needs a new fast shape — the shipped raw machinery
(`p-foreach-range-raw`, `p-incf-raw`, raw lets, `p-raw-params`) is excellent
**when the verdict fires**.  What was reviewed is WHEN it fires:
`Pl/VarAnnotator.pm`'s reason list, probed shape by shape against the control

```perl
sub hot { my $s = 0; for my $i (1..1000) { $s += $i } return $s }
# → (let (($s 0)) (p-foreach-range-raw ($i 1 1000) :my t (p-incf-raw $s $i)))
```

which emits fully raw — the class that BEATS perl (cfor 0.24×).  Four
verdict-coverage gaps keep real code off that path:

| # | veto today | probe result | fix | task |
|---|---|---|---|---|
| 1 | **`eval-in-region` fires on BLOCK eval too** — any `eval` Word in the region boxes EVERY name; the code comment ("Block eval still fires … a separate, later decision") was never scheduled | adding one `my $r = eval { 1 };` to the control sub boxes the accumulator (`make-p-box` + boxed loop) | veto STRING eval only — the capture alist (#296-B1) that needs cells is a string-eval mechanism; `eval {}` is plain control flow with no name capture | **#758** |
| 2 | **write families are OPERAND-derived** — `$s = $s + $_` is `write-shape` (B-DEBUG: `reasons=[write-shape] uses={num,opaque}`) because `$_` is opaque, though `p-+` yields num by the OPERATOR | `$s = $s + $i` raw, `$s = $s + $_` boxed; `$s += $_` raw (compound counts as arith) — the same value, three verdicts | derive the family from the operator's result type for the closed arith/string op set; overload is already gated file-wide for the freeze class | **#759** |
| 3 | **`nested-sub-ref` boxes every name captured by an anon sub**, categorically | a closure that only READS `$s` still boxes it | CL closures capture `let` bindings natively (shared, mutable) — the box is needed only for a REAL boxing event (`\$x`, `local`, string eval), which the event walk already detects inside nested bodies; narrow the veto to capture+event | **#760** |
| 4 | **the topic variable keeps the LOOP boxed**: `for (1..N) { … }` always takes the boxed `p-foreach-range ($_ …)` (dynamic global `$_` per iteration), even when `$s` itself goes raw | the exact `intloop+=` bench spelling emits `(p-foreach-range ($_ …) (p-incf-raw $s $_))` — raw accumulator inside a boxed topic loop | bind `$_` raw per iteration when the body has NO dynamic `$_` reader (no user-sub calls, no eval, no `local $_`) — the front-end's `$_`-default machinery already spells implicit uses explicitly | **#761** |

**This fully explains the two losing loop rows in §0.1** (both bench
spellings use the topic variable):

* `intloop=` **4.86×** = gap 2 (boxed accumulator) + gap 4 (boxed topic loop);
* `intloop+=` **2.07×** = gap 4 alone;
* the same loop with an explicit counter (`cfor`) is **0.24×** — the target
  both rows reach when the two gaps close.

### 13.1 All four SHIPPED (s456af, round 13) — and what they cost

| # | gate (`PCL_OPT`) | what changed | bench |
|---|---|---|---|
| #758 | `raw-block-eval` | an `eval` Word whose next sibling is a `Structure::Block` is not a boxing event; a STRING eval (incl. one nested inside a block eval) still is | — |
| #759 | `raw-op-family` | under an `%ARITH_OP` root the family comes from the OPERATOR; the `_tw_operand_ok` walk stays for the NO-operator case, which is the aliasing hazard it was written for | `intloop=` 4.83x → **3.01x** |
| #760 | `raw-closure-capture` | capture alone is not an event; the veto is capture + a `_text_gate_tags` hit on the closure BODY | — |
| #761 | `raw-topic` | `for (A..B)` binds `$_` to the RAW counter when `_topic_raw_ok` passes | `intloop+=` 2.02x → **0.28x**, `intloop=` → **0.29x**, `arrhash` 2.17x → **1.45x** |

Four separate Kind-A names rather than one, because `-raw-slot` can only turn
the whole verdict off — these are bisectable individually, and
`Pl/t/raw-verdict-coverage-01.t` is inverse-guarded per gate.

**#761 did NOT need the new emission the task predicted.**  Measured first
(§9 recipe, 5M iterations, one image per variant): special bind + fresh box
**0.1680 s**, special bind + RAW value **0.0160 s**, plain lexical + raw
**0.0150 s** — the box ALLOCATION is the whole tax and the special bind costs
7 %.  So `$_` keeps its name and its dynamic binding (a callee still sees the
current element, the outer `$_` is still restored) and only the value goes
raw; `%expand-foreach-range`'s existing raw arm already applies.

**What the gate must prove, and how.**  Reading a raw value is always safe —
every `p-op` coerces — so `_topic_raw_ok` only has to exclude a body that
WRITES THROUGH the box, aliases it, or reaches code the compiler cannot see.
It is an ALLOWLIST (a rejected body only loses an optimization), in three
parts: the shared `text_gate_tags` list, a blanket rejection of every regex
token (a bare `//`/`s///`/`tr///` acts on `$_` with no `=~` to see, and two of
the three write it), and a short word list with the exclusions documented.
**corpus-diff found the hole reasoning missed**: `closure.t` calls
`$foo[$_]->(4 - $_)`, and a code-ref call carries no Word — `->` before a
List and an `&` Cast are rejected too, `->` before a Subscript is not.

Two review side-findings, both now measured:

* **Tier-2 N2 ("in-place box write, ~1.3×") is STRUCK — its premise is
  false, and the truth is worse.**  `p-my-=` already expands to `box-set`
  (mutate in place, `pcl-runtime.lisp:4761`), which is exactly what N2 asked
  for.  Re-measuring the §3 variants one-per-image (5M) shows today's REAL
  emission is slower than the fresh-box variant N2 proposed to replace:

  | variant | inner CL | exec(s) | §3 said |
  |---|---|---:|---:|
  | `acc0_boxed` | `(setf $s (make-p-box (p-+ (unbox $s) $_)))` | 0.2560 | 0.2237 |
  | `acc2_boxmut` | `(setf (p-box-value $s) (p-+ (p-box-value $s) $_))` | 0.1870 | 0.1706 |
  | **`acc_today`** | **`(p-my-= $s (p-+ $s $_))` — what PCL emits** | **0.2880** | *not measured* |
  | `a0_current` | raw slot `(setf $s (p-+ $s $i))` | 0.0170 | 0.0160 |

  The §3 numbers reproduce, so the harness is sound; the new row is the
  finding.  The remaining boxed-write tax is not allocation, it is
  **`box-set`'s store-semantics dispatch** (tie / magic / dualvar /
  adjustable-vector / copy semantics) — **1.54× over a plain slot mutation,
  on every still-boxed scalar write in every program**.  A fast path for "no
  tie, no magic, simple scalar value" is **task #811**.
  *(Run the variants one per SBCL image: they allocate 5M boxes each, so in
  one image the later ones measure the earlier ones' garbage — that is how a
  first pass read the raw slot at 0.030 s where a fresh image reads 0.017.)*
* Two feared blanket vetoes are NARROWER than they read: `_overload_in_file`
  gates only the B-regime freeze (the A-verdict still fired in a
  `use overload` file — probed), and sub params already have a raw path
  (`p-raw-params ($n)` emitted for `my ($n) = @_` — §6/F2 is further along
  than its text says).

Priority within the four: #758 first (one-line narrowing, `eval {}` is the
exception idiom so it robs whole subs in real code), #759 second (closes a
bench row by itself), then #760, then #761 (the only one needing a new-ish
loop emission).  Every widening transfers to the JS backend for free
(`js-target-plan.md` II.0 — the backend inherits verdicts).
