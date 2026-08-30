# Generating faster CL — measured variants, per category

**What this is.** A catalogue of PCL's hot constructs.  For each one: the
Common Lisp PCL emits today, an alternative shape that computes the identical
result, and a head-to-head timing of the two.  Every entry says concretely
*"emit shape B instead of shape A, it is N× faster"*.

It is the measured basis of **Target A** — general program speed must beat
Perl (`v2-endgame-plan.md` §6 holds the acceptance criteria and sequencing).

**Written** 2026-07-19 against the v2 pipeline; §0.1 re-measured 2026-08-25.

## Where this stands (2026-08-25)

Every shipped transform is a **named, switchable emission** in the
optimization registry [`Pl/Passes.pm`](../Pl/Passes.pm) (`PCL_OPT`):
`raw-slot`, `raw-numeric`, `str-buffer`, `foreach-range`, `insensitive-call`,
`elem-setf`.  Tier 1 is half done:

| tier-1 item | task | state |
|---|---|---|
| **S1 + N1** — string buffer, raw numerics | #62 | **done.**  `str-buffer` killed the O(n²) append class (`strcat` 756× → 5.7×); `raw-numeric`/`raw-slot` put the counting loops and `collatz` *ahead of perl*.  See the re-measured [§0.1 table](#01-re-measured-baseline-2026-08-25-after-62--the-73-first-cut). |
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

* **Baselines** — [§0 whole-program vs perl](#0-whole-program-baseline-vs-perl-this-session) · [§0.1 re-measured, 2026-08-25](#01-re-measured-baseline-2026-08-25-after-62--the-73-first-cut) · [§0.2 re-measured, 2026-08-30 + the #680 m//g result](#02-re-measured-baseline-2026-08-30-round-12-perf-agent-s454ac) · [§0.5 headline results](#05-headline-results-what-the-experiments-proved)
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
- **N2. In-place box write** as a cheap partial win where a variable must stay
  boxed but is only ever *re-assigned a scalar*: have `p-my-=`/`p-scalar-=`
  mutate `(p-box-value)` instead of allocating a fresh box. ~1.3× and it cuts
  per-iteration GC garbage. Independent of N1, helps every still-boxed write.

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
  - **FUTURE ITEM — PCRE2 via `sb-alien` (not CFFI).** Investigated 2026-07-19.
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
- **pack/unpack (1175–1587×).** The transpiled pure-Perl oracle **re-parses the
  template string every call**. **P1: memoize the template parse** keyed on the
  constant template (biggest local win); **P2:** for a literal template, emit a
  specialized unrolled packer at transpile time.
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
| **N2** in-place box write | ~1.3× | every still-boxed write | Small but universal; also cuts GC. Ship with N1. |
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
7. **N2 in-place box write** (~1.3× on every still-boxed write; cheap).
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

Two review side-findings, no task needed:

* **Tier-2 N2 ("in-place box write, ~1.3×") appears ALREADY SATISFIED** —
  `p-my-=` expands to `box-set` (mutate in place, `pcl-runtime.lisp:4761`),
  not the `make-p-box`-per-write the §3 `acc0` variant modeled.  The perf
  agent should re-run the acc variants and strike N2 from §12 if confirmed.
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
