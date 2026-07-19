# Generating Faster CL — measured variants, per category

**Written:** 2026-07-19 (Opus 4.8), against the v2 default pipeline.
**Method.** Two layers of measurement:
1. `perl tools/bench-exec.pl` — whole-program, execution-only, startup
   subtracted, best-of-5, vs Perl (the §0 table).
2. **Variant experiments** — for each hot category I took the *actual emitted
   CL*, hand-wrote alternative CL that computes the identical result, and timed
   them head-to-head against a fresh runtime core (same big-N−small-N,
   best-of-5 method). These say concretely *"emit shape B instead of shape A,
   it is N× faster"* — the §2–§8 tables. Harness + all variant `.lisp` files
   are reproducible; the recipe is in §9.
**Companions (the *why* and the soundness proofs):**
`docs/where-the-time-goes.md` (the four taxes), `docs/raw-numeric-verdict.md`
(use-proven eager coercion), `docs/bench-exec-investigation.md` (counting
loop), `docs/ir-spec.md` §2.2 (the box/raw invariant).
**Policy:** CLAUDE.md §2 — speed wins over readable CL. Every change below is
opt-in narrowing with a **boxed fallback**: if the analysis can't prove the
precondition, emit today's code unchanged. A wrong analysis loses a speed-up,
never correctness.

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

---

## 0.5 Headline results (what the experiments proved)

| category | emit THIS instead of today's shape | measured | precondition |
|---|---|---:|---|
| **String append** | fill-pointer buffer (`vector-push-extend`) instead of copy-`.=` | **~2400×** | ref doesn't escape, string-only use |
| **Method dispatch** | monomorphic inline cache / direct call instead of `p-method-call` string walk | **~15×** | per-call-site class guard |
| **Boxed accumulator** | raw slot instead of `make-p-box`-per-write | **~13×** | raw-numeric verdict |
| **Box write** | mutate `p-box-value` in place instead of re-allocating a box | **~1.3×** | any boxed write that keeps the box |
| **Arithmetic op** | *(nothing — `p-+` on raw slots already equals native `+`)* | **~1.0×** | — |
| **Native fixnum** | `(the fixnum (+ …))` under `(safety 0)` | ~10× | **range proof** (unsound without it) |
| **Hash incr (const key)** | *(nothing — already ~5 ns/iter)* | ~1.0× | — |

The two surprises that correct the intuition (and my own first draft):
**native `+` is not a win** (R1's `p-+` already open-codes the fast path), and
**a hash *value* box is not the cost** — the key stringification and lookup
are. Chase the four rows at the top, not the operator pipeline.

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
  qualifies). Empirical ceiling on this shape: **13×**.
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

### Suggestions
- **A1. Single-lookup update.** The real hash win is *one* hash probe per
  `++`/`+=`, not raw values. `p-gethash-box` already returns a place in one
  probe — keep that shape; make sure `+=`/`=`-into-element don't compile to a
  read-probe followed by a separate write-probe.
- **A2. Don't re-stringify a stable key.** For `$h{$w}` where `$w` is a
  loop-invariant-typed scalar, cache the stringified key; under `raw-string`
  `$w` is already a string slot. Under a future raw-element hash keyed on the
  raw value, skip stringification entirely — but note (finding 2) this only
  pays when it *also* removes a lookup.
- **A3. `push @out, X`** on a non-escaping local `@out` → `fill-pointer` vector
  + `vector-push-extend` (same mechanism as S1, arrays). Pre-size when the
  final length is known (`(0) x N`, `$a[$big]=…`) to a simple-vector.

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
- **pack/unpack (1175–1587×).** The transpiled pure-Perl oracle **re-parses the
  template string every call**. **P1: memoize the template parse** keyed on the
  constant template (biggest local win); **P2:** for a literal template, emit a
  specialized unrolled packer at transpile time. Same "hoist the constant parse
  out of the hot loop" pattern applies to constant `sprintf` formats.

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

## 10. Priority (by measured win ÷ effort)

**Tier 1 — measured huge, mostly local:**
1. **S1 `raw-string` append buffer** — ~2400× on `.=` (a complexity class).
2. **M1 method inline cache** — ~15× on OO dispatch; pure runtime, no analysis.
3. **N1 `raw-numeric` verdict** (+`+=` as arith write) — ~13× on boxed-accum
   loops; fixes `intloop+=`/`intloop=`/`collatz`/`cfor` from one design.
4. **P1 pack template memoization** — the 1000×+ oracle rows.

**Tier 2 — needs the type-flow / Phase-4 spine:**
5. **A1/A3 single-lookup + fill-pointer aggregates** (arrhash, push loops).
6. **F1 `dynamic-extent @_`**, then **F2/F3**.
7. **N2 in-place box write** (~1.3× on every still-boxed write; cheap).

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
