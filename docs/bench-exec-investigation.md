# Execution-speed investigation — RESOLVED (s286): no regression, ever

**Answer to the s285 question ("faster than perl → multiple times slower,
what regressed?"): nothing regressed.**  Verified s286 (Fable 5):

1. **Emission is byte-identical** between the s276b commit (9ca0026) and HEAD
   for the loop shapes (worktree transpile diff).
2. **Runtime speed is identical**: the same generated `.lisp` loaded against a
   core built from s276b's runtime vs HEAD's runtime runs in 0.397s vs 0.400s
   (5M-iteration raw-slot range loop).  The 60-line runtime diff since s276b
   touches no loop hot path ($^S in eval, print-$_ default moved to codegen,
   `my %h = %h` copy fix, UNIVERSAL-on-unknown-class fallback).
3. **PCL still beats perl today on the canonical shapes** (literal bounds, raw
   slots; core-load method, startup subtracted, best-of-5, @2M):

   | shape @2M | perl | pcl | ratio |
   |---|--:|--:|--:|
   | `while ($i<2_000_000)` | 0.060 | 0.017 | **0.29× (3.5× faster)** |
   | cfor literal bound | 0.040 | 0.017 | **0.43× (2.4× faster)** |
   | nested cfor | 0.040 | 0.020 | **0.51×** |
   | `for my $i (1..2_000_000)` | 0.020 | 0.152 | **7.6× SLOWER** |

The apparent "crash" was 100% a **bench-shape difference**.  The old
(s272d/s276b) canonical intloop/cfor used literal bounds → raw slots →
R1-inlined ops, ~8.5ns/iter — genuinely faster than perl, still true.  The new
`tools/bench-exec.pl` shapes hit three independent taxes:

- **Tax 1 (dominant): `for (1..N)` materializes the range** — `p-..` builds
  the full vector, while a literal range is *perl's fastest loop* (counting
  loop, no list).  PCL's slowest loop vs perl's fastest = 7.6×.
- **Tax 2: `my $n = $ENV{N}` keeps `$n` BOXED** → every `(p-< $i $n)` compare
  unwraps the box generically: cfor @2M goes 0.017s (literal) → 0.073s
  (env-N), ~4×/iter.  This is bench-exec's *own method* (run-time N for
  compile-cancellation) — fair for naive idioms, but it's what makes even
  cfor read "1.5× slower".
- **Tax 3: `$s += $_` keeps the accumulator boxed** (annotator treats only
  `$s = $s + X` as an arith write, not `+=`) — minor next to 1–2.

Note: s285's finding #3 ("p-+ generic dispatch, never native +") is
**overstated** — literal-bound cfor at 8.5ns/iter shows the R1 inline fast
path effectively open-codes the add; generic dispatch is not a dominant cost
when operands are raw.

**Fix menu (leverage order):**
1. **Counting-loop lowering for `for [my $v] (A..B)`** when the list is
   exactly one range: emit a `p-foreach-range`-style loop — endpoints
   evaluated ONCE (also neutralizes tax 2 for range loops), no vector.
   Semantics to respect: range elements are READ-ONLY aliases in perl
   (writing `$_` dies), so no write-through needed; keep last/next/redo +
   labels + continue via the same loop-macro protocol.  W15 perf-menu item.
2. **Annotator: treat `$s += NUMERIC` / `$s -= …` like `$s = $s + …`** for
   the raw-slot verdict (tax 3).
3. strcat `.=` O(n²) — already planned, §W15.8 (unchanged).

The original s285 notes follow (historical).

---

# Execution-speed investigation (s285, Opus 4.8) — WHERE WE STOPPED

**Question from the user:** early in v2 development the speed tests showed PCL
*faster* than Perl; now simple loops look like v1 (except fib, which is still
~4× faster). What regressed?

**Tool:** `tools/bench-exec.pl` (committed this session) — execution-only,
startup-subtracted (big-N minus small-N with N passed at run time via
`$ENV{N}`, best-of-K), runs against a fresh runtime core.

## Current numbers (v2, best-of-5, startup subtracted)

| bench | perl(s) | pcl(s) | pcl/perl |
|---|---:|---:|---:|
| intloop `+=` `for(1..n)` | 0.066 | 0.44 | 6.5× |
| intloop `=`  `for(1..n)` | 0.064 | 0.50 | 7.8× |
| cfor (C-style counter)   | 0.105 | 0.16 | **1.5×** |
| arrhash                  | 0.133 | 0.31 | 2.4× |
| fib(27) recursive        | 1.49  | 0.37 | **0.25× (4× faster)** |
| collatz                  | 1.97  | 3.92 | 2.0× |
| strcat `.=`              | 0.001 | 1.55 | ~1200× (known O(n²), §W15.8) |

## Findings so far (three separate taxes, ranked)

1. **`for (1..$n)` MATERIALIZES the whole range.**  `p-..`
   (`cl/pcl-runtime.lisp:4535`) *"returns a vector from start to end"*, and
   `p-foreach` (6429) iterates that vector (via `%p-flatten-for-list`).  So
   `for (1..5_000_000)` allocates a 5M-element vector; Perl special-cases
   `for (1..N)` as a bare **counting loop** with no list.  This is the prime
   suspect for the 6–8× gap on the `1..n` intloops — cfor (a C-style counter,
   no range) is only **1.5×**, and 1.5× is exactly R1's documented steady state
   (see below), i.e. cfor is NOT regressed.

2. **Implicit `$_` loop var boxes the accumulator.**  `for (1..$n){ $s=$s+$_ }`
   keeps `$s` BOXED (`(make-p-box nil)` + `p-my-=`), whereas the *named*
   `for my $i (1..$n){ $s=$s+$i }` keeps `$s` a **raw slot** (`(let (($s 0))…
   (setf $s (p-+ $s $i)))`).  Probable cause: VarAnnotator's arith-RHS / shape
   check treats `$_` (magic) as a non-simple operand → the write is "not arith"
   → `$s` boxed.  Secondary: the raw-named form is still 0.40s (≈6× perl), so
   boxing is NOT the dominant cost here — the range materialization + `p-+` are.

3. **`p-+` generic dispatch every iteration** (never native `+` on
   declared-fixnum slots).  Even with a raw `$s`/`$i`, the body is
   `(setf $s (p-+ $s $i))`; `p-+` handles string/overflow/undef coercion, so
   SBCL can't reduce it to a machine add.

## The historical contradiction to resolve NEXT

Two prior numbers disagree by ~4×:

- **R1** (`docs/parser2-prototype.md:102`): v2 intmath @2M = **0.11s = 1.5×
  SLOWER** than perl 0.070s.  ← matches current cfor.
- **s276b** (`docs/session-log.md:794`): "PCL beats perl 7/8 — intloop/cfor
  ~**0.03s** vs perl ~0.08s" using a **fasl-compiled** method.  ← 4× faster
  than R1 for the "same" shape.

**Hypotheses for the s276b 0.03s (verify in order):**
  1. s276b's canonical "intloop"/"cfor" used a **C-style counter, not the
     `1..N` range** (so no materialization) — the naming may be misleading.
     Find the exact s276b/s272d canonical bench SOURCE (session-log around
     s272d "canonical bench shapes … byte-identical CL"; parser2-prototype
     line ~844) and run it through CURRENT v2.
  2. **fasl (`compile-file`) vs `--load` source** changes optimization policy
     (my harness `--load`s the `.lisp`; SBCL's evaluator-mode is `:compile` so
     this *should* match, but confirm: `compile-file` the generated lisp, load
     the fasl, re-time).
  3. Something between s276b (2026-07-06) and now genuinely regressed the
     native path — least likely given R1's own 1.5× number predates s276b and
     agrees with today.

**Most likely conclusion (unconfirmed):** there is no broad regression; the
"faster than perl" memory was the *canonical* (C-counter, native-arith,
no-range) shapes, and the naive-idiom `for (1..N)` was always taxed by range
materialization.  If confirmed, the real perf win is a **lazy/counting-loop
lowering for `for (LITERAL_RANGE)`** (a `p-foreach-range` / `dotimes`-style
form that never allocates the vector) — a clean W15 perf-menu item, and the
single highest-leverage fix for idiomatic Perl loops.

## To resume
1. `git show` the tool: `tools/bench-exec.pl`; run `perl tools/bench-exec.pl`.
2. Find + run the s276b canonical shapes to settle the contradiction.
3. If range-materialization confirmed: design `for (1..$n)` → counting-loop
   lowering (no vector); measure.  Then revisit `$_`-boxing and native `+`.
