---
name: project_bench_exec_investigation
description: "Exec-speed 'regression' RESOLVED s286: nothing regressed — old 'beats perl' = literal-bound raw-slot shapes (still 2-3.5× faster today); new bench shapes hit range-materialization + env-N boxing. Next: counting-loop lowering for for(A..B)."
metadata:
  node_type: memory
  type: project
  originSessionId: 9abe7e94-688c-4f93-a12b-976c0c74175d
---

**Tool:** `tools/bench-exec.pl` — execution-only, startup-subtracted PCL-vs-perl
bench (big-N minus small-N via `$ENV{N}`, best-of-K, own runtime core).

**RESOLVED (s286, Fable 5): NO regression, ever.**  Proof: (a) emission
byte-identical s276b(9ca0026)↔HEAD for loop shapes; (b) same generated lisp
under s276b-runtime core vs HEAD core = 0.397s vs 0.400s; (c) canonical shapes
still beat perl TODAY @2M: while 3.5× faster, cfor-literal 2.4× faster, nested
2× faster — only `for my $i (1..2M)` is 7.6× SLOWER.

**The three shape taxes** (full data `docs/bench-exec-investigation.md` §top):
1. **`for (1..N)` materializes the range vector** (`p-..`) — perl's fastest
   loop is PCL's slowest.  DOMINANT.
2. **`my $n = $ENV{N}` stays boxed** → generic `(p-< $i $n)` each iter: cfor
   @2M 0.017s(literal)→0.073s(env-N), ~4×/iter.  bench-exec's own env-N method
   causes this — its "cfor 1.5×" is this tax, not a regression (R1's 1.5×
   intmath number likewise).
3. **`+=` keeps accumulator boxed** (annotator raw-slot verdict only fires on
   `$s = $s + X`).  Minor.

s285's "p-+ generic dispatch" concern was overstated — R1 inline fast path
gives 8.5ns/iter on raw slots.

**COUNTING-LOOP SHIPPED (s286b, c7ba84a, gen v2-28, task #61 DONE):**
`for my $i (1..5M)` = **0.021s vs perl 0.058s — 2.8× FASTER** (was 0.50s /
7.8× slower; 24× swing).  `p-foreach-range`/`-raw` + `%p-range-classify`
(extracted from p-..) + `Pl::VarAnnotator::foreach_range_split` (shared
oracle; bare Words reject — `reverse 1..3` list-op swallows the range!) +
annotator foreach-alias veto refined (sole-range my-var = decl, not veto →
raw loop var; $_/globals/captures stay boxed).  Verified: 20/20 perl-diff
battery, gate 114/4023 PASS, 8-file sweep byte-identical to HEAD baseline.
**Left → folded into #62:** `+=` arith-write raw verdict + postfix
`EXPR for A..B` (that shape still ~3.6×).  strcat §W15.8 unchanged.  See
[[project_v2_per_statement_void_wrap]] (separate perf issue).

**THEN (task #62, blocked by #61): `raw-numeric` + `raw-string` verdicts** —
user-approved design `docs/raw-numeric-verdict.md` (s286): use-proof (all uses
numeric resp. string) → wrap non-coerced writes in `%pcl-to-number` / `to-string`
(eager `+0` / `.""`), real host value in the raw slot.  **Sound for refs with NO
no-refs flag**: PCL ref identity (`==`, `ARRAY(0x…)`) = stable monotonic counter
ID (object-address, weak eq-table, pcl-runtime:1206), so frozen ≡ live; only
gate = no `use overload` in corpus + no string-eval (or manual flag).  Boolean
context DISQUALIFIES numeric ("0.0" is TRUE) but LICENSES string; `defined`/
`ref()`/deref/call-args disqualify both; divergence = warning count/timing only.
Kills tax 2 generally; %ENV whitelist SUBSUMED (getenv returns a string — raw
string bound would re-numify per iter, box's nv-cache wouldn't).  raw-string
`.=` accumulator = the W15.8 append-fix slot (verdict first, append rides on it).
**STRICT wrappers (user): die at the write on overload-capable blessed ref
(catches string-eval hole) or genuine dualvar (sv-ok ∧ nv-ok ∧ nv≠to-number(sv),
share predicate with isdual) — write-only cost, never weaken the check.**
