# Where the Time Goes — a plain-language tour of PCL performance

**Written:** 2026-07-02
**Audience:** both a reader with no compiler background and the implementer.
Each section says what happens in ordinary language first, then gives the
technical version and the numbers.
**Companion:** `docs/codegen-rewrite-review.md` (the go/no-go review; its §3.2b
has the flag measurements), `docs/codegen-rewrite-spec.md` (the transform
spec), `docs/type-flow-and-codegen-plan.md` (the analysis design).
**Policy note (user decision 2026-07-02):** when speed of generated code and
readability of generated code conflict, **speed wins**. Readability remains a
nice-to-have (Perl-like names where free), not a constraint.

---

## Status update (2026-08-25) — what has shipped since this was written

This document's body is the July 2026 analysis and is kept as written; the
numbers in §1–§3 describe the tree of that date.  Since then:

* **The measured worklist moved to `docs/faster-codegen-suggestions.md`**
  (per-construct variant experiments, 2026-07-19), and every speed transform
  became a **named, switchable emission** in the optimization registry
  `Pl/Passes.pm` (`PCL_OPT`, s411).  Registered so far: `raw-slot`,
  `raw-numeric`, `str-buffer`, `foreach-range`, `insensitive-call`,
  `elem-setf`.
* **§5.1 (FPU modes) and §5.2 (R1 inline ops)** shipped 2026-07-02, as noted
  inline below.
* **§5.6's core landed for scalars (task #62, DONE):** the `raw-slot` /
  `raw-numeric` verdicts (unshared `my` scalars become raw slots; use-proven
  eager coercion) and the **`str-buffer`** append verdict (S1: `.=` into a
  fill-pointer buffer — the O(n²) class is gone).
* **§5.8 (per-call-site inline caches) is REJECTED in that form** (USER,
  s444, 2026-08-24).  Profiling (`sb-sprof`) showed the measured ~15× method
  dispatch gap was **not lookup-dominated**: ~45% was `p-method-call` calling
  `sb-mop:finalize-inheritance` on *every* call (re-finalizing the CLOS class
  under a recursive lock), ~15–20% per-call string manufacture, only ~10–15%
  the actual package walk.  The **finalize-once guard shipped** (2.2× on a
  2M-call method loop; the `ovlsub` bench 7.27× → 5.28×).  The remainder is a
  cache-free plan in task #73: stash-in-box at bless (perl's own SvSTASH
  shape) → own-package fast path → pre-built `pl-NAME` from codegen; a
  per-CLASS stash table only if inherited dispatch still lags — never a
  per-call-site cell.
* **Boxed aggregates** (§5.7) are a parked design item (post-v0.1, Fable).

**Current whole-program numbers** (`tools/bench-exec.pl`, 2026-08-25,
startup subtracted, best-of-5; ratio < 1 = PCL faster than perl):

```
bench          perl(s)     pcl(s)  pcl/perl
intloop+=       0.0666     0.1377     2.07x
intloop=        0.0662     0.3218     4.86x
cfor            0.1064     0.0257     0.24x   <- 4.1x FASTER
arrhash         0.1327     0.2816     2.12x
fib(27)x        1.4728     0.4439     0.30x   <- 3.3x FASTER
gcdrec          0.1945     0.0952     0.49x   <- 2.0x FASTER
collatz         1.9829     0.7987     0.40x   <- 2.5x FASTER
strcat          0.0012     0.0068     5.71x   (was 756x -- O(n^2) gone)
pack            0.0035     5.0354  1432.92x   (#74: template re-parse)
packunpk        0.0040     5.1136  1270.47x   (#74)
arrfill         0.0500     0.1972     3.94x
slices          0.0704     0.3276     4.65x
sliceasgn       0.0264     0.0760     2.88x
ovlsub          0.0417     0.2078     4.98x   (#73: method dispatch)
symref          0.0215     0.1601     7.44x
```

The §1 sentence "currently 5–10× slower than interpreted Perl" is history:
counting loops, recursion and `collatz` now **beat perl** (0.24×–0.49×).
What remains slow is concentrated and owned: the pack/unpack oracle's
per-call template re-parse (task #74), the method-dispatch remainder
(task #73), and aggregate/slice traffic (the boxed-aggregate design).

---

## 1. The puzzle, stated honestly

PCL compiles Perl to native machine code via SBCL — and the result is
currently **5–10× slower than interpreted Perl**. That sounds paradoxical:
compiled code losing to an interpreter. The resolution is that Perl is not a
naive interpreter — it is 30 years of hand-tuned C — and PCL currently makes
the machine do enormous amounts of *bookkeeping* around every tiny operation.
SBCL compiles that bookkeeping faithfully. Compiling slow instructions fast
still gives slow code.

The proof that the approach is fine: when the *same loop* is written the way
SBCL likes, it runs **6–40× faster than Perl** (measured; see §3). The whole
performance project is therefore one sentence:

> **Stop asking the machine to do bookkeeping that this particular line of
> code cannot possibly need — and prove it, so the result stays correct.**

The proving is what the compiler rewrite's "variable analysis" is for.

---

## 2. The four taxes

Every Perl operation PCL emits currently pays up to four taxes. In lay terms:

### Tax 1 — the envelope tax (boxing)

Every Perl scalar (`$x`) is stored inside a little container — a "box" — not
as a bare number or string. Think of every value living in an envelope:
every time you *use* `$x` you open the envelope, and every time you *assign*
you put a new value in. The envelope exists for good reasons — Perl lets you
take a reference to a variable (`\$x`), tie it to magic behaviours, `local`
it — and the envelope is the thing those features hold on to.

But the overwhelming majority of variables never use any of that. A loop
counter is opened and resealed millions of times for nothing.

*Technical:* every scalar is a `p-box` struct (value + cached numeric form +
cached string form + class slot + magic hooks). Reads call `unbox`/
`to-number`, writes call `box-set`. Measured: removing the envelopes from an
arithmetic loop, once the other taxes are gone, is a further **13×**
(0.187 s → 0.014 s).

### Tax 2 — the "what are you?" tax (generic dispatch)

Perl's `+` must work on anything: numbers, numeric strings (`"42"`), undef,
objects with overloaded operators. So PCL's `p-+` interrogates both operands
every single time: *Are you a box? Are you undef? Are you a blessed object
with an overloaded `+`? Are you a string that looks like a number?* — dozens
of machine instructions where a native add is **one**.

Perl pays this tax too (it's what an interpreter is), but Perl's version is
extremely optimized C. Ours can be *eliminated*, not just optimized: when the
compiler can see that `$i` only ever holds a counter, the questions have
known answers and the `+` compiles to one instruction.

*Technical:* the 2026-06-25 brackets show generic-`p-+` dispatch vs native
arithmetic is where ~95% of the arithmetic gap lives — not the box
allocation itself.

### Tax 3 — the safety-goggles tax (float trap masking) — *measured 7.4×!*

The CPU has an alarm that goes off on strange floating-point results
(dividing by zero, overflow). Perl runs its whole life with that alarm
switched off — that's why `9**9**9` prints `Inf` instead of crashing. PCL
gets the same effect today by **switching the alarm off and back on around
every single arithmetic operation** — and, worse, wrapping each operation in
a little parcel of code (a closure) allocated on the heap just to do so.
It's like putting on and taking off safety goggles for every individual
hammer tap, and buying a new pair each time.

This one was only discovered by measurement during the 2026-07-02 review:
removing the per-op goggles ritual makes the arithmetic pipeline **7.4×
faster** (1.378 s → 0.187 s) with *everything else unchanged* — still boxed,
still generic.

And the fix is almost embarrassing: **switch the alarm off once, at
startup.** Verified 2026-07-02: `(sb-int:set-floating-point-modes :traps
nil)` at image start gives Perl's exact model — `1/0.0` → Inf, `Inf−Inf` →
NaN, overflow → Inf — with zero per-operation cost. `%pcl-ieee-arith` and
its per-op `with-float-traps-masked` can then be **deleted entirely**.
(Integer division by zero still signals an error, as in Perl. Any place that
deliberately wants a float trap can set modes locally.)

### Tax 4 — the parcel tax (function calls)

Every sub call currently: packs all arguments into a freshly allocated
parcel (the `@_` list/vector), pins a note to a global corkboard saying what
context the call is in (the `*wantarray*` dynamic binding), makes the call,
then reads and interprets the return through a context-sensitive helper.
Perl does a version of this too — it's actually one of Perl's *slowest*
features — which is why calls are a place PCL can **beat** Perl rather than
merely catch up: SBCL's native calling convention (arguments in registers)
is nearly free, and most calls can use it once the compiler proves the
callee never looks at `@_` as a whole and never asks about its context.

*Technical:* this is R2 in the review — callee `&optional`/`&rest` lambda
lists (spec #3) plus caller-side elision of the `*wantarray*` bind for
provably context-insensitive callees.

### Why removing taxes compounds

The runtime is already full of fast paths that check "is this a plain
value?" and skip the ceremony. Today they **never fire**, because every
value is boxed. The moment variables start living outside envelopes, every
one of those pre-existing shortcuts lights up at once. This is why the
phases multiply rather than add.

---

## 3. The numbers in one table

All measured, startup excluded. "intmath" = 2M-iteration `$sum +=
($i*3+7)%100` loop; op-level emulation for the variants (same loop skeleton,
only the operator pipeline varies).

| configuration | intmath | vs Perl |
|---|---:|---|
| Perl 5.40 (reference) | 0.091 s | 1× |
| PCL today (full program) | ~0.69 s | ~7.5× slower |
| op-level: current pipeline (A) | 1.432 s | — |
| A minus overload probes (= a `--no-overload` flag) | 1.378 s | **−4% — not the lever** |
| minus the goggles ritual (Tax 3 gone; still boxed+generic) | 0.187 s | ≈2× slower |
| minus the envelopes (Taxes 1–3 gone) | 0.014 s | **6.5× faster** |
| hand-written native-fixnum CL (floor) | 0.0045 s | 20× faster |

Read bottom-up: the destination exists and is far below Perl. Read top-down:
the order of the taxes by size is **goggles (7.4×) → envelopes+dispatch
(13×) → overload probes (4%)**. This re-ranks intuition: the scary-sounding
`use overload` checks are nearly free (they already had a fast bail-out);
the invisible FPU ritual was the monster.

---

## 4. What the rewrite does about each tax (lay version)

The planned compiler pass reads a whole sub and builds a dossier on every
variable: where it's declared, every place it's read or written, and **what
each use demands** (used as a number? a string? a true/false test? does
anyone take a reference to it? does it cross into a nested sub? …). Then:

- If nothing ever needs the envelope → the variable lives as a bare value
  (kills Tax 1 for that variable).
- If every use demands a number → arithmetic on it compiles to native
  machine ops (kills Tax 2 at those sites).
- If a string variable is only ever *appended to* → use a growing buffer
  instead of copy-the-whole-string-each-time (turns an O(n²) pattern into
  O(n) — a complexity-class win, bigger than any constant factor).
- If a sub never touches `@_` beyond its parameter line and never asks its
  context → real native parameters, no parcel, no corkboard note (Tax 4).

Crucially, every decision is **opt-in narrowing with a safe fallback**: if
the analysis can't prove something, that variable simply stays in its
envelope and the code is byte-identical to today's. A wrong analysis can
lose an optimization, never correctness.

Taxes 3 and (half of) 2 don't even need the analysis — they are runtime
fixes shippable this week (§5.1, §5.2).

---

## 5. The full menu for making *arbitrary* CPAN code fast

The review (R1–R3) covers the core. This section is the extended menu,
including everything proposed since, ordered roughly by
(expected win) ÷ (effort). Items marked ★ are new relative to the review.

### 5.1 ★ Set the FPU modes once at startup — delete `%pcl-ieee-arith`
**SHIPPED 2026-07-02.** The runtime prologue now runs
`(sb-int:set-floating-point-modes :traps '(:divide-by-zero))` (plus an
`sb-ext:*init-hooks*` re-apply for saved cores): overflow→Inf and
invalid→NaN silently, exactly Perl's model, while integer AND float division
by zero still die (verified: perl dies on `1/0.0` too, so `:divide-by-zero`
must stay trapping — the review's "`1/0.0` → Inf" claim was wrong).
`%pcl-ieee-arith` and its per-op closure+mask are deleted.

### 5.2 R1 — inline fast-path operators
**SHIPPED 2026-07-02** for `+ - * / % == != < > <= >= <=> .` and the six
string comparisons + `cmp`, plus accessors `unbox to-number to-string
p-true-p p-bool`: inline wrappers with numberp/stringp fast paths over
out-of-line `%p-…-slow` overload/coercion paths (overloaded objects stay
correct — no global no-overload assumption). Two implementation lessons,
both recorded in `docs/parser2-prototype.md` "R1 landed":
- **inline-before / notinline-after / re-inline-at-EOF** keeps the runtime's
  own source-load at ~1.15 s (naive global inline+speed-2 made every SBCL
  spawn pay ~4.9 s);
- **SBCL 2.6.0 ICEs** on inline + narrowed return ftype — keep `(t) t`.

Measured (whole-program minus null-baseline, with the v2 prototype shapes):
intmath **7.5× → 1.5×** of perl, fib(29) **5× → 2.1×** — the Phase-1+R1
checkpoint (§ headline "≥3×") is met. Gate: 113 files / 3740 tests green.

### 5.3 ★ Stack-allocate the argument parcel (`dynamic-extent @_`)
Even before real lambda lists: when the analysis (BlockAnalyzer already
collects this) shows `@_` never escapes the sub — no `\@_`, not stored, not
returned — declare the `&rest` list `dynamic-extent`. SBCL then allocates
the parcel on the **stack**: zero heap garbage per call, no GC pressure.
**Expected: noticeable on call-heavy code; nearly free to implement; fully
sound with the existing convention.**

### 5.4 R2 — the calling convention (both halves)
Callee: `&optional`/`&rest` lambda lists (spec #3). Caller: skip the
`*wantarray*` dynamic bind when the callee is provably context-insensitive
(per-sub bit from a body scan). **Expected: turns call-bound code from
"2–4× slower" into "faster than Perl" — Perl's calls are its weak spot.**

### 5.5 ★ Recognize the classic sort comparators
`sort { $a <=> $b } @xs` currently funcalls a generic comparator per
comparison, through boxes and dynamic `$a`/`$b`. Pattern-match the handful
of idioms that cover ~95% of real code — `{ $a <=> $b }`, `{ $b <=> $a }`,
`{ $a cmp $b }`, `{ $b cmp $a }`, and the key-extractor forms
`{ $a->{k} <=> $b->{k} }` / Schwartzian transforms — and emit native
`(sort vec #'<)` (etc.) on unboxed keys. This attacks the worst measured
row (arrays/sort: **15× slower**). **Expected: order-of-magnitude on
sort-heavy code; a day of pattern-matching in codegen.**

### 5.6 Phase 4 — unboxing + repr (the spec's core)
The dossier-driven envelope removal (§4). With §5.1+§5.2 in place its
measured ceiling on arithmetic loops is 0.014 s vs Perl's 0.091 s.
**Expected: the big one; arithmetic/loop code goes well below Perl.**

### 5.7 ★ Aggregate element representation (raw hash/array contents)
The plan unboxes scalar *variables*; CPAN hot loops are element traffic
(`$count{$_}++`, `push @out, $x`). Same dossier lifted to a `my %h`/`my @a`
whose reference never escapes and whose elements are never referenced/tied:
store **raw values** in the table/vector, skip per-element envelopes and key
re-stringification. **Expected: recovers the hash 6.4× and much of the array
15× rows; medium effort once Phase 4 machinery exists.**

### 5.8 ★ Method-dispatch inline caches (no sealing needed) — **superseded: see the status update at the top (s444: cache-free plan in task #73; per-call-site cells rejected)**
`$obj->name` currently does a string-keyed package walk every call.
Cache, *at each call site*, the last seen class and the resolved function
(one cons or a two-slot vector patched at runtime): next call with the same
class = one pointer compare + direct call. Monomorphic sites — the vast
majority — become nearly as fast as plain calls, and the guard makes
runtime redefinition automatically safe (cache just misses). This is the
classic Smalltalk/JavaScript "polymorphic inline cache", and it does **not**
require the sealed-world flag. **Expected: 2–5× on OO/Moo-heavy code;
medium effort, pure runtime+codegen, no analysis.**

### 5.9 ★ Infer the two "flags" instead of asking for them
From the follow-up review (§3.2b of the review): a user-facing
`--no-overload` flag is worth only ~4% at runtime, and sealed-subs is
unnecessary for plain calls. But both are valuable **as inferred facts**:
PCL transpiles every module it loads, so it can *know* "no `use overload`
anywhere" (closes the eager-stringify soundness hole, simplifies Gate 1) and
"no `*foo =` / AUTOLOAD / `local *foo` / string `eval`" (→ closed world:
inline small subs, propagate return types, devirtualize aggressively).
Programs that do use string `eval` simply don't get the aggressive mode.
**Expected: enables the interprocedural layer for most real programs with
zero user ceremony.**

### 5.10 ★ Block-compile our own code
The runtime (`pcl-runtime.lisp`) and the shipped shims (`lib/*.pm` output)
are *ours* — nobody redefines them at runtime. Compile them with SBCL's
block compilation / `(declaim (inline …))` for the small hot helpers so
calls **within** them become direct jumps and tiny helpers disappear into
their callers. User code keeps the safe cell-based calls. Also: the global
`(declaim (optimize (speed 2) (safety 1)))` and honest `ftype`s from Tier 3
(#9–#11) — still unimplemented, still free. **Expected: broad 1.2–2×
baseline lift on everything.**

### 5.11 ★ Fuse `map`/`grep` chains (deforestation)
`for (grep {…} map {…} @xs)` currently materializes every intermediate
list. When the consumer is a `for`/`while`/void context, fuse the chain
into one loop with no intermediate vectors. **Expected: modest CPU win, big
allocation/GC win on list-pipeline code (very common CPAN style); medium
effort, purely local rewrite on the IR (needs R3).**

### 5.12 ★ Profile-guided specialization (the long game)
PCL controls transpilation end-to-end, so a two-run mode is possible: run
once with per-call-site type recording (cheap counters), re-transpile with
that feedback — specialize the sites that were monomorphic in practice,
guarded so behaviour changes fall back. This is how JS engines get
interpreter-beating speed on untyped code. **Expected: recovers wins static
analysis can't prove (opaque call sources); significant machinery — only
worth it after §5.1–5.8 plateau.**

### 5.13 Regex — the honest wall
cl-ppcre is measured ~3.7× behind Perl's engine on regex-heavy code, and no
variable analysis touches that. Steps: (a) measure how much of the 3.7× is
PCL's own plumbing (capture boxing, `*wantarray*` wraps around `=~`) vs the
engine; (b) ensure every constant pattern is compiled once at load time;
(c) if the engine gap dominates, an FFI bridge to PCRE2 for the scanner is
the real fix — a separate, well-contained project. **Until then: regex-bound
code will not beat Perl; everything else can.**

### 5.14 ★ GC and allocation hygiene
Boxes, parcels, adjustable vectors, and per-op closures make garbage; most
of the items above reduce allocation as a side effect (that is much of why
they win). Additionally: size the nursery for the workload
(`--dynamic-space-size`, gencgc tuning in the launcher), prefer simple
vectors over adjustable ones where size is known, and audit the remaining
per-call/per-op allocations after §5.1–5.4 with SBCL's allocation profiler.
**Expected: the long tail; measure before tuning.**

### Priority order

**Week-one, no architecture change:** 5.1 (FPU modes) → 5.10 (declaims) →
5.2 (inline ops) → 5.3 (dynamic-extent `@_`) → 5.5 (sort comparators).
These are all runtime/local-codegen fixes; together they should move the
6.7× geometric-mean gap to roughly 2–3× *before the rewrite proper starts*,
and they de-risk the cost model.

**The rewrite:** R3 (IR) + Phase 2 analysis spine → 5.4/R2 → 5.6/Phase 4 →
5.7 aggregates → 5.8 inline caches → 5.9 inferred flags.

**After plateau:** 5.11, 5.12, 5.13(c), 5.14.

---

## 6. Q&A: does getting a value from a sub call force boxing? (2026-07-02)

**Question:** isn't boxing largely decided by whether a variable's value
comes from a sub call — and since subs are replaceable, how much less boxing
would we get if we could analyze callees and assume they're never redefined?

**Answer: sub calls don't decide boxing at all — boxing is decided by the
variable's own local uses.** The box is the variable's *cell*, not its
value: `my $x = f();` stores whatever `f` returned into `$x`'s slot, and
whether that slot needs to be a box depends only on what *this sub* does
with `$x` — is `\$x` taken, is it `local`ized/tied, is it a `m//g` target,
is it written through a closure, is there a string `eval` in scope. (Passing
`$x` *to* a sub doesn't box either, because PCL deliberately doesn't support
`@_` aliasing — arguments are copies. If aliasing were ever added, this
answer would flip.) What an opaque call source blocks is only **Gate 2**,
the narrower step — proving "this is definitely a number/string" so
operators compile guard-free-native and coercions can be folded.

**Measured on real CPAN code** (static survey, 2026-07-02, name-based
heuristic over ~1,400 `my`-scalar declarations in Data::Dumper,
Text::Balanced, JSON::PP, HTTP::Tiny, Getopt::Long, File::Temp, Text::Wrap,
Time::Local, Text::ParseWords, and `cl/pack-impl.pl`):

| | share of `my`-scalars |
|---|---:|
| Gate-1 disqualified by **local** use (ref-taken/`local`/`pos`/`m//g`) | **~11%** (range 0–32%; Data::Dumper 32% — it passes `\$refs` everywhere; HTTP::Tiny 24% — `\$`-callbacks) |
| initialized **from a sub call** (= where callee knowledge has any upside) | **~11%** |
| files containing string `eval` (the §4.2 blanket disqualifier) | 2 of 10 |

Caveats: the heuristic can't see closure-mutation or foreach-alias
disqualifiers, so true local disqualification is somewhat higher — call it
**15–25%**. The conclusion survives: **the large majority of scalars unbox
with purely local analysis, and a sealed-subs assumption adds approximately
zero additional unboxing.**

What sealed-callee knowledge *does* buy for the ~11% call-sourced scalars is
Gate-2 narrowing: skip the R1 type-guard and allow folded coercions. The
brackets bound that upside: guarded-generic vs native on unboxed values is
only ~1.6× *on pure arithmetic* — and the R1 guard branch-predicts perfectly
in loops — so the **blended effect on typical code is a few percent**. The
places sealing genuinely pays are the ones listed in the review §3.2b(ii):
inlining accessors, devirtualizing method dispatch, trusting the per-sub
context bit (R2), and return-type propagation for a specific proven-hot
numeric path. Prioritize accordingly: local analysis first; sealing is an
OO-dispatch and inlining story, not a boxing story.

One flag from the survey worth acting on: the §4.2 rule "string `eval` in
scope boxes every visible lexical" hit 2 of 10 modules *at file level*. It
must be applied per-scope (only lexicals actually visible at the `eval`
site), or those files lose all unboxing.

## 7. What "faster than Perl" realistically means

With the full menu: arithmetic, loops, string building, sub calls, and OO
dispatch — the things that make *programs* slow — go at or well below
Perl's times (the brackets put the ceiling at 6–40× faster). Hashes and
arrays reach parity or better once elements are raw. Regex-dominated code
stays behind until/unless the PCRE2 bridge happens. Blended CPAN code:
**at or below Perl is a defensible target; the pure-compute parts of it,
far below.** The one thing that could invalidate this is skipping the
measurement checkpoints — every claim above traces to a measured bracket,
and the phase gates in the review (§6) keep it that way.
