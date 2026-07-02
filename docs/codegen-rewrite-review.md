# Review: the Codegen Rewrite Plan — soundness, gaps, and the go/no-go call

**Written:** 2026-07-02
**Reviews:** `codegen-rewrite-spec.md`, `type-flow-and-codegen-plan.md`,
`two-phase-compiler.md`, `ast-annotation-plan.md`, `intra-sub-goto-plan.md`,
plus the current runtime (`cl/pcl-runtime.lisp`) and live codegen output
(re-verified 2026-07-02).
**Purpose:** decision basis for the compiler rewrite. The stated requirement:
generated code must be **at least as fast as native Perl**, and the compiler
must be **simple and easy to extend**.

---

## 1. Verdict

**The plan is architecturally sound and the rewrite should proceed — but the
plan as written does not meet the stated performance goal, by its own
numbers.** `type-flow-and-codegen-plan.md` §"Measured payoff" projects that
full unboxing lands blended CPAN-style code at **~2–3× slower than Perl**
(only tight numeric loops go below Perl). If "faster than native Perl" is the
bar, three things missing from the plan must be added. None of them
invalidates the existing docs; they extend them:

- **R1 — inline fast-path operators** in the runtime. Without this, even
  fully *unboxed* code pays, per `+`: a full function call, two
  `p-find-overload` probes, a closure allocation (`%pcl-ieee-arith` takes a
  `lambda`), and `sb-int:with-float-traps-masked` (FPU control-word
  save/restore) — for integer+integer, where none of that can matter. The
  docs' "compounding effect" section correctly observes the box-check fast
  paths exist but never schedules making the *operators themselves* inline
  with a numeric guard. This is the single highest-leverage runtime change
  and it is Phase-1-grade (no analysis needed). Sketch in §5.2.

- **R2 — lean calling convention.** The measurement section itself names the
  calling convention as overhead #2 (`p-flatten-args` + `*wantarray*` dynamic
  rebind + context-sensitive `p-return-value`), then the plan only addresses
  the callee half (#3, lambda lists, Phase 4). Verified today: every call
  site emits `(let ((*wantarray* nil)) (pl-f …))` — a dynamic bind per call —
  and every callee is `(&rest %_args)` — a cons list per call. Perl's own sub
  calls are notoriously slow (~its weakest area), so this is exactly where a
  compiled backend can *win*, not just catch up — but only if both halves are
  fixed. The caller half (elide the `*wantarray*` rebind when the callee is
  provably context-insensitive) is cheap and should move earlier. Sketch in
  §5.4.

- **R3 — structured emission (an s-expression IR) as an explicit phase.**
  `intra-sub-goto-plan.md` diagnoses the deepest problem in the current
  compiler — declarations are an interleaved stream of `let`-opens over
  *text*, control flow is regex surgery on emitted lines, and codegen output
  differs between the 1-pass and 2-pass paths — and says plainly that the
  cure is an IR. The rewrite spec cites that doc as "acceptance tests" but
  the unified phase table never schedules building the IR. This is the item
  that delivers the user requirement "simple and easy to extend"; the
  annotation passes alone do not. Sketch in §5.5.

With R1–R3 added, "faster than native Perl" is **credible for compute-,
call-, and data-structure-bound code** (the brackets prove ~10–100× headroom
on arithmetic; CL direct calls beat Perl's sub calls; hash/array paths reach
parity or better once element access is native). It is **not credible for
regex-dominated code** as long as the engine is cl-ppcre (measured 3.7×; see
§3.3). The go/no-go should be decided against the honest target in §6, not a
blanket "faster on everything".

---

## 2. What was verified during this review

Claims in the docs were spot-checked against the tree as of 2026-07-02:

| Claim | Status |
|---|---|
| Evidence baseline (fib output: dead `$i`/`$c` boxes, `*wantarray*` wraps, `&rest` + `p-list-=`) | **Confirmed**, byte-for-byte shape. Additionally the whole sub body is now wrapped in `(let ((*wantarray* :void)) …)` — the CLAUDE.md §8 VOID_CTX regression, which must be fixed before spec item #2. |
| Phase 0 done (`BlockAnalyzer` + `_emit_scoped_block`) | **Confirmed** (`Pl/BlockAnalyzer.pm` exists, wired into `Parser.pm`). |
| Tier-3 items #9–#12 | **All still open.** No global `optimize` declaim; `unbox`/`to-number`/`to-string` not inlined (only `%p-real-hash-key-p` is); `ftype` declaims still `(function (t) t)`; overload table still cons-keyed at 3 sites (`pcl-runtime.lisp:1179/1196/1252`). |
| Perf baselines | **Consistent.** Re-measured (startup excluded): fib(27) PCL ~0.27 s vs perl 0.056 s (≈5×); intmath 2M iters PCL ~0.69 s vs perl 0.091 s (≈7.5×). Docs' 7.1×/10.0× used larger runs; same regime. |
| Call-site shape | **Confirmed**: `(let ((*wantarray* nil)) (pl-fib (p-- $n 1)))` per call; callee `(&rest %_args)` + `p-args-body` + `p-list-=`. |
| `p-find-overload` fast path exists but never fires (all scalars boxed) | **Confirmed** (`p-box-p` bail at :1191). |

**New finding not in any doc:** `%def-overloaded-arith` routes every
non-overloaded `+`/`*` through `(%pcl-ieee-arith (lambda () (op (to-number a)
(to-number b))))` — one heap closure **and** one `with-float-traps-masked`
(FPU state save/restore) per arithmetic op. `p--`, `p-/`, `p-%` have the same
shape. For two integer operands no float trap is possible; this cost is pure
waste on the hot path and is not listed under Tier 3. Folded into R1.

---

## 3. Performance: can generated code beat native Perl?

### 3.1 Where the headroom is proven

The 2026-06-25 brackets are the decisive data and this review accepts them:
hand-written compiled CL runs the same fib loop **8× faster than Perl even
with a box allocation per op** (0.067 s vs 0.55 s), and generic-unboxed CL is
~20× faster than Perl on fib, ~30× on intmath. SBCL is not the bottleneck;
the emitted shapes are. So "faster than Perl" is not physically blocked — the
question is only which shapes the compiler can safely emit, for how much of
real code.

### 3.2 Where the plan-as-written stalls, and what R1/R2 recover

Per-workload, combining the docs' table with the review findings:

| workload | now | plan as written | with R1+R2 added | why |
|---|---|---|---|---|
| arithmetic loops | 7–10× slower | **below Perl** | **10–100× faster** | fully unboxable; R1 makes even *unproven* operands cheap (guard + native op) |
| call-heavy (fib-shaped) | 5–7× slower | ~2–4× slower | **faster than Perl** | R2 removes the per-call dynamic bind + rest-cons; CL direct calls ≪ Perl sub calls |
| string building | ~5× slower | ~1.5–2.5× | **≈parity or faster** | accumulator (#7) fixes the complexity class; R1 helps `eq`/`cmp` guards |
| hash/array code | 6–15× slower | ~2–4× | **≈parity**, faster with element-repr (§3.4) | key stringify + boxed elements remain the floor until aggregates specialize |
| regex-heavy | 3.7× slower | ~1.2–1.5× | ~1.2–1.5× | **cl-ppcre wall — R1/R2 don't touch it** |

### 3.2b Two proposed compiler flags, measured/assessed (2026-07-02)

Follow-up question: how much would (i) a `--no-overload` guarantee and (ii) a
"subs are never redefined at runtime" (sealed/closed-world) assumption buy?

**(i) measured.** The intmath loop emulated at operator level (boxes kept,
same loop skeleton, only the operator pipeline varied; SBCL, 2M iters):

| variant | time | delta |
|---|---:|---|
| A — current `p-+` pipeline (2 overload probes + `%pcl-ieee-arith` closure/trap-mask + `to-number`) | 1.432 s | — |
| B — A minus the overload probes (= the `--no-overload` flag at runtime) | 1.378 s | **−4%** |
| C — B minus the closure + `with-float-traps-masked` (= R1's slow-path fix) | 0.187 s | **7.4×** |
| D — C minus boxes (`to-number`/`box-set` gone; = Phase-4 unboxed) | 0.014 s | 13× more (6.5× **faster than Perl's** 0.091 s) |

So a no-overload flag **buys ~4% at runtime — it is not the lever**. The
probes were already guarded by a cheap `p-box-p`/class-slot fast path; the
expensive parts are the per-op closure + FPU trap-masking (R1) and the box
model (Phase 4). The flag's real value is **for the analysis, not the
runtime**: it closes the §4.1 eager-stringify hole (no overloaded `""` can
exist) and lets codegen open-code `(+ (to-number a) (to-number b))` at any
site without an overload fallback. But R1's inline `numberp` guard obtains
nearly the same code shape *soundly* (overload objects are never `numberp`,
so they fall to the slow path automatically). Verdict: implement R1; derive
the no-overload fact **automatically** (PCL transpiles every module it loads,
so "no `use overload` anywhere in the program" is checkable — spoiled only by
string `eval`) and use it in Gate 1 rather than as a user-facing flag.
Caution: it can never be a default for the CPAN goal — Math::BigInt,
JSON::PP booleans, DateTime, URI all overload.

**(ii) assessed (nothing to measure yet).** This is exactly type-flow §g.4's
option (b). Key fact: **ordinary calls need no sealing in CL** — SBCL calls
through the symbol's function cell, which is both redefinition-safe and
already fast, and the `&optional`/`&rest` calling convention (#3) is uniform
across subs, so caller-side positional passing survives redefinition too.
R2's wins therefore do **not** depend on sealing. What sealing actually
unlocks is the interprocedural layer: (a) **inlining** small subs/accessors —
the big one for Moo/OO code; (b) the **A4 return-type table** → unboxing
propagates across call boundaries; (c) **devirtualized method dispatch** —
`p-method-call`'s string-keyed lookup + MRO walk collapses to a
class-checked direct call at monomorphic sites; (d) trusting the per-sub
context-insensitivity bit without guards. Rough sizing: little for
procedural code beyond R1/R2 (call cells are already cheap), potentially
**1.5–3× on method-heavy OO code** via (a)+(c). The per-call-site guard
(§g.4 option (c), one pointer compare) recovers most of it soundly; the flag
mainly saves building the guard machinery. Like (i), the closed-world fact
is often **inferable**: no glob assignment (`*foo = …`), no AUTOLOAD, no
`local *foo`, no string `eval` → sealed for free, per program, no user
promise needed.

### 3.3 The regex wall — the one honest "cannot promise faster"

Perl's regex engine is its crown jewel; cl-ppcre is a good portable engine
but measured 3.7× off (some of that is PCL's match plumbing — capture
boxing, `*wantarray*` wraps around `=~` — worth separating before blaming
the engine). Options, in order of increasing effort: (a) shave PCL's own
wrapper overhead and re-measure the pure engine gap; (b) compile constant
patterns at load time (cl-ppcre supports this; verify we use it everywhere);
(c) an FFI bridge to PCRE2 for the scanner while keeping cl-ppcre semantics
for the exotic corners. **Recommendation: do (a)+(b) as measurements during
Phase 1, and treat (c) as a separate, post-rewrite project.** The go/no-go
target (§6) should accept ~1.3× on regex-dominated code.

### 3.4 The deferred item that matters for "CPAN code fast": aggregate elements

The plan unboxes **scalars**; array/hash *elements* stay boxed, and every
hash key pays `to-string`. Real CPAN hot loops are `$count{$_}++`,
`push @out, $x`, `$h->{k}[$i]` — element traffic. The same VarInfo machinery
extends naturally: a `my %h` whose reference never escapes and whose elements
are never ref-taken/aliased can hold **raw values** in a raw
`equal`-hash-table (sketch §5.3). This should be named as a scheduled phase
(new Phase 5 item), not left implicit, because it is where the hash 6.4× and
array 15× rows live.

---

## 4. Correctness holes found in the docs

These are places where the plan, implemented as written, would emit wrong
code. Each needs a one-line amendment in its home doc.

### 4.1 Eager `to-string` at the binding is unsound for opaque sources
(`type-flow-and-codegen-plan.md` §c.3 #1, examples E1/E3.)

`my $x = some_sub(); … "$x" … "$x"` with `repr=string` folds stringification
to the binding. But if `some_sub()` returns a **blessed ref with overloaded
`""`**, Perl stringifies **at each use**; if the object mutates between uses
(through another alias) or its `""` has side effects, eager folding is
observable. Gate 1's checkbox "value cannot be a blessed/overloaded ref"
cannot be *proven* for a `source_kind='call'` — which is precisely the case
§c.1 sets out to optimize.

**Fix:** `repr=string` with a *single* eager coercion requires the source to
be provably non-object (string literal, concat/`sprintf` result, forward-typed
string builtin) **or** exactly one read. For opaque call sources with
multiple string reads, use `repr=any` (unboxed, lazy `to-string` per use) —
still kills the box, keeps per-use semantics. Note the flagship win, the
`.=`-accumulator, is unaffected: its source is a `''` literal.

### 4.2 String `eval` lexical capture must disqualify unboxing

Session 250 implemented lexical capture for `eval "…"`: the eval'd code can
**read and write** the enclosing scope's `my` lexicals (via their boxes).
An unboxed lexical is unreachable by that mechanism. No doc lists this.

**Fix (Gate 1 addition):** any `eval STRING` (or anything that can reach the
capture table) syntactically in scope disqualifies unboxing for **every
lexical visible at that point**. Coarse but correct; refine later if a real
hot path demands it.

### 4.3 `foreach` variables are aliases

`for my $x (@a) { $x *= 2 }` mutates `@a` — the loop var binds the element's
box. Spec item #6 gates its unboxing on "range counter / C-style", which is
right, but the Gate-1 disqualifier list never states the general rule.

**Fix:** a foreach loop var is unboxable only when iterating a fresh list
(range, function result, list literal) **or** when its role set is read-only.
Iterating a named array/hash with any write use → boxed.

### 4.4 Regex-target magic: `pos` / `m//g` / `s///` / `tr///`

`pos($x)` lives in the box (`ast-annotation-plan.md` lists it;
**type-flow's Gate 1 checklist omits it** — the lists must be merged into
one authoritative table). Beyond bare `pos`: any scalar that is the target
of a **`/g` match** stores match position in its box — `while ($x =~ /…/g)`
is common CPAN code. `s///`/`tr///` mutate in place through the box today.

**Fix:** add to the merged disqualifier table: target of `m//g`, `pos`,
`\substr`/`\vec`/`\pos` magic cells (see `reference_box_magic_hook`). `s///`
and `tr///` targets need not disqualify *if* lowering rewrites them
functionally (`(setf $x (p-s-value $x …))`) — note as a lowering task,
default to disqualify until that exists.

### 4.5 `two-phase-compiler.md` Step 6 / Phase 2b is superseded and unsound

Its `_infer_type` concludes `fixnum` from "arith-context uses only". That is
wrong twice: consumption-as-numeric doesn't prove the value is a number
(`"3abc" + 0`), and arith results aren't fixnums (`/` → float/ratio,
overflow → bignum/float). The type-flow doc already supersedes this with the
demand lattice + Gate 2 (and its own open-question 2 correctly keeps
`number` generic). **Fix:** add a header note to `two-phase-compiler.md`
marking Step 6, Phase 2b, `_infer_type`, `_both_fixnum`, and the `_let_init`
type hints as **superseded — do not implement**; keep its still-authoritative
parts (pexpr-cache reuse, `_emit_scoped_block`, VarAnnotator sketch).

### 4.6 `ast-annotation-plan.md` D3 mis-states `do {}` capture

D3 says a var is `closure_captured` if referenced inside a nested `sub {}`
**"or `do {}` block"**. A `do {}` block is not a closure boundary
(`two-phase-compiler.md`'s VarAnnotator correctly uses `is_sub => 0` for
`inline_lambda`). Implementing D3 as written would spuriously rename/box
vars used in do-blocks. **Fix:** strike "or `do {}`".

### 4.7 `returns_list` builtin table duplicates Config.pm

Annotation A hardcodes ~20 list-returning builtins — the same knowledge
`Pl/PExpr/Config.pm` param specs already encode. Per the reuse rule
(CLAUDE.md §11), derive the set from Config.pm (add a `returns_list` bit to
the spec table) instead of a second literal list that will drift.

---

## 5. Sketches — what the code looks like with whole-variable-use analysis

These extend the spec's north stars N1/N2 (which stand as written) to the
areas the plan leaves unsketched. Helper spellings are illustrative; the
shape is the contract.

### 5.1 S1 — recursive fib with R2 (the "beat Perl on calls" shape)

VarInfo facts: `$n` — demand ⊆ {numeric, opaque-return}; not captured, not
ref'd. Sub facts (new, per-sub side table): body never calls `wantarray`,
return value is a scalar expression on every path → **context-insensitive**.

```lisp
(defun pl-fib (&optional ($n (p-undef)) &rest --extra--)   ; #3: real lambda list
  (declare (ignore --extra--))
  (let ((n (to-number $n)))               ; numify once; demand is numeric-only
    (if (< n 2)
        n                                 ; context-insensitive: plain value return
        (+ (pl-fib (- n 1))               ; R2: no (let ((*wantarray* nil)) …),
           (pl-fib (- n 2))))))           ;     no rest-cons, direct call
```

Expected: low tens of milliseconds for fib(27)-equivalents vs perl 0.056 s —
i.e. **several × faster than Perl**, where the plan-as-written projects 2–4×
slower. Every removal is annotation-gated: the `*wantarray*` elision by the
callee's context-insensitivity bit, the lambda list by `@_`-untouched (#3),
native `+`/`<` by numeric demand (+ R1 as the fallback when unproven).

### 5.2 S2 — R1: inline fast-path operators (runtime, no analysis needed)

```lisp
(declaim (inline p-+))
(defun p-+ (a &optional (b nil bp))
  (if (and bp (numberp a) (numberp b))
      (+ a b)                     ; SBCL open-codes the fixnum case inline
      (p-+-slow a b bp)))         ; boxes, undef, overload, IEEE masking

(defun p-+-slow (a b bp)          ; = today's %def-overloaded-arith body, minus
  …)                              ;   the per-op closure: pass args, not a lambda
```

Same pattern for `- * < > <= >= == != %` and `p-.`/`p-str-eq` with `stringp`
guards. Two effects: (1) after unboxing, hot sites compile to a type test +
native op — this is the mechanism that turns `repr=any` (unboxed but
unproven) into near-native speed, which the plan currently has no story for;
(2) even **before** unboxing, literal operands (`$i < 10`, `$n + 1`) take the
guard's false branch at half cost because the closure alloc and
trap-masking move off the common path. Also: gate
`with-float-traps-masked` on "at least one float operand" inside the slow
path — for integers it is pure FPU-state churn.

### 5.3 S3 — aggregate element repr (the hash-loop shape, new phase item)

```perl
my %count;
$count{$_}++ for @words;
print "$_: $count{$_}\n" for sort keys %count;
```

VarInfo facts: `%count` — never `\%count`, never tied, never passed, elements
never ref-taken; element demand ⊆ {numeric} for writes, {string} for reads
(interpolation). → raw `equal` hash table with **raw values**:

```lisp
(let ((%count (make-hash-table :test #'equal)))
  (p-foreach ($w @words)
    (incf (gethash (to-string $w) %count 0)))          ; no box per element
  (p-foreach ($k (p-sort-strings (p-hash-keys %count)))
    (p-print (p-. $k ": " (gethash $k %count) #\Newline))))
```

This is where the hash 6.4×/array 15× rows are recovered; without it,
"CPAN data-munging faster than Perl" doesn't happen. The gates are the same
VarInfo disqualifiers lifted to the aggregate + its element set — no new
machinery, one new pass consumer.

### 5.4 S4 — caller-side context elision (R2's cheap half, do it early)

Two designs considered:

- **(a) context as a hidden argument** — `(pl-f ctx …)`: fastest, but changes
  every call site and breaks interop with code refs/`&$f` uniformly.
- **(b) keep the dynamic `*wantarray*`, but bind it only when it can be
  observed** — a per-sub `context_sensitive` bit computed by a syntactic scan
  of the body (mentions `wantarray`, returns an array/list/hash expr, calls
  another *sensitive* sub in tail position — over-approximate transitively,
  defaulting to sensitive for unknown/dynamic callees).

**Recommend (b)**: no calling-convention change, no interop risk, and it
composes with the existing cross-file prototype extraction
(`_extract_file_prototypes` precedent) for knowing callees at call sites.
Call sites to statically-known insensitive subs drop the `let` entirely:

```lisp
;; today, every call:   (let ((*wantarray* nil)) (pl-fib (p-- $n 1)))
;; after:               (pl-fib (- n 1))          ; callee provably insensitive
;; unknown callee:      (let ((*wantarray* nil)) (funcall f …))   ; unchanged
```

### 5.5 S5 — R3: structured emission (the "simple and easy to extend" core)

Today a statement emits **text**: `$self->_emit("(let (" . join(…) . ")")`,
`indent_level++`, `_pending_let_closes`, and later passes
(`_wrap_runtime_labels`) re-parse that text with a paren scanner to insert a
`tagbody`. The rewrite target: every handler returns a **form** (nested Perl
arrayrefs); one printer renders the tree at the end.

```perl
# A statement handler returns a form, not a string:
sub lower_my_decl {
    my ($self, $vinfo, $init_form, @rest_of_block) = @_;
    my $init = $vinfo->{repr} eq 'box' ? [ 'make-p-box', $init_form ] : $init_form;
    return [ let => [ [ $vinfo->{cl_name}, $init ] ], @rest_of_block ];
}

# Scopes are nodes, so a block KNOWS its statement list; intra-sub goto becomes:
sub lower_labelled_block {
    my ($self, @stmts) = @_;            # some stmts are [label => 'FOO'] markers
    return [ tagbody => map { $_->[0] eq 'label' ? $_->[1] : $_ } @stmts ];
}

# One printer; indentation = 2 × depth BY CONSTRUCTION:
sub print_form {
    my ($form, $depth) = @_;
    return $form unless ref $form;
    my ($head, @args) = @$form;
    return '(' . join(' ', $head, map { print_form($_, $depth+1) } @args) . ')'
        if total_len_small($form);
    return "($head\n" . join("\n", map { ('  ' x ($depth+1)) . print_form($_, $depth+1) } @args) . ')';
}
```

What this buys, concretely:

- **The whole `intra-sub-goto-plan.md` bug class dies**: Blocker A (splice
  into a text bucket discarded downstream) and Blocker B (1-pass vs 2-pass
  producing different text) cannot exist when the sub body is one tree
  handed to one printer. Labels/`goto`/`my`-scopes are resolved before any
  text exists — exactly what that doc asks for.
- **CLAUDE.md's paren-discipline problem disappears for generated code** —
  balance and depth-encoding indentation are properties of the printer, not
  of every emission site.
- **Extension = adding a lowering function** that pattern-matches forms,
  not finding the right place in an 8000-line emit stream to interleave
  another `_emit` + close-bookkeeping pair. This is the requirement
  "simple and easy to extend", made structural.
- Peephole passes (spec #4 `case`-lowering, #5 dead if-ret, #8 folding)
  become tree rewrites — trivial to add, test, and order.

**Migration strategy** (the risk is churn, not design): convert
*inside-out*. `ExprToCL::gen_*` already returns strings compositionally — the
signatures don't change when they return forms instead; the printer is the
only new code. Statement/block level (`Parser.pm`'s `_emit` stream and
section buckets) converts per-construct behind the existing entry points,
with the 3720-test gate + sweep count as the safety net. The buckets
(preamble/declarations/definitions/runtime) stay — they just hold forms.

---

## 6. Revised phase plan and go/no-go criteria

Amendments to the spec's unified table (which otherwise stands):

| Phase | Additions / changes |
|---|---|
| **1** | Add **R1** (inline fast-path ops + de-lambda `%pcl-ieee-arith` + trap-mask only for floats). Precondition: fix the VOID_CTX `*wantarray*` body-wrap regression (CLAUDE.md §8) before #2. Add a **benchmark harness** (`bench/` with the 6 workloads; numbers tracked in a tsv, run before/after each phase) — today the numbers live only in doc prose. Measure regex overhead split (PCL plumbing vs cl-ppcre engine) here. |
| **2** | Unchanged (VarAnnotator spine) **plus R3 begins**: `ExprToCL` returns forms + printer lands here (expression level first). Doc fixes from §4 (merge disqualifier tables, strike D3 `do{}`, supersede two-phase Step 6) before implementation starts. |
| **3** | Unchanged, plus **R2 caller half**: per-sub `context_sensitive` bit; elide call-site `*wantarray*` binds. R3 statement/block level (scope tree; retire `_wrap_runtime_labels` text surgery — goto tests are the acceptance gate). |
| **4** | Unchanged (repr, #6/#7, #3 lambda lists = R2 callee half). Gate-1 list now includes §4.2–4.4 disqualifiers; `repr=string` eager-coercion restricted per §4.1. |
| **5** | Unchanged, plus **aggregate element repr** (§5.3) and the regex decision (PCRE2 bridge yes/no, based on Phase-1 measurements). |

**Go/no-go, made falsifiable.** Proceed now (Phase 1 + R1 is free speed and
zero architecture risk). Then decide at two checkpoints:

1. **After Phase 1 + R1** (cheap, weeks not months): intmath/fib should
   improve ≥3× with zero analysis work. If they don't, the cost model in the
   docs is wrong and the plan must be re-derived before Phase 2.
2. **After Phase 4 on the harness**: targets — arithmetic loops **faster
   than Perl** (bracket-proven headroom, so failure means implementation
   bugs, not physics); fib-shaped call code **faster than Perl** (needs
   R2); strings/hashes **≥ parity**; regex **≤1.5×** accepted. Blended
   goal: **parity or better on the non-regex mix**.

If the requirement is instead a hard "faster than Perl including
regex-dominated workloads", the existential risk is the regex engine, not
the codegen — that decision (PCRE2 bridge) should then be pulled forward,
because no amount of variable analysis changes it.

**Recommendation: do not give up.** The measured brackets show the target
shapes run far below Perl's times on SBCL; every transform is
correctness-gated with `box`/status-quo as fallback; Phase 0 is already
shipped and green. The plan needed (a) honesty that its own projection
missed the stated bar, and (b) the three additions above that close the gap
for everything except regex — where the bar itself should be renegotiated.

## See also
- `docs/codegen-rewrite-spec.md` — the spec this reviews (phases table amended by §6)
- `docs/type-flow-and-codegen-plan.md` — repr model (Gate 1 amended by §4.1–4.4)
- `docs/intra-sub-goto-plan.md` — the IR motivation (elevated to scheduled work as R3)
- `cl/pcl-runtime.lisp:1685` — `%def-overloaded-arith` (R1 target), `:1188` `p-find-overload`
