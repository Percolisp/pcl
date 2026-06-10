# PCL: Codegen Rewrite Spec — What the Generated CL Should Look Like

**Written:** 2026-06-11
**Status:** Spec — the target output for the compiler rewrite
**Reads with:**
- `docs/type-flow-and-codegen-plan.md` — the *representation* model (boxing axis,
  `repr` lattice, the bool/numify asymmetry). **That doc decides per-variable
  representation; this doc decides the surrounding code shape.**
- `docs/two-phase-compiler.md`, `docs/ast-annotation-plan.md` — the analysis
  passes that supply the facts these transforms consume.

This document has two layers, on purpose:
- **§Why** — the rationale, for a human deciding whether the plan is sound.
- **§Spec** — the concrete transforms, file targets, north-star output, and
  acceptance tests, for whoever (human or AI) implements it later.

---

## §Why — the one idea

> **SBCL is an excellent optimizing compiler. Our job is to stop handing it code
> it cannot optimize, and start handing it code it can.**

Two kinds of problem show up in real generated output:

1. **Work the codegen *adds* that SBCL is not allowed to remove** — dead heap
   allocations, dynamic-variable rebinds, `&rest` list conses. These have (or
   might have) side effects, so the compiler must keep them. Only *we* can stop
   emitting them.
2. **Shapes SBCL *could* optimize but isn't given the chance** — boxed loop
   counters instead of fixnums, `eq`-chains instead of `case`, O(n²) string
   building instead of a fill-pointer buffer.

The proving ground is **`cl/pcl-pack.lisp`**: it is `cl/pack-impl.pl` run through
`pl2cl`, i.e. 4700 lines of *our own codegen output* doing byte-level work. Every
transform below was found by reading it (and a transpiled `fib`). Fixing codegen
speeds up pack, every CPAN module, and the test suite at once — no hand-written CL
to maintain. **Treat pack as the optimization oracle, the way
`tools/difftest-ops.pl` is the correctness oracle.**

All transforms are **correctness-preserving and independently shippable**; each
either keeps behaviour identical or is gated on an analysis fact that proves it
safe. The fallback for every one is "emit what we emit today."

---

## §Spec

### Evidence baseline (current output, captured 2026-06-10/11)

`fib` today — note the two dead top boxes, the `*wantarray*` wraps on plain
assignments, the `&rest`/`p-list-=` arg handling, and every scalar boxed:

```lisp
(p-sub pl-fib (&rest %_args)
  (p-args-body
    (block nil
      (let (($i (make-p-box nil)) ($c (make-p-box nil)))      ; ← both DEAD (rebound below)
        (let (($n (make-p-box nil)))
          (let ((*wantarray* nil)) (p-list-= (vector $n) @_)) ; ← rest+vector arg handling
          (let (($a (make-p-box nil)))
            (p-my-= $a 0)
            (let (($b (make-p-box nil)))
              (p-my-= $b 1)
              (p-foreach ($i (p-.. 2 $n))                      ; ← $i rebound here
                (let (($c (make-p-box nil)))                   ; ← $c rebound here
                  (p-my-= $c (p-+ $a $b))                      ; ← boxed arithmetic
                  (let ((*wantarray* :void)) (p-my-= $a $b))   ; ← special rebind for `$a=$b`
                  (p-my-= $b $c)))
              (p-return $b))))))))
```

### Tier 1 — Stop emitting work SBCL cannot remove

| # | Transform | Evidence | Gate (when safe) | Lives in |
|---|-----------|----------|------------------|----------|
| 1 | **No dead double-bound boxes.** Don't hoist a var that the scoped pass or `p-foreach` will bind at its real site. | `fib` `$i`/`$c`; `_pack_str_one` ~21 boxes | var is bound by an inner scoped `let`/`p-foreach` on every path | `Parser.pm` `_with_declarations` supplemental-hoist vs `_emit_scoped_block` |
| 2 | **No spurious `*wantarray*` rebind.** Only wrap a statement in `(let ((*wantarray* …)) …)` when its RHS can *observe* context. | `(let ((*wantarray* :void)) (p-my-= $a $b))` | RHS contains no sub/method call, `eval`, or `wantarray()` | codegen of assignment / `_process_expression_statement` (the CLAUDE.md §8 over-broad VOID_CTX regression) |
| 3 | **Fixed-arity lambda lists.** When `@_` is only consumed by one leading `my (...) = @_` and never otherwise touched (no `@_`, `shift`, `goto &`, `$_[n]`), emit real params. | every `(&rest %_args)` + `(p-list-= (vector …) @_)` | BlockAnalyzer proves `@_` untouched after the binding | sub codegen + BlockAnalyzer flag |
| 4 | **`eq`/`==`-chain → `case`.** A chain of `eq`/`==` of one scrutinee against constants becomes a CL `case` (jump table). Single-char string consts → `(case (char (the simple-string $x) 0) …)`. | `_pack_str_one` `$ch eq 'a'/'A'/'Z'/…` | scrutinee is one pure expr; all arms compare to compile-time constants | if/elsif codegen |
| 5 | **Don't write `--pcl-if-ret--N` when the if's value is unused** (void/non-tail position). | `(setf --pcl-if-ret--6 (p-str-eq …))` on every arm | if-statement context is VOID and not the sub's tail | tail-if transform |

### Tier 2 — Emit shapes SBCL is good at

| # | Transform | Evidence | Gate | Lives in |
|---|-----------|----------|------|----------|
| 6 | **fixnum/integer loop counters.** A `for(my $k=0; …)` / range counter that is never `\`-referenced or captured → unboxed CL integer, native `<`/`incf`. fixnum only if overflow provably impossible; else `integer` (CL bignum = Perl auto-promote). | `_pack_str_one` `$k`/`$bit`/`$bs`; `fib` `$i` | `repr` analysis (type-flow doc Gate 2) + bounded | C-style/range loop codegen |
| 7 | **String accumulator.** Append-only `.=` builds a fill-pointer string / output stream, not repeated `concatenate`. O(n²)→O(n). | `_pack_str_one` `$$result_ref .= …` per byte | var (or referent box) is only `.=`-appended then read as string | type-flow doc §c.3 #4 |
| 8 | **Constant folding & single-char lowering.** `chr(0)`→`"\0"`; `substr($s,$k,1)`→`(char s k)` when length is literal 1; `ord(substr(...))`→`char-code`. | `_pack_str_one` `chr(0)`, `substr($arg,$k,1)`, `ord(...)` | args are compile-time constants / length literal 1 | builtin codegen |

### Tier 3 — Let SBCL compile the *runtime* hot functions well

These are edits to `cl/pcl-runtime.lisp`, independent of codegen.

| # | Transform | Evidence | Notes |
|---|-----------|----------|-------|
| 9 | **`(declaim (optimize (speed 2) (safety 1) (debug 0)))`** globally; `(speed 3 safety 0)` locally on proven-safe hot fns. | nothing sets optimize → default `(speed 1 debug 1)` | start at file top of `pcl-runtime.lisp` |
| 10 | **`(declaim (inline …))`** the per-op accessors: `unbox`, `box-set`, `p-box-p`, `to-number`, `to-string`, `p-true-p`. | `unbox` is a non-inlined `defun` hit on every read | lets SBCL fold the `p-box-p` test when arg type is known |
| 11 | **Tighten `ftype` declaims** from `(function (t) t)` to real return types: `to-number`→`real`, `p-length`→`fixnum`, etc. | `pcl-runtime.lisp:403` all-`t` ftypes give SBCL nothing | callers can then skip re-checks |
| 12 | **De-cons the overload table.** `p-find-overload` does `(gethash (cons cls op-str) *p-overload-table*)` — a cons per check, fired on every `p-.`/`p-+` because every scalar is boxed. Use a nested hash `cls → op → handler`. | `pcl-runtime.lisp:1014` | zero-alloc lookup |

### The compounding effect (why Tier 1/type-flow multiplies Tier 3)

`p-find-overload`, `p-.`, `p-+` each open with `(when (p-box-p val) …)` — a fast
path that returns immediately for non-boxes. **Today it never fires, because every
scalar is a box.** The moment the variable work makes scalars raw, those fast
paths start short-circuiting, so unboxing doesn't only save the box alloc — it
*activates* every overload/coercion bypass already in the runtime. Tier 3 (#12
especially) and the type-flow work reinforce each other.

---

## §North-star output

The acceptance target. Spelling of helper macros (`p-foreach-int`, etc.) is a
codegen detail to finalize during implementation; the **shape** is the contract.

### N1 — `fib` after Tier 1 + Tier 2

```lisp
(p-sub pl-fib ($n)                         ; #3 fixed arity, @_ untouched
  (block nil
    (let ((a 0) (b 1))                     ; #1 no dead boxes; #6 unboxed integers
      (declare (integer a b))              ;    (CL bignum = Perl auto-promote)
      (p-foreach-int (i 2 (to-number $n))  ; #6 native integer range counter
        (let ((c (+ a b)))                 ; #6 native add, no box, no p-+
          (declare (integer c))
          (setf a b)                       ; #2 no *wantarray* rebind
          (setf b c)))
      (p-return b))))
```

Line-by-line provenance: fixed param `$n` ← #3; `(let ((a 0)(b 1))` with no top
`$i`/`$c` ← #1; `integer` declares + native `+`/range ← #6; plain `setf` ← #2.
`$n` stays a received value and is numified once at the range (it is opaque, used
numerically). Everything else is unboxed because none of `a/b/c/i` is
ref-taken, tied, captured, or numeric-only-but-overflowing.

### N2 — string accumulator

```perl
sub join_words { my @w = @_; my $s = ''; $s .= "$_," for @w; return $s; }
```

```lisp
(p-sub pl-join_words (&rest %_args)
  (p-args-body
    (let ((w (p-flatten-args %_args)))
      (let ((s (make-array 0 :element-type 'character        ; #7 O(n) buffer
                             :adjustable t :fill-pointer 0)))
        (p-foreach (it w)
          (let ((piece (p-string-concat it ",")))
            (loop for ch across piece do (vector-push-extend ch s))))
        (p-return s)))))                                      ; s IS a CL string
```

`$s` has demand ⊆ `{string}`, append-only mutation, terminal read → `repr=string`
+ accumulator path. A fill-pointer character array *is* a CL string, so `p-return`
and any later string op consume it directly.

### N2-ref — the pack variant (accumulate through a scalar ref)

pack appends via `$$result_ref .= …`, so the **box must stay** (the ref needs its
identity). But the box can *hold* the fill-pointer buffer, and `p-.=` through the
deref becomes amortized-O(1):

```lisp
;; result box holds an adjustable character vector instead of a plain string;
;; (p-.= (p-cast-$ $result_ref) piece)  →  push piece's chars onto that vector
```

This keeps ref semantics while killing the O(n²). Decide at the box level
(a "string-builder box") — see type-flow doc §c.3 #4.

---

## §Acceptance tests

Add `Pl/t/codegen-shape-01.t` (transpile + `like`/`unlike` on the CL string).
These are regression guards that lock in each transform:

```
unlike($cl, qr/\(let \(\(\$i \(make-p-box/, 'no dead hoisted loop-var box');   # #1
unlike($cl, qr/\(let \(\(\*wantarray\* :void\)\) \(p-my-= \$\w+ \$\w+\)/,
        'no *wantarray* wrap around plain var-copy assignment');               # #2
like  ($cl, qr/\(p-sub pl-fib \(\$n\)/, 'fixed-arity lambda list');            # #3
like  ($cl, qr/\(case /, 'eq-chain lowered to case') if $has_char_dispatch;    # #4
```

Plus two **microbenchmarks** (time under SBCL, before/after), reported as numbers
not asserted:
- `pl-fib` of a large n (Tier 1/2 combined).
- `pack "A*", $big` / a `B`-format loop (accumulator + counters; reads pack
  directly as the codegen oracle).

Regenerate `cl/pcl-pack.lisp` from `cl/pack-impl.pl` after codegen changes (see
its header REBUILD PROCEDURE) and re-run the pack.t sweep — pass count must not
drop, and the file should shrink / lose boxes visibly.

---

## §Unified implementation phases  *(single source of truth)*

This is the **authoritative end-to-end ordering** for the whole rewrite — it
merges this doc's perf items (#1–#12) with the analysis passes from
`type-flow-and-codegen-plan.md` §(f) and the scoping/declarator work from its
§(s). Where the two docs previously had separate ordering tables, *this* is the
one that governs; the per-pass detail tables stay in their home docs but are
sequenced here.

The guiding property: **Phase 1 is free speed, Phase 2 is the foundation
everything else needs, Phases 3–4 cash it in.** Every item is independently
shippable and correctness-preserving — the specialization steps are opt-in
narrowings from `box`, so an incomplete analysis loses an optimization, never
correctness.

| Phase | Goal | Work | Gate |
|-------|------|------|------|
| **0** *(done)* | Statement scoping for sub bodies | `BlockAnalyzer` + `_emit_scoped_block` (`two-phase-compiler.md`) | suite green |
| **1** — free wins | Speed with **no architecture change** | Tier 3 runtime: **#9** optimize declaims, **#10** inline accessors, **#11** tighten `ftype`s, **#12** overload de-cons. Tier 1 codegen: **#2** kill spurious `*wantarray*` wraps, **#1** dead double-bound boxes. | `prove -j8` green; no fully-passing drop; microbench moves |
| **2** — analysis spine | The foundation; **still 100 % boxed** | `VarAnnotator`: recursive scope stack for **all** blocks (retire sub-body-only), **decl-site keying** (§s.1), position-aware resolution (§s.3), closure capture across `is_sub` frames, `var_kind` + my/our/local/state classification (§s.6), rename decisions (§s.2). Retire `_vars_referenced_in_closures` + parse-time renames. *(= type-flow §(f) step 1, extended with declarators.)* | closure.t + shadowing cases (§s.4) + full suite green; **no repr yet** |
| **3** — expression annotations | Retire the codegen hacks | `ASTAnnotator`: `returns_list` + `needs_wantarray` (delete `_child_is_list_expr` + the `p-=~` string-match), then `lvalue` (remove the `lvalue_context` flag). *(= type-flow §(f) steps 2–3.)* | `gen_tree_val` + aref/href lvalue tests |
| **4** — representation | The payoff | Gate 1 `unboxable` → Gate 2 `repr` (string/number/any), from the use-site lists *(= type-flow §(f) steps 4–5)*. repr-dependent codegen: **#6** fixnum/integer loops, **#7** string accumulator. **#3** fixed-arity lambda lists (calling-convention change — safest now `@_`-usage is solid). Declarator-correct shapes (§s.7) for unboxable `my`/`state`. | north-stars **N1**/**N2**; new `Pl/t/type-flow-*.t`; no fully-passing drop |
| **5** — polish + deferred | Cleanups & future tiers | **#4** `case` lowering, **#5** unused if-ret writes, **#8** constant folding. *Deferred:* A4 interprocedural return types (type-flow step 7), SSA flow-sensitivity (step 8). | per-item tests |

**Dependencies / parallelism:**
- **Phase 1 and Phase 2 are independent** — Phase 1 fixes existing emission and
  the runtime; Phase 2 rebuilds the analysis. They can proceed in parallel.
- Phase 3 depends on Phase 2's OpcodeTree-metadata infrastructure.
- Phase 4 depends on Phase 2 (use-site lists) and is *amplified* by Phase 1 #12:
  once scalars unbox, the runtime's `(when (p-box-p val) …)` fast paths finally
  fire (see "compounding effect" above).

**Cross-cutting rule for every phase:** end on a clean `prove -j8 Pl/t/` **and** a
`perl-tests/` sweep with **no drop in fully-passing count** (the guard-the-count
rule) **and** a `cl/pack-impl.pl` → `cl/pcl-pack.lisp` rebuild as a codegen sanity
check (pass count must not drop; boxes/gaps should visibly shrink).

Do **not** sketch the remaining items further before implementing — the two
north-stars (N1, N2) are the contract; everything else is specified enough to
build from. More paper design here is the over-planning trap.

## See also
- `docs/type-flow-and-codegen-plan.md` — representation decisions (Gate 1/2, `repr`)
- `docs/two-phase-compiler.md`, `docs/ast-annotation-plan.md` — the analysis passes
- `cl/pack-impl.pl` → `cl/pcl-pack.lisp` — the codegen oracle (rebuild procedure in the .lisp header)
- `cl/pcl-runtime.lisp` — Tier 3 targets: `unbox` (:647), `to-number` (:1666),
  `p-.` (:1691), `p-find-overload` (:1006/:1014), `ftype` declaims (:403)
