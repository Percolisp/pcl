# Variable Declarations in PCL — Current Behaviour Spec

Status: descriptive (as of session 228, 2026-06-01). Written to map the existing
machinery before changing it. Pairs with `docs/two-phase-compiler.md`
(the `my`-scoping pass), `docs/declaration-ordering.md`, and
`docs/closure-lexical-scoping.md`.

---

## 1. The boxing model (the thing everything else rides on)

A Perl **scalar** is represented at runtime by a `p-box` struct: a mutable cell
holding the value (plus cached numeric/string views and tie/magic hooks). The
indirection is what makes Perl aliasing, `local`, references (`\$x`), and
in-place mutation (`$x++`, `$x .= "y"`) work.

- **Read** a scalar: `(unbox $x)` / `(to-string $x)` / `(to-number $x)` — all
  accept either a box or a raw value, so *reads* are forgiving.
- **Write** a scalar: must mutate the *box* — `(box-set $x v)`. Writing requires
  `$x` to actually hold a box; writing to a raw value is a no-op or an error.

Arrays/hashes are adjustable vectors / hash-tables of boxes (not boxed
themselves).

There are **two assignment macros**, and which one codegen picks is the crux of
the bugs in §4:

| macro | used for | behaviour |
|-------|----------|-----------|
| `p-my-=` | lexical (`my`) vars | plain `(box-set place v)` — assumes `place` is already a let-bound box |
| `p-scalar-=` | package globals | `(box-set place v)` **but first** `(unless (boundp 'place) (proclaim '(special place)) …)` — i.e. it will *globalise* the symbol if it looks unbound |

`p-scalar-=` proclaiming the symbol `special` is safe for real globals but
**toxic when applied to a name that is currently a lexical** (see §4.1).

---

## 2. The five declaration kinds and how each lowers to CL

### 2.1 `my` (lexical) — inside a sub
Handled by the **two-phase block compiler** (`Pl/BlockAnalyzer.pm` +
`_emit_scoped_block` / `_with_declarations` in `Pl/Parser.pm`).

- Phase 1 (`BlockAnalyzer::analyze`) walks PPI to find each `my` declaration and
  the statement index where it first appears, plus which vars are captured by a
  nested `sub {}` (closure capture) and a rough type hint.
- Phase 2 (`_emit_scoped_block`) opens `(let (($x (make-p-box nil))) …)` **at the
  statement where `$x` first appears**, not hoisted to the sub top (this fixed
  the mid-sub-shadows-global class of substr.t bugs — see
  `docs/let-scoping-problem.md`).
- Closure-captured vars are **renamed** to `$x__lex__N` (a name that is never
  `defvar`'d) so the `let` stays a genuine *lexical* binding the lambda can
  close over, instead of a dynamic rebinding of a special global. Case-collision
  renames append `__case__N`.
- Assignment lowers to `p-my-=` (plain `box-set`).

### 2.2 `my` (lexical) — at file scope (`in_subroutine == 0`)
`_process_my_toplevel_declaration`: emitted as `(p-eval-always (defvar $x …))`
so BEGIN blocks and load-order see it. At top level a `my` is effectively a
package-ish global box.

### 2.3 `our` (package global)
`_process_our_declaration`: emits a `(p-eval-always (defvar $x (make-p-box nil)))`
into the *declarations* bucket and the initialiser into the body. `$x` becomes a
CL **special** variable holding a box. Assignment uses `p-scalar-=`.

### 2.4 `state` (persistent)
`_process_state_declaration` / `_process_toplevel_state_declaration`: each `state
$x` gets a unique name `$state__sub__x__N` plus an `…__init` guard flag. Stored
either in a `let` wrapping the `defun` (normal case) or as a `defvar` (when the
sub contains inner named subs that must see it). The init expression runs once,
guarded by the flag.

### 2.5 `local` (dynamic save/restore)
`_process_local_declaration`: the common `local $x = EXPR` lowers to a CL `let`
that **rebinds the special symbol** to a fresh box for the rest of the enclosing
block: `(let (($x (p-box-for-local EXPR))) …rest of block…)`. CL's dynamic
unwinding of the special binding *is* the restore. Glob/array-elem/hash-elem
variants use dedicated `p-local-*` macros with explicit unwind-protect.

Key precondition: `local` only restores correctly when it is emitted as a
*statement* that wraps the remaining body. In **expression position** there is
no enclosing form to wrap, and PExpr currently treats a `local` prefix as an
identity no-op (§4.2).

### 2.6 Signature parameters (`sub f ($x, $y = D, @rest)`)
`_parse_signature` + `_process_sub_statement` (session 226). The sub captures all
args via `&rest %_args`, flattens to `@_`, arity-checks, then binds params
positionally in a **`let*`**:

```lisp
(let* (($x (aref @_ 0))                                  ; required → the @_ box
       ($y (if (> (length @_) 1) (aref @_ 1) D)))        ; optional → box OR raw default
  (block nil …body…))
```

Defaults are compiled by `_compile_default_expr` through the normal
PExpr/ExprToCL path.

Since session 228 each scalar param is bound to a **fresh `p-box`** via
`p-copy-scalar-arg` (so a param is its own mutable cell and a copy of `@_`, not
an alias of the caller's box), and the param names are registered in
`_sig_param_lexicals` so the body's `$x = …` lowers to `p-my-=` (`box-set`) —
i.e. params behave like the `my` lexicals they are. The actual generated form
is therefore `($x (p-copy-scalar-arg (aref @_ i)))` for a required param and
`($y (p-copy-scalar-arg (if (> (length @_) i) (aref @_ i) DEFAULT)))` for an
optional one. See §4.1/§4.2 for the history and the `local`-default wrinkle.

---

## 3. Supporting passes

- **Auto-`defvar` pass** (`Pl/Parser.pm` ~line 516): after codegen, scans output
  for package-var references that were never declared and injects
  `(defvar $x (make-p-box nil))`. **`$a` and `$b` are *always* `defvar`'d**
  (sort comparator support), so those two names are *always* special globals.
- **Closure rename map**: `environment->state_var_renames` carries the
  `$x → $x__lex__N` substitutions consumed by ExprToCL when emitting symbols.

---

## 4. Obvious problems to correct (found while writing this spec)

### 4.1 ★ Signature parameters were effectively immutable — ✅ FIXED (session 228)

`sub f ($x) { $x = $x + 1; return $x }` then `f(10)` used to return **10**, not 11.

Root cause: params were bound in `let*` but **not** treated as lexical vars, so
`$x = …` lowered to `(p-scalar-= $x …)`. `p-scalar-=` saw the symbol `$x` as
(seemingly) unbound at the global level, `(proclaim '(special $x))`d it, and the
write landed on the freshly-globalised special cell while the body kept reading
the `let*` lexical — a **silent no-op**. (A literal default was also a raw
non-box, so even a correct `box-set` had nothing to mutate.)

Consequences (all now resolved): any sub mutating its own param was wrong; a
param named like a package global (`$a`, `$b`, an `our`) clobbered that global;
`signatures.t` lost the `t128` `($a = 333)`-in-default block (and, compounded
with 4.2, the `'124' vs '123'` cascade).

**Fix (`Pl/Parser.pm` `_process_sub_statement` + `cl/pcl-runtime.lisp`):**
- Each scalar param is bound to a **fresh `p-box`** via the new
  `p-copy-scalar-arg` (copies the `@_` box or boxes the default, preserving the
  reference/blessed flags and FETCHing through tie/magic). Because
  `p-flatten-args` keeps the *caller's* boxes in `@_`, copying is required so
  mutating a param does not write through to the caller.
- Param names are recorded in a dedicated set `_sig_param_lexicals` that `_emit`
  consults to rewrite `(p-scalar-= $p …)` → `(p-my-= $p …)` (plain `box-set`, no
  `proclaim`). This set is kept **separate** from `_let_bound_vars` on purpose:
  `_let_bound_vars` gates nested-named-sub hoisting (a sub inside a `let` body
  stays inline to capture the lexicals), and putting params there wrongly forced
  an *independently-called* inner named sub to stay inline instead of hoisting
  (caught by the `t160x`/`t161x` commonality tests).

### 4.2 `local` in a signature default was dropped — ✅ FIXED (session 228)

`sub t124 ($b = (local $a = $a + 1)) { "$a/$b" }` used to compile the default to
`(p-scalar-= $a (p-+ $a 1))` — the `local` was **discarded** (PExpr treats
`local` as an identity prefix in expression context, §2.5 precondition), so `$a`
was permanently clobbered to 124 with no restore, corrupting every later
`is $a, 123` (the `signatures.t` `'124' vs '123'` cascade).

**Fix (`Pl/Parser.pm`):** `_parse_signature` peels a `local $G = RHS` default
apart — it compiles only `RHS` as the param's default value and records `$G` as
the param's `local_var`. `_process_sub_statement` then wraps the sub body in a
conditional dynamic rebinding:
```lisp
(let* (($b (p-copy-scalar-arg (if (> (length @_) 0) (aref @_ 0) (p-+ $a 1)))))
  (let (($a (if (> (length @_) 0) $a (p-box-for-local (unbox $b)))))   ; localise only when default taken
    (block nil …)))
```
When the default is taken, `$a` is localised to the param's value and restored on
sub exit by CL's dynamic unwinding; when an arg is supplied the default never
ran, so `$a` is rebound to itself (a no-op that restores to the same box). NB
this expression-context `local` handling is currently scoped to signature
defaults; a general `local` in arbitrary expression position is still an identity
no-op in PExpr.

Combined, 4.1 + 4.2 took **`signatures.t` 672 → 775** (+103) with zero
regressions; full Pl/t gate green (90 files / 3150 tests).

### 4.3 Declaration / post-inc inside a default returns undef (open)

`sub t125 ($c = (our $k)++) { $c }` → every call yields undef instead of
0,1,2,… The `(our $k)++` default (declare-and-post-increment) does not produce
the pre-increment value. Lower priority; isolated to a few tests; **not yet
fixed**.

---

## 5. Summary table

| kind | scope | CL lowering | mutate via | boxed? |
|------|-------|-------------|-----------|--------|
| `my` (in sub) | lexical | `(let (($x (make-p-box nil)))…)` at first use; `__lex__N` rename if captured | `p-my-=` | yes |
| `my` (file) | ~global | `(p-eval-always (defvar $x …))` | `p-scalar-=` | yes |
| `our` | pkg global (special) | `(p-eval-always (defvar $x …))` | `p-scalar-=` | yes |
| `state` | persistent | unique name + `__init` guard, `let`/`defvar` | `p-my-=`/`p-scalar-=` | yes |
| `local` | dynamic | `(let (($x (p-box-for-local v)))…rest…)` | rebinds special box | yes |
| sig param | lexical (per Perl) | `let*` to `(p-copy-scalar-arg …)` (fresh box) | `p-my-=` (via `_sig_param_lexicals`) | yes ✓ (fixed §4.1/4.2) |

---

## Appendix A — Future plan: unboxing non-reference scalars

> Planned ~2 weeks out: rewrite codegen so that strings, numbers (and possibly
> *all* scalars not used for references) are emitted as **direct CL values**
> instead of `p-box` cells. This appendix asks: *will that work?* Short answer —
> **yes, but only as an analysis-gated transform, not a blanket one.** The
> motivation is real (over-boxing is listed as problem #2 in
> `docs/two-phase-compiler.md`), the infrastructure to decide it already half
> exists (`BlockAnalyzer`'s usage/type pass), and as a bonus it would *fix bug
> 4.1* for free. The caveats below are what make it "gated."

### A.1 What a box buys us today (= what must be preserved)

A `p-box` is a *shared, mutable, identity-bearing* cell. Five distinct features
ride on it; a var can drop its box only if it needs **none** of them:

1. **Reference identity** — `\$x` must yield something that mutates `$x`
   (`$$r = 5` writes through). A direct value has no shared cell to point at.
2. **Aliasing** — `foreach $x (@a)`, `for (substr …)`, `@_` element aliasing
   (already not-supported), `*glob` aliasing. Aliases share one cell.
3. **`local`** save/restore.
4. **`tie` / magic** — tie proxies and the `p-magic-cell` hooks (arylen,
   `\substr`, `\pos`, `\vec`) intercept at the box's get/set.
5. **Holding a reference** — PCL encodes SCALAR-vs-REF partly by box nesting
   (`\$x` = box-in-box) and the session-217 `is-ref` flag. A var that ever holds
   a ref participates in this and must stay boxed.

### A.2 The deciding question: escape / "is this scalar ever special?"

A scalar may become a direct value iff, **over its entire scope (including
nested closures)**, it is *never*:

- address-taken (`\$x`, or passed to something that takes its address/alias),
- aliased (`foreach` loop var over a real array, glob alias, lvalue-substr loop),
- `tie`d,
- `local`ized *as an aggregate element* (`local $h{k}` needs the element cell), and
- ever assigned a value that **could be a reference**.

The first four are decidable by a conservative PPI/OpcodeTree scan — and
`BlockAnalyzer` already does most of this shape of analysis (closure-capture
detection in `_find_closure_captures`, per-var usage roles in `_collect_usages`).
Add an "address-taken / aliased / tied" predicate and you have the gate.

The **fifth is the hard one** and is why "all non-reference variables" can't be
taken literally: you usually *cannot prove statically* that an opaque RHS
(`my $x = some_call();`) never returns a ref. So the sound rule is the
contrapositive — **a scalar is direct-eligible only when its value is provably a
number/string at every assignment** (literal, arithmetic/string/comparison
result, or another direct-eligible var). Anything opaque ⇒ stay boxed (or carry
a runtime-tagged representation; see A.5). The existing `type_hint`
(`fixnum`/`string`/`any`) in `_build_var_map` is exactly this signal, but today
it is a *heuristic hint* keyed on operators used, not a *sound* proof — it would
need tightening (e.g. `any`, `call`, `ref` contexts force a box) before it can
gate correctness rather than just optimisation.

### A.3 Things that look scary but are actually fine

- **Closures.** Counterintuitively, unboxing *simplifies* them. The current code
  boxes captured `my` vars and renames them `$x__lex__N`; the box was a
  workaround for the defvar-makes-it-special hazard
  (`docs/closure-lexical-scoping.md`). A genuine CL lexical `let` binding is
  *already shared* by the closures over it — `(let ((x 0)) (lambda () (incf x)))`
  works natively. So a captured-but-never-referenced counter can be a direct
  value mutated with `setq`, **no box**. The `$x__lex__N` rename (to keep the
  binding lexical, not dynamic) is still needed; the box is not.
- **`local` on globals.** `local $G = v` is just CL dynamic rebinding of a
  special var — `(let (($G v)) …)` saves/restores whether `$G` holds a box or a
  bare number. So `local` does **not** by itself force a box (only `tie`d or
  element-`local`ised vars do). Note: `local` only applies to package
  globals/aggregate elements, never to `my` lexicals, so direct-value lexicals
  never meet `local`.
- **Signature params.** Params are lexicals passed by value (Perl's `@_`-alias
  semantics is already not-supported). They are now mutable boxes (§4.1); under
  the unboxing plan a param that is never referenced/aliased becomes a direct
  `let*`/`let` binding mutated with `setq` instead — the box added for §4.1 drops
  out, same as for any other value-only lexical.

### A.4 Things that stay boxed (the residual)

- Aggregate **elements** (`@a`/`%h` remain vectors/tables of boxes) so
  `\$a[0]`, slices, and element aliasing keep working. Unboxing standalone
  scalars does not touch this.
- Any var that is address-taken, aliased, tied, or holds a ref.
- Special/magic vars (`$1`, `$&`, `$_`, `$.`, …).

So the realistic win is: **standalone scalars proven to hold only
numbers/strings** — loop counters, accumulators, indices, plain locals, and most
signature params. That is the majority of scalars in real code, and the ones
where boxing hurts most (per-iteration allocation, unreadable output).

### A.5 Representation choice & the box/unbox boundary

Two viable shapes:

1. **Static partition (recommended).** Each var is decided box-or-direct at
   declaration time by the analysis. Reads are already representation-agnostic
   (`unbox`/`to-string`/`to-number` accept raw values today). Writes branch:
   `setq`/`setf` for direct, `box-set` for boxed — codegen already distinguishes
   `p-my-=` vs `p-scalar-=`, so this is a third lowering, not a new concept. The
   only new obligation is **boxing at the boundary**: when a direct value flows
   into a slot that needs a box (stored in an array, returned where the caller
   might `\` it), insert an explicit `(make-p-box v)`. Crucially, *the decision
   to box must be made at the declaration*, because you cannot retroactively make
   an already-shared direct value gain identity.

2. **Runtime-tagged scalar (rejected for v1).** Represent a scalar as
   "immediate-or-box" and promote to a box on first `\`. Promotion changes the
   binding's location, so every reader would have to go through an accessor —
   which is just boxing with extra steps. Skip it.

### A.6 Migration risks / sequencing

- **Soundness of the gate is load-bearing.** A var wrongly classified direct
  that later needs identity = a silently-wrong program, not a crash. Start
  *conservative*: only unbox vars the analysis can prove `fixnum`/`string`, never
  captured-and-referenced, never address-taken, never tied/localized — and box
  on any doubt. Widen the gate as confidence grows.
- **Every builtin/operator must accept a direct operand.** Reads are mostly
  ready; audit the write/lvalue paths and the `\`/ref paths for box assumptions.
- **Whole-program, not per-statement.** Address-taken in a *later* statement or a
  *nested closure* must retroactively box the declaration — so the analysis must
  complete before codegen for the whole lexical scope (the two-phase split
  already gives us this ordering).
- **Suggested first slice:** C-style/`foreach`-counter integers and
  provably-`string` accumulators inside subs (never captured by a closure that
  also `\`s them), plus signature params — landing 4.1 as the pilot. Gate behind
  a flag, diff the full sweep, widen.

### A.7 Verdict

Yes — direct (unboxed) scalars are sound and worthwhile **as an
analysis-gated, conservative transform** with a static box/direct partition,
boxing at flow boundaries, and the residual (elements, refs, tied, magic, aliased)
staying boxed. It is *not* sound as a literal "every non-reference variable is
unboxed" rule, because non-ref-ness of an opaque RHS isn't statically decidable —
the gate must be "provably value-only," not "not observed to be a reference."
Done that way it also retires the over-boxing problem, simplifies closures, and
fixes the immutable-parameter bug (4.1) as a side effect.
