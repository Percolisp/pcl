# Eval free-variable detection — do it at the AST level (via BlockAnalyzer)

> **Status:** PLAN (session 253b, 2026-06-14). Supersedes the regex approach
> attempted-and-reverted this session. Companion to
> `docs/method-modifiers-plan.md` (the consumer) and
> `docs/eval-lexical-capture.md` (s250, the runtime mechanism that consumes the
> free-var list).

## The problem with the current detection

`eval "STRING"` capture (s250) needs to know which variables in the eval body are
**free** — referenced but not declared inside the eval — so they can become the
`p-eval-thunk` lambda's parameters and be bound to the caller's containers.

Today that set is computed in `Pl/Parser.pm::_insert_variable_forward_declarations`
by **regex-scanning the generated CL text** (`%referenced` / `%declared` over
emitted lines, gated on `eval_mode`). This is:

- **Scope-blind.** It uses flat per-name hashes, so it cannot tell that `$z` is
  bound in sub A but free in sub B. Any widening (e.g. "bound anywhere ⇒ not free
  anywhere") mis-handles shadowing: `eval "sub a { my $z } sub b { $z }"` would
  drop `$z` from `b`'s captures.
- **Post-codegen.** It reads emitted Lisp, so it's coupled to codegen spelling and
  breaks silently when codegen changes — the exact "string-match the generated
  code" anti-pattern PCL forbids elsewhere.
- **Blind to named-sub bodies.** References inside a named `sub { … }` are at
  `sub_depth > 0` and never collected, so the modifier idiom
  (`eval "sub $name { … \$wrapped … }"`) never captures `$wrapped`. This is the
  immediate method-modifiers blocker.

## What already exists (use it)

`Pl/BlockAnalyzer.pm` (`docs/two-phase-compiler.md`) does **AST-level** scope
analysis:

- `analyze($block, $outer, $pexpr_factory)` returns `{ declarations, vars,
  outer_refs }`.
- `_collect_usages` walks the **OpcodeTree** (`_walk_tree`), not text, recording
  every symbol use with role/context/stmt-index.
- `_collect_declarations` records `my`/`our`/`state`/`local` bindings (PPI-level).
- `_find_outer_refs` = `usages − in_block` = **exactly the free-variable set**.
- `vars{$x}{scope}` is `'local'` (declared in block) or `'outer'` (free).

It's already wired through `_emit_scoped_block` for per-block `my` hoisting, with a
`$pexpr_factory` available in Parser. So the machinery and the call pattern exist;
the eval path just doesn't use them.

### The one real gap: named subs

`_collect_declarations` / `_find_closure_captures` **intentionally do not recurse
into named subs** (BlockAnalyzer.pm line ~71) — correct for their current job
(per-block `my` hoisting), wrong for eval capture, where a named sub defined in
the eval that references an enclosing lexical *is* capturing it (Perl compiles the
whole string in one pad).

## The plan

### Step 1 — give BlockAnalyzer an eval/whole-string mode
Add an option (e.g. `analyze($block, $outer, $factory, { descend_named_subs => 1 })`)
that, when set:
- **descends into named-sub bodies** during usage collection, AND
- treats a named sub's own `my`/params as bindings *local to that sub* (so a var
  bound inside the sub is NOT free), i.e. proper nested-scope subtraction rather
  than a flat "bound anywhere" rule.

Implementation note: this is the same shape as `_find_closure_captures` (which
already descends into *anonymous* subs and intersects with `in_block`); generalize
it to (a) also visit named subs and (b) compute free = used − (block decls ∪
this-sub decls ∪ params) per scope, unioned upward. Keep the default (no descend)
untouched so the existing `_emit_scoped_block` behaviour and `block-analyzer-01.t`
are unaffected.

### Step 2 — eval_mode uses analyze(), not regex
In `eval_mode`, build `_eval_free_vars` from
`analyze($eval_top_block, …, { descend_named_subs => 1 })->{outer_refs}` instead
of the regex scan. Exclusions stay the same and become *principled* (per-scope):
runtime vars (`@_`, `$_`, `$1`, `@ARGV`…), `state__*`, pure-`foreach` lexicals,
and the `$a`/`$b` special-case (keep their `defvar`, still list as params when
referenced — see `eval-lexical-capture.md`). The non-eval path keeps its current
forward-declaration logic untouched.

Where to hook: the eval body is parsed by the same `parse_code(..., eval_mode=>1)`
entry; run the analyzer on the document's top-level statements there (the factory
that builds a `Pl::PExpr` per statement already exists for `_collect_usages`).

### Step 3 — pair with the runtime `in-package` fix (already done)
`p-eval` now reads/evals form-by-form so `package X;` inside the eval routes a
named sub to X (committed separately as Fix 1). With Steps 1–2 supplying the
captured lexicals as lambda params, the named sub installs in X *and* closes over
them. That is the whole method-modifiers chain.

### Step 4 — retire the regex
Delete the `eval_mode` branch's reliance on `%referenced`/`%declared` text scan
once `analyze()` drives it. (The non-eval forward-declaration scan can stay; it's
a different job — declaring package globals — though it is itself a candidate for
the same AST treatment later. Out of scope here.)

## Will there be more bugs here? (the user's question)

Yes, with the regex approach — it cannot be made scope-correct. With BlockAnalyzer
driving it, the remaining risks are bounded and testable:

- **Nested closures / shadowing** — handled by per-scope subtraction (the thing
  the regex can't do). Test with `sub a { my $z } sub b { $z }`.
- **`our`/`local`/`state` inside the eval** — `_collect_declarations` already
  classifies these; map them as bindings (not free). `our $g` referenced should
  resolve to the global via `p-eval-lex-lookup`'s fallback, not be captured.
- **Nested string eval** — still a documented limitation (eval-lexical-capture.md);
  unchanged.
- **Performance** — `analyze` runs once per eval-string transpile (already cached
  by `*p-eval-string-cache*`); negligible.

## Test plan

`Pl/t/eval-free-vars-01.t` (differential vs perl) — analysis correctness via
observable behaviour:
1. Named sub in eval captures an enclosing lexical:
   `sub make { my $s="X"; eval 'sub g { $s }' } make(); g()` → `X`.
2. `package` + named sub: `eval "package P; sub g { 42 }"; P::g()` → 42
   (Fix 1; already passing — keep as a regression).
3. Shadowing: `eval "sub a { my $z=1 } sub b { $z }"` with an enclosing
   `my $z=9` → `b()` sees the enclosing `$z` (9), `a`'s `my` doesn't leak.
4. A var free in one eval sub, bound in another — captured only where free.
5. `our $g` referenced inside an eval sub resolves to the package global, not a
   stale capture.
6. Method modifiers end-to-end: `before`/`after`/`around`, stacked `around`,
   modifier from a role (the `method-modifiers-plan.md` acceptance set).
7. Regression: full `eval-capture-01.t` (30), `eval-*.t`, `block-analyzer-01.t`,
   and the gate stay green.

## Summary

The eval free-variable set should come from `BlockAnalyzer`'s AST-level
`outer_refs`, not a regex over generated Lisp. The only new analyzer capability
needed is descending into *named* subs (with proper per-scope binding subtraction)
for the eval/whole-string case. Combined with the already-landed form-by-form
`p-eval` (in-package) fix, this makes `eval`-defined named subs install in the
right package and close over the captured lexicals — which is what method
modifiers need, generally.
