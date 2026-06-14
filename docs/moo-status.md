# Moo support status (PCL)

**As of session 253c (2026-06-15).** Moo is broadly working — essentially all of
everyday Moo runs, verified differentially against perl 5.40. This file is the
quick status; mechanism details live in the session log and
`memory/project_moo_progress.md`.

## Verified working (vs perl 5.40)

- **Construction:** `new`; `has` with `is => 'ro' | 'rw'`.
- **Defaults:** scalar, `default => sub {...}` (coderef), ref defaults
  (`sub {[]}` / `sub {{}}`).
- **Attribute options:** `required`, `lazy` + `builder` / `_build_*`,
  `predicate`, `clearer`, `builder`, `trigger`, `isa` (coderef constraint).
- **Lifecycle:** `BUILD`.
- **Inheritance:** `extends`, multi-level; `isa()`.
- **Roles:** `with`, `does`, `requires`, multi-method composition,
  **role attributes** (a role's own `has`).
- **Method modifiers:** `before`, `after`, `around` — single **and stacked**.
- **Delegation:** `handles => { method => 'target' }` to an **object**-valued
  attribute.

## What got it there (the hard fixes)

- **Source-order compile-time stream** (s253b): named subs no longer hoisted
  above `use`/`BEGIN`, so `use Moo::Role`'s `make_role` sees the right subs.
  Unblocked roles. See `docs/declaration-ordering-fix-plan.md`.
- **eval named-sub free-var capture** (s253b): a named sub defined inside
  `eval "STRING"` closes over the enclosing lexicals + installs in the eval's
  package. Unblocked `before`/`after`. See `docs/eval-free-vars-plan.md`,
  `docs/method-modifiers-plan.md`.
- **`around` quartet** (s253c): `$$ref->()` precedence; `return` inside string
  eval; assignment-to-a-non-lvalue-sub as a (propagating) transpile error so
  `Class::Method::Modifiers::_sub_attrs`' compile-error feature-probe works; and
  live `\$ref->{k}` / `\$ref->[i]` refs so stacked `around` re-wrapping is seen.

## Remaining (most are NOT Moo-specific)

1. **Identifier collisions with CL builtins** — `package Car` → CLOS `car`,
   `has log` → `log`, also `list`, etc. → `SYMBOL-PACKAGE-LOCKED-ERROR`. A
   *general* name-mangling gap (distinct from the s252 case-collision work), and
   the **highest-impact** remaining item for real modules: a class named `Car`
   should not explode. **Next target.**
2. **Native array/hash delegation** (`handles => { push_x => 'push' }` on an
   arrayref) needs `Sub::HandlesVia`; **plain Moo + perl also die** on it, so it
   is not a PCL gap.
3. **Module compile-load double-execution** — worked around with
   `*pcl-cache-fasl* nil` (FASL caching off). Correctness is fine; this is a
   **performance** item. See `memory/project_module_compile_load_double_exec.md`
   / `docs/module-double-exec-bug.md`.
4. **`DESTROY` / `DEMOLISH` via GC** — permanent not-supported
   (`docs/not-supported.md`).
5. **`Type::Tiny` / coercions, `BUILDARGS` edge cases** — untested; the likely
   next real-module frontier.

## Bottom line

Moo itself is in good shape. The thing most likely to bite a real module now is
the **builtin-name collision** (#1), not a Moo feature — fixing it helps far
beyond Moo.
