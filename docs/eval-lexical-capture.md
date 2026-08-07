# String-eval lexical capture

**Status:** implemented (session 250, 2026-06-13). Replaces the limitation
formerly described in `docs/not-supported.md` → "Context propagation into
string eval" (the lexical half of it).

> **v2 extension (s295):** the v2 pipeline *renames* some file-scope
> lexicals (`$x__file__N` cells), which this design's by-name alist and
> global fall-through cannot see. The extension — ordered shadow pairs in
> the alist, cross-package span pairs, and the **alias rule**
> (`p-alias-eval-cell` writing the cell into the original-name global at
> the declaration's run position) — is specified normatively in
> `docs/ir-spec.md` §9.1. This file remains the design rationale for the
> underlying capture mechanism (the thunk protocol, the lookup, `our`, and
> the edge cases below), which is unchanged.

## The problem

Perl's `eval "CODE"` can see the enclosing sub's `my` lexicals:

```perl
sub make { my $captured = "SECRET"; return eval 'sub { "got: $captured" }'; }
make()->();   # Perl: "got: SECRET"
```

PCL transpiles `eval "..."` by sending the string to a `pl2cl --server`
subprocess and `eval`-ing the returned CL in the caller's package. Two facts
made the lexical invisible:

1. **CL `eval` runs in the null lexical environment** — by spec it can only see
   *special* (dynamic) variables and globals, never the caller's lexical
   bindings. PCL compiles a sub's `my $x` to a CL `let` binding (lexical), so it
   is unreachable from `eval`. (Top-level `my` compiles to a `defvar`/special,
   which *is* reachable — that case always worked.)
2. **The subprocess has no knowledge of the caller's frame.** Seeing `$captured`
   used but never declared, it emitted `(defvar $captured (make-p-box nil))` — a
   fresh empty box in the eval package, disconnected from the caller's box. That
   is why the eval read `undef`.

## The fix — free vars become lambda parameters

A free variable inside an eval has no lexical binding *inside* the eval, but it
*does* have one in the caller. So we turn each free variable into a parameter of
a lambda that wraps the eval body, and bind those parameters to the caller's
live containers. Because a lambda creates a genuine **lexical** binding, any
closure built inside the eval body captures it correctly (the whole point — the
Sub::Defer/Moo idiom above).

Three cooperating pieces:

### 1. Subprocess side — `Pl/Parser.pm`, `eval_mode`

`pl2cl --eval-pkg` / `--server` set `eval_mode => 1`. In this mode:

- `_insert_variable_forward_declarations` does **not** emit `defvar`s for the
  undeclared (free) variables — a `defvar` would proclaim the symbol *globally
  special*, which would turn the wrapping lambda's parameter into a *dynamic*
  binding and defeat lexical capture. Instead it records them in
  `_eval_free_vars`.
- `_assemble_output` wraps the body (definitions + runtime) in

  ```lisp
  (pcl:p-eval-thunk (list "$captured" "@items")
   (lambda ($captured @items)
     ...body...))
  ```

  The **free-variable list is the lambda's parameter list.** Keeps preamble and
  `$a`/`$b`/cross-package decls outside the lambda.

### 2. Call site — `Pl/ExprToCL.pm`, `_eval_lexical_alist`

`eval STRING` (the non-block form) now generates

```lisp
(p-eval "<string>" (list (cons "$captured" $captured) (cons "@items" @items)))
```

The in-scope lexicals are read from the **parser's `_let_bound_vars`** (the
rolling set of `my`/let-bound names, saved/restored around every closure),
reached via `$self->expr_o->parser`. *Not* `Environment->scope_stack`, whose
`declared_vars` is not populated for `my` (an early wrong turn — see history).
Top-level eval has no let-bound vars, so the alist is empty and codegen emits a
plain `(p-eval STRING)`.

### 3. Runtime — `cl/pcl-runtime.lisp`

- `*p-eval-lex-alist*` — dynamic var holding the caller's `(name . container)`
  alist; `p-eval` binds it.
- `p-eval-thunk (free-names fn &optional region-pkg)` —
  `(apply fn (mapcar #'p-eval-lex-lookup free-names))`, with `*package*` bound
  to REGION-PKG for the extent when it is given.  The emitter passes it (as
  `_cl_pkg_designator`'s spelling) for an eval whose whole body is one
  `package X; …` region — #226's collapse — and only then; the thunk is
  emitted in that case even with an empty free-name list, because the binding
  matters most to the BODY.  Perl says the current package inside the region
  IS X, and every unqualified-name resolution in the runtime (stop 2/3 below,
  `%p-symref-box` and siblings, `p-use`'s import target, `p-bless`'s empty
  class, the symbolic funcall/coderef resolvers) reads `*package*` to answer
  it.  Without the binding all of them answered "the caller" — #240, a silent
  wrong in three spellings (bare write, bare read satisfied by the caller's
  global, `our` declared-then-read-back), closed in s351.
- `p-eval-lex-lookup (name)` resolves each free var to:
  1. the caller's lexical, if present in `*p-eval-lex-alist*`;
  2. else the real package global (when the interned symbol is `boundp`) — so
     `our`/top-level vars still read and write correctly;
  3. else a fresh undef container (Perl auto-vivifies the global as undef),
     sigil-correct (`@`→array, `%`→hash, else box).

Because the caller passes the **box** (not the value), writes inside the eval
propagate back to the caller's lexical — matching Perl's `eval '$x = 5'`.

## What works

Verified differentially against real `perl` (`Pl/t/eval-capture-01.t`, 30
cases):

- Read an enclosing sub lexical: `eval '$captured'`.
- Write back to one: `eval '$x = 99'` mutates the caller's `$x` (shared box).
- A closure built inside the eval captures the enclosing lexical (Sub::Defer).
- Lexical array/hash, full and element: `eval '@a'`, `eval '$items[1]'`,
  `eval '$a[0] = 99'`, `eval 'keys %h'`.
- **`foreach` loop variable**: `for my $x (...) { eval '$x*10' }` — the loop var
  is added to `_let_bound_vars` for the body (it lives in `_lexical_foreach_vars`
  otherwise) so it is captured.
- **Closure-renamed lexicals** (`$x__lex__N`): the alist key is stripped back to
  the original Perl name, so a var that is *also* visibly closure-captured is
  still captured by the eval.
- **`$a`/`$b`**: kept `defvar`'d (special, so `sort` comparators inside the eval
  work) AND listed as lambda params when referenced, so a caller's lexical
  `my $a`/`my $b` *is* captured. Being special, the param is a dynamic rebinding:
  a bare `$a` sees the caller's box; `sort { $a <=> $b }` still rebinds it.
- `local`-bound and `our`/package globals visible via the dynamic fallback;
  magic vars (`$_`, `@_`, `$1`) untouched; recursion; `return` inside eval;
  `$@`/die; caching; the persistent server.

## Deliberate divergences from Perl

Probed and documented (not asserted as correct in the test):

1. **`my $a` masking a `sort` block inside the same eval** — Perl lets the
   lexical shadow the sort variable, producing a *broken* order; PCL sorts
   correctly (PCL's `sort` always uses the special `$a`/`$b`). Replicating Perl's
   breakage isn't worthwhile.
2. **Nested string eval** (`eval 'eval "$x"'`) — the inner eval cannot capture
   the outer eval's free variable (the outer's lambda params aren't tracked as
   in-scope lexicals while the inner eval is being generated). Rare.
3. **eval inside a returned closure referencing a var ONLY through the eval
   string** — Perl's closure optimizer never closes over a lexical that is
   mentioned solely inside a string, so the eval sees the package global (undef);
   PCL captures it. PCL is the *more permissive* direction (more programs work).

## Other limits

- **Multi-package eval strings** (`eval 'package Foo; ...'`) wrap all sections in
  one lambda; symbol resolution uses the single read-time `*package*`. Fine for
  single-package evals (the norm).
- `wantarray()` context inside eval is handled separately via
  `*pcl-caller-wantarray*` (unrelated to capture).

## Pre-existing bugs surfaced (NOT caused by this feature, NOT yet fixed)

The differential battery flagged two failures that also fail on the pre-feature
HEAD, so they are out of scope here but worth a separate fix:

- `sub f { ...; sort cmp LIST }` where the named comparator `cmp` is defined
  *after* use, *inside* a sub → returns the list unsorted. A sort/named-comparator
  resolution bug, unrelated to `eval`.
- `our $x` declared *inside a sub* + `local $x` + `eval '$x'` → the eval reads the
  global, not the `local`-bound value. (The *top-level* `our` form works.)
