# PCL Generated CL: Elegance Review

## Context

Session 111 fixed foreach loop variable closure capture (`for my $n (LIST)`) by adding
a `$n__lex__N` renaming step in `_process_foreach_loop`. This prompted a review question:
is the `__lex__` approach the most elegant solution, and are there other places in the
generated CL that deserve a similar look?

---

## The foreach loop var problem

### Why closures were broken

PCL forward-declares all package-level variables with `defvar`:

```lisp
(defvar $n (make-p-box nil))
```

Once a symbol is `defvar`'d in CL, **all** `let` bindings of that symbol are dynamic
(special), not lexical — forever, for the life of the image. The `p-foreach` macro
does `(let ((,var (ensure-boxed (aref ,vec ,i)))) body)`, which creates a *dynamic*
binding because `$n` is special. A lambda inside the body that references `$n` captures
the symbol name, not the per-iteration value. After the loop, the dynamic binding is
gone → lambda returns nil.

### Current fix: `__lex__` renaming (session 111)

`_process_foreach_loop` detects that `$n` is captured by an inner `sub { }` and emits:

```lisp
(p-foreach ($n (p-.. "A" "E"))
  (let (($n__lex__2 (make-p-box (unbox $n))))   ; fresh box, never defvar'd → lexical
    ...body with all $n → $n__lex__2...))
```

`$n__lex__2` is never `defvar`'d, so the `let` is genuinely lexical. Per-iteration
box → closure captures correctly.

**Downside**: verbose generated code; N-renamed symbols clutter output.

### Better option A: don't defvar the foreach loop variable

`for my $n (...)` declares `$n` as a Perl lexical scoped to the loop — it should
never be a package global. If we exclude such variables from `_insert_variable_forward_declarations`,
`p-foreach`'s existing `(let ((,var ...)))` becomes naturally lexical with no renaming:

```lisp
;; No (defvar $n ...) in preamble
(p-foreach ($n (p-.. "A" "E"))
  (p-setf (p-gethash %foo $n)           ; clean: $n, not $n__lex__2
          (lambda (&rest %_args)
            ... $n ...)))               ; captures $n lexically per iteration
```

**How to implement**: in `_insert_variable_forward_declarations`, skip `defvar` for
variables that:
- appear **only** as a foreach loop variable in the AST (never as standalone `my $var`
  or bare package var), AND
- are captured by a closure inside that loop.

The `p-foreach` scanner at line 387 already finds foreach-bound vars to put in
`%let_bound`. We could instead put them in a `%foreach_only` set and exclude them
from `defvar` entirely when they are closure-captured.

**Risk**: if `$n` also appears outside loops (e.g. reused as a package var elsewhere in
the file), skipping `defvar` would cause an "unbound variable" error on first use. The
analysis needs to be precise.

### Better option B: a `p-foreach-lex` variant macro

A new macro `p-foreach-lex` that uses a gensym for the actual binding and passes the
value via a lexical helper, hiding the detail:

```lisp
(defmacro p-foreach-lex ((var list) &rest body)
  ;; Like p-foreach but creates a fresh lexical box per iteration.
  ;; Use when 'var' is a special variable but closures in body need per-iteration capture.
  (let ((iter (gensym "LEX")) ...)
    `(...
      (let ((,iter (ensure-boxed (aref ,vec ,i))))
        ;; iter is never special → lexical → closures capture correctly
        (symbol-macrolet ((,var ,iter))
          ,@body)))))
```

`symbol-macrolet` makes `$n` in the body expand to the gensym at compile time.
The lambda then closes over the gensym lexically.

**Problem with this approach**: `p-scalar-=` has `(unless (boundp ',place) (proclaim '(special ,place)) ...)`.
When `symbol-macrolet` expands `$n` to `#:lex1234` in `(p-scalar-= $n val)`, the
resulting `(proclaim '(special #:lex1234))` would convert the gensym to special,
breaking lexical capture for any iteration where `$n` is assigned.
Fixing this requires `p-scalar-=` to detect it's in a `symbol-macrolet` context —
not practical at macro-expansion time.

This approach only works safely for **read-only** loop variables.

### Recommendation

Option A (don't defvar) is the cleanest. It requires:
1. In `_insert_variable_forward_declarations`, detect vars that are **only** `p-foreach`-bound
   (not referenced outside a foreach loop) and are closure-captured → skip their `defvar`.
2. Revert the `_process_foreach_loop` `__lex__` renaming (it would no longer be needed).

---

## Other places to review for elegance

These areas in the generated CL may have similar readability or correctness issues worth
auditing in a review pass:

### 1. `$state__` variables (state.pm)

State vars use a file-scope `let` binding (not `defvar`) for persistence. The naming
convention `$state__foo__1` is functional but verbose. Could be `$foo--state` or similar.

### 2. `p-my-=` vs `p-scalar-=` split

The `_let_bound_vars` tracking that switches between `p-my-=` and `p-scalar-=` is
implicit and easy to get wrong. Any missed entry in `_let_bound_vars` silently causes
`(proclaim '(special ...))` on a variable that should stay lexical.

A cleaner model: always use `p-my-=` (box-set) for variables that are `let`-bound
anywhere in the enclosing scope, and only use `p-scalar-=` for true package globals.
This requires the code generator to know at the point of an assignment whether the
variable is lexical or global — which the current Parser.pm tracks but incompletely.

### 3. Forward declarations section bloat

The `defvar` forward declaration section grows large for files with many variables.
Many of those variables ARE declared with `my` inside subs but are also referenced
at file scope. The `defvar` is correct but the output looks messy. A comment grouping
them (e.g. `;;; Package globals:`) would help readability.

### 4. Anonymous sub wrappers

Anonymous subs generate:
```lisp
(lambda (&rest %_args)
  (let ((@_ (p-flatten-args %_args)))
    (catch :p-return
      (block nil
        body))))
```

The `%_args` → `@_` wrapping is needed for `&rest` semantics, but the `catch :p-return`
+ `block nil` double wrapper for every anon sub is verbose. Review whether the `block nil`
is always necessary (it's for unlabeled `last`; most lambdas don't use it).

### 5. `p-setf` macro complexity

`p-setf` has ~15 cases handling different combinations of `p-aref`, `p-gethash`,
`p-aref-deref`, `p-gethash-deref` for auto-vivification. Some combinations are missing
(e.g. `p-aref/p-aref` — the "Case 0 PExpr preprocessing removes `->`" bug). A table-driven
or recursive approach might be cleaner and more complete.

---

## Next session action items

1. **Implement Option A** for foreach loop variables: modify `_insert_variable_forward_declarations`
   to track "pure foreach-only" vars, skip their `defvar`, revert the `__lex__` renaming logic
   from `_process_foreach_loop`. Verify closure-01.t tests 14-15 still pass.

2. **Audit anonymous sub wrappers**: check whether `block nil` is genuinely needed in all
   lambda bodies, or only when the body contains `last`/`next` without a label. Could
   emit it conditionally.

3. **Document the `p-scalar-=` / `_let_bound_vars` hazard** in a comment in Parser.pm
   so future changes don't accidentally omit the `_let_bound_vars` update.
