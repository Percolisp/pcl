# Method Modifiers — really: the named-sub-in-string-eval closure fix

> **Status:** PLAN (session 253b, 2026-06-14). Method modifiers
> (`before`/`after`/`around`) are the first real consumer, but the fix is
> GENERAL: *a named sub defined inside `eval "STRING"` must (a) be installed in
> the package the eval string selects, and (b) close over the eval's captured
> lexicals and survive past the eval.* Builds on `docs/eval-lexical-capture.md`
> (s250, expression/anon-sub capture) and `docs/eval-string-plan.md`.

## Why this matters

`Class::Method::Modifiers::install_modifier` (used by Moo's `before`/`after`/
`around`) builds the wrapped method as a RUNTIME string and `eval`s it:

```perl
my $before  = $cache->{before};      # lexicals in install_modifier's scope
my $after   = $cache->{after};
my $wrapped = \$cache->{wrapped};
my $generated = "package $into;\n";
$generated .= "sub $name {";
$generated .= ' for my $m (@$before) { $m->(@_) } ' if @$before;   # uses $before
$generated .= ' $$wrapped->(@_); ';                                # uses $wrapped
$generated .= '}';
eval $generated;        # installs $into::$name, a NAMED sub closing over the lexicals
```

In Perl the string eval is compiled in the enclosing lexical pad, so the new
`$into::$name` closes over `$before`/`$after`/`$wrapped` and keeps working after
`install_modifier` returns. PCL gets `$BEFORE is unbound` (or the method lands in
the wrong package). The same pattern appears anywhere a module `eval`s a string
that defines a named sub referencing the surrounding lexicals — so fixing it once
clears a whole class.

## The three concrete bugs (all confirmed by inspection, session 253b)

Repro `/tmp/evnamed.pl`:
```perl
sub make_method {
    my $secret = "SECRET";
    eval "package Target; sub greet { return \$secret; }";
}
make_method();
print Target::greet(), "\n";   # perl: SECRET ; PCL: broken
```

### Bug A — free-var detection skips named-sub bodies
The eval subprocess (`Pl/Parser.pm`, `eval_mode`) records *free* (undeclared)
variables in `_eval_free_vars`, which become the thunk-lambda's parameters
(s250). But it only sees vars used at the eval's top level — a var used **only
inside a named `sub` body** in the eval string (`$secret`, `$wrapped`) is NOT
collected. Transpiling `package Target; sub greet { return $secret }` yields:

```lisp
(pcl:p-eval-thunk (list "$a" "$b")      ; <-- $secret MISSING
 (lambda ($a $b)
   (p-sub pl-greet (&rest %_args) ... (p-return $secret) ...)))
```

`$secret` is neither a lambda param nor a defvar → unbound inside `greet`.

### Bug B — `in-package` inside the eval is ignored (wrong install package)
`p-eval` (`cl/pcl-runtime.lisp` ~6476) reads the WHOLE generated text in one shot:

```lisp
(cl-form (let ((*package* *package*))
           (read-from-string (concatenate 'string "(progn " cl-text ")"))))
(result  (let ((*package* *package*)) (eval cl-form)))
```

`read-from-string` interns **every** symbol under the initial `*package*` (the
caller's, e.g. `main`) BEFORE any `(in-package :Target)` form in the text runs.
So `pl-greet` in the eval becomes `main::pl-greet`, and the `package Target;` in
the string is silently defeated. Confirmed: after the repro,
`main->can('greet')` is true, `Target->can('greet')` is false. `load` does not
have this bug because it reads-then-evals form by form.

### Bug C — (consequence, no separate fix) closure structure
Named subs in the eval already sit *inside* the thunk lambda (they ride the
`definitions` bucket → `@body` → the lambda in `_assemble_output` eval-mode), so
once Bug A supplies the params, the `(p-sub …)` lambda closes over them
correctly. No extra work expected — verify, don't pre-build.

## The fix

### Fix 1 — `p-eval` must read-and-eval form-by-form (honor `in-package`)
Replace the single `read-from-string "(progn …)"` + `eval` with a loop over the
text stream that reads ONE form, evals it, then reads the next — exactly what
`load` does — so an `(in-package :Target)` takes effect before the following
forms are read. Sketch:

```lisp
(with-input-from-string (in cl-text)
  (let ((*package* *package*) (result nil) (eof '#:eof))
    (loop for form = (read in nil eof)
          until (eq form eof)
          do (setf result (eval form)))
    result))
```

Keep the `*package*` rebinding so a stray `in-package` doesn't leak into the
caller's dynamic scope (it's restored on exit). Keep the existing caching of
`cl-text` (the transpile result), the `$@`/exception handling, and
`*pcl-caller-wantarray*` / `*p-eval-lex-alist*` bindings. The only change is the
read/eval strategy. (This also makes multi-package eval strings —
`docs/eval-lexical-capture.md` "Other limits" — work in general.)

### Fix 2 — collect free vars from named-sub bodies (eval_mode only)
In `Pl/Parser.pm`'s eval-mode free-variable collection
(`_insert_variable_forward_declarations` / wherever `_eval_free_vars` is built),
descend into named-`sub` bodies when scanning the eval string for undeclared
variables, so a lexical used only inside `sub greet { … $secret … }` is captured
as a thunk parameter. Care:
- Only in `eval_mode` (normal top-level transpilation must keep treating a
  sub-body var as that sub's own concern).
- A variable that the named sub itself declares (`my $x` inside `greet`) is NOT
  free — don't capture those.
- `@_`, `$_`, `$1`, and the sub's own params stay untouched (same exclusions the
  existing scan already applies).
- The captured var must reach the call-site alist too: `_eval_lexical_alist`
  (ExprToCL) reads the caller's in-scope `_let_bound_vars`; `$secret`/`$wrapped`
  ARE let-bound in the caller (`install_modifier`), so the alist side already
  has them — the gap is purely the subprocess's free-var list. Verify the
  names match (sigil + closure-rename stripping, as s250 already does).

### Order of work
Fix 1 first (smaller, self-contained, independently testable with the
non-capturing `eval "package X; sub f {...}"` case). Then Fix 2 (capture). Then
the modifier end-to-end.

## STATUS (session 253b) — before/after DONE, around BLOCKED on a parser bug

**Done & gated (commit 4870b7f + 699baff):** eval named-sub free-var capture
(AST-level, `_eval_free_vars_from_ppi`) + interpolated-eval alist + form-by-form
`p-eval`.  `before`/`after` modifiers work end-to-end vs perl 5.40 (`/tmp/mod.pl`
→ `before: 0 / after: 1 / final: 1`).

**`around` BLOCKED by a SEPARATE, general parser precedence bug — NOT eval:**
`$$ref->()` (deref a scalar-ref-to-coderef, then call) is mis-associated.
- Perl: `$$r->()` == `(${$r})->()` — deref first, then call.
- PCL emits `(p-cast-$ (p-funcall-ref $r))` == `${ $r->() }` — call first, then
  deref. The AST is built as `cast-$(funcall($r))` instead of
  `funcall(cast-$($r))`.
- Root: in `Pl/PExpr.pm` the leading scalar Cast (`$` of `$$r`, a
  `PPI::Token::Cast` + `Symbol`) consumes `$r->()` (symbol + its postfix arrow)
  as its operand, so the cast ends up OUTSIDE the `->()`. The fix is in the
  arrow/cast precedence loop (~lines 762–916), which is dense (KV slices, method
  calls, deref subscripts) → do it in a focused session with the full gate.
- `before`/`after` only limp past this because `p-funcall-ref` double-unboxes a
  ref-to-coderef (`cl/pcl-runtime.lisp` ~8703), so `(p-funcall-ref $wrapped)`
  accidentally derefs+calls. `around` adds a second eval'd wrapper layer where
  the accident no longer covers it.
- Isolated repros (no Moo): `/tmp/derefcall.pl` (`my $cv=sub{...}; my $r=\$cv;
  print $$r->()` → perl `ORIG`, PCL dies "Undefined subroutine &main::"),
  `/tmp/wrapmech2.pl`, `/tmp/around1.pl`.
- Acceptance when fixed: `$$r->()` outside eval returns ORIG; `/tmp/around1.pl`
  → 50; stacked `around` (`/tmp/around.pl`) → 60.

## Risks

- **R1 — `p-eval` form-by-form changes return value / side-effect timing.** The
  result must remain the LAST form's value (Perl eval returns the last
  statement). The loop above preserves that. Run `Pl/t/eval-*.t` +
  `eval-capture-01.t` (30 cases) to confirm no regression.
- **R2 — over-capturing free vars.** Descending into sub bodies might capture a
  var that should resolve to a package global, changing behavior. The s250
  `p-eval-lex-lookup` fallback (caller lexical → package global → fresh undef)
  already makes a non-captured name resolve sanely; capturing one that has a
  caller lexical is the *correct* Perl behavior. Differential tests guard this.
- **R3 — coderef identity / `MODIFIER_CACHE`.** `around` stores
  `\$cache->{wrapped}` and reassigns it; the eval'd wrapper derefs `$$wrapped`
  each call. Needs the captured container to be the SAME box the caller mutates
  (s250 passes the box, not the value — so this should hold). Verify `around`
  re-wrapping (two `around`s stack) after the basic case works.
- **R4 — performance.** Form-by-form eval is marginally slower than one
  `read-from-string`; negligible (eval strings are short, and transpile +
  subprocess already dominate).

## Test plan (differential vs perl)

New `Pl/t/eval-named-sub-01.t` (or extend `eval-capture-01.t`):
1. `eval "package X; sub f { 42 }"` then `X::f()` → installs in X (Bug B).
2. `make_method()` repro → `Target::greet()` returns the captured lexical (A+B).
3. Moo `before`/`after` on a method: prints in order, `$self` flows, return value
   preserved.
4. Moo `around`: wraps, can call/skip `$orig`, modify args + return.
5. Two stacked `around`s (re-wrapping via `\$cache->{wrapped}`).
6. `before`+`after`+`around` together on one method.
7. A modifier defined in a ROLE applied via `with` (composition + modifiers).
8. Regression: full `eval-capture-01.t` + `eval-*.t` still green; gate green.

Acceptance: `/tmp/mod.pl` prints `before: 0`, `after: 1`, `final: 1`; gate green;
sweep shows no new failures.

## One-paragraph summary for future-me

Method modifiers fail because Class::Method::Modifiers `eval`s a string that
defines a NAMED sub closing over the enclosing lexicals. Two real PCL bugs: (A)
the eval's free-var scan ignores named-sub bodies, so the captured lexicals never
become thunk-lambda params; (B) `p-eval` reads the whole eval text under one
`*package*`, so `package X;` inside the eval doesn't route the sub (it lands in
`main`). Fix B = read/eval form-by-form like `load`; fix A = descend into
named-sub bodies when collecting eval free vars. The closure structure is already
correct once the params exist. This is general (any eval-defined named sub), not
Moo-specific.
