---
name: project_string_eval_lexical_capture
description: "String-eval lexical capture IMPLEMENTED (s250): free vars→lambda params via p-eval-thunk + caller passes in-scope lexicals alist. Read/write/closure/array/hash/foreach/$a$b all capture. 3 documented divergences + 2 pre-existing bugs surfaced."
metadata: 
  node_type: memory
  type: project
  originSessionId: 7af6ebee-0865-4499-b0c0-edbf2a4bd7ce
---

# String-eval lexical capture — IMPLEMENTED (session 250, 2026-06-13)

Was the deferred "option B". Now done. Full design: `docs/eval-lexical-capture.md`.
Replaces the lexical half of not-supported.md "Context propagation into string eval".

## Mechanism (3 cooperating layers)
1. **Subprocess** (`pl2cl --eval-pkg`/`--server`, `eval_mode=1` threaded via
   `parse_code`): the eval body's free vars (used-but-not-declared = the old
   `@undeclared` set) become the params of a wrapping lambda; their
   forward-decl defvars are SUPPRESSED (a defvar would proclaim them special →
   lambda param becomes dynamic → kills closure capture). Emits
   `(pcl:p-eval-thunk (list "$x" …) (lambda ($x …) …body…))` in
   `Parser.pm` `_assemble_output` (only when `eval_mode`).
2. **Call site** (`ExprToCL._eval_lexical_alist`): `eval STRING` →
   `(p-eval STR (list (cons "$x" $x) …))`. In-scope lexicals come from the
   PARSER's `_let_bound_vars` (NOT `Environment->scope_stack`, whose
   declared_vars is empty for `my`!), reached via `$self->expr_o->parser`.
   Alist KEY = original name (strip `__lex__N`); VALUE = live CL symbol.
3. **Runtime** (`cl/pcl-runtime.lisp`): `*p-eval-lex-alist*` (bound by p-eval's
   new optional 2nd arg), `p-eval-thunk (free-names fn)` =
   `(apply fn (mapcar #'p-eval-lex-lookup free-names))`. `p-eval-lex-lookup`:
   alist hit → caller's container; else boundp global → symbol-value; else
   fresh sigil-correct container. (boxes passed ⇒ writes propagate back.)

## $a/$b (the subtle one — user flagged)
Force-declared special (sort comparators need them dynamic). In eval_mode KEEP
the defvar AND list referenced `$a`/`$b` as lambda params (`%forced_sort_var`
flag in `_insert_variable_forward_declarations`). Special+param = DYNAMIC
rebinding: bare `$a` sees caller's box; `sort{$a<=>$b}` still rebinds. Fixes
`my $a=5; eval '$a+1'` → 6.

## Verified (differential vs real perl, `Pl/t/eval-capture-01.t`, 30 cases, ONE sbcl run)
read / write-back / closure-in-eval (Sub::Defer idiom) / array+hash full+elem+set /
foreach loop var / closure-renamed lexical / $a$b ordinary+sort / local / our /
magic ($_ @_ $1) / recursion / return-in-eval / nested-block / many-vars /
list-return / mixed-global. Also +4 capture tests in eval-01.t (now 44).

## Deliberate divergences (documented, NOT asserted)
1. `my $a` masking sort INSIDE same eval — perl gives garbage order, PCL sorts right.
2. nested eval `eval 'eval "$x"'` — inner can't capture outer's free var.
3. eval in returned closure ref'ing var ONLY via string — perl closure-opt skips
   it (undef), PCL captures (PCL more permissive = safe direction).

## Pre-existing bugs SURFACED by the battery (fail on clean HEAD too — separate follow-up)
- `sub f { sort cmp LIST }` named comparator defined AFTER use, inside a sub →
  UNSORTED. Not eval-related.
- `our $x` declared INSIDE a sub + `local $x` + `eval '$x'` → eval reads global
  not local-bound value. (top-level `our` form WORKS.)

## Method that worked
Differential fuzzing: write snippet to file, run `perl file` vs `./runpcl file`,
diff. NEVER inline into `perl -e '...'` (shell mangles `$`). Two batteries
(/tmp/eval_edge_cases.txt, eval_edge2.txt) found 5 bugs total; fixed 3 (the
feature's), 2 are pre-existing. Sidestep via `lib/Sub/Util.pm` (s249) still in
place but now redundant for the eval path. See [[project_moo_progress]].
