# V2 Completion Plan — from 32-file prototype to default pipeline

**Written:** 2026-07-04 (session 271b), for implementation by Claude Opus 4.8.
**Prereq reading, in order:** `docs/parser2-prototype.md` (architecture +
session history), `CLAUDE.md` (§10 paren discipline, §11 reuse-don't-duplicate),
`docs/test-debugging-runbook.md`, `docs/eval-lexical-capture.md`.
**Baseline commits:** `899c3ba` (A3), `0a645e0` (A2).

This document is prescriptive. Where it says "die → v1", that is a
`die "Parser2 TODO: …\n"` which pl2cl's `parse_with_fallback` catches and
retries through v1 — the *sanctioned* way to not support something. Where it
says "verify parity", that is the full Working Loop in §2, not a spot check.

---

## 1. Where things stand

- `PCL_V2=1` selects `Pl::Parser2` in `pl2cl` (see `parse_with_fallback`
  there; eval-mode and any special opts always route to v1; `--lenient-ppi`
  is deliberately ignored for the v2 attempt).
- **32 of 111 `perl-tests/*.t` lower fully through v2** at exact v1 sweep
  parity: 1175 pass / 9 fail / 29 fully-passing — identical on both
  pipelines. The 9 fails are pre-existing v1 bugs (flip.t 3, delete.t 3,
  grent.t 1 env-dependent, …), NOT v2 work.
- Guard file: `Pl/t/parser2-01.t` (90 tests). Every work item below must
  extend it.
- The v2 modules: `Pl/Parser2.pm` (statements/scopes/sections),
  `Pl/ExprToCL2.pm` (native expression subset), `Pl/VarAnnotator.pm`
  (unbox gate), `Pl/CLForm.pm` (the only printer).
- Remaining first-gate census (each file shows only its FIRST gate):

  | files | gate | work item |
  |---:|---|---|
  | 65 | string eval (`eval EXPR` text scan) | W2 + W3 |
  | 4 | package block form | W1 |
  | 2 | `package` inside a block | W1 (assess) |
  | 2 | sub with prototype/signature | W4 |
  | 4 | lexical captured by a named sub | W5 |
  | 1 | loop with continue block | W6 (optional) |
  | 1 | `my $aa, $bb, $cc;` | W6 (optional) |

---

## 2. The Working Loop (non-negotiable, after EVERY work item)

```bash
# 0. Syntax + paren discipline (after every edit)
perl -I. -c Pl/Parser2.pm          # and any other touched module
# after ANY .lisp edit: run the CLAUDE.md §10 paren checker on it

# 1. Guards
prove Pl/t/parser2-01.t            # must be 100%

# 2. First-gate census (which files lower through v2 now?)
for f in perl-tests/*.t; do
  r=$(PCL_V2=1 PCL_V2_VERBOSE=1 perl -I. pl2cl --no-cache --lenient-ppi $f \
      2>&1 >/dev/null | grep -m1 'fell back');
  echo "$f|${r:-V2-NATIVE}";
done > /tmp/census.txt
grep -c V2-NATIVE /tmp/census.txt

# 3. Parity sweep over ALL v2-native files, BOTH pipelines
FILES=$(grep V2-NATIVE /tmp/census.txt | cut -d'|' -f1 | tr '\n' ' ')
PCL_V2=1 perl sweep-perl-tests.pl --jobs 8 $FILES   # v2
perl sweep-perl-tests.pl --jobs 8 $FILES            # v1 baseline
```

**Parity means the two sweeps match EXACTLY** — same totals, same per-file
pass/fail counts, same fully-passing list. Two hard rules learned in
session 271:

1. **v2 scoring HIGHER than v1 is a bug until proven otherwise.** grent.t
   "passed" 3/3 under v2 because a broken `while (<GR>)` silently processed
   zero entries and the test's degenerate path passed. When v2 beats v1 on
   any file, transpile both (`./pl2cl --no-cache --lenient-ppi FILE` vs
   `PCL_V2=1 …`), diff the generated CL, and explain the delta before
   accepting it.
2. **A file that regresses is debugged the same way**: generate both `.lisp`
   outputs, run under `PCL_V2=1 ./runt <name>` for TAP + SBCL backtraces,
   find the first divergent form. Minimal repro files go in the scratchpad,
   run via `PCL_V2=1 ./runpl file.pl` vs `perl file.pl` (note: runpl's CWD
   is the repo root; tests that open relative files need care, and runpl
   does not load `test.pl` — use `./runt` for anything calling ok()/is()).

---

## 3. The five silent-failure classes (check before writing code)

Session 271 found five latent v2 bugs that produced **silently wrong code**
— no fallback, no crash at transpile time. Every new statement/expression
form you lower natively must be checked against this list:

1. **v1 does more than the obvious emission.** Before lowering construct X
   natively, read v1's handler for X end-to-end and list every *extra*
   rewrite it applies. Example: v1's while-condition path applies
   `_auto_defined_cond` (each/readline/readdir/glob → implicit `$_` assign +
   `p-defined` wrap). v2 missed it; loops mis-terminated. The fix pattern:
   apply v1's rewrite function at the raw seam (`_auto_defined_raw`) —
   native forms can't contain those calls, so raw-only is complete.
2. **Never silently drop a child node you don't recognize.** v2's while
   branch took "the first Block" and dropped `continue` blocks → infinite
   loop. If a construct can carry extra children (continue, labels, attrs),
   either handle them or `die "Parser2 TODO: …"` when they're present.
3. **The printer's one-line flattening vs raw text.** `Pl::CLForm::_flat`
   must return undef for any chunk containing a newline OR a `;` outside a
   string literal (a flattened `;; comment` raw swallows every sibling after
   it on that line). This is fixed; don't weaken it. Any new CLForm node
   type must define its `_flat` behaviour explicitly.
4. **Environment state the fallback reads must be mirrored.** The embedded
   v1 machinery consults shared state; v2 must maintain ALL of it:
   `environment->in_subroutine` (bumped in `_lower_sub` — drives bare
   shift/pop → `@_` vs `@ARGV`), `environment->package_stack`
   (`_set_cur_package`), `fallback_parser->{_let_bound_vars}` (my-vs-package
   decisions, `p-scalar-=`→`p-my-=` raw rewrite, `_eval_lexical_alist`),
   `environment->state_var_renames`. When you route a new construct through
   the seam, grep its v1 handler for `$self->environment->` and
   `$self->{_}` reads and ask what v2 needs to have set.
5. **VarAnnotator must see every WRITE shape.** A missed write → raw slot →
   the write vanishes (`($a,$b) = each @a` wrote into `(vector 7 …)`).
   Current write detectors: `$x = RHS` statements, `$x++/--`, compound
   assigns, `=~`, `local`, `pos`, foreach vars (my and plain), list-assign
   LHS `($x,…)=`, chomp/chop/undef/read/sysread/recv args. If you make a
   new construct native and it can WRITE a scalar, add a gate + a guard
   test. Over-firing (keeping a box) is always safe.

Also inherited PPI gotchas: `find` returns `0` not undef → `|| []`;
`PPI::Statement::Scheduled` ISA `Statement::Sub` (exclude from sub
handling); `for(;;)` sections are positional with `Statement::Null`
placeholders; a `Statement`'s content includes its `;`.

---

## 4. Work items, in order

### W1. Package block form `package Foo { … }` (4 files) + versioned packages

**Where:** `Pl::Parser2::parse()`, the segment-split loop (search for
"PACKAGE SEGMENTS"). Currently dies on block form and on
`$child->version`.

**Approach — block form is a segment sandwich:**
1. Track `$cur_pkg` through the split loop (statement-form `package Foo;`
   sets it permanently, as today).
2. On a block-form package statement: push a segment
   `{ pkg => 'Foo', stmts => [block schildren], return_to => $cur_pkg }`,
   then push a fresh segment `{ pkg => $cur_pkg, is_return => 1,
   stmts => [] }` and continue collecting into it. `$cur_pkg` does NOT
   change past the block.
3. In the assembly loop, a section for a package we have already opened (or
   `main`) must NOT re-emit the full preamble. Read v1's
   `_process_package_statement` (Pl/Parser.pm, `sub _process_package_statement`,
   block branch) and mirror what it emits when *returning*: for a return
   section emit only `(in-package <cl_pkg>)` + `(p-set-current-package
   <cl_pkg> "<pkg>")`, not `p-defpackage`/`defclass plc-*`/
   `p-register-pkg-name` again. Concretely: keep a `%opened` set keyed by
   pkg; first section for a pkg gets the full preamble, later ones the
   short form. `main`'s "first" section is section 0 (which today emits no
   preamble) — a later return-to-main section therefore takes the short
   form.
4. **Versioned** `package Foo 1.2;` / `package Foo 1.2 { … }`: read what
   the v1 handler does with `$stmt->version` (it registers/emits a
   `$VERSION`) and do the same at the segment level. If it is more than a
   one-liner, keep versioned packages gated and note it — 0 census files
   need it standalone.
5. `_check_my_spanning` / `_check_sub_captures` run per segment already; a
   package-block's `my`s are block-scoped in Perl, so they genuinely cannot
   span — but leaving them IN the `%live` set only over-fires the gate
   (→ v1), which is safe. Do the simple thing first.

**`package` inside a block (2 files):** first inspect what those files
actually do (`grep -B3 -A3 'package '` on the two census hits). If it is
the common `{ package Foo; … }` idiom, note that v2 cannot switch reader
packages mid-form (an `in-package` inside a form is a no-op for the rest of
that form — the whole reason for the section model), so the honest options
are (a) keep the gate, or (b) per-statement fallback of the entire
enclosing block through v1 (v1 handles package-in-block with its section
machinery). Do (a) unless the two files matter; record the decision in
`docs/parser2-prototype.md`.

**Acceptance:** the 4 block-form files lower natively; parity sweep exact;
new guards: transpile-shape test for the sandwich (preamble once, short
return form) + an end-to-end run mirroring the existing package e2e test
but with block form.

### W2. String eval, stage 1 — replace the text-scan gate with a PPI test

**Where:** `Pl::Parser2::parse()`, the
`die … if $src =~ /\beval\b(?!\s*\{)/` near the top.

The text scan fires on comments, strings, POD, `eval {` split across lines,
and hash keys. Replace with a walk over the parsed document (move the gate
AFTER `PPI::Document->new`):

```perl
my $evals = $doc->find(sub {
  my $t = $_[1];
  return 0 unless $t->isa('PPI::Token::Word') && $t->content eq 'eval';
  my $prev = $t->sprevious_sibling;
  return 0 if $prev && $prev->isa('PPI::Token::Operator')
           && $prev->content =~ /^(?:->|=>)$/;          # ->eval / key => 
  my $next = $t->snext_sibling;
  return 0 if $next && $next->isa('PPI::Structure::Block');   # eval { }
  return 0 if $next && $next->isa('PPI::Token::Operator')
           && $next->content eq '=>';                   # eval => …
  return 1;                                             # eval EXPR
}) || [];
die "Parser2 TODO: string eval\n" if @$evals;
```

Comments/strings/POD never appear as Word tokens, so those false positives
vanish for free. After this lands, re-run the census: every file that only
*mentions* eval or only uses `eval BLOCK` is recovered. Expect a large
jump (many of the 65). Then run the parity sweep on all new natives — this
is the highest-risk-of-exposure step in the plan, because dozens of files
newly exercise v2 paths; budget time for triage and expect to find more
class-1/class-5 bugs (§3).

**Acceptance:** census jump; exact parity on every newly-native file; guard
tests: `eval { }` file lowers natively, `eval "str"` file still dies to v1,
`$h{eval}`/`->eval` don't gate.

### W3. String eval, stage 2 — enable `eval EXPR` natively via the existing capture seam

**Read first:** `docs/eval-lexical-capture.md` and
`Pl::ExprToCL::_eval_lexical_alist` (Pl/ExprToCL.pm, search the sub name).

**Key insight (this supersedes older "demotion/rename" sketches in
parser2-prototype.md):** v1's capture mechanism needs NO defvars. At each
`eval EXPR` call site the expression generator emits
`(p-eval STR (list (cons "$x" $x) …))` — an alist of *in-scope lexical
names → their live boxes*, read from `$parser->{_let_bound_vars}`. The
eval'd string is transpiled by a subprocess into a `p-eval-thunk` whose
lambda binds those boxes as parameters. Boxes are the capture medium.
v2's true-lexical `let`s holding boxes work as capture sources as-is, and
the eval-side transpile always runs v1 (pl2cl routes eval_mode to v1 in
`parse_with_fallback` — verify this stays true).

So `eval EXPR` can flow through the ordinary expression fallback seam,
PROVIDED two invariants hold at the call site:

1. **Every captured var is boxed.** Already guaranteed: VarAnnotator's
   region-wide `$has_eval` (`$text =~ /\beval\b/`) disqualifies unboxing in
   any region whose text mentions eval. Do NOT narrow that scan as part of
   this work item.
2. **`_let_bound_vars` is scope-accurate at the call site.** It currently
   never shrinks. A name from an already-closed sibling let would land in
   the alist as a free CL symbol → unbound-variable at load. Fix: restore
   `fallback_parser->{_let_bound_vars}` at scope exit exactly like
   `_live_lex` (save/restore in `_lower_scope` and `_lower_sub`; simplest
   is to snapshot both hashes in the same place). **But** two consumers
   need the CUMULATIVE set — keep a separate accumulator
   (`$self->{_all_lex}`, appended in `_reg_lex`) for:
   - `_forward_global_decls`: a name let-bound ANYWHERE in the section must
     never be defvar'd (the defvar would proclaim it special and poison the
     lexical lets). Switch its `$lb` lookup to the accumulator.
   - `_check_my_spanning` interplay: unchanged (its own collector).
   The `p-scalar-=`→`p-my-=` raw rewrite in `_lower_expr` should use the
   *scoped* set (more accurate than today; a package `$x` used after a
   closed `my $x` scope stops being mis-rewritten).

**Then remove the W2 gate** (delete the die) and let `eval EXPR` statements
lower through `_lower_stmt` → `_lower_expr` → fallback, which calls v1's
`gen_funcall` → `_eval_lexical_alist` with a now-accurate set.

**Sub bodies:** params registered via `_reg_lex` are in the set → captured,
matching v1. **Known divergence to preserve:** context propagation
(`wantarray` inside string eval) stays deferred — documented in
`docs/not-supported.md` "Context propagation into string eval".

**Test order for this item:** before touching perl-tests, run the dedicated
capture suite under v2: `PCL_V2=1 prove Pl/t/eval-capture-01.t` (30 tests)
— it must match v1 exactly. Then census + parity. Expect eval-heavy files
(eval.t itself, caller.t, …) to stay imperfect for OTHER reasons — parity
with v1 is the bar, not absolute pass counts.

**Acceptance:** eval-capture-01.t identical under both pipelines; census
jump; exact parity on newly-native files; guards: alist emitted with only
in-scope names (transpile-shape test with a closed sibling scope), write-
back through eval end-to-end (`my $x = 1; eval '$x = 2'; print $x` → 2).

### W4. Prototype/signature subs (2 files)

**Where:** the pre-pass die in `parse()` (`sub with prototype/signature`)
and the two sub-lowering sites (top-level loop in `parse()`,
`_hoist_nested_sub`).

1. In the pre-pass, instead of dying: register the prototype the way v1's
   `_process_sub_statement` does (find its `add_prototype` call and the
   prototype-string parsing that feeds it; reuse the same helper — do not
   re-implement parsing). This makes CALL SITES parse correctly (arity,
   imposed scalar context for `($)`, block-form for `(&@)`).
2. **Exclude prototyped subs from `sub_info`** so ExprToCL2's native
   direct-call path never fires for them — imposed contexts are fallback
   territory. (One line: `next if $sub->prototype;` after registration.)
3. Lower the DEFINITION through `_fallback_stmt` (v1 emits the p-sub with
   signature binding into the definitions bucket, which the seam hoists via
   `_captured_decls`). Route: in the `parse()` top-level loop and in
   `_lower_block`'s nested-sub branch, `if ($child->prototype) { … 
   _fallback_stmt … }` instead of `_lower_sub`.

**Acceptance:** the 2 gated files lower (or reveal their next gate); a
guard: `sub f ($) { … }` transpiles with the call site imposing scalar
context (compare against v1's output shape), definition via fallback.

### W5. File lexicals captured by named subs (4 files, incl. qq.t)

The `{ my $x = 0; sub f { $x++ } }` static-variable idiom and qq.t's
`my $test = 1; sub is { $test++ }`. Named subs hoist to the definitions
bucket OUTSIDE the lets, so the capture needs a shared cell both can see.

**Design (single-declaration case only — gate the rest):**
1. Applies wherever the capture gates fire today: `_check_sub_captures`
   (top-level) and `_hoist_nested_sub` (nested). Precondition for the new
   path: the captured name is declared by exactly ONE `my` in the segment
   (count via a text/PPI scan like VarAnnotator's `decl_count`); otherwise
   keep the die → v1 (shadowing needs v1's `__lex__N` rename machinery —
   out of scope).
2. For each such name `$test`: allocate `$test__file__N` (N = counter),
   emit `(defvar $test__file__N (make-p-box nil))` into the section decls,
   and **rewrite the PPI Symbol tokens** (`$token->set_content`) for every
   occurrence in the segment before lowering. Token-level rewrite means
   both the native path and every fallback raw see the new name with zero
   further plumbing. The `my $test = INIT;` statement then lowers as a
   plain assignment to the defvar'd box (drop the `my`, keep the init;
   reuse the `our`-declaration lowering shape in `_lower_our_decl` as the
   model — it already does defvar-plus-plain-assignment).
3. The rewritten name must NOT be registered in `_let_bound_vars` (it is a
   package var now) and VarAnnotator must treat it as boxed (it will: the
   name has no single `my` declaration anymore).
4. Sigil variants (`@a`, `%h`) same treatment; `$#a` references are caught
   by the same token rewrite only if you match ArrayIndex tokens too —
   check qq.t's actual needs first and gate what you don't cover.

**Warning:** do the PPI rewrite on the segment's statement list BEFORE the
pre-pass captures sub bodies' text (`_sub_ctx_insensitive` reads content),
or cache-of-content mismatches will confuse you. Simplest: perform it at
the very top of the per-segment loop in `parse()`, driven by a pre-scan
that detects the capture situation.

**Acceptance:** qq.t + the other capture-gated files lower; parity exact;
guards: the static-variable idiom end-to-end (counter increments across
calls, file lexical stays private), shadowed case still dies to v1.

### W6. Small gates (optional, do only if cheap)

- **continue blocks (1 file):** native lowering is genuinely small: the
  loop macros already accept `:continue FORM` (see `parse-loop-keys` in
  cl/pcl-runtime.lisp — `:label` must come first, `:continue` is found by
  position). Emit `':continue', ['progn', @cont_body]` in the while/foreach
  branches instead of gating; bare-block continue keeps the gate. Check
  what v1 emits for foreach+continue first (p-foreach's handling differs
  from p-for, which IGNORES continue — gate `for(;;) … continue`).
- **`my $aa, $bb, $cc;` (1 file):** Perl declares only `$aa`; the rest are
  comma-op reads of package vars (it warns "Parenthesize"). Match v1's
  behaviour (read `_process_variable_statement`); if v1 defvars all three
  at file scope, the cheap v2 route is: lower as `my $aa;` (let) and ignore
  the trailing names (they become forward-declared globals via
  `_forward_global_decls` when used). Verify against real perl before
  choosing.
- **Standalone labels / computed goto:** keep gated. Intra-sub goto is
  v1-partial (`docs`/memory: project_intra_sub_goto); don't open it here.

### W7. Tier B1 — full-sweep parity

```bash
perl sweep-perl-tests.pl --jobs 8                    # fresh v1 baseline
PCL_V2=1 perl sweep-perl-tests.pl --jobs 8           # v2
tools/sweep-diff.pl                                   # per-test diffs
```

- Re-measure the v1 baseline the same day (memory records 18089 pass / 62
  fully passing, with known open regressions — do not compare against
  stale numbers; `docs/fail-baseline.tsv` re-blessing is a separate open
  TODO).
- The sweep writes `.faillog/` (`_status.tsv` col6 localizes crashes — see
  memory project_partial_stop_analysis). Save both runs' logs and diff.
- Every per-file delta gets one of: (a) v2 bug → fix; (b) v2 legitimately
  better → PROVE it (diff generated CL, run the specific test against real
  perl) and document in `docs/parser2-prototype.md`; (c) flaky → re-run
  (memory: project_sweep_flakiness_investigation).
- Iterate until zero unexplained deltas. This is the longest item; expect
  several find-fix-resweep cycles at ~20 min per full sweep.

### W8. Tier B2 — the Pl/t gate under v2

```bash
PCL_V2=1 prove -j8 Pl/t/          # ~6 min; must match v1's counts
```

Some Pl/t tests assert on v1's exact output shapes; where a v2 shape is
semantically equivalent but textually different, the test needs a
pipeline-conditional branch or a shape-agnostic assertion — NEVER weaken
what the test verifies (CLAUDE.md §5). If a Pl/t test fails under v2
because of missing v2 behaviour, that's a bug to fix, not a test to edit.

### W9. Tier B3 — flip the default

1. **Cache keying first** (hard prerequisite): `~/.pcl-cache` entries must
   be keyed by pipeline, or flipping poisons every cached module transpile
   (memory: STALE CACHE GOTCHA is about codegen changes generally). Find
   the cache-key computation (grep `pcl-cache` in cl/pcl-runtime.lisp and
   pl2cl) and mix in a pipeline tag + a v2 version counter. Verify: run a
   module-using file under v1, flip, re-run — must retranspile, not reuse.
2. In pl2cl: default `$PARSER_CLASS` to Parser2; `PCL_V1=1` becomes the
   escape hatch; `PCL_V2` stays accepted (no-op). Whole-file fallback stays
   for all remaining gates.
3. Run W7 + W8 once more with the default flipped and nothing in the env.
4. Re-run the perf benches (fib(29) recursive + loop, intmath 2M — the
   programs and numbers are in `docs/parser2-prototype.md` §R1; recreate
   them in the scratchpad) and record before/after in the doc. v2 exists
   for speed: fib must stay ≈0.08 s territory, beating perl.
5. Update: CLAUDE.md quick-reference, `docs/parser2-prototype.md` status
   header, memory (project_parser2_prototype), runners' comments if any
   mention PCL_V2.

### W10. Tier B4 — fix the v1 my-across-package bug properly

The open v1 bug (memory, s270): `my $g; package Foo; print $g;` — v1
defvars `$g` under `:main` but the Foo section reads `Foo::$g` → unbound.
v2 currently gates this (`_check_my_spanning`). The proper fix reuses W5's
machinery: a my-lexical that spans a package boundary gets the
rename+defvar treatment, with references in LATER segments rewritten to the
package-qualified Perl form `$main::g__file__N` (the fallback machinery
already compiles qualified vars; `main` must be in the pre-declared package
set — it is skipped today, so check `_forward_global_decls`' skip list).
Same single-declaration precondition; shadowing spans stay gated.
Acceptance: the s270 repro produces the right answer under v2, and a guard
test pins it.

### W11. Tier C1 — native hash/array element access (measure first)

**Do not start until coverage (W1–W6) is done and B1 parity holds.**

1. Write an aggregate-heavy bench (tight loop over `$h{$k}` reads/writes
   and `$a[$i]`), measure perl vs v1 vs v2-today. Only proceed if the gap
   is real (expect it is: every element access round-trips the fallback).
2. ExprToCL2: native lowering for `$h{k}` / `$a[$i]` where the base is a
   known let-bound container and the subscript is native — READ position
   compiles to `(p-gethash %h KEY)` / `(p-aref @a IDX)`. These return
   boxes; that is fine as OPERANDS of the R1 ops (they coerce), but a bare
   `my $x = $h{k}` must stay boxed (class-5 rule: never store a box in a
   raw slot). The existing `_arith_rhs` "operator coerces" logic already
   models this — element reads count as `others`, like sub calls.
3. WRITE position (`$h{k} = EXPR`, `$a[$i] = EXPR`) compiles to v1's
   p-setf shapes — copy the exact form v1 emits (`(p-setf (p-gethash …)
   …)`), don't invent one; tied hashes and autovivification live behind
   those entry points.
4. VarAnnotator: no new scalar gates needed for reads; container names
   (`%h`/`@a`) are never unboxed anyway.
5. Re-measure; record in the doc. Then consider `keys/values/each` native
   iteration if the bench says it matters.

### W12. Tier C2 — OpcodeTree-walk VarAnnotator

Replace the text-scan gates with facts from the PExpr OpcodeTree that
ExprToCL2 already builds per expression (see
`docs/type-flow-and-codegen-plan.md` §(s)). Sketch: per statement, after
`parse_expr_to_tree`, walk the tree collecting per-name (read, write,
ref-taken, magic-target) events; aggregate per block; keep the same
conservative rules but keyed on AST facts instead of regexes. The win is
un-gating names the text scan falsely disqualifies (`"$x"` in strings,
shadowing, `++` outside the C-for carve-out). Keep BOTH annotators runnable
during bring-up and assert the AST one is never LESS conservative than the
text one on the full census corpus before switching (any name AST-unboxable
but text-boxed needs a manual proof or a fix).

### W13. Tier C3 — lean p-sub round 2

Only with fresh measurements (`docs/parser2-prototype.md` "Lean p-sub"
table): remaining per-call residue was ~18 ns catch/throw for `return` +
~15 ns for the five special binds. The catch elision needs a "no closure
in the body can re-throw :p-return after the frame exits" analysis; the
bind elision needs a whole-program "nobody calls caller()" bit (note
caller() reads the whole chain — it is NOT a per-sub decision). If the
numbers haven't changed, expected win is small; prefer W11/W12.

---

## 5. Reference card — seams and invariants

- **Three fallback levels:** expression (`_lower_expr` → raw leaf),
  statement (`_fallback_stmt` / `_fallback_stmt_capture` → scratch-section
  run of v1's `_process_element`; decl buckets hoist to `_captured_decls`;
  a `_local_let_depth` surplus is only legal via `_lower_local`'s
  raw_wrap), whole file (`die "Parser2 TODO: …"` → pl2cl retries v1).
- **CLForm is the only printer.** Forms: string atom, `[head, @args]`,
  `['list', @elems]`, `raw($text)` (balanced), `raw_wrap($open, $n, @body)`
  (open text with exactly $n unclosed parens, from v1's counter). Never
  string-rewrite generated CL outside the two sanctioned seam rewrites
  (`p-scalar-=`→`p-my-=` for let-bound names; `_auto_defined_cond` on raw
  loop conditions).
- **Scope tracking:** `_reg_lex` registers a `my` name into
  `_let_bound_vars` (fallback decisions) and `_live_lex` (capture gate);
  `_lower_scope`/`_lower_sub` snapshot+restore `_live_lex` (and, after W3,
  `_let_bound_vars` too, with `_all_lex` as the cumulative set for
  `_forward_global_decls`).
- **Sections:** one per package segment; assembly order per section is
  decls → per-pkg `$a`/`$b` defvars → forward global decls → captured
  decls → defs → `p-set-current-package` (non-first sections) → runtime.
  All later-section packages and Environment-registered undeclared
  packages are `p-defpackage`'d at the file top (the reader must be able
  to intern qualified symbols form-by-form).
- **Context rules:** funcall args bind `*wantarray*` t; statement position
  `:void` (elided — v2 emits no void wraps); `return`/sub-tail `'inherit'`
  = no bind; the bind is emitted only for context-SENSITIVE callees
  (`_sub_ctx_insensitive`).
- **Numbers:** leading-zero integer literals are octal — never native
  (`#o100` via fallback). `*read-default-float-format*` is double-float.

## 6. Bookkeeping (every session)

- Extend `Pl/t/parser2-01.t` for every behaviour added or bug fixed; the
  paren_balance helper there guards raw seams.
- Update `docs/parser2-prototype.md` (session section + census table) and
  `docs/session-log.md` (newest first). Keep this plan's checkboxes
  current by striking through completed items with the commit hash.
- One commit per work item on `main`, message style `feat(v2): …` /
  `fix(v2): …`, ending with the Co-Authored-By trailer per CLAUDE.md.
- Never edit `perl-tests/*.t`; not-supported cases go through
  `cl/skip-registry.lisp` with a `docs/not-supported.md` entry — but for
  v2 work the normal outcome is a gate (die → v1), not a skip.
