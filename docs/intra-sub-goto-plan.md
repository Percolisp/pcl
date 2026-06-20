# Intra-sub `goto LABEL` + declaration codegen — status & plan

**Session 263 (2026-06-21).** Started from "make Text::Balanced's `_match_tagged`
work" (intra-sub forward `goto LABEL`). Shipped partial support; hit a deeper
declaration-codegen tangle. This doc records exactly where we are, the general
problem, and the plan to finish it.

---

## What works now (committed, `a0fa56a`)

`goto LABEL` inside a sub body, and top-level `goto` nested inside a multi-line
form, now compile to a CL `(tagbody …)`:

- **`_scan_lisp_lines`** (`Pl/Parser.pm`): paren/string/comment/`#\`-aware scan
  of generated CL lines → per-line `{depth, in_lambda}`. `in_lambda` is true
  when a `(lambda`/`(p-sub` is open (a `go` can't cross a function boundary).
- **`_wrap_runtime_labels`** rewritten to **complete-form granularity**: groups
  lines into balanced forms, finds labels (at region depth 0) and their reachable
  gotos (any depth, not `in_lambda`), and wraps the minimal form-range spanning
  each label+goto in a `(tagbody …)`, hoisting definition forms out. Post-label
  forms stay outside the tagbody, so implicit-return values are preserved.
- **`_process_block`** captures its emitted statement region and runs the wrapper
  on it (so the tagbody lands *inside* the declaration `let`).
- **`_block_has_standalone_label`** forces the **flat-let** declaration path for
  sub bodies containing a standalone `LABEL:` (so the label and its siblings
  share one scope and gotos can reach the tag).

Covered by `Pl/t/goto-label-01.t` (7 tests): forward error-goto, backward retry,
implicit return after label, multiple labels, goto out of a `while`, top-level
goto-in-`if`, `use` pragma in the same block. Full gate green (100 files /
3535 tests).

## Also fixed this session (do-block scoping leak) — `_find_all_declarations`

`do { my $x; … }` is its own CL lambda scope, but `_find_all_declarations`
recursed into it and **hoisted `my $x` to the enclosing sub's declaration let**.
That (a) double-binds `$x` (the do-block emits its own `(let (($x …)))`) and
(b) leaves the hoisted outer let open around the rest of the body, nesting every
following statement (and any trailing labels) one level deeper. Fix: treat a
`do`-prev-sibling block like `sub`/`eval` blocks — do **not** recurse for
hoisting. This is an independent correctness fix (scoping), regardless of goto.

After this fix, in `_match_tagged` the trailing labels drop from paren depth ~9
to depth 4 (= the flat-let body level) and **all** gotos are at depth ≥ the
labels (reachable) — the precondition a tagbody needs.

---

## What still does NOT work — `_match_tagged` (real Text::Balanced)

`extract_tagged` still aborts: `attempt to GO to nonexistent tag: :MATCHED`.
A `(tagbody …)` is never emitted around the labels even though, in isolation,
`_wrap_runtime_labels` produces one for the exact region. Two distinct blockers,
both pointing at the declaration/section machinery rather than the goto logic:

### Blocker A — the wrap result is discarded in the 2-pass (`pl2cl`) path
In `pl2cl` (which uses `parse_file` → prototype pass + real pass), the sub-body
region passed to `_wrap_runtime_labels` is 57 lines, the wrapper **does** insert
a `(tagbody …)` (verified by re-running it on the dumped region), yet the final
output contains **zero** tagbody lines. So the in-place `splice @$arr, …` on the
`definitions` bucket is being thrown away or overwritten downstream. Suspects:
named-sub-body emission interacts with bucket routing (`_cur_bucket` →
`definitions`) and/or a temp-section collection (`parse_block_as_function` swaps
`_sections` for `return_lambda`; do/eval/anon bodies use it). Need to trace the
bucket arrayref identity from splice-time to `_assemble_output`.

### Blocker B — codegen differs between 1-pass and 2-pass
Standalone `Pl::Parser->parse()` (1 pass) produces a **46-line** sub-body region
for the same source where `_wrap_runtime_labels` **cannot** wrap (labels not at
region depth 0 → the flat-let path wasn't taken, or BlockAnalyzer/`_find_all_
declarations` results differ without the prototype pre-pass). `pl2cl` (2 pass)
produces a 57-line region that *is* wrappable. **Codegen must not depend on
whether a prototype pre-pass ran.** This nondeterminism is a latent hazard well
beyond goto.

---

## The general problem (why this was hard)

Declarations are emitted as an **interleaved stream of `let` opens** whose closes
are deferred to block-end via side bookkeeping (`_pending_let_closes`,
`_local_let_depth`). Consequences:

1. **No scope tree.** "These statements are siblings in one scope" is not
   represented; it has to be recovered by counting parens in *generated text*.
   Any construct that must be inserted at statement-sibling level (a `tagbody`,
   here) does paren archaeology on output strings.
2. **Two declaration paths chosen by heuristics** (two-phase scoped lets vs
   flat-let), now with a third heuristic (`_block_has_standalone_label`) layered
   on. The choice can even differ by compilation pass (Blocker B).
3. **Control flow is post-hoc text surgery.** Labels/gotos/loops are recognised
   by regex over emitted lines, not as IR nodes. Works for simple shapes; breaks
   when declarations nest the label relative to its goto, or when the emitted
   lines live in a temp section.
4. **Scope leaks.** `do`/`eval`/`map`/`grep`/`sort` blocks each become a CL
   lambda; any `my` hoisting that crosses them (we just fixed `do`) corrupts
   scoping. `map`/`grep`/`sort` block bodies are **not yet** excluded from
   `_find_all_declarations` hoisting — a likely latent instance of the same bug.

---

## Plan

### Specific (finish `_match_tagged`) — do next, in order
1. **Fix Blocker A.** Trace the `definitions` bucket arrayref from the
   `splice` in `_process_block` to `_assemble_output`. Likely the named-sub body
   is collected/relocated after `_process_block` returns; make the goto-wrap run
   on the *final* line stream for the sub (or move the wrap to the point where
   the body lines are in their final home). Add an assertion/test that a
   label-containing sub emits exactly one matching `(tagbody …)`.
2. **Fix Blocker B.** Make the declaration-path choice (and `_find_all_
   declarations`/BlockAnalyzer results) **pass-independent**. Add a regression
   that compares `parse()` output to `parse_file()` output for a label+do sub.
3. **Exclude `map`/`grep`/`sort` block bodies** from `_find_all_declarations`
   hoisting (same fix shape as `do`/`eval`), with a `my`-in-grep-block test.
4. Re-run real `Text::Balanced` `extract_tagged`/`extract_*`; add a `Pl/t`
   regression once green. Then `_match_tagged` also needs intra-sub `goto`
   reaching labels from inside its `while` (already supported) — should fall out.

### General (the real cure) — aligns with the existing rewrite plan
This is exactly what `docs/codegen-rewrite-spec.md` and
`docs/type-flow-and-codegen-plan.md` target. The goto work is a clean
north-star for it:
- Declarations become **scope nodes in an IR**, not emit-time `let` opens with
  deferred closes. A block knows its own `my`/`our`/`local`/`state` set and its
  child statements; lowering emits the `let`(s) structurally.
- Control flow (labels, `goto`, `next`/`last`/`redo`, loops) are **first-class
  IR nodes**; `tagbody`/`go`/`block`/`return-from` are emitted during lowering,
  never by regex over text.
- `my` scoping for `do`/`eval`/`map`/`grep`/`sort`/anon-sub is resolved on the
  IR (each is a scope boundary) before any text exists — Blockers A/B and the
  whole class of "hoist leaked across a lambda" bugs disappear.

Recommendation: land the specific fixes (1–4) to unblock real CPAN code now;
treat them as acceptance tests that the IR rewrite must keep passing.
