# Duplicated-code census — the worklist (s411, Fable, 2026-08-18)

*Task #379 §ADDED (USER, s409): "a comprehensive search for doubled code —
lots of small bits can be extracted to subs."  This is the census, the
verdict per family, and the order.  CLAUDE.md rule 11 is the standing rule
for NEW code; this is its application to what exists.*

## 0. How it was measured (re-runnable; the numbers below drift with every commit)

```
tools/sub-call-census.pl --sample 12 --out calls.tsv        # ~3 min: per-sub call counts + time
tools/dup-census.pl --calls calls.tsv --top 60               # families + clusters, hot/cold tagged
tools/dup-census.pl --tsv dups.tsv --top 500                 # the whole thing as a table
```

`dup-census.pl` finds every run of ≥ 5 normalized lines that occurs twice or
more, at two levels — EXACT (whitespace/comments normalized) and SHAPE
(also `$name`→`$V`, numbers→`N`, string literals→`"S"`) — and groups
clusters whose extents overlap into FAMILIES (one extraction target).  Over
52 files (Pl/**, the hand-written cl/*.lisp, tools/**, the runners) at
s411: **462 clusters, 4 348 "deletable" lines** (a copy's lines × (copies −
1); an upper bound — boilerplate scores too), Perl 3 900 / Lisp 450.  By
file: Parser.pm 1 178, PExpr.pm 768, ExprToCL.pm 749, pcl-runtime.lisp
509, StringInterpolation.pm 487, Parser2.pm 337.

`sub-call-census.pl` (Devel::NYTProf, installed s411 into the perlbrew
perl) gives each member's enclosing sub a call count and time so a cluster
on a per-token path is visibly HOT.

## 1. Verdict rules (apply these to anything the tables below do not name)

1. **DELETED-BY a plan phase → do not touch.**  `Pl/ExprToCL2.pm`, the three
   bucket-dance copies, the native attempt, `_seam_lex_assign_fix`, the text
   twins in ExprToCL (`gen_binary_op`, `gen_leaf`, `gen_internal_node_text`,
   `gen_inline_lambda`) — all go in `docs/plan-one-compiler-s411.md` Phases
   A–C.  And **v1's `_process_*` statement handlers in `Pl/Parser.pm`** —
   retired class by class post-release (§4.1 there).  Extraction inside them
   is work the delete throws away.  Exception: a family whose members
   straddle a surviving file (e.g. Parser.pm ↔ Parser2.pm) — extract into
   the surviving side and call it from both.
2. **SUPERSEDED-BY a planned port → do the port, not the extraction.**
   The StringInterpolation subscript scanners are what InterpScan consumer
   3 replaces (`docs/interp-scan.md` wiring step 3); the Parser2
   `_interp_*` copies are consumer 2.  A piecemeal helper there is throw-
   away unless the port slips past v0.1.
3. **EXTRACT — cold code: free.**  Bar: `tools/corpus-diff.pl` IDENTICAL,
   `tools/prove-core`; for `cl/**` the sweep (WHAT-TO-RUN table); one
   commit per family, the commit message names the family.
4. **EXTRACT — hot code (a member's sub ≥ ~50 k calls in the 12-file
   sample, or per-token/per-node): only if it adds NO call layer on the hot
   path** — collapse in place (a 7-line predicate becomes a 1-line
   predicate; a Lisp `flet`/`macrolet` inside the same function is free), or
   measure: whole-corpus transpile time before/after (68.4 s at s411), and
   `tools/bench-exec.pl` for runtime Lisp.  A slower compile is a NO.
5. **LEAVE — boilerplate that reads better repeated**: 5-line argument
   unpacks, accessor bodies, the `for … { $self->add_child_to_node … }`
   idioms with nothing behind them.  Say so in the family's row so nobody
   re-reviews it.
6. Every extraction that touches Parser2 name-resolution/scoping code (the
   `my`-scan family) makes **the full sweep the gate** (#296 rule), not
   just corpus-diff — a rename region bug is invisible to the corpus diff
   only when the emitted names happen to coincide.
 7. **SCOPE (USER, s413): the compiler and the runtime are the product —
   `Pl/**` and `cl/pcl-runtime.lisp`; `tools/**` and the two runner
   scripts are scaffolding that may be replaced, so their families are OUT
   OF SCOPE (LEAVE) from s413 on**; family 6 landed before the ruling
   (`s413a`) and stays.  Test files are never an optimization target — only
   guard rows are ADDED to them.

## 2. The families, ranked, with verdicts

"~N" = deletable lines the census attributes to the family (an over-count
where clusters nest).  Numbers/lines are s411's; re-run the tool for
current ones.

| # | family (file · subs) | ~N | verdict | note |
|---|---|---|---|---|
| 1 | `PExpr/StringInterpolation.pm` · `parse_array_subscript` / `parse_hash_subscript` / `parse_braced_expression` / `parse_interpolated_variable` — the hand-rolled "scan to the matching `}`/`]`, then parse the subscript chain" walkers | ~289 (+36 fam 15, +22 fam 33) | **SUPERSEDED — InterpScan consumer 3** | one EXACT 36-line copy (`:898–935` vs `:1047–1082`) plus a 6-line brace-depth walk in NINE places.  The port replaces the whole scanning layer with `scan_one` events (interp-scan.md step 3) and closes divergences 3–7 vs perl.  **Schedule it** — it is structural and deletes ~350 lines.  If it slips past v0.1: extract `_skip_balanced($content, $i, '{', '}')` (the 9× walk) alone. |
| 2 | `ExprToCL.pm` · `gen_funcall_form` — the `tied`/`pos`/`delete`/`exists`/… arms that each spell "is the arg an `a_acc`/`h_acc`? take container + key, swap the sigil" twice (array arm, hash arm) | ~175 (+47 fam 10) | DONE `s414b` (`_elem_container_key`, 201 lines → 62; corpus IDENTICAL; found+fixed #397 and the empty-slice arity split) — **EXTRACT** | one `_elem_container_key($arg_id)` → `($kind, $container_form, $key_form)` or `()`; each builtin arm becomes three lines.  ExprToCL is THE generator after Phase A — this survives.  `gen_funcall_form` ≈ 6.7 k calls/sample: per call, not per token; free.  Do it AFTER Phase A1 (A1 touches the same function). |
| 3 | `PExpr.pm` · `parse` — "attach the subscript's children to the new node, flattening a slice `progn`" | ~159 | DONE `s413d` (add_child_flattening + _kv_slice_node, −62) — **EXTRACT** | `_add_subscript_children($id, $ix_id, $type)`, 5× (`:1383`, `:1440`, `:1467`, `:1495`, `:1524`) + the interp `string_concat` walk (`:691`, ×6).  Pure mechanics, IDENTICAL by construction — but this is the postfix-`->` loop of the term maze (`docs/pexpr-term-parsing-review.md`): one helper per commit, corpus-diff each, no rule changes. |
| 4 | `PExpr.pm` · `parse` — "reduce `PRE -> NXT` to a node: `$e->[$i-1] = $node; splice @$e, $i, 2; $i--; next`" | ~69 | DONE `s413e` (_reduce_pre 12 sites + _prefix_op_node 4) — **EXTRACT** | `_splice_reduce($e, \$i, $node, $n)` 6× (`:1175`, `:1191`, `:1206`, `:1251`, `:1576`, `:1599`).  Same maze caveat as 3.  `parse` is the hottest sub (62 k calls); the helper runs once per reduction, not per token — measure the corpus time anyway (rule 4). |
| 5 | `Parser2.pm` · `_stmt_declares_canon` / `_embedded_my_syms` / `_cond_my_names` / `_pkgblock_shadows_file_lexical` / `_shadow_rename_blocker` / `_state_container_blocker` / `_rename_exception_mys` — "the symbols declared by every `my`/`state` word directly under NODE, skipping nested blocks" | ~67 (+10 fam 70) | DONE `s413b` (_decl_syms_under; sweep at batch end) — **EXTRACT — sweep is the gate** | `_decl_syms_under($root, %opt)` (words `my`/`state`/both, `nested => 0/1`, `stop_at`) — EIGHT hand copies of one scope walk, exactly what `docs/var-handling-review-s379.md` counted; extraction REDUCES the walk count (rule "no new scope walk").  Callers up to 2.8 k/sample: per statement, fine.  Rule 6: full sweep. |
| 6 | `sweep-perl-tests.pl` ↔ `tools/run-perl-suite.pl` · `run_isolated` (54 lines EXACT) + `mem_report`/`record_result` shape | ~59 | DONE `s413a` (before the s413 scope ruling; tools now OUT OF SCOPE) — **EXTRACT → `tools/lib/PCLProc.pm`** | fork/setsid/exec + timeout + process-group kill — the #366/#367 fixes had to land in BOTH copies; a drift here is a runner that kills differently.  Bar (WHAT-TO-RUN runners row): each runner once, verdicts file-by-file identical, `PCL_SHOW_SBCL=1` before/after. |
| 7, 9, 11, 12, 16, 19, 24, 25, 26, 28, 29, 40, 46 … | `Parser.pm` · `_process_block` / `_process_scheduled_block` / `_process_local_declaration` / `_process_expression_statement` / `_process_my_toplevel_declaration` / `_process_if_*` / `_process_bare_block` … | ~450 in all | **LEAVE — DELETED-BY E5.3** | v1's statement layer, retired class by class post-release (`local` and `scheduled` are the two biggest and the two most duplicated — that IS the port's argument).  Do not extract; a fix that must land there is filed on its class. |
| 8 | `PExpr.pm` · `handle_subcalls` — "parse the rest of `@$e` as a comma list, attach, splice into place" (13 lines × 3 EXACT: `:3517`, `:3595`, `:3638`) | ~54 (+24 fam 35, +24 fam 36) | DONE `s413f` (_take_rest_as_args) — **EXTRACT** | `_take_rest_as_args($e, $i, $top_id, $top_node)`.  Once per block-form call.  Maze caveat. |
| 10 | `ExprToCL.pm` · `gen_funcall_form` `exists`/`delete` arms | ~47 | DONE `s414b` (with family 2) — **EXTRACT** | same helper. |
| 13 | `cl/pcl-runtime.lisp` · `p-array-fill` / `p-copy-array` — the per-item `cond` written once for `(loop across …)` and once for `(loop in …)`, in two functions | ~38 | DONE `s413k` = `8a7ffe8` (%p-array-fill-item macro; merged to main s414 after gate+bench+sweep, #395) — **EXTRACT (Lisp) — `macrolet`/`flet` inside each function** | zero-cost by construction (local, inlined); the runtime row of WHAT-TO-RUN: sweep + `tools/bench-exec.pl` on array assignment.  Rule 12 check while there: the `cond` ends in a `t` arm that stores — legal, it is the general case, not a missing one. |
| 14, 26 | `Parser.pm` · `_parse_expression_internal` / `_parse_expression_form` / `_compile_constant_value` / `_compile_default_expr` / `parse_hash_block_to_cl_string` / `_form` — the `Pl::PExpr->new(…) … Pl::ExprToCL->new(…)` construction, 6× | ~62 | DONE `s414c` (`_expr_parser` + `_expr_generator`, −20 net; corpus IDENTICAL) — **EXTRACT** | Phase A2/A4 rewrite what these sites do (Parser2 constructs the generator; the four TEXT `->generate(` become `to_flat(gen_node_form)`).  Then ONE `_expr_compiler($parts, $stmt)` in Parser.pm serves the survivors.  Written earlier it is written twice. |
| 15, 33 | `StringInterpolation.pm` · `parse_interpolated_variable` / `_parse_subscript_chain` | ~80 | **SUPERSEDED — family 1** | — |
| 17 | `ExprToCL.pm` · `gen_array_slice_form` / `gen_hash_slice_form` / `gen_kv_*_slice_form` | ~29 | DONE `s413g` (_slice_index_forms) — **EXTRACT** | one `_slice_form($head, $container_id, $index_ids, %opt)`; four thin wrappers.  Cold. |
| 18 | `tools/difftest-ops.pl` · `add_builtin` / `prog_stmts` | ~49 | OUT OF SCOPE (tools, USER s413) — **LEAVE (or a data table)** | a fuzzer's generator cases; the repetition is the case list.  If touched: a table of `[name, arity, ctx]` rows and one loop. |
| 20 | `ExprToCL.pm` · `gen_hash_access_form` / `gen_hash_ref_access_form` (14 lines EXACT) | ~26 | DONE `s413g` (_hash_key_form) — **EXTRACT** | the key-lowering half is identical; `_hash_key_form($key_id)`.  344 + 146 calls/sample; per node; free. |
| 21 | `cl/pcl-runtime.lisp` · `p-hslice` / `p-kv-hslice` / `p-delete-hash-slice` / `p-delete-kv-hash-slice` | ~25 | DONE `s413l` = `c1f7142` (%p-flatten-slice-args — 8 users; found+fixed #394; merged s414) — **EXTRACT (Lisp)** | one `%p-flatten-slice-keys` (the "flatten vectors/lists in the key list" loop, 3× EXACT) — or a macrolet; sweep + bench (hash slices are common). |
| 22 | `ExprToCL.pm` · `gen_binary_op` (TEXT) ↔ `gen_binary_op_form` — the 25-line lvalue check | ~25 | **DELETED-BY Phase A4** | the text twin goes when v1's `->generate(` sites switch to forms. |
| 23 | `cl/pcl-runtime.lisp` · `%p---slow` / `%p-/-slow` / `%p-%-slow` / `%p-.-slow` / `%p-<=>-slow` / `%p-str-cmp-slow` — the "try overload on a, then on b, else coerce" 5-liner | ~25 | DONE `s413j` = `4c88bd9` (%with-binary-overload — 7 copies + %def-overloaded-arith; merged s414) — **EXTRACT (Lisp) — `define-slow-binop` macro** | six copies of the overload-dispatch prologue; a macro keeps the code identical after expansion (bench-neutral by construction).  Sweep (overload.t is the guard). |
| 31 | `ExprToCL.pm` · `_build_form_handlers`; `StringInterpolation.pm` · `_wrap_case_group`; `run-perl-suite.pl` · `timeout_for` | ~30 | **LEAVE** | three unrelated 5-line shapes glued by the SHAPE level (a table-building idiom); nothing to share. |
| 34 | `PExpr/TokenUtils.pm` · `is_arr_braces` / `is_hash_braces` / `is_inline_hash` / `is_inline_arr` (7 lines × 4) | ~21 | DONE `s413c` (5 predicates; PExpr calls them as functions; compile A/B neutral) — **COLLAPSE IN PLACE, no helper** | `is_hash_braces` is 176 k calls/sample — the hottest predicate in the compiler.  Each becomes a one-line `return ref($s) eq 'PPI::Structure::Subscript' && $s->start eq '{' ? 1 : undef;` (same truth table); NO shared sub (rule 4). |
| 37 | `cl/pcl-runtime.lisp` · `p-array-set` / `p-autoviv-aref-for-array` / `p-autoviv-aref-for-hash` | ~23 | DONE `s413m` = `a6bad40` (%p-extend-to inline, 6 sites; merged s414) — **EXTRACT (Lisp)** | the "extend the vector to index, filling with nil" 6-liner; `%p-extend-to`.  Sweep + bench (element writes are the hot path — `flet`/inline). |
| 38 | `InterpScan.pm` · `_scan_dollar` / `_scan_snail` | ~21 | DONE `s414d` (`_name_event`, FOUR copies not two: a deref and a plain one per sigil; corpus IDENTICAL) — **EXTRACT** | the ONE scanner's own two copies of "read a name after the sigil" — worth one `_scan_name_after` (InterpScan is the survivor of family 1). |
| 39, 43 | `Parser.pm` · `_subscript_key_cl_list` / `_subscript_key_groups`; `_process_include_statement` / `_process_variable_statement` | ~41 | **LEAVE — DELETED-BY E5.3** | — |
| 42, 46–48 | `cl/pcl-runtime.lisp` · `%p-symref-*`, `p-setf` arms | ~52 | DONE `s413n` = `637712b` (%p-symref-symbol); `s413o` = `71d2506` (p-setf twins); merged s414 — **EXTRACT (Lisp)** where the arm bodies are exact (`p-setf` has two 18-line EXACT pairs at `:4986/:5010` and `:5043/:5064` — a `macrolet` per pair); `%p-symref-array/hash/box` share the "split NAME into package + variable, find-or-make the package" prologue → one `%p-symref-package-and-name`. Sweep. |
| 44 | `ExprToCL.pm` · `_process_dq_escape` / `_process_tr_escape` (20 lines, 19 EXACT) | ~20 | DONE `s413i` (_decode_escape; found+fixed #393 first, `s413h`) — **EXTRACT** | the `\x{…}`/`\N{…}`/octal escape decoder; tr and dq differ only in which escapes are legal — one `_decode_escape($str, $i, %legal)`.  Cold. |
| 27 (Parser2 `_fix_interp_token` / `_interp_canon` / `_interp_names`, 8 lines EXACT ×3) | | ~25 | **SUPERSEDED — InterpScan consumer 2** | `_interp_names` is 1.65 s EXCLUSIVE in the 50 s sample (11 k calls) — the port is also the compile-time fix.  Until then, do not touch. |
| the tail (~380 clusters, ~5–12 lines each) | | ~1 700 | **rule 1–5 apply**; most are LEAVE (boilerplate) or v1 | re-run the tool; anything EXACT ≥ 8 lines in a surviving file is an EXTRACT candidate — check the family's verdict above first. |

## 3. Order of execution (Opus; each item its own commit)

**STATUS s413 (Fable): items 1–6 landed (`s413a`–`s413i` on `main`; the Lisp
families of item 6 on branch `s413-lisp-dedup`, verify+merge = task #395,
`docs/opus5-handoff-s413.md` §2).  Item 7 (families 2+10, 14/26) is
UNBLOCKED (Phase A done) and NEXT.  Item 1 (tools) would not be done today —
tools are OUT OF SCOPE since the USER's s413 ruling (§1 rule 7).**

Cold + surviving + IDENTICAL first, so the worklist starts landing without
waiting for the plan's phases:

1. Family 6 (`PCLProc.pm`) — runners; verdicts compared.
2. Family 5 (`_decl_syms_under`) — Parser2; **full sweep**.
3. Family 34 (collapse in place) — hot; corpus time measured.
4. Families 3, 4, 8 (PExpr mechanics) — one helper per commit; corpus-diff
   each; the maze rule (no behaviour change, ever).
5. Families 17, 20, 44 (ExprToCL cold helpers).
6. Lisp: 23 (`define-slow-binop`), 13, 21, 37, 42/46–48 — sweep + bench
   per commit; `tools/check-parens.lisp` after each edit.
7. **After Phase A**: families 2 + 10, then 14/26.
8. Family 1 + 15/33 + Parser2 27: **the InterpScan consumers 2 and 3
   port** — schedule as its own session (structural; #237's remaining
   halves).  Family 38 rides along.

Everything else: LEAVE, or DELETED-BY, as tabled.  When a phase of
`plan-one-compiler-s411.md` lands, re-run the census — the deleted files
drop out and the ranking shifts; do not carry this table's numbers
forward, carry the verdict rules.

## 4. What the census is NOT

Not a dead-code census (that was `docs/compiler-duplication-review-s386.md`
→ #303, done), not a call-count profile (that is `sub-call-census.pl`'s
other use — the `excl_s` column is the #213 worklist input), and not a
verdict on design duplication that no line-shingle can see (two
predicates that agree on qualifying lists, #266's sibling autoquote copies
— those stay where the s370/s374 rulings put them).
