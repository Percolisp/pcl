# What happens next in v2 — a browsing guide

*The human-readable companion to `docs/v2-opus5-execution-plan.md` (the
executor's instructions) and `docs/v2-endgame-plan.md` (the plan).  One
card per work package: what you'll notice, which subs change, and a
before → after where it helps.  Written s316t.*

---

## W0 · The splitter fix — **before R1** (1 session)

**What you'll notice:** three Perl shapes that today compute the wrong
value — or silently *delete code* — start matching perl.

| shape | today | after |
|---|---|---|
| `my $x = 1, $y = 2;` | `$x` gets **2** | `$x` gets 1, `$y` (global) gets 2 |
| `my ($a) = @_, g();` | **`g()` never runs** (silently dropped) | `g()` runs |
| `state $s = f(), h();` in a sub | tail folded into the init | init `f()`, then `h()` |

**Subs that change** (all `Pl/Parser2.pm`):
- `_tail_below_assign_prec` → generalized into a new shared helper
  **`_split_at_lowprec`** — the one place that knows "`,`/`or` bind
  looser than `=`".
- `_single_scalar_decl` — stops slicing "everything after `=`" as the
  init; calls the splitter.
- the `state $x = INIT` peel inside the block-decl path — same.
- `_extract_params` — the `my (LIST) = @_;` sub-header recognizer becomes
  exact-arity, so it can never eat trailing code again.
- New test battery in `Pl/t/transpile-test-07.t`.

*Background: this is the family of the bug fixed in s316t
(`$a = readlink 'x', 'y'`).  Full analysis: `docs/v2-code-review.md` §2.*

---

## W1 · The release window — until R1 ships

No structural changes.  Fix whatever the release checks surface, and
keep harvesting near-green files from the perl-suite snapshot
(`baselines/perl-suite-run.tsv`).  Likely next: `lib/Tie/Hash.pm` +
`lib/Tie/Scalar.pm` shims (`Tie::StdHash`/`Tie::StdScalar` — clears
`op/avhv.t`, `op/warn.t`), an `Internals::getcwd` shim
(`io/getcwd.t`).  Shim files only — no compiler changes.

---

## W2 · E4.1: one pipeline — **after R1 ships** (1–2 sessions)

**What you'll notice:** `PCL_V1=1` stops existing.  Every file compiles
through v2 or dies with a named reason — there is no quiet fallback left,
and the module cache drops its pipeline dimension.

**What changes:**
- `pl2cl`: bundle mode ported off `Pl::Parser->parse_file` (its last
  direct v1 call); then `parse_with_fallback`, `PCL_V1`, `PCL_V1_FILES`
  deleted.
- `cl/pcl-runtime.lisp`: `p-compute-cache-path` loses the v1/v2 branch.
- `Pl/Parser.pm`: only the file-level chunks go — `parse`,
  `_assemble_output`, `_insert_variable_forward_declarations`,
  `parse_file`/`parse_code` (~550 lines).  **The rest of Parser.pm
  stays** — it is still reached through the per-statement seams; it
  shrinks in E5.3, not here.

---

## E5.1 · The seam object (1–2 sessions)

**What you'll notice:** nothing — this is a pure refactor (corpus-diff
must be byte-empty).  It replaces the most bug-prone copied code in the
compiler.

**What changes:** a new **`Pl::SeamSession`** guard object absorbs, from
`Pl/Parser2.pm`:
- the two hand-written bucket save/reset/drain/restore dances (in
  `_lower_expr`'s fallback branch and `_fallback_stmt_capture`);
- the eight save/restore pairs around `_let_bound_vars` (in
  `_lower_sub`, `_lower_scope`, the if/while/for/foreach condition
  scopes, `lower_embedded_block`, `_lower_embedded_anon`);
- the `_v2_embed` arming rules and the `_local_let_depth` /
  `_block_depth` juggling.
Restore happens when the guard goes out of scope — an exception can no
longer leak parser state.

---

## E5.2 · Embedded blocks lowered structurally (1–2 sessions)

**What you'll notice:** the last text-generated Lisp disappears from the
output (anon-sub/map/grep bodies that still come out as pre-rendered
strings today).

**What changes:** `lower_embedded_block` (Parser2) handles every block
shape instead of declining; then these v1 text producers are deleted:
`parse_block_to_cl_string`, `parse_block_as_function`,
`parse_hash_block_to_cl_string`/`_form` (`Pl/Parser.pm`), and the four
`raw(...)` escape-hatch sites in `Pl/ExprToCL.pm` that carried their
output.  (This is task #78's long-standing tail.)

---

## E5.3 · The fallback burn-down (4–8 sessions) — *where Parser.pm actually shrinks*

**What you'll notice:** `Pl/Parser.pm` loses thousands of lines, one
statement family at a time; each commit deletes a `_process_*` handler.

Twelve statement classes still route whole statements through v1
(`_fallback_stmt`).  Each session natively lowers 2–3 of them and
deletes the v1 handler in the same commit:

| class | v1 handler that dies |
|---|---|
| loop statement-modifiers (`EXPR while COND` …) | part of `_process_expression_statement` |
| multi-element `return $a, $b` | same |
| `goto`/`next`/`last`/`redo` | same |
| anon-hash-as-bare-block | `_bare_block_is_anon_hash` path |
| `local` / `delete local` | `_process_local_declaration` |
| `my` with while/for modifier | `_process_variable_statement` |
| `use`/`require`/`no` | `_process_include_statement` |
| `BEGIN`/`END`/`CHECK` blocks | `_process_scheduled_block` |
| subs with prototypes/signatures | `_process_sub_statement` |
| two nested named-sub shapes | same |
| eval-mode residue | misc |

---

## E5.4 · One expression brain (2–3 sessions)

**What you'll notice:** simpler internals, and future calling-convention
work costs half as much (task #131 had to be implemented twice — that
class of double work ends here).

**What changes:** `Pl/ExprToCL2.pm` (the 264-line native-subset emitter)
is folded into the main emitter as early-return branches in
`gen_node_form`, sharing one operator table; then the text half of
`Pl/ExprToCL.pm` is deleted — `gen_node`, `gen_internal_node_text`, ~36
string-returning `gen_*` subs, and `raw`/`raw_wrap` in `Pl/CLForm.pm`.
After this, the emitted tree is *closed*: an optimization pass can walk
it exhaustively.

---

## E5.5 · Shared predicates and the file split (1–2 sessions)

**What you'll notice:** the compiler becomes navigable for outsiders.

**What changes:**
- `native_root_write` — one predicate shared by `Pl/VarAnnotator.pm` and
  the statement fast path, replacing the comment-coordinated "these must
  agree" contract.
- One set of context constants replacing the three parallel encodings
  (v2's `':void'`/`'inherit'` strings, PExpr's 0–3 integers).
- `Pl/Parser2.pm` (6,500 lines) split along its real phase boundaries:
  pre-pass rewrites / lexical facts & promotion / statement lowering /
  seam.  `CODEGEN_DESIGN.md` rewritten against the final shape.

---

## After that

The target picture — data structures, and how an optimization plugs in
as a registered pass (`docs/v2-target-architecture.md`).  The queued
perf items land as the first passes: method-dispatch inline cache (#73),
pack/sprintf template memoization (#74), return-family transfer (#77).

**Total: ~16–29 sessions post-R1, every step individually shippable.**
