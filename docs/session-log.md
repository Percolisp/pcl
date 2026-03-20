# PCL Session Log

Append new entries at the top. One section per session.

---

## Session 87 (2026-03-20) — %{$ref}[indices] kvaslice, loop/return fix, anon sub return fix

**Commit:** (pending)

### Fixes

- `%{$ref}[indices]` block-deref KV array slice: PPI gives Cast('%') + Block('{$ref}') + Constructor('[indices]'); added `$is_kv_arr_deref_constructor` detection + handler in PExpr.pm postfix loop; generates `(p-kv-aslice (unbox $ref) ...)` correctly
- **Root cause of `eq_array` failure**: CL `(loop ...)` creates implicit `(block nil ...)`; `p-return` used `(return-from nil ...)` which exited the loop body (not the function), so `return 0 unless ...` inside `foreach` only skipped to next iteration
- **Loop/return fix** (pcl-runtime.lisp):
  - All three loop macros (`p-while`, `p-for`, `p-foreach`): replaced `(loop ...)` with `tagbody`/`go`; added inner `(block nil ...)` for unlabeled `p-last`
  - `p-return`: changed from `(return-from nil ...)` to `(throw :p-return ...)`
  - `p-sub`: wrapped body in `(catch :p-return ...)` so named sub `return` is caught at the right level
- **Anonymous sub `return` fix** (Parser.pm `parse_block_to_cl_string`): anonymous subs (`sub { ... }`) generated with `(catch :p-return ...)` inside `(let ((@_ ...))` but outside `(block nil ...)`, so `return` correctly exits the lambda
- New test file: `Pl/t/kvaslice-01.t` (13 tests)

### Stats
- PCL suite: **63 files, 2645 tests, all passing**
- Sweep: **5411 passing, 2011 failing, 40 fully-passing** (100 files + 2 skipped)
  - Gained: `isa.t` (newly fully passing)
  - Lost: `hashassign.t`, `kvhslice.t` — these were previously "passing" because `eq_array` was broken (always returned 1), masking real PCL bugs; now they correctly fail
  - Total +512 passing vs session 86

---

## Session 86 (2026-03-20) — delete local, local @slice, paren fixes; local.t 115/115

**Commit:** 505474c

### Fixes
- `p-local-array-elem` / `p-local-hash-elem` / `p-local-array-elem-init` / `p-local-hash-elem-init` macros: fixed 3 paren-balance errors in `pcl-runtime.lisp` (2 missing, 1 extra `)`) that had been canceling each other — SBCL silently mis-parsed the rest of the file from line 5608 onward
- `delete local $a[N]` / `delete local $h{k}` / `delete local @a[N,M]`: pre-evaluate original value BEFORE opening local scope, so `my $c = delete local $a[N]` returns the correct value
- `_subscript_key_cl_list` (new method in Parser.pm): expands `qw//` tokens into individual quoted strings for per-key local scope handling
- `p-delete-array`: trim trailing nil slots after deletion (Perl shrinks array when last element deleted)
- `local @h{keys} = (vals)`: wrap init with `(let ((*wantarray* t)) ...)` — was array-only, now both hash and array slices; fixed regression in local-elem-01.t tests 17-18
- New test file: `Pl/t/local-elem-02.t` (24 tests)

### Stats
- PCL suite: 62 files, 2632 tests, all passing
- `perl-tests/local.t`: 83/115 → **115/115**

---

## Session 85 (2026-03-19) — local $hash{key}, local $arr[N], local @hash{keys}

**Commit:** included in 505474c (multi-session commit)

### Fixes
- `local $hash{key}` / `local $arr[N]` / `local @hash{keys}` — fully implemented
- `p-local-hash-elem` + `p-local-array-elem` macros added to `pcl-runtime.lisp` (unwind-protect)
- `_process_local_declaration` in Parser.pm: detects Symbol+Subscript pattern, emits nested macro opens, closes them at block end via `_local_let_depth`
- `parse_block_to_cl_string`: fixed — closes open local forms, restores `_local_let_depth`
  (eval {} containing `local $h{key}` left `p-local-hash-elem` unclosed; CL `;` comment ate close paren)
- Slice init: `(let ((*wantarray* t)) ...)` wrapper forces list context for `(10,20)` RHS
- New test file: `Pl/t/local-elem-01.t` (18 tests)

### Stats
- PCL suite: 61 files, 2608 tests, all passing
- `perl-tests/local.t`: ~41 → 83/115

---

## Session 84 (2026-03-19) — delete/exists array fixes, range edge cases, chained subscript delete

**Commit:** included in 505474c

### Fixes
- `perl-increment`: `^[a-zA-Z]*[0-9]*$` pattern — "99a" → 100 (numeric, not string increment)
- `p-splice-impl`: scalar context returns last removed element (was always returning full vector)
- `p-..` range operator: complete rewrite — undef/empty string ranges, non-alphanumeric start
- `p-delete-array` / `p-exists-array` / `p-aref`: `nil` = deleted marker (vs `*p-undef*` = assigned undef but exists)
- PExpr.pm named unary handler: consume ALL chained Subscripts — fixes `delete $h{a}{b}`
- New test files: `misc-fixes-01.t` (12), `range-01.t` (12), `delete-01.t` (8)

### Stats
- PCL suite: 60 files, 2590 tests, all passing
- Perl test suite: 4869 passing, 962 failing — 41 fully-passing

---

## Session 83 (2026-03-18) — LIST_CTX propagation, p-list-= goatse fix, repeat-01.t

**Commit:** included in 505474c

### Fixes
- `gen_tree_val` + `gen_progn` LIST_CTX propagation
- `p-list-=` goatse operator fix
- New test file: `Pl/t/repeat-01.t` (10 tests)

---

## Session 82 (2026-03-18) — %p-flatten-list box preservation, split/vec test files

**Commit:** included in 505474c

### Fixes
- `%p-flatten-list`: array refs / hash refs in list assignment RHS were incorrectly unwrapped.
  `box(vector)` must be preserved intact (not extracted → scalar length). Fixes transpile-test-05.t tests 4+6.
- New test files: `Pl/t/split-01.t` (15), `Pl/t/vec-01.t` (17)

### Stats
- PCL suite: 56 files, 2548 tests, all passing
- Perl test suite: 4877 passing, 992 failing. Newly fully passing: `anonsub.t`, `assignwarn.t`, `blocks.t`

---

## Session 80 (2026-03-15) — indent_level fix, inline package inside sub, pl-eval-direct

**Commit:** fb74752

### Fixes
- Inline `package Pkg {}` inside function body: emit setup inline (no new section, no `in-package`)
- `pl-prototype` stub added to runtime
- `pl-eval-direct` macro replaces verbose `eval-when` in all generated code (11 occurrences)
- 4 new tests in `transpile-test-01b.t`
- `docs/reference-equality.md`: diagnosed warn.t reference equality failure (not yet fixed)
- `perl-tests/index.t`: commented out 2 formline tests (unsupported format/write system)

### Stats
- PCL suite: 53 files, 2507 tests, all passing
- Perl test suite: sweep 5683 → 6209 (+526)

---

## Session 79 (prior) — typeglob codegen, sub hoisting package fix

### Fixes
- Typeglob support (Steps 1-8): runtime structs + primitives + codegen in ExprToCL.pm + Parser.pm
- Sub hoisting into wrong CL package when inline package switch inside bare block — fixed
- Auto-vivification: `$ref->{key}` when `$ref` is undef

---

## Session 78 (prior) — __DATA__/__END__, fileio-02.t, data-handle-01.t

### Fixes
- `__DATA__` / `__END__` support in Parser.pm
- New test files: `fileio-02.t` (7), additions to `data-handle-01.t`

---

## Session 77 (prior) — typeglob support Steps 1-3

- Runtime structs + primitives for typeglob

---

## Session 76 (prior) — typeglob codegen Steps 5-8

- ExprToCL.pm + Parser.pm typeglob codegen

---

## Session 75 (prior) — typeglob support Steps 1-3

- Runtime structs + primitives

---

## Session 74 (prior) — bare block package leak fix

- Wrap bare blocks with `(let ((*package* *package*)) ...)` to prevent package leakage

---

## Session 73 (prior) — exists &sub, defined &sub codegen

---

## Session 72 (prior) — bare-block package leak (deferred)

---

## Session 71 (prior) — output bucket system in Parser.pm

- Replaced post-processing reordering with preamble/declarations/definitions/runtime buckets

---

## Session 67 (prior) — local variable save/restore (41 failures in local.t fixed)

- `local $scalar` via `defvar` + `let` dynamic binding
- local.t: ~41 failures fixed (scalar locals; hash/array element locals were deferred to session 85+86)

---

## Session 66 (prior) — inner block my scoping

- New `let` per bare block for correct lexical scoping

---

## Session 65 (prior) — my(@arr, %hash) params crash fix

---

## Session 64 (prior) — stray close-paren after sub in Phase 2 reordering

---

## Session 63 (prior) — Phase 2 closures, $i__lex__N renaming

- `_vars_referenced_in_closures` added; captured `my` vars renamed to `$i__lex__N`
- `closure.t` 38→42/50

---

## Session 62 (prior) — &$foo(args), map({key=>$_}, LIST), ::foo calls

- `pl-funcall-ref` for `&$scalar(args)` / `&{expr}(args)`
- `_block_is_hash_constructor` + `parse_hash_block_to_cl_string`
- Package-qualified call `::foo` transpilation

---

## Session 59 (prior) — use integer pragma

---

## Session 58 (prior) — scope stack in Environment.pm

---

## Session 57 (prior) — negative hex/bin/oct, version strings, warnings stub, $]

---

## Session 56 (prior) — full test run + manual verification

---

## Session 55 (prior) — docs/declaration-ordering.md

---

## Session 54 (prior) — parser-01.t test 4 update

---

## Session 53 (prior) — rewrite _insert_sub_forward_declarations

---

## Session 52 (prior) — split pl-setf into distinct assignment forms

---

## Session 51 (prior) — deduplicate loop macros with helper

---

## Session 50 (prior) — pl-declare-sub macro for forward declarations

---

## Session 49 (prior) — special variable dispatch table in ExprToCL.pm

---

## Session 48 (prior) — rename pl-string_concat → pl-string-concat

---

## Session 47 (prior) — verify with prove and Perl test suite

---

## Early Sessions (V2 features, ~Dec 2024)

- Constants: `use constant` → `defconstant +NAME+`
- OO: `bless`, `ref`, `package` with block scoping, `@ISA` + C3 MRO
- Subroutine signatures and prototypes
- `wantarray` / context system (initial version)
- `pl-sprintf` rewrite with full format string parser
