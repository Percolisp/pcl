# PCL: Features Left To Implement

This document lists Perl features that are **planned for implementation**
but not yet done.  Unlike `docs/not-supported.md` (design decisions), every
item here is a real bug or missing feature worth fixing.

Items are grouped by tier and roughly ordered by number of tests affected.
Historical implementation notes live in `docs/session-log.md`.

---

## IR cleanups (deferred by user decision, 2026-07-07)

### Bare `print;`/`say;` should emit `(p-print $_)` explicitly

**What's inconsistent:** the transpiler materializes every implicit `$_`
operand at parse time (`add_implicit_default_param` — `uc;` → `(p-uc $_)`)
**except** bare `print`/`say`, which emit `(p-print)` and rely on a
runtime-side default inside `p-print` (the "bare `print;`" branch in
`cl/pcl-runtime.lisp`).  `docs/ir-spec.md` §1/§8 documents this as the sole
exception to "the tree is explicit".

**Fix:** route `print`/`say`'s empty-list case through the same parse-time
insertion as the named-unary family, then delete the runtime default.
Mind the filehandle forms: `print FH;` (`:fh` + empty list) and
`print { EXPR };` must also receive `$_`.  Emission-changing → bump
`*pcl-cache-generation*`, full parity sweep.  Update ir-spec.md's two
"sole exception" mentions when done.

---

## Infrastructure Bugs

### "Fully passing" files may be false positives — crash-before-failure masking

**What's broken:** `sweep-perl-tests.pl` counts a file as "fully passing" if
all tests it *runs* pass.  If the transpiled CL crashes (e.g. unbound variable)
before reaching a group of tests, those tests never execute and are never counted
as failures.  When the underlying crash is later fixed, the previously-hidden
tests are exposed and the file is no longer "fully passing".

**Example:** `for.t` had 129/0 because `++$Dog::VERSION` crashed (unbound var),
preventing tests 127–138 (typed for-loop via string eval) from running.  After
the cross-package `defvar` fix the crash stopped, tests 127–138 ran, and 9 of
them failed → for.t dropped to 129/9.

**Impact:** The "fully passing" count is an optimistic overcount.  Any fix that
stops an early crash may reveal new failures and *reduce* the fully-passing count.

**Fix area:** Consider making the sweep detect abnormal termination (non-zero
SBCL exit, missing TAP `1..N` header, or test count mismatch) and flag those
files distinctly from clean passes.

---

## Tier 1 — High value (many tests, clear root cause)

### `$SIG{__DIE__}` handler  (~50 tests)

**What's broken:** `pl-die` does NOT invoke `$SIG{__DIE__}` before
unwinding.  `$SIG{__WARN__}` is already invoked correctly by `pl-warn`.

**Fix area:** `cl/pcl-runtime.lisp` `pl-die` — call the handler before
unwinding.  Requires CL condition restarts to do cleanly.

---

### `bop.t` — prototype arg-limiting + string bitwise ops  (307/146)

**Status:** 307 passing / 146 failing (session 104), running in sweep.

**Root causes:**
1. **Prototype arg-limiting**: `sub _and($) { ... }` has prototype `($)`.
   PCL does not track user-defined sub prototypes, so `is _and 0, '0', 'str'`
   is transpiled as `(pl-is (pl-_and 0 "0" "str"))` — all args go to `_and`,
   leaving `is` with only the return value and crashing.
   **Fix area:** `Pl/Parser.pm` / `Pl/PExpr/Config.pm` — record `($)` etc.
   at sub definition time, limit args at call site.  `Pl/t/bop-01.t` has
   regression tests.

2. **String bitwise ops**: `"AAAAA" & "zzzzz"` returns `0` instead of
   character-by-character AND result (tests 21–26).
   **Fix area:** `p-band`/`p-bor`/`p-bxor` in `cl/pcl-runtime.lisp`.

3. **`$SIG{__WARN__}` counting**: Tests 202, 215 count warnings via
   `$SIG{__WARN__}` — blocked on the handler protocol.

4. **`~.` string-force operator** (tests 172, 186): PPI tokenizes `~.22`
   as `~` + `.22` (float).  Needs a parse fix in `Pl/PExpr.pm`.

---

## Tier 2 — Medium value (tens of tests, clear approach)

### Wire tie for hashes/arrays (TIEHASH/TIEARRAY element dispatch)  (~1 session each)

**Status (probed session 247):** `tie` is one-quarter done, and the missing
three quarters are mechanical, not architectural.

- **TIESCALAR WORKS end-to-end.**  The `p-tie-proxy` lives in the scalar's
  box; `unbox` → FETCH, `box-set` → STORE.  Verified: a self-incrementing
  FETCH counter and STORE both match perl.  (Same hook `p-magic-cell` reuses
  for `\substr`/`\pos`/`\vec`.)
- **TIEHASH/TIEARRAY: the proxy IS created but never consulted.**  `p-tie`
  (cl/pcl-runtime.lisp ~9737) already dispatches to TIEHASH/TIEARRAY by
  container type and stores the proxy — but the element primitives
  (`p-gethash`, `(setf p-gethash)`, `p-delete`, `p-exists`, `p-keys`,
  `p-each`, and the `p-aref`/`p-array-set`/push/pop/shift/unshift/splice
  family) never check for it.  A tied `$h{k}` read returns nothing instead
  of calling FETCH.
- **TIEHANDLE: absent.**

**Fix shape:** add a `p-tie-proxy-p` arm to each container primitive — the
exact pattern the `%ENV-MARKER%`/`%INC-MARKER%`/stash arms already use at
the same chokepoints.  Method map: hash FETCH/STORE/EXISTS/DELETE/CLEAR +
FIRSTKEY/NEXTKEY (drives `keys`/`each`); array FETCH/STORE/FETCHSIZE/
STORESIZE/PUSH/POP/SHIFT/UNSHIFT/SPLICE/EXTEND.  Estimate ~1 session per
container kind; TIEHANDLE smaller and rarer.

**Follow-ups once wired (sweep catalog):** double-FETCH ordering
(`$tied || $var` TODO row), `local.t`'s hang via the real Tie::Array.

**Constraint:** DESTROY is permanently not supported (user decision
2026-06-12, see not-supported.md) — tie tests that need DESTROY-on-untie
stay skipped; untie itself just detaches the proxy.

---

### `prototype()` function  (signatures.t, small)

**What's broken:** `prototype(\&foo)` should return the prototype string
for `&foo`, or `undef` if none.  Currently always returns `undef`.

**Fix:** A stub returning `*pl-undef*` unconditionally is already correct
for the common guard pattern; the only missing piece is returning the actual
string.  Storing prototype strings requires threading them from parse time
to a runtime lookup table.

---

## Tier 3 — Low value or high complexity

### Named inner sub closures  (uncommon)

**What's broken:** `sub outer { my $x = 1; sub inner { $x } }` — `inner`
is emitted as a global `pl-sub`, not a closure over `$x`.  The `__lex__`
renaming from session 63 only helps anonymous subs (lambdas).

**Fix:** Detect that a named inner sub references outer-scope lexical vars;
generate a closure stored in a package variable instead of a bare `pl-sub`.

---

### Flip-flop operator `..` in scalar/boolean context  (flip.t — 3 tests)

**What's broken:** In scalar (boolean) context `if ($. == 1 .. $. == 5)`
is a stateful flip-flop that toggles on when the LHS becomes true and off
when the RHS becomes true.  Currently generates a range (wrong).

**Fix:** `docs/v1-implementation-plan.md` section C4 has the full plan:
per-call-site `*flip-state-N*` defvar + `p-flip-flop` macro.

---

### `qr//` objects — remaining failures  (qr.t — 17 tests)

**Status:** Basic `qr//` works (stringify → `(?^mods:pat)`, `p-reftype` →
`"REGEXP"`).  17 remaining failures in `qr.t` are blocked on `use overload`
(overloaded comparisons and tie interaction).  `use overload` is now implemented
(session 116) — these may improve; re-sweep needed.

---

### `DESTROY` method (object finalizers)

**What's broken:** Perl calls `DESTROY` when an object is garbage-collected.
PCL does not hook into SBCL's finalizer system.

**Fix:** Register a `trivial-garbage:finalize` callback for each blessed
hash-table that has a `DESTROY` method.

---

### `concat2.t` — likely resolved (re-sweep needed)

**Status:** All 3 tests use overloaded string concatenation.  `use overload`
is now implemented (session 116) including `"."` dispatch.  Re-sweep expected
to show these passing.

---

### `ref(\$h{key})` returns wrong type when slot holds a reference  (pre-existing)

**What's broken:** `\$h{key}` where `$h{key}` is itself an array/hash ref
should give `ref()` = `"REF"` (a reference-to-a-scalar).  PCL returns `"ARRAY"`
or `"SCALAR"` instead, and `$$nested->[0]` returns undef.

**Root cause:** `p-backslash(slot-box)` produces `outer-box → slot-box → raw-vector`,
which is structurally identical to `\[10,20]` producing `outer-box → arrayref-box → raw-vector`.
`p-ref` peeks two levels deep, sees the vector, and cannot tell whether the intermediate
box is "a scalar slot holding an array-ref" vs "the array-ref itself."

**Fix area:** `p-backslash` / `p-ref` — either mark the box produced by
`p-gethash-box`/`p-aref-box` paths with a distinct tag, or introduce a
`p-backslash-lvalue` variant that wraps differently so `p-ref` can distinguish
"ref to scalar container" from "ref to array/hash directly."

**Scope:** Rare in real CPAN code (`\$h{key}` is unusual; just returning `$h{key}`
is the normal pattern).  Basic write-through (`$$ref = 42; $h{key} == 42`) works correctly.

---

## Known Warnings / Minor Bugs

### `indent_level` going negative — "Negative repeat count does nothing"

**Symptom:** During transpilation of some inputs:
```
Negative repeat count does nothing at Pl/Parser.pm line 3695.
Negative repeat count does nothing at Pl/ExprToCL.pm line 253.
```
Both lines are `"  " x $self->indent_level`.

**Root cause:** `indent_level` is decremented below zero somewhere — a
block-close decrements without a matching open, or indent state is not
properly saved/restored across recursive calls.

**Impact:** Cosmetic only — the `x` operator silently produces `""` for
negative counts.  Generated CL is correct.

**Fix area:** Guard every `indent_level` decrement in `Pl/Parser.pm` and
`Pl/ExprToCL.pm` so it never goes below 0.  Audit `parse_block_to_cl_string`
and similar recursive helpers that save/restore indent state.

---

## Summary table

| Feature | Tests affected | Tier |
|---------|---------------|------|
| ~~`use overload`~~ | ~~1100~~ | ✅ done session 116 |
| `$SIG{__DIE__}` handler | ~50 | 1 |
| `bop.t` (prototype + string bitwise) | 307/146 | 1 |
| `prototype()` | small | 2 |
| Named inner sub closures | small | 3 |
| Flip-flop `..` in scalar context | 3 (flip.t) | 3 |
| `qr//` remaining (re-sweep needed) | 17? | 3 |
| `DESTROY` finalizers | rare | 3 |
| `concat2.t` (re-sweep needed) | 3? | 3 |
| `indent_level` negative (cosmetic) | — | — |
