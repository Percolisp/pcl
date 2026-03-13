# PCL Built-in Naming: Fix for Tie::Array Infinite Recursion

## The Problem

CL is case-insensitive. Perl's built-in `push` and Perl's Tie interface method `PUSH`
both get the `pl-` prefix in PCL's generated code:

- Built-in call: `push @arr, 1` → `(pl-push @arr 1)` → CL symbol `PL-PUSH`
- Tie method def: `sub PUSH { push(@$o,@_) }` → `(pl-sub pl-PUSH ...)` → CL symbol `PL-PUSH`

Same symbol. The `shadow` in `pl-sub` prevents the user method from clobbering the
global built-in, but it also makes all subsequent unqualified `pl-push` references
in that package resolve to the user's local method — including the `push(@$o, @_)` call
inside `PUSH`'s own body, and `(pl-push @ISA "Tie::Array")` elsewhere in the same file.
Result: infinite recursion, process hang.

Affects: `reverse.t`, `local.t`, `sort.t`, `kvaslice.t` (~200+ tests blocked).

The collision-prone Perl names (Tie interface mirrors built-ins by design):
- Array tier: `PUSH`, `POP`, `SHIFT`, `UNSHIFT`, `SPLICE`
- Hash tier: `EXISTS`, `DELETE`
- Filehandle tier: `PRINT`, `READLINE`, `WRITE`

Regular OO code can also trigger this with natural method names: `$stack->push()`,
`$cache->delete()`, `$fh->print()`.

---

## Alternative Approaches

### Option A — Rename PCL built-ins: `pl-` → `p-` prefix ★ Recommended

Rename all PCL runtime built-in functions from `pl-*` to `p-*`. User-defined Perl
methods keep their existing `pl-METHODNAME` naming convention.

**How it fixes the problem:**
- `p-push` → CL symbol `P-PUSH`
- `pl-PUSH` → CL symbol `PL-PUSH`
- `P-PUSH` ≠ `PL-PUSH` — different names, no collision, no shadow issue.

**Generated code before/after:**
```lisp
; Before:
(pl-push @ISA "Tie::Array")
(pl-sub pl-PUSH (&rest %_args)
  (p-push (pl-cast-@ $o) @_))   ; <-- body calls built-in

; After:
(p-push @ISA "Tie::Array")      ; built-in call
(p-sub pl-PUSH (&rest %_args)   ; p-sub is the macro; pl-PUSH is the user method name
  (p-push (p-cast-@ $o) @_))   ; built-in call — different symbol from pl-PUSH ✓
```

**Scope of change:**
- `cl/pcl-runtime.lisp`: rename ~340 `defun`/`defmacro` definitions + `:export` list
- `Pl/ExprToCL.pm`: `OP_EXCEPTIONS` values (`pl-` → `p-`); `cl_name()` prefix logic
  — needs to distinguish built-in calls (`p-`) from user function calls (`pl-`)
  via a static `%BUILTIN_NAMES` set derived from Config.pm's `known_no_of_params`
- `Pl/Parser.pm`: ~40 hardcoded built-in emissions (`pl-push` → `p-push`, etc.)
  User sub name generation in `_qualified_sub_to_cl()` is unchanged.
- `Pl/t/*.t`: ~13 test files, update expected output strings (mechanical)

**What doesn't change:**
- User method names in generated code: `pl-PUSH`, `pl-new`, `pl-foo` — unchanged
- Package-qualified calls: `|Foo::Bar|::pl-method` — unchanged
- Variable names, special vars — unchanged

**Pros:** Clean generated code, permanent fix, `p-` vs `pl-` visually distinguishes
built-in calls from user method names at a glance.

**Cons:** Large rename (~340 symbols in runtime). All 13 test files with expected
output need updating. Slightly less obvious than `pl-` that `p-` means "Perl built-in".

---

### Option B — `pcl:` package qualification in generated code

Add `pcl:` qualifier to every built-in call in generated code:
`(pl-push ...)` → `(pcl:pl-push ...)`. No rename of runtime functions needed.

**Generated code:**
```lisp
(pcl:pl-push @ISA "Tie::Array")
(pl-sub pl-PUSH (&rest %_args)
  (pcl:pl-push (pcl:pl-cast-@ $o) @_))  ; always refers to pcl package
```

**Scope of change:**
- `Pl/ExprToCL.pm`: `cl_name()` returns `"pcl:pl-$name"` for built-in calls
- `Pl/ExprToCL.pm`: `OP_EXCEPTIONS` values add `pcl:` prefix
- `Pl/Parser.pm`: ~40 hardcoded emissions add `pcl:` prefix
- `cl/pcl-runtime.lisp`: no changes needed

**Pros:** Runtime unchanged. Smaller scope than Option A.

**Cons:** `pcl:` prefix everywhere makes generated CL code significantly noisier and
less readable. Breaks the "Perl programmers can read the output" design goal. Every
built-in call in every generated file would have `pcl:` prefix.

---

### Option C — Rename user method convention (no `pl-` prefix for user methods)

Instead of changing the built-ins, change how user-defined Perl methods are named in CL.
User methods get no prefix (or a different prefix like `pm-`):

- `sub PUSH` in Tie::StdArray → CL name `PUSH` (or `pm-PUSH`)
- Built-ins keep `pl-push`
- `pl-method-call` dispatch looks up `PUSH` (or `pm-PUSH`) instead of `pl-PUSH`

**Generated code:**
```lisp
(pl-push @ISA "Tie::Array")     ; built-in — unchanged
(pl-sub PUSH (&rest %_args)     ; user method — no prefix
  (pl-push (pl-cast-@ $o) @_)) ; built-in — no collision ✓
```

**Scope of change:**
- `Pl/Parser.pm`: `_qualified_sub_to_cl()` drops the `pl-` prefix for user methods
- `cl/pcl-runtime.lisp`: `pl-method-call` and `pl-super-call` change how they look up
  method symbols (currently uses `format nil "PL-~A" method-name`)
- `pl-sub` macro: no longer needs `shadow` (since user methods don't inherit from `:pcl`)
- `Pl/t/*.t`: update expected output strings (user method names change)

**Pros:** Runtime built-in functions unchanged. `pl-sub` simplifies (drop `shadow`).
The `PUSH`/`push` distinction is now visible as `PUSH`/`pl-push` in CL.

**Cons:** Bare `PUSH` (or `pm-PUSH`) is less readable than `pl-PUSH` for Perl developers.
The OO dispatch mechanism needs updating in the runtime. Need to ensure bare `PUSH` doesn't
conflict with a CL built-in or commonly used name. All test files with user method
expectations need updating.

---

### Option D — Store user methods in a hash table (no symbol-based dispatch)

Keep ALL built-in names as `pl-*`. Store user-defined Perl methods in a per-package
hash table instead of as Lisp function symbols:

```lisp
; User method storage:
(defvar *user-methods* (make-hash-table))  ; or per-package

; pl-sub stores method in hash, not as symbol:
(setf (gethash (cons pkg 'PUSH) *user-methods*) (lambda ...))

; pl-method-call looks up in hash:
(gethash (cons class method-name) *user-methods*)
```

Direct built-in calls `(pl-push ...)` always resolve to `pcl:pl-push` (inherited,
no shadow). No name collision possible.

**Pros:** Clean architectural separation. No `shadow` needed. No naming convention
change. Direct built-in calls are always unambiguous.

**Cons:** Major refactor of the OO system (method definition, dispatch, MRO walk all
change). Performance overhead (hash lookup instead of symbol function lookup, though
this can be cached). Significant risk of regressions. Not a quick fix.

---

### Option E — Qualify ONLY the ~10 collision-prone names

A targeted version of Option B: only add `pcl:` to the specific built-in names that
appear in Perl's Tie interface and common OO patterns:
`pl-push`, `pl-pop`, `pl-shift`, `pl-unshift`, `pl-splice`, `pl-exists`, `pl-delete`,
`pl-print`, `pl-readline`.

Everything else stays unqualified. The qualification is global (all generated code),
not package-specific.

**Pros:** Minimal change. Only ~9 names affected in generation logic and tests.
Generated code mostly readable with just a few `pcl:` qualifications.

**Cons:** Fragile — future collisions with other names would require adding more
qualifications. Still adds `pcl:` noise, just less of it. Not a complete fix.

---

## Recommendation

**Option A (`p-` prefix)** is the recommended approach because:
1. Generated code stays clean and readable
2. The `p-` vs `pl-` visual distinction is informative (built-in vs user method)
3. It's a complete fix — no future collisions possible regardless of method names
4. The rename is mechanical and scriptable

**Option C (no prefix for user methods)** is a reasonable alternative if the rename
scope of Option A is too large. It changes fewer files overall but requires updating
the OO dispatch mechanism.
