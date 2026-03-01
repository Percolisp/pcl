# `tie` Implementation

## What `tie` does

`tie VARIABLE, CLASSNAME, LIST` binds a variable to an object. Every subsequent
access to the variable invokes a method on that object instead of reading/writing
memory directly:

| Variable | Constructor  | Read          | Write              | Iterate                      |
|----------|-------------|---------------|--------------------|------------------------------|
| Scalar   | `TIESCALAR` | `FETCH`       | `STORE(val)`       | —                            |
| Array    | `TIEARRAY`  | `FETCH(idx)`  | `STORE(idx,val)`   | `FETCHSIZE`, `EXISTS`, etc.  |
| Hash     | `TIEHASH`   | `FETCH(key)`  | `STORE(key,val)`   | `FIRSTKEY`, `NEXTKEY`, etc.  |
| Handle   | `TIEHANDLE` | `READ`/`READLINE` | `WRITE`/`PRINT` | —                          |

`tied(VAR)` returns the tie object (or undef). `untie VAR` removes the tie,
calling `UNTIE` then `DESTROY` on the tie object.

## CPAN value

`tie` is foundational to many widely-used modules:

- **`Readonly`** — read-only variables via TIESCALAR/TIEARRAY/TIEHASH
- **`DB_File`, `GDBM_File`, `NDBM_File`** — tie %hash to on-disk databases
- **`Env`** — tie %ENV keys to real environment variables
- **`Tie::IxHash`** — insertion-ordered hash (used by many modules)
- Various logging/monitoring modules that intercept `$_`

## Current state

Stubs exist in `cl/pcl-runtime.lisp` (~line 5126): `pl-tie` warns and returns
undef, `pl-untie` returns 1, `pl-tied` returns undef. All args ignored.

Codegen is already correct for the scalar case:
- `tie $x, "Foo", 1` → `(pl-tie $x "Foo" 1)` — `$x` evaluates to the pl-box ✓
- `tie @arr, "Foo"` → `(pl-tie @arr "Foo")` — `@arr` evaluates to the raw CL
  vector (not a box). This is the core difficulty for Phase 2.
- `tied($x)` → `(pl-tied $x)` ✓

`tie`, `untie`, `tied` are in `Pl/PExpr/Config.pm` `known_no_of_params` (~line 414).

## Architecture

### The key insight: scalars are already boxed

`$x` is stored as a `pl-box` wrapping the actual value. `pl-tie` receives the
box (because `$x` evaluates to its box). We can replace the box's contents with
a `pl-tie-proxy` sentinel struct, and then intercept FETCH/STORE at the two
points where boxes are read (`unbox`) and written (`box-set`).

Arrays and hashes are *not* boxed — they are raw CL vectors/hash-tables stored
directly in CL special variables. This requires a different approach (see Phase 2).

### Phase 1: Scalar tie

**New struct:**

```lisp
(defstruct pl-tie-proxy
  tie-obj       ; the object returned by TIESCALAR
  saved-value)  ; value in box before tie was installed (restored on untie)
```

**`pl-tie`** — installs the proxy:

```lisp
(defun pl-tie (box classname &rest args)
  (let* ((constructor (cond
           ((vectorp (unbox box)) "TIEARRAY")
           ((hash-table-p (unbox box)) "TIEHASH")
           (t "TIESCALAR")))
         (tie-obj (unbox (pl-method-call classname constructor args)))
         (proxy (make-pl-tie-proxy :tie-obj tie-obj
                                   :saved-value (pl-box-value box))))
    (setf (pl-box-value box) proxy)
    box))
```

**`unbox`** — intercepts FETCH (one cheap type-check added to the hot path):

```lisp
(defun unbox (val)
  (if (pl-box-p val)
    (let ((v (pl-box-value val)))
      (if (pl-tie-proxy-p v)
        (unbox (pl-method-call (pl-tie-proxy-tie-obj v) "FETCH"))
        v))
    val))
```

**`box-set`** — intercepts STORE (add check at top, before existing logic):

```lisp
(defun box-set (box value)
  (unless (pl-box-p box) (return-from box-set value))
  (let ((current (pl-box-value box)))
    (when (pl-tie-proxy-p current)
      (return-from box-set
        (pl-method-call (pl-tie-proxy-tie-obj current) "STORE"
                        (if (pl-box-p value) (unbox value) value)))))
  ... ; existing logic unchanged
```

**`pl-scalar-=` reference path** — this path (lines ~1665-1675) directly sets
`pl-box-value`, bypassing `box-set`. It needs its own STORE check:

```lisp
;; At the top of the reference-assignment branch:
(let ((current (pl-box-value ,place)))
  (when (pl-tie-proxy-p current)
    (return (pl-method-call (pl-tie-proxy-tie-obj current) "STORE" ,val))))
;; ... then the existing setf follows for untied case
```

**`pl-tied`** and **`pl-untie`**:

```lisp
(defun pl-tied (box)
  (if (pl-box-p box)
    (let ((v (pl-box-value box)))
      (if (pl-tie-proxy-p v)
        (make-pl-box (pl-tie-proxy-tie-obj v))
        *pl-undef*))
    *pl-undef*))

(defun pl-untie (box)
  (when (pl-box-p box)
    (let ((v (pl-box-value box)))
      (when (pl-tie-proxy-p v)
        (ignore-errors
          (pl-method-call (pl-tie-proxy-tie-obj v) "UNTIE"))
        (setf (pl-box-value box) (pl-tie-proxy-saved-value v)))))
  (make-pl-box 1))
```

**`++`/`--` operators**: already go through `box-set` in their boxed-scalar
paths (`pl-pre++`, `pl-post++`, etc.) — STORE is triggered for free.

**`local $tied_var`**: `pl-local-save` saves and restores `pl-box-value`, which
is the proxy struct. So `local` preserves the full tie state automatically. ✓

**String interpolation** `"$tied_var"`: calls `to-string` which calls `unbox`.
FETCH is triggered automatically. ✓

**Codegen**: No changes needed. `pl-tie` dispatches to the right constructor
by inspecting the box contents at runtime (scalar/array/hash).

### Phase 1 effort estimate

~55 lines new/modified in `cl/pcl-runtime.lisp`:
- `pl-tie-proxy` defstruct: ~5 lines
- `pl-tie` implementation: ~15 lines
- `pl-tied` / `pl-untie`: ~15 lines
- `unbox` modification: +3 lines
- `box-set` STORE check: +7 lines
- `pl-scalar-=` reference path fix: +7 lines
- Export list additions: ~3 lines

Tests: ~15-20 cases (TIESCALAR/FETCH/STORE/tied()/untie()/UNTIE/DESTROY,
`local` interaction, string interpolation).

**Estimated time: half a day.**

### Phase 2: Hash tie (future)

Arrays and hashes are raw CL containers stored directly in CL special variables.
There is no box to put a sentinel into, and assignments like `@arr = (1,2,3)`
create a new CL vector — bypassing any external registry.

**Approach**: Wrap hash/array variables in a container struct (analogous to
pl-box for scalars). `%h` would become a `pl-hash-container` holding either a
raw `hash-table` or a `pl-tie-proxy`. Every hash/array operation (`pl-gethash`,
`pl-sethash`, `pl-aref`, `pl-push`, etc.) unwraps the container and dispatches.

This is a larger change touching all generated code and all hash/array runtime
operations. Estimated 2-3 days per container type.

### The `unbox` two-version optimization (future)

Adding a type-check to `unbox` affects every scalar read, even in programs that
never use `tie`. The cost is small (one struct predicate = 1-3 machine instructions,
always branch-predicted false), but non-zero.

A cleaner approach for a future version: two separate functions:

- `unbox` — the current fast path, no tie check. Used for variables the
  transpiler knows are never tied.
- `unbox-tied` — adds the `pl-tie-proxy-p` check. Used only for variables
  that have had `tie` called on them.

The transpiler already knows the sigil statically. It could also track, per
variable, whether `tie` was called on it in the same scope, and emit `unbox-tied`
only for those. This makes the common case completely free.

This optimization is straightforward to add later without changing the semantics.
