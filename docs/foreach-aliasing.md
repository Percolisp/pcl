# foreach aliasing (`for (LIST) { $_ = ... }`)

In Perl, `foreach` aliases the loop variable to **each element of the list in
turn** — not a copy.  Writing the loop variable writes through to whatever the
element actually is, *if* that element is a real lvalue container.

```perl
my @a = (1,2,3);
for (@a) { $_++ }        # @a is now (2,3,4) — $_ IS each element
for ($h{k}) { s/x/y/ }   # edits the hash slot in place
```

## How PCL implements it (the box model)

Every Perl scalar is a `p-box`; an array is an adjustable vector of boxes, a hash
maps keys to boxes.  `p-foreach` binds the loop variable to **the same box object**
the container holds:

```lisp
(let ((,var (ensure-boxed (aref ,vec ,i)))) ...)   ; $_ = the element box
```

`ensure-boxed` is a no-op on an already-boxed element, so `$_` *is* the box.
`$_++` / `$_ = X` call `box-set` on it, mutating the shared container.

So **aliasing works iff the foreach-list codegen hands back the actual container
box(es)** rather than a fresh value-box.  `%p-flatten-for-list` (`cl/pcl-runtime.lisp`)
has a special arm: when the list is an array box it returns that array's internal
box vector *directly*.  A single non-vector value is wrapped as `(vector raw)`,
preserving `raw`'s identity — so a single box flows straight through to `$_`.

The general rule (Perl): **alias ⇔ lvalue-ness** — the same test as "is this legal
on the left of `=`".  Persistent containers alias; transient computed values do not.

## Supported (aliased) — verified by tests

| `for (EXPR)` | mechanism | test |
|---|---|---|
| `@array` (whole) | flatten returns the element boxes | `foreach-aliasing-01.t` |
| `$scalar` | the scalar's box is passed | — |
| `$h{k}` (hash element) | codegen `p-gethash` → **`p-gethash-box`** | `foreach-aliasing-01.t` |
| `$a[i]` (array element) | codegen `p-aref` → **`p-aref-box`** | `foreach-aliasing-01.t` |
| `substr/pos/vec(...)` | codegen → **`p-…-lvalue-cell`** (magic cell) | `lvalue-ref-01.t` |

The element + magic-lvalue rewrites are gated by an **AST-level check** in
`Pl/Parser.pm` `_foreach_alias_rewrite` (sole list element is a Symbol+Subscript,
or a substr/pos/vec Word+List), then a single rewrite of the generated call head to
its box-returning form.  A two-part match guards against multi-element lists like
`for ($a[0], $a[1])`.

## Correctly NOT aliased (Perl agrees — pinned by tests)

- Computed values: `for ($x+1)`, `for (uc $x)`, `for ("a"."b")` — the SV is a
  throwaway temp; writing `$_` is harmless and discarded.
- Normal (non-`:lvalue`) sub returns: the value is copied into a temp.

`foreach-aliasing-01.t` pins these so the boundary can't silently drift.

## Divergence

- **Literals**: `for (1,2,3) { $_++ }` *dies* in Perl ("Modification of a read-only
  value"); PCL silently allows it because read-only scalars aren't emulated
  (`docs/not-supported.md`).
- **`: lvalue` subs**: `for (lvalue_sub()) { $_ = ... }` aliases in Perl; PCL does
  not support user `: lvalue` subs at all (`docs/not-supported.md`).

## DEFERRED — slices and `values %h` (do NOT redo the investigation; this is it)

These **should** alias in Perl but currently do not in PCL:

```perl
for (@a[0,1])    { $_++ }     # should bump @a[0], @a[1]; PCL: no-op
for (@h{qw/a b/}){ $_++ }     # should bump those hash values; PCL: no-op
for (values %h)  { $_++ }     # should bump every value in place; PCL: no-op
```

**Why deferred (the real reason — not difficulty of the idea):** unlike a single
element, a slice / `values` does not go through one box.  The list-builder
(`%p-flatten-list` and the slice/`values` codegen) **copies** the values into a
fresh vector, losing box identity.  That flattening machinery is **shared with
every other list context** — `my @copy = @orig`, function-call args, plain list
assignment — all of which **must keep copying** (aliasing them would be a bug:
`my @copy = values %h` must not alias).

So the fix is *not* "make the flattener preserve boxes".  It is: **surface the
container boxes only in the foreach-list (lvalue) position**, while every other
list position keeps copying.  Concretely, one of:

1. A foreach-list-specific lvalue generation mode for slices/`values` that emits a
   box-returning slice (`p-aslice-box` / `p-hslice-box` / `p-hash-values-box`),
   parallel to the `p-aref-box` / `p-gethash-box` element forms — gated by the same
   `_foreach_alias_rewrite` AST check, just for the multi-element slice/`values`
   shapes.  Lowest blast radius; preferred.
2. A `*p-foreach-alias*`-style dynamic flag read by the flattener — rejected: it
   couples the shared flattener to loop context and is easy to leak into nested
   list builds.

**Cost note:** like the element case, this is *cheaper* at runtime than today —
aliasing returns existing boxes instead of allocating fresh ones.  Performance is
not a reason to defer; blast radius is.

**Risk to watch when implementing:** after any change, assert that
`my @copy = @orig; $copy[0]++ ;` does **not** mutate `@orig`, and that
`my @v = values %h; $v[0]++` does not mutate `%h`.  Those are the copy-semantics
that the slice/`values` aliasing must not break.

Frequency: `for (values %h)` is a real idiom (moderate); slices are rarer.  Element
aliasing (done) covers the most common in-place-edit pattern.
