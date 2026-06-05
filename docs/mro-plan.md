# `mro` — Method Resolution Order pragma + API (implementation plan)

Status: **PLAN ONLY — not yet implemented** (session 234).

**Decision (session 234, with the user): "brute force and ignorance" — C3-only.**
PCL always uses C3, so we do NOT implement DFS, dual algorithms, the introspection
API (`set_mro`/`get_mro`/`get_isarev`), or `next::method`. We ship only the minimal
crash-fix: make `require mro` / `use mro` parse as a module, and provide a C3-only
`mro::get_linear_isa`. The DFS-vs-C3 divergence is documented in
`docs/not-supported.md` ("mro pragma") and revisited only if a real module is shown
to depend on DFS order or the wider API. Justified by `Test2::Util::HashBase`'s own
comment: "for our use we don't care about order." So below, only **P1** is in scope;
P2/P3 are explicitly deferred.

## Why we need it

Real-world OO modules pull in the core `mro` facility. It first surfaced while
transpiling the real `Test::More` → `Test2::Util::HashBase`:

```perl
# Test2/Util/HashBase.pm
*_isa = ($] >= 5.010 && require mro) ? \&mro::get_linear_isa : sub { ... };
```

So PCL must (a) let `require mro` / `use mro` succeed, and (b) provide
`mro::get_linear_isa` as a **real coderef** (it is taken with `\&`).

`mro` is a **core, partly-XS facility** (the `.pm` is a thin wrapper; the real
work — `mro::get_linear_isa`, `next::method`, the method-cache hooks — is built
into the interpreter). PCL therefore cannot transpile the stock `mro.pm`; we must
provide our own.

### Current breakage

1. **Parse**: `require mro` in *expression* context (`$] >= 5.010 && require mro`)
   compiles to `(p-require (let ((*wantarray* nil)) (pl-mro)))` — i.e. `mro` is
   treated as a **function call** whose result is passed to `require`, instead of
   `(p-require "mro")`. (Statement-level `require Foo;` is fine; the expression
   path is not.)
2. **No provider**: even parsed correctly, there is no `mro` PCL can load, and no
   `mro::get_linear_isa`.

## How `mro` maps to CL (this is mostly already here)

| Perl `mro` concept | CL / PCL equivalent |
|---|---|
| C3 linearization | `sb-mop:class-precedence-list` of the CLOS class |
| `@ISA` graph | the `@ISA` special var per package (PCL already walks it) |
| method dispatch order | PCL already dispatches via CLOS **C3** (`CODEGEN`/runtime walk `class-precedence-list`) |
| pkg name ↔ CLOS class | `perl-pkg-to-clos-class` / `clos-class-to-pkg` (exist in `cl/pcl-runtime.lisp`) |

So the *data* mro exposes is already computed inside PCL for dispatch. The work is
packaging it behind the `mro::*` API.

### The one real divergence: DFS vs C3 default

Perl's **default** MRO is **DFS** (depth-first); C3 applies only under
`use mro 'c3'`. PCL's dispatch is **C3 always** (see CLAUDE.md "Inheritance via
@ISA with C3 MRO (CLOS-based)"). Concretely, for the diamond
`D → (B, C) → A`:

```
Perl default (dfs):  D B A C
Perl 'c3':           D B C A
PCL dispatch (CLOS): D B C A   (always C3)
```

Implications:
- `mro::get_linear_isa($c)` with **no type arg** must return **DFS** order to match
  stock Perl, *even though PCL dispatches in C3*. (For non-diamond hierarchies DFS
  == C3, so most code is unaffected; HashBase's `_isa` use is order-insensitive in
  practice.)
- This means `get_linear_isa` should compute order **from `@ISA` directly**, not
  from `sb-mop:class-precedence-list`, so we can honour the requested type
  (`dfs` default, `c3` on request) independently of how PCL happens to dispatch.
- Separately documents a *pre-existing* PCL divergence: PCL dispatches C3 even when
  Perl would dispatch DFS. Out of scope to change here; note it in
  `docs/not-supported.md` if it ever bites.

## API surface

Tier the API by how much real code needs it.

**Tier 1 (unblocks HashBase / Test2; pure data over `@ISA`):**
- `mro::get_linear_isa($class[, $type])` — arrayref of class names, `$class`
  first, `$type` ∈ {`dfs` (default), `c3`}.
- `use mro;` / `use mro 'c3';` / `require mro` — accepted; `use mro 'c3'` records
  C3 for the current package (Tier 2 honours it).

**Tier 2 (introspection; small state table):**
- `mro::get_mro($class)` → `'dfs'` | `'c3'` (default `'dfs'`).
- `mro::set_mro($class, $type)`.
- `mro::get_isarev($class)` → arrayref of classes that (transitively) inherit
  `$class` (reverse-ISA; walk all known packages' `@ISA`).
- `mro::is_universal($class)` → bool (true if `UNIVERSAL` is in its ISA or it
  manipulates `@UNIVERSAL::ISA`).
- `mro::invalidate_all_method_caches()`, `mro::method_changed_in($class)` —
  **no-ops** in PCL (CLOS has no Perl-style method cache we must poke).

**Tier 3 (the hard part — runtime current-method awareness):**
- `next::method`, `next::can`, `maybe::next::method` — call the next method in the
  MRO after the one currently executing. Needs the *current class + method name* at
  the call site, which PCL does not thread today (CLOS `call-next-method` is the
  natural target but only exists inside a CLOS method body, and PCL's `p-sub`
  methods are plain functions, not `defmethod`s). **Defer**; document as
  not-supported until a real module needs it.

## Implementation

### Recommended split

- **Parser fix** (`Pl/Parser.pm` / `Pl/PExpr.pm`): make `require mro` and
  `use mro [LIST]` resolve as a **module/pragma**, not a function call, in *both*
  statement and expression context. Likely: recognise `require BAREWORD` in PExpr
  (bareword → string module name) and add `mro` handling alongside the existing
  `use`-pragma path. Verify `\&mro::get_linear_isa` (a `\&` on a qualified name)
  produces a coderef to the provider.

- **Provider = pure-Perl `lib/mro.pm`** for Tier 1–2. Rationale:
  - `get_linear_isa` is a plain recursive `@ISA` walk; doing it in Perl gives us
    **correct DFS *and* C3** (implement both algorithms) and works for classes that
    exist **only via `@ISA`** (no CLOS class, e.g. never `bless`ed) — which a
    CL/CLOS-CPL mapping would miss.
  - Arbitrary-class `@ISA` access uses symbolic refs `@{"${class}::ISA"}`, which
    PCL already supports (`%p-symref-array`).
  - Consistent with the existing `lib/` shim pattern (Carp, POSIX, Scalar::Util…),
    found first in `@INC`.
  - `package mro;` is a single-segment package → CL pkg `MRO`; `mro::get_linear_isa`
    → `|mro|`? No: single segment, so codegen emits `mro::pl-get_linear_isa` →
    reader upcases to `MRO::PL-GET_LINEAR_ISA`. The shim's `package mro;` must land
    in the same `MRO` package — confirm casing via `perl-pkg-to-cl-pkg-name`
    ([[session-log]] §234 package-casing fix).
  - `\&mro::get_linear_isa` must yield a coderef to the shim's sub — verify PCL's
    `\&Pkg::sub` codegen resolves to `MRO::PL-GET_LINEAR_ISA`.

  C3 merge (for the `'c3'` arg) is the standard linearization:
  `merge(L[parents], parents)` taking heads that appear in no tail.

- **`next::method` (Tier 3)**: defer. If needed later, the realistic route is to
  thread `(*current-class* *current-method*)` dynamic vars through `p-method-call`
  and have `next::method` resume the `@ISA`/CPL walk *after* `*current-class*`.

### Files touched (anticipated)

- `Pl/Parser.pm` and/or `Pl/PExpr.pm` — `require mro` / `use mro` recognition.
- `lib/mro.pm` (new) — `get_linear_isa` (dfs+c3), `get_mro`, `set_mro`,
  `get_isarev`, `is_universal`, no-op cache hooks, `import` that records
  `use mro 'c3'`.
- Possibly `cl/pcl-runtime.lisp` only if a function genuinely needs CLOS
  introspection (Tier 1–2 should not).

## Phased plan

1. **P1 — unblock Test2.** Parser: `require mro`/`use mro` recognised. `lib/mro.pm`
   with `get_linear_isa` (dfs default + c3). Verify `\&mro::get_linear_isa` coderef.
   Target: `Test2::Util::HashBase` loads past line 36.
2. **P2 — introspection.** `get_mro`/`set_mro`/`get_isarev`/`is_universal` + no-op
   cache hooks; `use mro 'c3'` recorded and honoured by `get_linear_isa` default.
3. **P3 — (deferred) `next::method` family.** Only when a target module needs it.

## Tests

- `Pl/t/mro-01.t` (new): `get_linear_isa` dfs vs c3 on the diamond `D→(B,C)→A`
  (expect `D B A C` dfs, `D B C A` c3); single-inheritance chain (dfs==c3);
  `@ISA`-only class (never blessed); `get_isarev`; `is_universal`;
  `\&mro::get_linear_isa` is callable; `require mro` in `X && require mro`.
- If `perl-tests/mro.t` exists, treat as the acceptance target (sweep).

## Open decisions (for the user)

1. **DFS default vs C3 default for `get_linear_isa`.** Plan says match Perl: DFS
   default, C3 on request — computed from `@ISA`, independent of PCL's C3 dispatch.
   Acceptable that dispatch (C3) and `get_linear_isa` default (DFS) can differ, as
   they do in stock Perl too? (Recommended: yes.)
2. **Provider home**: pure-Perl `lib/mro.pm` (recommended) vs CL-backed in
   `pcl-runtime.lisp`. Pure-Perl is simpler, handles `@ISA`-only classes, and gives
   both orderings; CL-backed would force C3-only via CLOS CPL.
3. **Scope of `next::method`**: defer (recommended) vs implement now.
