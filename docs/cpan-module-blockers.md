# CPAN module blockers — what to fix next

> **Session 240 update (2026-06-09):** Moo now LOADS its whole stack, generates accessors, and
> rw set/get works. Six general bugs fixed this session (each a Moo wall), all gate-green:
> 1. **`package NAME;` inside a BEGIN/scheduled block is block-scoped** — was leaking past the
>    block, so later subs resolved unqualified calls in the inner package (Moo's
>    `Method::Generate::Accessor::_Generated` idiom). `_process_scheduled_block` snapshots the
>    package stack + reverts.
> 2. **`$obj->${ EXPR }(args)`** — method whose name/coderef is a scalar deref (Moo::Object).
>    New parser case + gen_methodcall routes computed methods through dynamic `p-method-call`.
> 3. **`Carp::short_error_loc`/`long_error_loc`** added to lib/Carp.pm shim.
> 4. **`caller()` returned UPCASED single-segment pkg** (`POINT` not `Point`) — the orig-case
>    name was registered only by `p-set-current-package`, emitted AFTER the package's `use`
>    stmts, so `pcl-pkg-perl-name` fell back to the CL name during import. Fix: new
>    `p-register-pkg-name` emitted in the package PREAMBLE (before `use`). **This was the key
>    Moo bug** — Moo keys all per-class state on `caller`, but blesses into the correct-case
>    name, so the mismatch silently broke construction.
> 5. **`CORE::<builtin>`** now behaves like the builtin: `CORE::shift()`/`CORE::shift` default
>    to `@_`, `CORE::ref $x` (no parens) is a named unary (was parsed as a bareword string).
>    handle_subcalls normalizes `CORE::foo`→`foo`; add_implicit_default_param strips `CORE::`.
> 6. **Nested ternary in the TRUE branch without parens** `A ? B ? C : D : E` failed to parse
>    entirely — the inner `?`'s false-branch scan used `prec < 15` and so swallowed the outer
>    `: E` (the `:` is also prec 15). **Our perl-tests/cond.t never tested `?:` at all** —
>    coverage gap. Fixed the false-end scan to stop at a `:`.
>
> **Moo's REMAINING wall (240):** `Point->new` now reaches MGC's generated constructor and the
> arg-copy works (MGC objects store `package`/`attribute_specs`/…). But it dies
> `assert_constructor: "Unknown constructor for Method::Generate::Constructor already exists"`
> — `_constructor_maker_for(MGC)` runs with a FRESH maker (`$self->{constructor}` unset) while
> MGC's own `new` already exists in the glob, so `$MAKERS{MGC}{constructor}` looks unset when it
> shouldn't (memoization/identity in Moo's self-referential bootstrap). NEXT: trace why
> `$Moo::MAKERS{"Method::Generate::Constructor"}{constructor}` is falsy after bootstrap (the
> subconstructor_handler calls `Moo->_constructor_maker_for($class)` when MAKERS{$class} is set
> but {constructor} is not — likely a coderef-identity or `weaken`-interaction nuance, or a
> stale-case `$class`). Repro: `package Point; use Moo; has x=>(is=>'ro'); Point->new(x=>3)`.

> **Session 239 update (2026-06-09):** Moo walls A–C are DOWN. Moo now loads its whole stack and
> runs into **constructor generation**. Four general fixes landed (see `session-log.md` §239):
> (1) glob-slot `*{$glob}{EXPR}` parses (variable + expression) — wall A; (2) nested-import `caller`
> binding in `%p-do-import` — unblocks `_set_loaded`; (3) symbolic `\%{"Pkg::Name"}` hash deref
> (`%p-symref-hash`) — unblocks `%Config`/Exporter-heavy; (4) `caller(N)` list-context + `[3]` subname.
> Plus: Errno shim regenerated from real module, and **`tools/shim-gaps.pl`** (diffs each lib/ shim vs
> the real module — run it to find missing exports/subs; 47 function-gaps remain, fill-as-needed).
>
> **Moo's NEW walls, in order:**
> 1. **`Carp::short_error_loc` undefined** — `lib/Carp.pm` shim is missing the internal location
>    helpers (`short_error_loc`, `caller_info`, …). Likely small: add them (now that `caller(N)[3]`
>    works, `short_error_loc` = `(caller(...))[lots]` can be real). DO THIS NEXT for Moo.
> 2. **`$self->${\(EXPR)}`** (Moo::Object lines 48/56, Method::Generate::Accessor) — a method call
>    whose *name* is a deref of a ref. Emits "Handle single node of unknown type: PPI::Token::Cast"
>    (×8); doesn't stop the load yet, but generated `new`/accessors won't dispatch. Parser/codegen gap.
> 3. **eval-lexical-capture** — accessor/constructor codegen via Sub::Quote string-eval closing over
>    the installer's lexicals. CL `eval` runs in the null lexical environment: globals/package vars
>    resolve, lexical `my` does not. The recurring deep wall (CMM family). Sub::Quote's own
>    `capture_unroll` works (it threads captures through a hash); naive `eval "sub {…$lex…}"` doesn't.

State after session 238g (2026-06-08). The generic `use Foo LIST → Foo->import(LIST)`
machinery is **done and solid**: custom-import dispatch, the real core `Exporter.pm`,
the multi-seg `\&{...}` fix, and pragma `->import` no-ops all landed (commits
`57fab5f`, `d436707`). Many modules now work end-to-end (Data::Dump, Try::Tiny,
Safe::Isa, Scalar::Util, List::Util[sum/max], JSON::PP, Class::Inspector,
Sub::Override). This file lists the modules that still **don't** fully work and the
exact next action for each, newest analysis first.

See also: [[project_cpan_module_survey]] (memory), `docs/test-more-plan.md`,
`docs/session-log.md` §238f/§238g.

---

## 1. Moo — wall A: `*{$glob}{$var}` variable-slot typeglob access  ← DO THIS NEXT

**Symptom:** `use Moo; has x => (is=>'ro');` → `Moo::PL-_SET_LOADED is undefined`.

**Status:** `use Moo` (a silent no-op for the whole pre-238 era) now LOADS Moo, runs
its custom `import`, clears the `strict->import`/`warnings->import` calls, and reaches
Moo's internals. Three walls already down. This is the next one.

**Root cause (traced, NOT import/Exporter related):** Moo does
`use Moo::_Utils qw(... _set_loaded ...)`, but **`Moo::_Utils` aborts mid-load on a
PARSE ERROR**, so `_set_loaded` (defined at line ~221, *after* the error) never gets
installed → Moo imports nothing → `Moo::_set_loaded` undefined. The offending
construct is its glob-copy loop:

```perl
foreach my $type (qw(SCALAR HASH ARRAY IO)) {
    next unless defined(*{$old}{$type});   # PARSE ERROR: "Bug. Fell through. Missing case"
    no strict 'refs';
    *$full_name = *{$old}{$type};           # same
}
```

`*{$old}{$type}` is a **dynamic typeglob-slot whose slot name is a VARIABLE** (`$type`
holds "SCALAR"/"HASH"/… at runtime). Our glob-slot parser only recognizes *literal*
slot barewords (`{CODE}`, `{SCALAR}`, … — `_block_is_glob_slot` in `Pl/PExpr.pm`),
a deliberate guard so it doesn't misread `*{$x}{$y}` as a glob slot. The variable-slot
form falls through.

**Fix direction:**
- Extend the dyn-typeglob-slot detection (`_precollapse_dyn_glob_slots` + the in-loop
  handler ~`Pl/PExpr.pm:1234`, and `_block_is_glob_slot`) to accept a **scalar
  variable** as the slot, emitting `(p-glob-slot <glob> <var-value>)`. The runtime
  `p-glob-slot` already takes the slot as a string, so `*{$old}{$type}` →
  `(p-glob-slot (p-dynamic-typeglob $old) $type)` and `defined(*{$old}{$type})` →
  `(p-defined (p-glob-slot …))`.
- **Ambiguity guard:** after a `*{...}` cast, `{$var}` IS a glob slot (Perl); but be
  careful the same relaxation doesn't make `$h{$k}` or `%{$ref}{$k}` mis-parse. Gate
  on the Cast being `*` (typeglob), which the existing detection already does — only
  the "slot must be a literal bareword" condition needs relaxing to "literal bareword
  OR a scalar Symbol".

**Repro:** `package F; sub x {1} package main; my $g=\*F::x;
for my $t (qw(CODE SCALAR)){ print +(defined(*{$g}{$t})?1:0) } print "\n";`
(perl prints `10`; PCL parse-errors).

## 1b. Moo — wall B (after A): accessor generation / eval-lexical-capture

Once `_set_loaded` resolves, the next (always-known) wall is
`Method::Generate::Accessor` building accessors via **Sub::Quote / `eval` closing over
the installer's lexicals**. PCL's string-`eval` runs in a subprocess and can't capture
outer lexicals. This is the recurring deep wall (same family as CMM). Sub::Quote itself
works (s236d); the capture-in-generated-eval is the hard part. Likely needs the
eval-lexical-capture project, not a point fix.

---

## 2. Role::Tiny — `$INFO{$target}` is `2`, not a hashref

**Symptom:** a role + `with` → `TYPE-ERROR: The value 2 is not of type HASH-TABLE`,
at `(P-GETHASH 2 "non_methods")`, called from `Role::Tiny->_non_methods("Comp")`.

**Status:** past the pragma cascade (fixed 238g); now runs its import into role
composition.

**Root cause (partially traced):** `_non_methods` does `$INFO{$target}{non_methods}`,
and `$INFO{Comp}` is the integer `2` instead of a hashref. So Role::Tiny's per-class
`%INFO` got a wrong value for the target class. **Not yet root-caused** — need to find
where `$INFO{$target}` is assigned `2` (instrument Role::Tiny's `import`/`apply_roles_to_package`,
or grep Role::Tiny.pm for `$INFO{...}` writes). Could be an autoviv/assignment bug
(cf. the `$a[N]{k}=v` family) or a mis-parse building `%INFO`.

**Fix direction:** instrument to find the bad write; likely a general PCL bug once
isolated. Lower priority than Moo wall A (Role::Tiny is less central than Moo).

---

## 3. List::Util `first { } LIST` returns empty  (pre-existing, not from 238f/g)

**Symptom:** `first { $_ > 2 } (1,2,3,4)` → undef/empty. `sum`/`max`/`reduce`? — `sum`
and `max` work; the **block-prototype** form (`first BLOCK LIST`, like grep/map) does
not.

**Root cause:** unknown — `first` has a `&@` prototype; PCL may not pass the block
correctly to List::Util's `first`. Predates this session's work (the use→import change
didn't touch it).

**Fix direction:** check how `first { } LIST` transpiles vs `grep { } LIST` (which
works); compare block-arg handling. Likely a prototype/block-arg plumbing gap for
List::Util's pure-Perl `first`.

---

## 4. `defined &glob_installed_sub` cosmetic (sub IS callable)

**Symptom:** `*{"main::foo"} = $coderef; defined &foo` → false, even though `foo()`
runs fine. Surfaces as e.g. `defined &getcwd` being false after `use Cwd` (getcwd is
callable). Cosmetic — only `defined &` is wrong.

**Root cause:** a glob-installed CODE slot (`%p-glob-assign-slots` → `setf fdefinition`)
fbinds the symbol but does NOT register it in `*p-declared-subs*` as `:defined`, so
`p-coderef-defined-p` (which checks for `:defined`) returns nil.

**Fix direction:** in `%p-glob-assign-slots`, when assigning a CODE slot, also
`(setf (gethash sym *p-declared-subs*) :defined)`. (Mirror in the @EXPORT-import path
`p-import-perl-symbol` if it has the same gap.) Small, low-risk.

---

## Quick re-survey command

```
for m in moo m_rt m_lu3 ; do echo "== $m =="; timeout 90 ./runpl /tmp/$m.pl 2>&1 \
  | grep -avE '^;|Runtime loaded|PCL Test' | head -4; done
```
(Recreate the tiny drivers from the survey memory if `/tmp` was cleared.)

## Suggested order next time
1. **Moo wall A** (`*{$glob}{$var}`) — highest leverage, well-isolated parser fix, and
   the gateway to seeing how far Moo really gets.
2. **#4** (defined-& cosmetic) — trivial, removes a recurring red herring.
3. **#3** (List::Util `first{}`) — contained, general block-arg fix.
4. **Role::Tiny #2** — needs root-causing first.
5. **Moo wall B / eval-lexical-capture** — the big project; tackle deliberately.
