# Plan: PCL's own `Test::More`

**Goal.** Make `use Test::More` work under *transpilation* so a CPAN module's own
`t/*.t` suite runs under PCL — driving each module's verification from the
author's full edge-case matrix instead of our hand-written smoke probes. This is
`pcl-rollout-plan.md` Phase 3–4 ("run user tests"), and the gateway that converts
every already-working module (Class::Inspector, Sub::Override, Try::Tiny,
Data::Dump, JSON::PP, Safe::Isa, Role::Tiny, …) into an "N/M of author's tests
pass" number.

---

## 1. Why it's blocked today, and what already works

A transpiled file that does `use Test::More; ok(1); is($x,$y)`:

- **`ok`/`is`/`like`/… already resolve** — user-sub codegen maps `is(…)` →
  `pl-is` (`ExprToCL.pm` `cl_name`), and `cl/pcl-test.lisp` defines `pl-ok`,
  `pl-is`, … in `:pcl`, **exported** (`export` form ~line 116). Generated code
  `(:use :pcl)`, so the TAP functions are **ambient**: no import is needed for
  them to be callable. This is exactly how `perl-tests/` get their assertions.
- **The mature TAP machinery lives in the runtime**: `*test-count*`,
  `*test-failures*`, the structured failure log (`.faillog`, consumed by
  `tools/sweep-diff.pl`), the declarative **skip-registry**
  (`cl/skip-registry.lisp`), `$TODO` support (`%current-todo`), and the
  crash-localization exit hook all hang off `test-ok` in `pcl-test.lisp`.

So the blocker is **not** resolving `is`/`ok`. It is two things:

1. **`use Test::More` loads the real module.** `(p-use "Test::More")` resolves
   `Test/More.pm` in `@INC` → site_perl's Test::More → Test2 stack → `%Config`
   unbound → crash. There is **no `lib/Test/More.pm`** shim (there is a
   `lib/Test/Simple.pm`, but Test::More is not covered, so the real one wins).
2. **Import args are dropped.** `use Test::More tests => 5` transpiles to bare
   `(p-use "Test::More")` — the `tests => 5` is lost, because
   `_parse_use_import_list` (`Pl/Parser.pm`) only recognizes `qw()` / quoted-list
   imports, not fat-comma pairs. So no `1..5` plan line is ever emitted.

Plus the obvious: several common Test::More functions are **not implemented**
(`is_deeply`, `isa_ok`, `can_ok`, `new_ok`, `subtest`, `use_ok`, `require_ok`,
`explain`).

---

## 2. Design decision — runtime-provided, NOT a `lib/Test/More.pm`

`docs/shipped-modules.md` says *prefer a pure-Perl `lib/` shim*. **Test::More is
the deliberate exception.** The reporting state (counter, failure log,
skip-registry, `$TODO`, crash exit-hook, the sweep's TAP parsing) already lives
in the runtime `pl-*`. A pure-Perl `lib/Test/More.pm` (like `lib/Test/Simple.pm`)
would run a **parallel** counter in Perl that does **not** feed `.faillog` /
sweep-diff / the skip-registry — so CPAN suites run through it couldn't use any of
the triage infrastructure we built for `perl-tests/`.

Therefore: **extend the runtime `pl-*` and intercept the `use`.** `Test::More`
becomes a *runtime-provided* module (skip its `.pm` load; the functions are
ambient). Record this exception in `shipped-modules.md` so the next person
doesn't "fix" it by adding a `lib/` shim. (`lib/Test/Simple.pm` stays — it's a
standalone counter for non-sweep use; it is not in the dependency path here.)

---

## 3. Phases

### Phase 0 — `use Test::More` stops crashing (skip-load)  *(smallest; unblocks `parent.t`)*

- Add a **`*p-runtime-provided-modules*`** set in `pcl-runtime.lisp`:
  `"Test::More"`, `"Test::Builder"`, `"Test::Builder::Module"` (Test::More's
  base), and — defensively — `"Test::More::Tools"` / `"Test2::*"` as
  encountered.
- In `p-use`, before the `@INC` load: if the module is runtime-provided,
  `return-from p-use t` **after** handling import args (Phase 1). This is the same
  shape as the existing `*p-xs-only-modules*` short-circuit (`p-use` ~line 7748),
  but a separate, clearly-named set (these are *provided*, not *XS-unsupported*).
- **Acceptance:** `perl-tests/parent.t` (a real `use Test::More` perl-test,
  currently 0-passing / no output) should now *run* — value TBD, but no longer a
  load crash.

### Phase 1 — route `use Test::More IMPORTS` plan directives to `plan()`

- Extend `_parse_use_import_list` (`Pl/Parser.pm`) to also capture **fat-comma
  pairs** (`tests => 5`, `skip_all => "reason"`) and bare strings (`'no_plan'`),
  not just `qw()` / quoted lists.
- Route them at runtime, not in the parser: pass the captured list to `p-use`,
  and in the Test::More runtime-provided branch translate:
  - `("tests" N)`        → `(pl-plan N)`
  - `("no_plan")`        → `(pl-plan "no_plan")`
  - `("skip_all" REASON)`→ `(pl-skip_all REASON)`
  - `("import" …)` / other named exports → ignore (functions are ambient).
  Keeping the translation in one runtime helper (`%p-test-more-import`) keeps the
  parser dumb and matches how `overload`/`parent` imports are already dispatched.
- Bare `use Test::More;` (no args) → no plan; the file is expected to call
  `done_testing` (already implemented as `pl-done_testing`).
- **Acceptance:** `use Test::More tests => 3; ok(1); ok(1); ok(1);` prints
  `1..3` then three `ok` lines and exits 0.

### Phase 2 — implement the missing functions (in `pcl-test.lisp`, exported)

Add, and add each to the `export` list:

- **`pl-is_deeply(got, expected, name)`** — the big one. Recursive structural
  compare over: scalars (Perl `eq` after the existing `is` undef rules),
  array-refs (length + element-wise), hash-refs (key set + value-wise), scalar
  refs, code refs (identity), and blessed refs (compare class + contents).
  Guard against **circular** structures with a seen-set. On mismatch, emit the
  Test::More-style `Structures begin differing at` diagnostic (best-effort; exact
  text is non-contract per `not-supported.md` "error message text").
- **`pl-isa_ok(obj, class, name)`** — `ref(obj)->isa(class)` (reuse
  `%pcl-isa-ancestry` / `p-isa`); also accept a class-name string invocant.
- **`pl-can_ok(obj, @methods)`** — all methods present via `p-can`.
- **`pl-new_ok(class, args, name)`** — `class->new(@args)` then `isa_ok`.
- **`pl-use_ok(module[, imports])` / `pl-require_ok(module)`** — actually
  `(p-use …)` / `(p-require …)` inside a `handler-case`, report ok/not-ok. (This
  loads the *module under test*; if that module isn't transpilable the test
  fails — a real result, not a harness bug.)
- **`pl-explain(@refs)`** — stringify args for diagnostics. Reuse `Data::Dump`
  (now working) or a small inline dumper; explain output is non-contract.
- Confirm already-present: `plan done_testing skip_all BAIL_OUT ok is isnt like
  unlike cmp_ok pass fail skip diag note`.

### Phase 3 — run a real CPAN suite end-to-end  *(acceptance)*

- Pick a small, already-working module's dist: **Class::Inspector** or
  **Sub::Override** (both now pass our probes; pure-Perl; few deps).
- Driver: `pl2cl dist/t/foo.t | sbcl (+pcl-runtime +pcl-test)`, compare TAP to
  `prove` under stock perl. Wrap as `tools/run-cpan-tests.pl` (or a `--cpan` mode
  on the sweep) so it's repeatable; reuse the `.faillog` plumbing.
- Iterate on real breakage (expected: `subtest`, `$TODO` edges, `done_testing`
  count check, an unimplemented function, a `use` of something that drags in
  Test2). Each fix is a general win.

### Phase 4 — `subtest` + polish  *(as needed)*

- **`pl-subtest(name, code)`** — save/restore `*test-count*`/`*test-failures*`
  on a small stack, run `code`, emit a nested `1..n` indented block, then report
  the whole subtest as one parent `ok`/`not ok`. Needs a counter-stack in
  `pcl-test.lisp`.
- `todo_skip`, `local $TODO` blocks (verify the existing `%current-todo` path
  covers a transpiled `use Test::More` file, not just `perl-tests/`),
  `done_testing` plan-mismatch diagnostic.

---

## 4. Risks / unknowns

- **`is_deeply` on blessed / circular / mixed ref graphs** — needs a seen-set and
  a defined ref-type order; get the common cases (AoH, HoA) byte-right, leave
  exotic SV-identity cases to `not-supported.md`.
- **`subtest` counter nesting** — the one piece of genuinely new state.
- **`use_ok` loads the module under test** — non-transpilable modules will fail
  there; that's correct behaviour, but it caps which dists' suites can pass.
- **Test::Builder introspection** — some modules poke `Test::Builder->new` /
  `->level` / `->todo`. We provide the *functions*, not the OO Builder; if a dist
  needs the object, stub `Test::Builder->new` to a singleton exposing the few
  methods used, or register Test::Builder `:xs`-style to fail cleanly.
- **Indirect Test2 pull-in** — a dist that `use`s a Test2-based helper still
  crashes; extend `*p-runtime-provided-modules*` as found.

---

## 5. Verification

- **Pl/t regression:** new `Pl/t/test-more-01.t` — transpile+run small snippets
  (`use Test::More tests => N`, `is_deeply([1,{a=>2}],…)`, `isa_ok`, `can_ok`,
  `done_testing`) and assert the exact TAP output.
- **Acceptance:** a real dist `t/` directory passing under PCL (Phase 3).
- Gate (`prove -j8 Pl/t/`) + full sweep `sweep-diff 0 new` after each phase, as
  always. Watch the **fully-passing (69)** count — `parent.t` is the first
  candidate to move.

---

## 6. Sequencing

**Phase 0 + 1 together** (skip-load + plan-arg routing) is the minimum that makes
`use Test::More tests => N` a working, non-crashing program — do it first and
re-check `parent.t`. Then **Phase 2** (`is_deeply`/`isa_ok`/`can_ok` are the
high-frequency ones). Then **Phase 3** acceptance on one real dist, fixing what it
surfaces. **Phase 4** (`subtest`) only when a target suite needs it.

Touch points: `Pl/Parser.pm` (`_parse_use_import_list`, the `use` emit ~5731),
`cl/pcl-runtime.lisp` (`*p-runtime-provided-modules*`, `p-use` branch,
`%p-test-more-import`), `cl/pcl-test.lisp` (new `pl-*` + exports),
`docs/shipped-modules.md` (record the runtime-provided exception).
