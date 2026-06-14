# Declaration Ordering — The Permanent Fix Plan

> **Status:** PLAN (not yet implemented as of session 253, 2026-06-14).
> Supersedes the "Known Limitations" section of `declaration-ordering.md`.
> Read `declaration-ordering.md` first for the Perl-vs-CL background; this doc
> is the *cure* for the bug class that keeps coming back.

## Why this keeps biting us (the recurring symptom)

Every few sessions a CPAN module breaks because PCL ran a `use`/`BEGIN` **before**
a sub that, in the Perl source, was written **before** it — or, dually, made a
sub visible to compile-time introspection that Perl had not compiled yet.
Instances seen so far:

- **Moo roles (s253):** `package Greet; use Moo::Role; sub hello {…}`. PCL hoists
  `sub hello` above `use Moo::Role`, so `Moo::Role`'s `make_role` snapshots
  `hello` into `non_methods` (because it already exists at use-time) and the role
  ends up with an empty method set. The consumer never gets `hello`.
- **The general shape:** any module that, at `use`/`BEGIN` time, *introspects the
  current package's subs* (Moo, Moo::Role, Moose-ish shims, Exporter-ish
  `@EXPORT` autogen, `Class::Method::Modifiers`, attribute registries) sees the
  WRONG set of subs because PCL has already installed all of them.
- **The dual shape:** a `use Foo` whose `import` must run before later code, but
  which PCL reorders relative to subs in a way Perl never would.

It recurs because the fix each time is local ("special-case this module") instead
of fixing the **ordering invariant**.

## The root cause, precisely

PCL assembles each package section from four buckets, in this fixed order
(`Pl/Parser.pm::_assemble_output`, ~line 422):

```
preamble  →  declarations  →  definitions  →  runtime
```

- `preamble`     — `defpackage`, `in-package`, `defclass`, `p-register-pkg-name`
- `declarations` — `defvar` special-proclamations, `p-declare-sub` stubs, **AND
                    every top-level named sub's real `(p-sub …)` body**
                    (`_process_sub_statement`, Parser.pm:5123)
- `definitions`  — `use` / `require` / `BEGIN` / `use constant`
- `runtime`      — everything else (prints, calls, assignments, control flow)

The bug is one line of policy: **top-level sub *definitions* are routed to
`declarations`, which is unconditionally assembled before `definitions`.** So
*every* sub runs before *every* `use`/`BEGIN`, regardless of source order. The
`p-declare-sub` stub (`cl/pcl-runtime.lisp:376`) also installs a real
`(defun … nil)`, so the sub is `fboundp` — visible to `exists &foo`, `can`, and
stash walks — before any `use`/`BEGIN` runs.

The current `declaration-ordering.md` *describes* a "Phase 2" that keeps subs in
source order relative to `use`; that description is **stale**. The bucket refactor
(see `Parser.pm:548` `# DELETED: _reorder_compile_runtime_forms`) replaced Phase 2
with the four-bucket model, and the bucket model lost the "source order relative
to use" property.

## The invariant we must enforce (the north star)

> **Within a package section, the generated CL must reproduce Perl's two
> timelines:**
>
> 1. **Compile-time stream** — `use` / `no` / `BEGIN` / `sub NAME {…}` /
>    `use constant` / the *declaration* half of `our` — emitted in **source
>    order**. Consequence: every compile-time form sees exactly the names
>    defined by *earlier* compile-time forms, and **none defined later**.
>
> 2. **Runtime stream** — everything else — emitted after the whole compile-time
>    stream, also in source order. By the time it runs, *all* subs exist.
>
> The ONLY things allowed to "jump the queue" above the compile-time stream are:
>   - `defvar` **special proclamations** (needed so `let`-based `local` creates a
>     dynamic binding — a CL compile-time requirement, not a Perl-visible fact);
>   - `p-declare-sub` **forward stubs**, which must be **invisible to package /
>     symbol introspection** — they exist only so a literal `\&foo` or `foo()`
>     written *before* `sub foo` can resolve, NOT to make `foo` look defined.

If we hold that invariant, all the recurring bugs vanish at once, because PCL's
compile-time side effects (Moo's `make_role`, Exporter autogen, …) see exactly
what Perl's would.

## Constraints catalog (why the current hoist exists, and how each is kept)

| # | Constraint | Today's mechanism | Under the fix |
|---|------------|-------------------|---------------|
| C1 | A runtime call `foo()` before `sub foo` in source must work (`foo(); sub foo {}`) | All subs hoisted above all runtime | Subs in compile-time stream, which is assembled **before** the runtime stream → still satisfied |
| C2 | `defvar $x` must be proclaimed special before any `defun` whose `let` binds `$x` (for `local`/dynamic scope) | `defvar` in `declarations` (before defuns) | **Unchanged** — `defvar` proclamations stay hoisted in `declarations`; only sub *bodies* move |
| C3 | An imported function symbol must be resolvable inside a sub that uses it (`use Foo qw(f); sub g { f() }`) | (was: shadowing-import timing) | **Improve** — with subs in source order, `use` precedes the sub, so this is *more* correct than today. (Verify whether imports are now late-bound; see Risk R3.) |
| C4 | A `BEGIN`/`use` may call/inspect subs defined **earlier in source** | All subs exist → trivially | Compile-time stream is source-ordered → earlier subs exist, later ones don't ✓ (this is the fix) |
| C5 | A `BEGIN`/`use` must **NOT** see subs defined **later in source** (the broken one) | VIOLATED (all subs pre-exist) | Source-ordered compile-time stream + introspection-invisible stubs ✓ |
| C6 | Mutual recursion `sub a{ b() } sub b{ a() }` | Both pre-defined | Both in compile-time stream; CL resolves the call at call-time → ✓ |
| C7 | Forward `\&foo` / `foo()` in a `BEGIN` *before* `sub foo` (rare) | stub makes it resolvable | Keep the stub, but mark it stub-only so C5 holds (see §"the stub subtlety") |

The key insight: **C1 and C5 do not actually conflict.** C1 is about *runtime*
calls (they happen after the whole file loads, when every sub exists). C5 is about
*compile-time* introspection (only earlier subs should exist). The current design
conflated them by hoisting subs above *both* `use`/`BEGIN` *and* runtime. Splitting
"before runtime" (keep) from "before use/BEGIN" (drop, use source order) resolves it.

## The fix, in stages

### Stage 1 — Move top-level sub *definitions* into the compile-time stream

In `Pl/Parser.pm::_process_sub_statement` (~line 5122), the file-scope branch
currently does:

```perl
if ($self->environment->in_subroutine == 0 && !%{$self->{_let_bound_vars} // {}}) {
    $self->_cur_bucket('declarations');     # <-- THE BUG
}
```

Change the target bucket from `declarations` to `definitions`. Because the parser
walks statements in source order and `_emit` appends, the sub body then lands in
`definitions` **interleaved with `use`/`BEGIN` in source order** — exactly the
compile-time stream we want.

Leave untouched:
- the `p-declare-sub` stub `unshift`ed into `declarations` (Parser.pm:5345) — keep
  it (forward refs), but see Stage 2;
- the `defvar $a/$b` and other `defvar` proclamations in `declarations`;
- the nested-named-sub branch (`is_nested_named`, already → `definitions`) and the
  let-bound in-place branch (closures) — unchanged.

After this stage, `use Foo; sub g {}` emits `use` then `g` (correct), and
`g(); ...; sub g {}` at top level still emits `g`-body in `definitions` (before
the `runtime` `g()` call) → C1 holds.

### Stage 2 — Make forward stubs invisible to introspection

`p-declare-sub` installs `(defun NAME (&rest args) nil)`, which makes `NAME`
`fboundp`. That is what still lets `make_role` "see" a sub that should not exist
yet. Two coordinated changes in `cl/pcl-runtime.lisp`:

1. The stub already records `(setf (gethash NAME *p-declared-subs*) :stub)`. Ensure
   the **real** `p-sub` installer flips that entry from `:stub` to `:defined` (or
   removes it) when it installs the body. (Check `p-sub`'s macroexpansion.)

2. Teach every *introspection* path to treat a `:stub`-only symbol as **absent**:
   - `p-stash` (the `name → code-ref` snapshot used by `keys %Pkg::`, Moo/Role::Tiny
     `_all_subs`/`_getstash`);
   - `defined &Pkg::sub` (`p-sub-defined`), `exists &Pkg::sub` (`p-sub-exists`);
   - `->can` / method dispatch enumeration if it walks the stash.

   A symbol is "really defined" iff it is `fboundp` **and** its `*p-declared-subs*`
   entry is not `:stub`. (Equivalently: real defun seen.) Runtime *calls* still go
   through `fboundp`, so a forward `foo()`/`\&foo` resolves via the stub as today.

This is the subtle half. Without it, Stage 1 alone is insufficient: even with the
real body emitted after `use`, the *stub* in `declarations` still pre-exists and
poisons introspection.

### Stage 3 — Documentation + invariant guard

- Rewrite `declaration-ordering.md`'s stale "Phase 2" / "Known Limitations"
  sections to describe the two-stream invariant above.
- Add an assertion/comment at the bucket-assembly site naming the invariant, so the
  next person who "just moves subs earlier for convenience" sees why not to.

## What does NOT change

- `defvar` proclamation ordering (C2) — still hoisted; `local`/dynamic scope tests
  in `decl-ordering-01.t` must stay green.
- BEGIN blocks still go to `definitions` and still carry the
  `p-set-current-package` first-statement fix from s253.
- `use constant` still expands to `(p-sub …)` — it now rides the compile-time
  stream in source order (which is *more* correct for
  `use constant A=>1; $x=A; use constant B=>2;`).
- Runtime forward calls (`foo(); sub foo{}`) — still work (compile-time stream
  precedes runtime stream).

## Risks and how to retire them

- **R1 — code that relied on the over-hoist** (`BEGIN { later_sub() }`). This is
  Perl-*incorrect* and will now fail like Perl. Real CPAN modules don't do it;
  the gate + a full `perl-tests` sweep confirm. If something breaks, it was
  latently wrong.
- **R2 — introspection-invisible stubs over-hide.** A sub that *is* really defined
  must remain visible. Guard: the `:stub`→`:defined` flip must be reliable; add a
  test that `defined &foo` / `keys %Pkg::` see `foo` *after* its body, and do
  *not* see it when only a stub exists.
- **R3 — C3 / late binding.** Verify empirically whether imported function symbols
  are resolved at call-time (runtime) or intern-time. If call-time (likely now),
  C3 is a non-issue and the old "sub before use" limitation can also be dropped.
  Test: `use File::Basename qw(basename); sub f { basename("/a/b") } print f();`
  and the reverse order.
- **R4 — multi-package / re-opened packages.** Each section reorders independently
  (unchanged). Verify a file that opens `package A; … package B; … package A;`.

## Test plan (lock it so it never returns)

Add to a small new `Pl/t/decl-ordering-02.t` (do **not** bloat `decl-ordering-01.t`),
differential vs real perl where possible:

1. **Moo role composition** (the motivating case): role with `sub hello` after
   `use Moo::Role`, consumed via `with` — consumer gains `hello`, `does` true.
2. **Generic introspection-at-use** (no Moo): a package whose `import` does
   `my @subs = grep …, keys %{caller()."::"}` and stores them; assert it sees only
   subs written *before* the `use`, not after. This is the Moo bug distilled to
   plain Perl — the regression canary.
3. **C1**: `foo(); sub foo { 42 }` at top level prints 42.
4. **C4**: `sub a {…}; BEGIN { a() }` — BEGIN sees the earlier sub.
5. **C5**: `BEGIN { ok( ! __PACKAGE__->can('later') ) } sub later {}` — BEGIN does
   NOT see the later sub (matches perl).
6. **C7**: `BEGIN { our $r = \&later } sub later { 7 } … $r->()` resolves to 7.
7. **C2 regression**: keep the existing `local`/dynamic-scope cases green.
8. Re-run the full `Pl/t` gate **and** a `perl-tests` sweep; compare against
   `docs/fail-baseline.tsv` with `tools/sweep-diff.pl` (R1 detector).

Acceptance: gate green, sweep shows 0 new failures (fixes welcome), and the Moo
role repro (`/tmp/moo_role.pl`) prints `Hello from Person` + `does=yes`.

## One-paragraph summary for future-me

The bug class is "PCL ran compile-time code (`use`/`BEGIN`) against the wrong set
of subs." It exists because top-level sub bodies are dumped in the `declarations`
bucket *before* the `definitions` bucket (use/BEGIN), and the forward stub makes
them `fboundp`. Fix = (1) emit top-level sub bodies into `definitions` in source
order so the compile-time stream matches Perl's, and (2) make forward stubs
invisible to introspection so `exists &/can/keys %Pkg::` only see really-defined
subs. Runtime forward calls still work because the whole compile-time stream is
assembled before the runtime stream. Don't special-case modules; hold the
two-stream invariant.
