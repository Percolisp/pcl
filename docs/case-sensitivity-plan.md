# Plan: resolving Perl/CL identifier case clashes

**Status:** plan only (2026-06-16). Targeted collision-only fix shipped s252; the
general fix below is deferred to the compiler rewrite. See
`memory/project_case_sensitivity_general_fix.md` for the s252 details.

## The problem, precisely

The Perl-name → CL-symbol mapping happens in the **CL reader** when SBCL reads
the generated `.lisp`. Default `readtable-case` is `:upcase`, so `$base_len` and
`$BASE_LEN` both read as one symbol `$BASE_LEN` — the mapping is non-injective.

Collisions are only possible **within one CL package** (each Perl package = its
own CL package via `(p-defpackage :|Pkg|)` + `in-package`), which is why they're
rare in practice — but real: `Math::BigInt::Calc` has file-`my $BASE_LEN` + a
lexical param `my $base_len`, and the fold broke `_base_len` (was the pack.t
TIMEOUT root cause).

There are exactly two structural ways to make the mapping injective.

## What the codebase already does (the key context)

- **Multi-segment packages already preserve case via bar-escape:** `package
  Foo::Bar` → `:|Foo::Bar|`. Bar-escape is proven in production for one
  identifier class. (Single-segment `Foo` → `:FOO`, with orig case carried
  out-of-band.)
- `sub FOO` emits `pl-FOO`; `sub foo` emits `pl-foo`. Note `pl-FOO` is a
  **mixed-case** token.
- Class names (`plc-Foo`), method names, and `ref()` output go through an
  **out-of-band orig-case string table** (`p-register-pkg-name … "Foo::Bar"`),
  so user-visible strings are already decoupled from CL symbol case.
- The s252 targeted rename (`Pl/Parser.pm::_compute_and_apply_case_renames` +
  `ExprToCL::_case_renamed`) handles real **variable** collisions by renaming
  all-but-one colliding spelling to `<name>__pcl_ci_N`. **Subs are NOT
  disambiguated.**

The `p-`/`pl-`/`plc-` prefix discipline turns every upper-case Perl name into a
**mixed-case** CL token — which matters a lot for approach (b).

## Approach (a): explicit `cl:intern` / bar-escape

**Mechanism.** Route every user identifier through one transform `M(name)` and
emit it case-preserving in source — in practice `|name|` (bar-escape), the
read-syntax equivalent of `(intern "name")`. Wherever the *runtime* builds a
symbol from a Perl name string (method dispatch, `*glob` install, `can`, stash),
call `(intern (M name) pkg)` with the **same** `M`. `M` can be identity (exact
case) since `||` preserves case.

```
$base_len      → |$base_len|        $BASE_LEN → |$BASE_LEN|     distinct
sub foo / FOO  → |pl-foo| / |pl-FOO|                            distinct
```

**Pros**
- Surgical: only *our* user-identifier emissions change. Standard CL forms
  (`defun`, `let`, `car`) stay bare and read normally. No global mode switch.
- No interaction with cl-ppcre/ASDF load; FASLs unaffected.
- Already proven for packages.

**Cons**
- **Pervasive across codegen**: every site that emits a var/sub/class/filehandle
  symbol must wrap in `||`, *unconditionally* (conditional = today's targeted
  rename, which can't see cross-file collisions).
- **Large test churn**: hundreds of `Pl/t/transpile-test-*.t` string-match
  assertions match `\$base_len`, `pl-foo`, etc. All would need `||`.
- Readability cost: `(box-set |$base_len| 1)`.
- Requires a crisp "user identifier vs PCL-internal symbol" boundary at every
  emission site, maintained forever.

## Approach (b): change the reader to not upcase — `:invert`

**Mechanism.** Read PCL's own source (runtime + generated + test lib) under
`(setf (readtable-case *readtable*) :invert)`. Per CLHS, `:invert` flips the
case of a token **only if all its letters are the same case**; mixed-case tokens
are left untouched:

```
foo  → FOO        (lower → upper; standard CL still works)
FOO  → foo        (upper → lower)
Foo  → Foo        (mixed, preserved)
```

Leveraging the existing prefixes:

| Perl            | emitted token | case  | reads to            |              |
|-----------------|---------------|-------|---------------------|--------------|
| `my $base_len`  | `$base_len`   | lower | `$BASE_LEN`         |              |
| `my $BASE_LEN`  | `$BASE_LEN`   | upper | `$base_len`         | distinct     |
| `sub foo`       | `pl-foo`      | lower | `PL-FOO`            |              |
| `sub FOO`       | `pl-FOO`      | mixed | `pl-FOO` (preserved)| distinct — **free, fixes subs** |
| builtin         | `p-print`     | lower | `P-PRINT`           | matches runtime |

**Crucial insight:** the `p-`/`pl-`/`plc-` prefix turns every upper-case Perl
name into a *mixed-case* CL token, which `:invert` preserves. So `:invert` fixes
**subs and classes for free**, with no rename machinery — exactly the gap the
s252 targeted fix left open.

**Pros**
- **Zero per-site codegen change**, zero test churn on the common (lower-case)
  path. A configuration + audit task, concentrated in one place rather than
  smeared across the codebase.
- Fixes subs/classes automatically.
- Composes with the compiler rewrite instead of fighting it.

**Cons / risk (the real work is here)**
- It's a **global mode**. Every all-uppercase or mixed-case symbol *shared
  between runtime and generated code* must be spelled identically on both sides
  (they mostly already are — codegen emits `STDERR`, runtime must too, both
  invert to `stderr`). Needs a **full audit** of `cl/pcl-runtime.lisp` +
  `cl/pcl-test.lisp` for all-caps/mixed symbols (constants like `+FOO+`,
  exported keywords).
- **Load orchestration**: cl-ppcre/ASDF must load *first* under the standard
  readtable; `:invert` must be bound only while reading PCL files. Every entry
  point (`pl2cl` load + FASL compile, `runpl`, `cl/test-runtime.lisp`, the `pcl`
  runner) needs the scoped binding.
- `:invert` is bijective and round-trips on print, and `ref()`/method-name
  strings are already orig-case out-of-band, so **user-visible output is
  unaffected**.

## Recommendation

**On "how": (b) `:invert` is the better target.** The prefix discipline already
does 90% of the work; `:invert` is the natural completion of it, fixes
subs/classes (the open gap) for free, and its cost is a bounded one-time audit
rather than permanent per-site overhead and test churn. Approach (a)/bar-escape
is the right tool for the *narrow* cases where you can't prefix — which is
exactly why packages already use it.

**On "if": defer the general fix.** The targeted s252 rename covers every real
collision observed. Sub/class case-collisions and cross-file same-package
globals have produced **zero** real CPAN failures to date. Don't pay the
`:invert` audit cost until either (1) a real module breaks on a sub/class case
collision, or (2) you're already in the compiler rewrite, where the reader-setup
audit is cheap to fold in and there's a single identifier→symbol chokepoint.

## If/when green-lit — phased implementation of (b)

1. **Audit** — script over `cl/*.lisp` (+ generated samples) listing every
   symbol with all-caps or mixed case; classify each as runtime-internal (must
   agree across sides) vs incidental. Output a normalization punch-list.
2. **Reader plumbing** — create one invert-readtable; load 3rd-party libs under
   the standard readtable first, then `(let ((*readtable* …invert…)) (load
   runtime/generated))`. Wire into all four entry points + the FASL compile step.
3. **Normalize** the punch-list so codegen and runtime spell shared
   all-caps/mixed symbols identically.
4. **Gate** — run `prove -j8 Pl/t/` (6+ min) and a full sweep; re-bless the fail
   baseline only from a clean sweep. Retire `_compute_and_apply_case_renames`
   and the `__pcl_ci_N` machinery once green.
5. **Regression test** in `Pl/t/misc-fixes-02.t`: `$base_len`/`$BASE_LEN` **and**
   `sub foo`/`sub FOO` both round-trip.
